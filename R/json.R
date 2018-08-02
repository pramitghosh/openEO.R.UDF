json2script = function(json)
{
  lang = json$code$language
  script = json$code$source
  sink(paste("tmp_udf", "R", sep = "."))
  cat(script)
  sink()
}

tile2raster = function(tile, time_num, proj)
{
  xmin = tile$extent$west
  xmax = tile$extent$east
  ymin = tile$extent$south
  ymax = tile$extent$north
  resx = tile$extent$width
  resy = tile$extent$height

  xtot = length(tile$data[[1]])
  ytot = length(tile$data[[1]][[1]])

  xyz = matrix(ncol = 3, nrow = xtot * ytot)
  xyz = as.data.frame(xyz)
  colnames(xyz) = c("x", "y","z")

  i = 0
  x = xmin + resx/2
  while(x < xmax)
  {
    i = i + 1
    j = 0
    y = ymin + resy/2
    while(y < ymax)
    {
      j = j + 1
      xyz$x[(i - 1) * ytot + j] = x
      xyz$y[(i - 1) * ytot + j] = y
      xyz$z[(i - 1) * ytot + j] = tile$data[[time_num]][[i]][[j]]
      y = y + resy
    }
    x = x + resx
  }
  r = rasterFromXYZ(xyz, crs = proj)
  r
}

json2stars = function(json)
{
  proj_string = json$data$proj
  num_bands = length(json$data$raster_collection_tiles)
  num_time = length(json$data$raster_collection_tiles[[1]]$start_times)

  bt_list = list()
  length(bt_list) = num_time

  timestamps = strptime(json$data$raster_collection_tiles[[1]]$start_times, format = "%Y-%m-%dT%T", tz = "Europe/Berlin")
  timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1]) #Need to start an issue in `stars`
  for(band_num in 1:num_bands)
  {
    tile = json$data$raster_collection_tiles[[band_num]]
    for(time_num in 1:num_time)
    {
      bt_list[[time_num]] = c(bt_list[[time_num]], tile2raster(tile, time_num, proj_string))
    }
  }

  stars_obj = NULL
  for(time_num in 1:num_time)
  {
    as_stars = lapply(X = bt_list[[time_num]], FUN = st_as_stars)
    # as_stars = c(as_stars[[1]], as_stars[[2]], along = "band") #Vectorized implementation of `stars.c()` not working!
    stars_bands = as_stars[[1]]
    if(length(as_stars > 1))
      for(times in 2:length(as_stars))
      {
        stars_bands = c(stars_bands, as_stars[[times]], along = "band")
      }
    if(is.null(stars_obj))
    {
      stars_obj = c(stars_bands, dim_name = "time", values = timestamps_padded[time_num:time_num+1])
      attr(stars_obj, "dimensions")[["time"]]$offset = timestamps[1]
      attr(stars_obj, "dimensions")[["time"]]$delta = timestamps[time_num+1] - timestamps[time_num]
    } else
    {
      tmp_stars = c(stars_bands, dim_name = "time", values = timestamps_padded[time_num:time_num+1])
      attr(tmp_stars, "dimensions")[["time"]]$offset = timestamps[time_num]
      stars_obj = c(stars_obj, tmp_stars)
      # Fixing time of final `stars` object manually
      attr(stars_obj, "dimensions")[["time"]]$to = dim(stars_obj)[["time"]]
      attr(stars_obj, "dimensions")[["time"]]$delta = mean(diff(timestamps))
    }
  }
  stars_obj
}

run_script = function(stars_obj, dim_mod, function_name, script_file = "./tmp_udf.R")
{
  # dim_mod = 1, 2 means space
  # dim_mod = 3 means band
  # dim_mod = 4 means time
  # dim_mod = 5 means whether raster or feature (default: raster)
  in_dim = dim(stars_obj)
  all_dim = 1:4
  if("x" %in% names(in_dim) && "y" %in% names(in_dim))
    all_dim[1] = 1 else
      all_dim[1] = NA
  if("band" %in% names(in_dim))
    all_dim[2] = 2 else
      all_dim[2] = NA
  if("time" %in% names(in_dim))
    all_dim[3] = 3 else
      all_dim[3] = NA
  all_dim[4] = 4 #Currently assuming `stars_obj` has rasters

  if(file.exists(script_file))
  {
    source(script_file)
    result = st_apply(stars_obj, FUN = function_name, MARGIN = all_dim[-c(dim_mod)])
    new_dim = all_dim
    new_dim[dim_mod] = NA
  } else
    stop("Script file is unavailable!")

  result
}

stars2json = function(stars_obj, json_in)
{
  json_out = json_in #Copying structure of JSON
  json_out$code = list()
  json_out$data$proj = attr(stars_obj, "dimensions")$x$refsys
  tot_bands = as.numeric(dim(stars_obj)["band"])
  json_out$data$raster_collection_tiles = json_out$data$raster_collection_tiles[-c(tot_bands+1:length(json_out$data$raster_collection_tiles))]
  for(bands in 1:tot_bands)
  {
    tmp_extent = json_out$data$raster_collection_tiles[[bands]][["extent"]]
    # Need another robust way to loop over bands & time since using `attr()` in the manner
    # below will not work for stars objects with arbitrary dimensions
    delta_x = attr(stars_obj[,,,bands,], "dimensions")$x$delta
    delta_y = attr(stars_obj[,,,bands,], "dimensions")$y$delta
    x1 = attr(stars_obj[,,,bands,], "dimensions")$x$offset
    x2 = attr(stars_obj[,,,bands,], "dimensions")$x$offset + delta_x * attr(stars_obj[,,,bands,], "dimensions")$x$to
    y1 = attr(stars_obj[,,,bands,], "dimensions")$y$offset
    y2 = attr(stars_obj[,,,bands,], "dimensions")$y$offset + delta_y * attr(stars_obj[,,,bands,], "dimensions")$y$to
    tmp_extent = list(north = max(y1,y2), south = min(y1,y2), west = min(x1,x2), east = max(x1,x2), height = if(sign(delta_y) < 0) -1 * delta_y else delta_y, width = if(sign(delta_x) < 0) -1 * delta_x else delta_x)
    json_out$data$raster_collection_tiles[[bands]]$extent = tmp_extent

    times = as.numeric(dim(stars_obj)["time"])
    t_start = seq(from = attr(stars_obj[,,,bands,], "dimensions")$time$offset, by = attr(stars_obj[,,,bands,], "dimensions")$time$delta, length.out = times)
    t_end = c(t_start[2:length(t_start)], t_start[length(t_start)] + attr(stars_obj[,,,bands,], "dimensions")$time$delta)
    json_out$data$raster_collection_tiles[[bands]]$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
    json_out$data$raster_collection_tiles[[bands]]$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))

    data = list()
    # length(data) = times
    for(t in 1:times)
    {
      bt_raster = as(stars_obj[,,,bands,t, drop = TRUE], "Raster")
      bt_df = as.data.frame(bt_raster, xy = TRUE)
      uy = unique(bt_df[,2])
      y_list = list()
      # length(y_list) = length(uy)
      for(ys in uy)
      {
        ux = as.list(bt_df$x[bt_df$y == ys])
        x_list = list()
        for(xs in ux)
          x_list = as.list(as.numeric(subset(bt_df, subset = bt_df$y == ys, select = "layer")[[1]]))

        y_list = c(y_list, list(x_list))
      }
      data = c(data, list(y_list))
    }
    json_out$data$raster_collection_tiles[[bands]]$data = data
  }
  # For writing to disk
  # write_json(x = json_out, path = "udf_response.json", auto_unbox = TRUE, pretty = TRUE)
  json_response = toJSON(x = json_out, auto_unbox = TRUE, pretty = TRUE)
}
