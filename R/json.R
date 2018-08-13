json2script = function(json)
{
  lang = json$code$language
  script = json$code$source
  # Todo: Source the string `script` directly without writing it to disk
  # Note: Need to remove escape characters in `script`
  sink(file = "tmp_udf.R")
  cat(script)
  sink()
  print(Sys.time())
  cat("Extracted script for JSON!\n")
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
  print(Sys.time())
  cat("Started converting JSON to stars...\n")
  proj_string = json$data$proj
  num_bands = length(json$data$raster_collection_tiles)
  num_time = length(json$data$raster_collection_tiles[[1]]$start_times)
  num_bands = 3 #Testing
  num_time = 2 #Testing

  bt_list = list()
  length(bt_list) = num_time

  timestamps = strptime(json$data$raster_collection_tiles[[1]]$start_times, format = "%Y-%m-%dT%T", tz = "Europe/Berlin")
  timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1]) #Need to start an issue in `stars`

  # #Todo: Get rid of the nested for loops below and in `tile2raster()`
  # #Todo: Think about using dataframes/martices
  # for(band_num in 1:num_bands)
  # {
  #   cat(paste(Sys.time(), "; Processing Band: ", band_num, "; ", sep = ""))
  #   tile = json$data$raster_collection_tiles[[band_num]]
  #   for(time_num in 1:num_time)
  #   {
  #     cat(paste(Sys.time(), "; Time: ", time_num, "...\n", sep = ""))
  #     bt_list[[time_num]] = c(bt_list[[time_num]], tile2raster(tile, time_num, proj_string))
  #   }
  # }

  on_bands = function(band_num, raster_collection, time_num, proj_string)
  {
    print(paste(Sys.time(), "; Processing Band: ", band_num, "; ", sep = ""))
    tile = raster_collection[[band_num]]
    tile2raster(tile = tile, time_num = time_num, proj = proj_string)
  }

  on_times = function(time_num, raster_collection, proj_string)
  {
    print(paste(Sys.time(), "; Time: ", time_num, "...\n", sep = ""))
    b_list = lapply(
      X = as.list(1:num_bands),
      FUN = on_bands,
      raster_collection,
      time_num,
      proj_string
    )
    b_list
  }

  raster_collection = json$data$raster_collection_tiles
  bt_list = lapply(X = as.list(1:num_time), FUN = on_times, raster_collection, proj_string)

  print(Sys.time())
  cat("Finished converting JSON to Raster objects!\n")

  stars_obj = NULL
  print(Sys.time())
  cat("Starting to convert Raster to stars objects...\n")
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
  print(Sys.time())
  cat("Converted JSON to stars object!\n")
  stars_obj
}

run_script = function(stars_obj, dim_mod, script_file = "tmp_udf.R")
{
  print(Sys.time())
  cat("Started executing UDF on stars object...\n")
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
    function_name = source(script_file)$value
    result = st_apply(stars_obj, FUN = function_name, MARGIN = all_dim[-c(dim_mod)])
    new_dim = all_dim
    new_dim[dim_mod] = NA
  } else
    stop("Script file is unavailable!")
  print(Sys.time())
  cat("Applied UDF on stars object!\n")
  result
}

stars2json = function(stars_obj, json_in)#, json_out_file = "udf_response.json")
{
  print(Sys.time())
  cat("Started converting stars object to JSON...\n")
  json_out = json_in[["data"]] #Copying structure of JSON but only the element "data"
  # json_out$code = list()
  json_out$proj = attr(stars_obj, "dimensions")$x$refsys
  tot_bands = as.numeric(dim(stars_obj)["band"])
  if(!is.na(tot_bands))
  {
    json_out$raster_collection_tiles = json_out$raster_collection_tiles[-c(tot_bands+1:length(json_out$raster_collection_tiles))]
    for(bands in 1:tot_bands)
    {
      cat(paste(Sys.time(), "; Band: ", bands, "; ", sep = ""))
      tmp_extent = json_out$raster_collection_tiles[[bands]][["extent"]]
      # Need another robust way to loop over bands & time since using `attr()` in the manner
      # below will not work for stars objects with arbitrary dimensions
      if("time" %in% dimnames(stars_obj))
      {
        delta_x = attr(stars_obj[,,,bands,], "dimensions")$x$delta
        delta_y = attr(stars_obj[,,,bands,], "dimensions")$y$delta
        x1 = attr(stars_obj[,,,bands,], "dimensions")$x$offset
        x2 = attr(stars_obj[,,,bands,], "dimensions")$x$offset + delta_x * attr(stars_obj[,,,bands,], "dimensions")$x$to
        y1 = attr(stars_obj[,,,bands,], "dimensions")$y$offset
        y2 = attr(stars_obj[,,,bands,], "dimensions")$y$offset + delta_y * attr(stars_obj[,,,bands,], "dimensions")$y$to
      } else
      {
        delta_x = attr(stars_obj[,,,bands], "dimensions")$x$delta
        delta_y = attr(stars_obj[,,,bands], "dimensions")$y$delta
        x1 = attr(stars_obj[,,,bands], "dimensions")$x$offset
        x2 = attr(stars_obj[,,,bands], "dimensions")$x$offset + delta_x * attr(stars_obj[,,,bands], "dimensions")$x$to
        y1 = attr(stars_obj[,,,bands], "dimensions")$y$offset
        y2 = attr(stars_obj[,,,bands], "dimensions")$y$offset + delta_y * attr(stars_obj[,,,bands], "dimensions")$y$to
      }
      tmp_extent = list(north = max(y1,y2), south = min(y1,y2), west = min(x1,x2), east = max(x1,x2), height = if(sign(delta_y) < 0) -1 * delta_y else delta_y, width = if(sign(delta_x) < 0) -1 * delta_x else delta_x)
      json_out$raster_collection_tiles[[bands]]$extent = tmp_extent

      times = as.numeric(dim(stars_obj)["time"])
      if(!is.na(times))
      {
        t_start = seq(from = attr(stars_obj[,,,bands,], "dimensions")$time$offset, by = attr(stars_obj[,,,bands,], "dimensions")$time$delta, length.out = times)
        t_end = c(t_start[2:length(t_start)], t_start[length(t_start)] + attr(stars_obj[,,,bands,], "dimensions")$time$delta)
        json_out$raster_collection_tiles[[bands]]$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
        json_out$raster_collection_tiles[[bands]]$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))
      } else
      {
        t_start = NA
        t_end = NA
        json_out$raster_collection_tiles[[bands]]$start_times = as.list(NA)
        json_out$raster_collection_tiles[[bands]]$end_times = as.list(NA)
      }
      data = list()
      # if(!is.na(times)) length(data) = times else length(data) = 1
      if(is.na(times))
      {
        print(Sys.time())
        cat("Time: 1...\n")
        bt_raster = as(stars_obj[,,,bands, drop = TRUE], "Raster")
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
      } else
      {
        for(t in 1:times)
        {
          cat(paste(Sys.time(), "; Time: ", t, "...\n", sep = ""))
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
      }
      json_out$raster_collection_tiles[[bands]]$data = data
    }
  } else
  {
    print(Sys.time())
    cat("Band: 1; ")
    json_out$raster_collection_tiles = json_out$raster_collection_tiles[-c(2:length(json_out$raster_collection_tiles))]
    tmp_extent = json_out$raster_collection_tiles[[1]][["extent"]]
    if("time" %in% dimnames(stars_obj))
    {
      delta_x = attr(stars_obj[,,,], "dimensions")$x$delta
      delta_y = attr(stars_obj[,,,], "dimensions")$y$delta
      x1 = attr(stars_obj[,,,], "dimensions")$x$offset
      x2 = attr(stars_obj[,,,], "dimensions")$x$offset + delta_x * attr(stars_obj[,,,], "dimensions")$x$to
      y1 = attr(stars_obj[,,,], "dimensions")$y$offset
      y2 = attr(stars_obj[,,,], "dimensions")$y$offset + delta_y * attr(stars_obj[,,,], "dimensions")$y$to
    } else
    {
      delta_x = attr(stars_obj[,,], "dimensions")$x$delta
      delta_y = attr(stars_obj[,,], "dimensions")$y$delta
      x1 = attr(stars_obj[,,], "dimensions")$x$offset
      x2 = attr(stars_obj[,,], "dimensions")$x$offset + delta_x * attr(stars_obj[,,], "dimensions")$x$to
      y1 = attr(stars_obj[,,], "dimensions")$y$offset
      y2 = attr(stars_obj[,,], "dimensions")$y$offset + delta_y * attr(stars_obj[,,], "dimensions")$y$to
    }
    tmp_extent = list(north = max(y1,y2), south = min(y1,y2), west = min(x1,x2), east = max(x1,x2), height = if(sign(delta_y) < 0) -1 * delta_y else delta_y, width = if(sign(delta_x) < 0) -1 * delta_x else delta_x)
    json_out$raster_collection_tiles[[1]]$extent = tmp_extent

    times = as.numeric(dim(stars_obj)["time"])
    if(!is.na(times))
    {
      t_start = seq(from = attr(stars_obj[,,,], "dimensions")$time$offset, by = attr(stars_obj[,,,], "dimensions")$time$delta, length.out = times)
      t_end = c(t_start[2:length(t_start)], t_start[length(t_start)] + attr(stars_obj[,,,], "dimensions")$time$delta)
      json_out$raster_collection_tiles[[1]]$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
      json_out$raster_collection_tiles[[1]]$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))
    } else
    {
      t_start = NA
      t_end = NA
      json_out$raster_collection_tiles[[1]]$start_times = as.list(NA)
      json_out$raster_collection_tiles[[1]]$end_times = as.list(NA)
    }
    data = list()
    if(is.na(times))
    {
      print(Sys.time())
      cat("Time: 1...\n")
      bt_raster = as(stars_obj[,,], "Raster")
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
    } else
    {
      for(t in 1:times)
      {
        cat(paste(Sys.time(), "; Time: ", t, "...\n", sep = ""))
        bt_raster = as(stars_obj[,,,t, drop = TRUE], "Raster")
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
    }
    json_out$raster_collection_tiles[[1]]$data = data
  }
  # For writing to disk
  # write_json(x = json_out, path = json_out_file, auto_unbox = TRUE, pretty = TRUE)
  # json_response = toJSON(x = json_out, auto_unbox = TRUE, pretty = TRUE)
  print(Sys.time())
  cat("Converted resulting stars object back to JSON!\n")
  # json_response
  json_out
}

json2fname = function(json_in)
  return(json_in$code$fname)

json2dim_mod = function(json_in)
  return(json_in$code$dim_mod)

#' @serializer unboxedJSON
#' @post /udf
run_UDF.json = function(req)
{
  print(Sys.time())
  cat("Started executing at endpoint /udf\n")
  json_in = fromJSON(req$postBody, simplifyVector = FALSE)
  json2script(json_in)

  # udf_func = json2fname(json_in)
  # dim_mod = json2dim_mod(json_in)
  # udf_func = "median" #Testing
  dim_mod = 4         #Testing

  stars_in = json2stars(json_in)
  stars_out = run_script(stars_obj = stars_in, dim_mod = dim_mod)
  json_out = stars2json(stars_obj = stars_out, json_in = json_in)#, json_out_file = "udf_response.json")

  # Generate HTTP response for "backend" with body as the JSON in the file `json_out_file`
  print(Sys.time())
  cat("Generating response to HTTP request")
  json_out
}
