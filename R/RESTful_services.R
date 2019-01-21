
# =============================================================
# RESTful web service with data as JSON arrays
# =============================================================

json2script = function(json)
{
  lang = json$code$language
  script = json$code$source
  script = gsub("\"", "'", script)
  script = gsub("\r", "", script)
  # Todo: Source the string `script` directly without writing it to disk
  # Note: Need to remove escape characters in `script`
  # sink(file = "tmp_udf.R")
  # cat(script)
  # sink()
  print(Sys.time())
  cat("Extracted script for JSON!\n")
  script
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
  y = ymin + resy/2
  xyz$x = rep(seq(from = x, to = xmax - resx/2, by = resx), each = ytot)
  xyz$y = rep(seq(from = y, to = ymax - resy/2, by = resy), xtot)
  xyz$z = rapply(object = tile$data[[time_num]], f = unlist)

  rasterFromXYZ(xyz, crs = proj)
}

json2stars = function(json)
{
  print(Sys.time())
  cat("Started converting JSON to stars...\n")
  proj_string = json$data$proj
  num_bands = length(json$data$raster_collection_tiles)
  num_time = length(json$data$raster_collection_tiles[[1]]$start_times)
  # num_bands = 3 #Testing
  # num_time = 2 #Testing

  bt_list = list()
  length(bt_list) = num_time

  timestamps = strptime(json$data$raster_collection_tiles[[1]]$start_times, format = "%Y-%m-%dT%T", tz = "Europe/Berlin")
  timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1]) #Need to start an issue in `stars`

  # #Todo: Get rid of the nested for loops below and in `tile2raster()`
  # #Todo: Think about using dataframes/martices

  on_bands = function(band_num, raster_collection, time_num, proj_string)
  {
    cat(paste("\n", Sys.time(), "; Processing Band: ", band_num, "; ", sep = ""))
    tile = raster_collection[[band_num]]
    tile2raster(tile = tile, time_num = time_num, proj = proj_string)
  }

  on_times = function(time_num, raster_collection, proj_string)
  {
    cat(paste("\n", Sys.time(), "; Time: ", time_num, "...", sep = ""))
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
  cat("\n")
  print(Sys.time())
  cat("Finished converting JSON to Raster objects!\n\n")

  stars_obj = NULL
  print(Sys.time())
  cat("Starting to convert Raster to stars objects...\n")
  stars_bands = list()
  for(time_num in 1:num_time)
  {
    as_stars = lapply(X = bt_list[[time_num]], FUN = st_as_stars)

    # stars_bands = as_stars[[1]]
    # if(length(as_stars > 1))
    #   for(times in 2:length(as_stars))
    #   {
    #     stars_bands = c(stars_bands, as_stars[[times]], along = "band")
    #   }
    append_stars = append(as_stars, values = c(along = "band"))
    stars_bands[[time_num]] = do.call(c, append_stars)
  }
  stars_obj = append(stars_bands, values = c(along = "time"))
  stars_obj = do.call(c, stars_obj)
  attr(stars_obj, "dimensions")$time$values = timestamps
  attr(stars_obj, "dimensions")$time$offset = timestamps[1]
  attr(stars_obj, "dimensions")$time$delta = mean(diff(timestamps))

    # if(is.null(stars_obj))
    # {
    #   stars_obj = try(c(stars_bands, along = list("time" = timestamps_padded[time_num:time_num+1])), silent = T)
    #   if(class(stars_obj) == "try-error")
    #     stars_obj = c(stars_bands, dim_name = "time", values = timestamps_padded[time_num:time_num+1])
    #
    #   attr(stars_obj, "dimensions")[["time"]]$offset = timestamps[1]
    #   attr(stars_obj, "dimensions")[["time"]]$delta = timestamps[time_num+1] - timestamps[time_num]
    # } else
    # {
    #   tmp_stars = try(c(stars_bands, along = list("time" = timestamps_padded[time_num:time_num+1])), silent = T)
    #   if(class(tmp_stars) == "try-error")
    #     tmp_stars = c(stars_bands, dim_name = "time", values = timestamps_padded[time_num:time_num+1])
    #
    #   attr(tmp_stars, "dimensions")[["time"]]$offset = timestamps[time_num]
    #   stars_obj = c(stars_obj, tmp_stars)
    #   # Fixing time of final `stars` object manually
    #   attr(stars_obj, "dimensions")[["time"]]$to = dim(stars_obj)[["time"]]
    #   attr(stars_obj, "dimensions")[["time"]]$delta = mean(diff(timestamps))
    # }
  # }
  print(Sys.time())
  cat("Converted JSON to stars object!\n\n")
  stars_obj
}

run_script = function(stars_obj, dim_mod, script_text)
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
  parsed_script = parse(text = script_text)
  if(is.expression(parsed_script))
  {
    function_name = eval(parsed_script)
    result = st_apply(stars_obj, FUN = function_name, MARGIN = all_dim[-c(dim_mod)])
    new_dim = all_dim
    new_dim[dim_mod] = NA
  } else
    stop("Script text is unavailable or is not a valid expression!")
  print(Sys.time())
  cat("Applied UDF on stars object!\n\n")
  result
}

run_script_raw = function(stars_obj, script_text)
{
  parsed_script = parse(text = script_text)
  if(is.expression(parsed_script))
  {
    function_name = eval(parsed_script)
    result = function_name(stars_obj)
  } else stop("Script text is unavailable or is not a valid expression!")
  if(class(result) == "stars")
    return(result) else return(stars_obj)
}

stars2json = function(stars_obj, json_in)#, json_out_file = "udf_response.json")
{
  print(Sys.time())
  cat("Started converting stars object to JSON...\n\n")
  json_out = json_in[["data"]] #Copying structure of JSON but only the element "data"
  # json_out$code = list()
  json_out$proj = attr(stars_obj, "dimensions")$x$refsys
  tot_bands = as.numeric(dim(stars_obj)["band"])

  calc_extent = function(stars_obj, bands)
  {
    if(!is.na(bands))
    {
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
    } else
    {
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
    }
    list(north = max(y1,y2), south = min(y1,y2), west = min(x1,x2), east = max(x1,x2), height = if(sign(delta_y) < 0) -1 * delta_y else delta_y, width = if(sign(delta_x) < 0) -1 * delta_x else delta_x)
  }

  calc_y = function(ys, bt_df)
  {
    as.list(as.numeric(subset(bt_df, subset = bt_df$y == ys, select = "layer")[[1]]))
  }

  calc_data = function(t, bands, stars_obj)
  {
    cat(paste(Sys.time(), "; Time: ", if(is.na(t)) 1 else t, "...\n", sep = ""))
    bt_raster = if(!is.na(t))
                    as(if(!is.na(bands))
                      stars_obj[,,,bands,t, drop = TRUE] else
                      stars_obj[,,,t, drop = TRUE], "Raster") else
                    as(if(!is.na(bands))
                      stars_obj[,,,bands, drop = TRUE] else
                      stars_obj[,,], "Raster")

    bt_df = as.data.frame(bt_raster, xy = TRUE)
    uy = as.list(unique(bt_df[, 2]))
    y_list = lapply(uy, calc_y, bt_df)
  }

  if(!is.na(tot_bands))
  {
    length(json_out$raster_collection_tiles) = tot_bands
    for(bands in 1:tot_bands)
    {
      cat(paste(Sys.time(), "; Processing Band: ", bands, "...\n", sep = ""))
      json_out$raster_collection_tiles[[bands]]$extent = calc_extent(stars_obj, bands)

      times = as.numeric(dim(stars_obj)["time"])
      if(!is.na(times))
      {
        t_start = seq(from = attr(stars_obj[,,,bands,], "dimensions")$time$offset, by = attr(stars_obj[,,,bands,], "dimensions")$time$delta, length.out = times)
        t_end = c(t_start[2:length(t_start)], t_start[length(t_start)] + attr(stars_obj[,,,bands,], "dimensions")$time$delta)
        json_out$raster_collection_tiles[[bands]]$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
        json_out$raster_collection_tiles[[bands]]$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))
        data = lapply(as.list(1:times), calc_data, bands, stars_obj)
      } else
      {
        t_start = NA
        t_end = NA
        json_out$raster_collection_tiles[[bands]]$start_times = as.list(NA)
        json_out$raster_collection_tiles[[bands]]$end_times = as.list(NA)
        data = lapply(as.list(NA), calc_data, bands, stars_obj)
      }
      json_out$raster_collection_tiles[[bands]]$data = data
    }
  } else
  {
    cat(paste(Sys.time(), "; Processing Band: 1;\n", sep = ""))
    length(json_out$raster_collection_tiles) = 1
    json_out$raster_collection_tiles[[1]]$extent = calc_extent(stars_obj, NA)
    bands = NA
    times = as.numeric(dim(stars_obj)["time"])
    if(!is.na(times))
    {
      t_start = seq(from = attr(stars_obj[,,,], "dimensions")$time$offset, by = attr(stars_obj[,,,], "dimensions")$time$delta, length.out = times)
      t_end = c(t_start[2:length(t_start)], t_start[length(t_start)] + attr(stars_obj[,,,], "dimensions")$time$delta)
      json_out$raster_collection_tiles[[1]]$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
      json_out$raster_collection_tiles[[1]]$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))
      data = lapply(as.list(1:times), calc_data, bands, stars_obj)
    } else
    {
      t_start = NA
      t_end = NA
      json_out$raster_collection_tiles[[1]]$start_times = as.list(NA)
      json_out$raster_collection_tiles[[1]]$end_times = as.list(NA)
      data = lapply(as.list(NA), calc_data, bands, stars_obj)
    }
    json_out$raster_collection_tiles[[1]]$data = data
  }
  # For writing to disk
  # write_json(x = json_out, path = json_out_file, auto_unbox = TRUE, pretty = TRUE)
  # json_response = toJSON(x = json_out, auto_unbox = TRUE, pretty = TRUE)
  cat("\n")
  print(Sys.time())
  cat("Converted resulting stars object back to JSON!\n")
  # json_response
  json_out
}

json2dim_mod = function(json_dim)
{
  dim_num = NA
  if(json_dim == "band") dim_num = 3
  if(json_dim == "time") dim_num = 4
  dim_num
}

#' @serializer unboxedJSON
#' @post /udf
run_UDF.json = function(req)
{
  cat(paste("\n", Sys.time(), "\n", sep = ""))
  cat("Started executing at endpoint /udf\n")
  json_in = fromJSON(req$postBody, simplifyVector = FALSE)
  script_text = json2script(json_in)

  # dim_mod = apply(as.array(json_in$code$dim_mod), 1, json2dim_mod)
  dim_mod = try(json2dim_mod(json_in$code$dim_mod), silent = T)
  if(class(dim_mod) == "try-error")
  {
    dim_mod = 4 # Testing
    cat(paste(Sys.time(), " Dimension to be modified set to: time (by default)\n", sep = ";"))
  } else
    cat(paste(Sys.time(), " Dimension set by the backend!\n", sep = ";"))

  stars_in = json2stars(json_in)
  stars_out = run_script(stars_obj = stars_in, dim_mod = dim_mod, script_text = script_text)
  json_out = stars2json(stars_obj = stars_out, json_in = json_in)

  # Generate HTTP response for "backend" with body as the JSON in the file `json_out_file`
  print(Sys.time())
  cat("Generating response to HTTP request")
  json_out
}

#' @serializer unboxedJSON
#' @post /udf/raw
run_UDF.json.raw = function(req)
{
  cat("\n")
  json_in = fromJSON(req$postBody, simplifyVector = FALSE)
  script_text = json2script(json_in)

  stars_in = json2stars(json_in)
  stars_out = run_script_raw(stars_obj = stars_in, script_text = script_text)
  json_out = stars2json(stars_obj = stars_out, json_in = json_in)

  #Generate HTTP response for "backend"
  print(Sys.time())
  cat("Generating resposne to HTTP request")
  json_out
}

# Florian's subsetting hack re-introduced
# this is just a temporal fix for an issue during subsetting stars objects with an variable that was defined not in the basenv environment
"[.stars" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
  missing.i = missing(i)
  # special case:
  if (! missing.i && inherits(i, c("sf", "sfc", "bbox")))
    return(st_crop(x, i, crop = crop))

  mc <- match.call(expand.dots = TRUE)
  # select list elements from x, based on i:
  d = attr(x, "dimensions")
  ed = stars:::expand_dimensions.dimensions(d)
  x = unclass(x)[i]
  # selects also on dimensions:
  if (length(mc) > 3) {
    mc[[1]] <- `[`
    if (! missing(i))
      mc[[3]] <- NULL # remove i
    mc[["drop"]] = FALSE
    for (i in names(x)) {
      mc[[2]] = as.name(i)
      x[[i]] = eval(mc, x, enclos = parent.frame())
    }
    mc0 = mc[1:3] # "[", x, first dim
    j = 3 # first dim
    for (i in names(d)) {
      mc0[[2]] = as.name(i)
      mc0[[3]] = mc[[j]]
      mc0[["values"]] = ed[[i]]
      d[[i]] = eval(mc0, d, enclos = parent.frame())
      j = j + 1
    }
  }
  if (drop)
    adrop(st_as_stars(x, dimensions = d))
  else
    st_as_stars(x, dimensions = d)
}

# =============================================================
# RESTful web service with data as a base64 encoded string
# =============================================================

# library(readr)
# legend = read_csv("data/example_udf_in/legend.csv") # [Test] Read CSV legend file
# code = fromJSON(txt = "data/example_udf_in/udf_body_raw_proj.json")["code"] # [Test] Extract code
# request = list(legend = toJSON(x = legend, dataframe = "rows")) # [Test] To convert legend to JSON
# request = append(request, list(code = code)) # [Test] Append code
# bin_string = base64encode(what = "data/binary_udf/disk.zip") # [Test] Base64 encode zip of dir structure
# request = append(request, list(base64str = bin_string)) # [Test] Appends the binary string
# post_body = toJSON(request, pretty = TRUE) # [Test] Creates a JSON
# write(x = post_body, file = "data/binary_udf/post_body.json")
#
# json = fromJSON(txt = "data/binary_udf/post_body.json") # [Test] Read JSON into R object
# write(bin_string, "data/binary_udf/bin_data") # [Test] Write Base64 encoded string to disk

close_relevant_conn = function(con_description)
{
  cno = as.numeric(rownames(as.data.frame(showConnections())[as.data.frame(showConnections())$description == con_description]))
  for(c in cno)
  {
    con = try(getConnection(what = c), silent = TRUE)
    if(any(class(con) != "try-error"))
    {
      close(con)
      cat(paste(Sys.time(), "Connection(s) closed successfully!\n", sep = " "))
    } else
    {
      cat(paste(Sys.time(), "\nNo connections with given description to close.\n", sep = " "))
      break
    }
  }
}

bin_unzip_string = function(string = "data/binary_udf/bin_data", file = TRUE)
{
  cat(paste(Sys.time(), "Decoding base64 encoded string...\n", sep = " "))
  # dir.create("temp")
  if(file)
    base64decode(file = string, output = file("temp.zip", "wb")) else
      base64decode(what = string, output = file("temp.zip", "wb"))
  while(!file.exists("temp.zip")) Sys.sleep(1)
  close_relevant_conn("temp.zip")
  cat(paste(Sys.time(), "Finished decoding string; Starting to uncompress ZIP file...\n", sep = " "))
  # closeAllConnections()
  unzip(zipfile = "temp.zip", overwrite = T, exdir = "disk") # Works with Windows
  # system("mkdir disk && cd disk && jar -xvf ../temp.zip", ignore.stdout = T) # Works with Linux; requires 'fastjar'
  cat(paste(Sys.time(), "Finished unzipping file; Removing ZIP file...\n", sep = " "))
  file.remove("temp.zip")
  cat(paste(Sys.time(), "Finished deleting ZIP file\n", sep = " "))
}

bin_read_legend = function(legend)
{
  cat(paste(Sys.time(), "Creating stars object...\n", sep = " "))
  num_time = max(as.numeric(legend$time_index))
  timestamps = unique(legend$timestamp)
  # timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1])
  num_bands = max(as.numeric(legend$band_index))
  bands = unique(legend$band)
  filewpaths = try(cbind(legend[,1], legend$filename)[,2], silent = TRUE)
  if(class(filewpaths) == "try-error")
    filewpaths = legend$filename
  stars_obj = read_stars(filewpaths, along = list(band = bands, time = timestamps))
}


#' @serializer unboxedJSON
#' @post /udf/binary
run_UDF.binary = function(req)
{
  cat(paste("\n", Sys.time(), " Reading JSON...\n", sep = ""))
  # post_body = fromJSON(txt = "data/binary_udf/post_body.json") # for testing locally
  post_body = fromJSON(req$postBody) # for use with plumber
  # post_body = fromJSON(req)
  cat(paste(Sys.time(), "Converted incoming JSON to R object\n", sep = " "))
  # bin_unzip_string(string = post_body$base64str, file = FALSE)
  bin_unzip_string(string = post_body$base64str, file = FALSE)

  cat(paste(Sys.time(), "Reading legend...\n", sep = " "))
  # legend = fromJSON(post_body$legend)
  legend = fromJSON(post_body$legend)
  
  legend$timestamp = as.POSIXct(legend$timestamp)
  stars_in = bin_read_legend(legend)
  cat(paste(Sys.time(), "Creating stars object from incoming data\n", sep = " "))
  unlink("disk", recursive = TRUE)
  cat(paste(Sys.time(), "Deleted directory disk\n", sep = " "))

  # script = json2script(post_body$code)
  script = post_body$code$code$source
  script = gsub("\"", "'", script)
  script = gsub("\r", "", script)
  
  cat(paste(Sys.time(), "Applying UDF on incoming stars object...\n", sep = " "))
  stars_out = run_script_raw(stars_obj = stars_in, script_text = script)
  cat(paste(Sys.time(), "Output stars object created\n", sep = " "))

  time_only = FALSE
  band_only = FALSE
  
  time_out = try(dim(stars_out)[["time"]], silent = TRUE)
  if(class(time_out) == "try-error")
  {
    time_out = 1
    time_only = TRUE
  }
  band_out = try(dim(stars_out)[["band"]], silent = TRUE)
  if(class(band_out) == "try-error")
  {
    band_out = 1
    band_only = TRUE
  }

  legend_out = matrix(ncol = ncol(legend), nrow = time_out * band_out)
  colnames(legend_out) = colnames(legend)
  legend_out = as.data.frame(legend_out)
  cat(paste(Sys.time(), "Outgoing legend created\n", sep = " "))

  out_dir = "results"
  dir.create(out_dir)
  if(!time_only) time_vals = attr(stars_out, "dimensions")[["time"]]$values else time_vals = NA
  if(!band_only) band_vals = attr(stars_out, "dimensions")[["band"]]$values else band_vals = NA
  cat(paste(Sys.time(), "Starting to write results...\n", sep = " "))
  for(time_num in 1:time_out)
  {
    cat(paste(Sys.time(), " Time:", time_num, "\n", sep = " "))
    out_path = paste(out_dir, "/t_", time_num, sep = "")
    dir.create(out_path)
    for(band_num in 1:band_out)
    {
      cat(paste(Sys.time(), " Band:", band_num, "\n", sep = " "))
      filename = paste(out_path, "/b_", band_num, ".tif",  sep = "")
      
      if(!time_only && !band_only)
        stars_subset = stars_out[,,,band_num, time_num, drop = T] else
        if(time_only)
          stars_subset = stars_out[,,,time_num, drop = T] else
        if(band_only)
          stars_subset = stars_out[,,,band_num, drop = T]
      
      st_write(obj = stars_subset, dsn = filename)
      index = ((time_num - 1) * band_out) + band_num
      # print(index)
      legend_out[index,] = c(index, filename, as.numeric(time_num), as.character.Date(time_vals[time_num]), as.numeric(band_num), band_vals[band_num])
    }
  }
  # out_legend_json = toJSON(legend_out, dataframe = "rows", pretty = TRUE)
  # out_legend_json = gsub('\"', '"', out_legend_json)

  filepaths = list.files("results", full.names = T, recursive = T)
  zip(zipfile = "results.zip", files = filepaths, recurse = TRUE)
  unlink("results", recursive = TRUE)
  out_bin_string = base64encode(what = "results.zip")
  cat(paste(Sys.time(), "Created outgoing base64 encoded string\n", sep = " "))
  file_removal = file.remove("results.zip")
  response = list(legend = as.list(legend_out), base64str = out_bin_string)
  # response = append(response, list(base64str = out_bin_string))
  # cat("Created body for POST response\n")
  # post_response_body = toJSON(response, dataframe = "rows")
  cat(paste(Sys.time(), "Converted R object to JSON for response\n", sep = " "))
  # post_response_body = gsub('\"', '"', post_response_body)
  # closeAllConnections()
  # response = as.character(post_response_body)
  return(response)
}

