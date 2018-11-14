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

bin_unzip_string = function(string = "data/binary_udf/bin_data", file = TRUE)
{
  cat("Decoding base64 encoded string...\n")
  # dir.create("temp")
  if(file)
    base64decode(file = string, output = file("temp.zip", "wb")) else
      base64decode(what = string, output = file("temp.zip", "wb"))
  cat("Finished decoding string; Starting to uncompress ZIP file...\n")
  closeAllConnections()
  unzip(zipfile = "temp.zip", overwrite = T, exdir = "disk") # Works with Windows
  # system("mkdir disk && cd disk && jar -xvf ../temp.zip", ignore.stdout = T) # Works with Linux; requires 'fastjar'
  cat("Finished unzipping file; Removing ZIP file...\n")
  file.remove("temp.zip")
  cat("Finished deleting ZIP file\n")
}

bin_read_legend = function(legend)
{
  cat("Creating stars object...\n")
  num_time = max(legend$time_index)
  timestamps = unique(legend$timestamp)
  timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1])
  num_bands = max(legend$band_index)
  bands = unique(legend$band)
  filewpaths = cbind(legend$X1, legend$filename)[,2]
  stars_obj = read_stars(filewpaths, along = list(band = bands, time = timestamps))
}

#' @serializer unboxedJSON
#' @post /udf/binary
run_UDF.binary = function(req)
{
  cat("Reading JSON...\n")
  # post_body = fromJSON(txt = "data/binary_udf/post_body.json") # for testing locally
  post_body = fromJSON(req$postBody) # for use with plumber
  # post_body = fromJSON(req)
  cat("Converted incoming JSON to R object\n")

  bin_unzip_string(string = post_body$base64str, file = FALSE)

  cat("Reading legend...\n")
  legend = fromJSON(post_body$legend)
  legend$timestamp = as.POSIXct(legend$timestamp)
  stars_in = bin_read_legend(legend)
  cat("Creating stars object from incoming data\n")
  unlink("disk", recursive = TRUE)
  cat("Deleted directory disk\n")

  script = json2script(post_body$code)
  cat("Applying UDF on incoming stars object...\n")
  stars_out = run_script_raw(stars_obj = stars_in, script_text = script)
  cat("Output stars object created\n")

  time_out = dim(stars_out)[["time"]]
  band_out = dim(stars_out)[["band"]]
  legend_out = matrix(ncol = ncol(legend), nrow = time_out * band_out)
  colnames(legend_out) = colnames(legend)
  legend_out = as.data.frame(legend_out)
  cat("Outgoing legend created\n")

  out_dir = "results"
  dir.create(out_dir)
  time_vals = attr(stars_out, "dimensions")[["time"]]$values
  band_vals = attr(stars_out, "dimensions")[["band"]]$values
  cat("Starting to write results...\n")
  for(time_num in 1:time_out)
  {
    cat(paste("Time:", time_num, "\n", sep = " "))
    out_path = paste(out_dir, "/t_", time_num, sep = "")
    dir.create(out_path)
    for(band_num in 1:band_out)
    {
      cat(paste("Band:", band_num, "\n", sep = " "))
      filename = paste(out_path, "/b_", band_num, ".tif",  sep = "")
      stars_subset = stars_out[,,,band_num, time_num, drop = T]
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
  cat("Created outgoing base64 encoded string\n")
  file.remove("results.zip")
  response = list(legend = legend_out, base64str = out_bin_string)
  # response = append(response, list(base64str = out_bin_string))
  cat("Created body for POST response\n")
  post_response_body = toJSON(response, dataframe = "rows")
  cat("Converted R object to JSON for response\n")
  post_response_body = gsub('\"', '"', post_response_body)
}

