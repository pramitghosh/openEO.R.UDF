# legend = read_csv("data/example_udf_in/legend.csv") # [Test] Read CSV legend file
# code = fromJSON(txt = "data/example_udf_in/udf_body_raw_proj.json")["code"] # [Test] Extract code
# request = list(legend = toJSON(x = legend, dataframe = "rows")) # [Test] To convert legend to JSON
# request = append(request, list(code = code)) # [Test] Append code
# bin_string = base64encode(what = "data/binary_udf/disk.zip") # [Test] Base64 encode zip of dir structure
# request = append(request, list(base64str = bin_string)) # [Test] Appends the binary string
# post_body = toJSON(request, pretty = TRUE) # [Test] Creates a JSON
# write(x = post_body, file = "data/binary_udf/post_body.json")

# bin_legend = fromJSON(txt = "data/binary_udf/legend_json.json") # [Test] Read JSON into R object
# write(bin_string, "data/binary_udf/bin_data") # [Test] Write Base64 encoded string to disk

bin_unzip_string = function(string = "data/binary_udf/bin_data", file = TRUE)
{
  # dir.create("temp")
  if(file)
    base64decode(file = string, output = "temp.zip") else
      base64decode(what = string, output = "temp.zip")
  unzip("temp.zip", overwrite = T, exdir = "disk")
  file.remove("temp.zip")
}

bin_read_legend = function(legend)
{
  num_time = max(legend$time_index)
  timestamps = unique(legend$timestamp)
  timestamps_padded = c(timestamps, timestamps[length(timestamps)]+diff(timestamps)[1])
  num_bands = max(legend$band_index)
  bands = unique(legend$band)
  filewpaths = cbind(legend$X1, legend$filename)[,2]
  stars_obj = read_stars(filewpaths, along = list(band = bands, time = timestamps))
}

bin_read_body = function(req)
{
  # post_body = fromJSON(txt = "data/binary_udf/post_body.json", simplifyVector = T)
}

