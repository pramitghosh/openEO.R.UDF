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
  # dir.create("temp")
  if(file)
    base64decode(file = string, output = file("temp.zip", "wb")) else
      base64decode(what = string, output = file("temp.zip", "wb"))
  # unzip(zipfile = "temp.zip", overwrite = T, exdir = "disk", unzip = "unzip") # Works with Windows
  system("mkdir disk && cd disk && jar -xvf ../temp.zip", ignore.stdout = T) # Works with Linux; requires 'fastjar'
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
  post_body = fromJSON(txt = "data/binary_udf/post_body.json")
  bin_unzip_string(string = post_body$base64str, file = FALSE)
  legend = fromJSON(post_body$legend)
  legend$timestamp = as.POSIXct(legend$timestamp)
  stars_in = bin_read_legend(legend)
  script = json2script(post_body$code)
  stars_out = run_script_raw(stars_obj = stars_in, script_text = script)

  time_out = dim(stars_out)[["time"]]
  band_out = dim(stars_out)[["band"]]
  legend_out = legend[1:(time_out * band_out), ]
  out_dir = "results"
  dir.create(out_dir)
  for(time_num in 1:time_out)
  {
    out_path = paste(out_dir, "/t_", time_num, sep = "")
    dir.create(out_path)
    for(band_num in 1:band_out)
    {
      # st_write(obj = stars_out[,,,band_num, time_num], dsn = paste(out_path, "/b_", band_num, ".tif",  sep = ""))
    }
  }
}

