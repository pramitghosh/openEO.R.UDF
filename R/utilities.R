#' Converts response of endpoint /udf/raw to stars
#'
#' @param json_out String representing JSON or path to JSON
#' @param file Logical. Needs to be TRUE if \code{json_out} represents the path to a JSON file
#'
#' @return A \code{stars} object created out of the JSON
#' @export
raw2stars = function(json_out, file = FALSE)
{
  if(file)
    json_out = readLines(con = file(json_out))
  json = fromJSON(json_out, simplifyVector = FALSE)
  json = append(x = json, values = list('data' = json))
  json = json['data']
  stars_out = json2stars(json)
}

#' Plots stars object from POST response
#'
#' @param json_out String representing JSON or path to JSON
#' @param method Either "raw" or "binary" depending on which endpoint the response is of
#' @param file Logical. Needs to be TRUE if \code{json_out} represents the path to a JSON file
#'
#' @export
plot_response = function(json_out, method, file = FALSE)
{
  if(method == "raw")
  {
    stars_out = raw2stars(json_out = json_out, file = file)
    stars_out = stars_out[drop = TRUE]
  } else if(method == "binary")
  {
    stars_out = binary2stars(json_out = json_out, file = file)
    stars_out = stars_out[drop = TRUE]
  } else
  {
    print("Unknown method!\n")
    return(NULL)
  }
  plot(stars_out)
}

#' Converts response of endpoint /udf/binary to stars
#'
#' @param json_out String representing JSON or path to JSON
#' @param file Logical. Needs to be TRUE if \code{json_out} represents the path to a JSON file
#'
#' @return A \code{stars} object created out of the JSON
#' @export
binary2stars = function(json_out, file = FALSE)
{
  if(file)
    json_out = readLines(con = file(json_out))
  json = fromJSON(json_out)
  bin_unzip_string(string = json$base64str, file = FALSE)
  legend = json$legend
  legend$timestamp = as.POSIXct(legend$timestamp)
  legend$time_index = as.numeric(legend$time_index)
  legend$band_index = as.numeric(legend$band_index)
  legend$filename = paste("disk", legend$filename, sep = "/")
  # if(length(unique(legend$band_index)) < length(legend$band_index))
  #   legend$band_index = seq(1:length(legend$band_index))
  if(is.na(any(legend$band)))
    legend$band = as.character(legend$band_index)
  stars_out = bin_read_legend(legend)
  unlink("disk", recursive = TRUE)
  stars_out
}

