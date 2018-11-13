library(plumber)
library(stars)
library(jsonlite)
library(raster)
library(base64enc)
library(zip)

r <- plumb("/opt/dockerfiles/R/json.R")
r$run(host="0.0.0.0",port=8010)
