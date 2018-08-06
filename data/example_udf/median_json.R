cat("Loading openeo.R.UDF package\n")

library(openeo.R.UDF)



cat("Defining median function\n")

udf_func = function(obj) {

  median(obj)

}



cat("Running UDF function\n")

# drop dimension is time = 4, input is default x,y,b,t,raster, function

# tryCatch({
#
#   run_UDF(drop_dim = 4, function_name = myfunc,legend_name = "legend.csv")
#
# }, error = function(e) {
#
#   cat(paste("UDF-EXEC-ERROR:",e))
#
#   stop(e)
#
# })



# cat("Finished running UDF function\n")

