#For use with UDF type "apply_pixel"

#Loading the package for interacting with input data and applying custom functions on it
library(openeo.R.UDF)

#User's custom function definition
my_func = function(obj) {median(obj)}

#Applying user's custom UDF (my_func) on a `stars` object created from the files
#listed in the look-up table ("legend.csv") and applying the function on the object
#through run_UDF() defined in the package `openeo.R.UDF` to maintain consistency.
#Here a median function is applied on all the time-steps over the all the bands.
#The result (containing 1 band) has as many time-steps as in the input are written
#to disk as single-band GeoTIFF files supported by another legend file in a similar
#directory structure as the input. `drop_dim = 3` indicates that the dimension for 
#bands has to be dropped.
run_UDF(legend_name = "legend.csv", function_name = my_func, drop_dim = 3)