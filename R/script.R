read_legend = function(legend_file)
{
  read.csv(legend_file, header = TRUE)
}

read_generics = function(legend_file, dimensionality)
{
  legend_df = read_legend(legend_file)

  if(dimensionality[5] == 1) #If raster
  {
    file_list = as.character(legend_df$filename)
    time_list = as.POSIXct(unique(legend_df$timestamp))
    band_list = as.character(unique(legend_df$band))

    time_cols = matrix(nrow = length(band_list), ncol = length(time_list))
    num_files = dim(legend_df)[1]
    for(tifs in 1:num_files)
    {
      time_cols[legend_df$band_index[tifs], legend_df$time_index[tifs]] = as.character(legend_df$filename[tifs])
    }

    stars_list = list()
    length(stars_list) = length(time_list)
    store_as_stars = function(ind_cols)
    {
      t_i = read_stars(.x = ind_cols, along = "band")
    }

    # Since a time duration is needed to calculate the delta for the time dimension,
    # a duration equal to the 1st element in the vector returned by diff() is added to the
    # last time observation to obtain the new time.
    padded_time_list = c(time_list, time_list[length(time_list)]+diff(time_list)[1])
    for(stars_obj in 1:length(stars_list))
    {
      t_i = store_as_stars(time_cols[, stars_obj])
      time_list_subset = padded_time_list[stars_obj:(stars_obj+1)]
      stars_list[[stars_obj]] = c(t_i, dim_name = "time", values = time_list_subset)
    }

    # To convert the list of stars objects to a single stars object
    stars_nD = stars_list[[1]]
    for(i in 2:length(stars_list))
    {
      stars_nD = c(stars_nD, stars_list[[i]])
    }
  } else if(dimensionality[5] == 0) #If vector
  {
    # create a stars object named `stars_nD` from the vector files...
  } else if(is.na(dimensionality[5])) #If neither raster or vector (e.g. pure time series of values)
  {
    # Create a consistent data structure (maybe stars object too?) for applying UDFs on
  }
  stars_nD
}

#' @title Run an UDF on a `stars` object
#'
#' @description Reads generic files from disk by looking up a legend file, runs the User Defined Function (UDF) specified by the user and
#' writes back the results to disk as a multi-band GeoTIFF file named `out.tif` in the directory specified by the user.
#'
#' @param legend_name Name of the legend file as a string
#' @param function_name Name of the User-Defined Function (UDF) the user defined in his own script
#' @param drop_dim Numeric value (or vector) representing the dimension number of the dimension to be dropped. (1,2 = Space, 3 = Band, 4 = Time, 5 = Whether raster)
#' @param out_dir Path of the directory where the resulting files are to be written to disk (a new directory will be created)
#' @param in_dim Dimensionality of the incoming Collection object (default: spatial multi-band, multi-temporal raster = c(1,1,1,1,1))
#'
#' @details The semantics of the written multi-band GeoTIFF depends on the argument `drop_dim`. If `drop_dim = 4`
#' the bands in the output file represent bands while if `drop_dim = 3`, the bands in the output file represent time.
#' The user has to mention the corresponding dimensionality of the output while defining the UDF in the process graph.
#' @export
#'
run_UDF = function(legend_name, function_name, drop_dim, in_dim = c(1,1,1,1,1), out_dir = "results")
{
  all_dims = 1:5 #For space (x,y), band (b), time (t) and whether raster? (r; 1 = raster, 0 = vector, NA = neither) for now
  if(file.exists(legend_name)) #Check if legend file exists
    stars_obj = read_generics(legend_file = legend_name, dimensionality = in_dim)
  else
    stop("Legend file is not accessible or does not exist!")
  #Need to keep check that drop_dim is consistent with the boolean typechecking framework suggested to Florian as
  #an extra layer of armour against inconsistent UDFs from the user
  result = st_apply(stars_obj, FUN = function_name, MARGIN = all_dims[-c(drop_dim)])
  dir_create_status = dir.create(out_dir)
  if(dir_create_status && dir.exists(out_dir)) #If new directory creation was successful
  {
    #Need to have separate write methods for resultant objects which have different dimensionality - say c(0,0,0,1) (a time-series only).
    #Resultant dimensionality can be calculated from the dimensionality of the Collection and the UDF (as suggested to Florian)
    st_write(obj = result, dsn = paste(out_dir, "out.tif", sep = "/"))
  }
}
