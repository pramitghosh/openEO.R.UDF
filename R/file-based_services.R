read_legend = function(legend_file)
{
  read.csv(legend_file, header = TRUE)
}

read_generics = function(legend_file, dimensionality)
{
  legend_df = read_legend(legend_file)
  legend_dir = dirname(legend_file)

  if(dimensionality[5] == 1) #If raster
  {
    file_list = paste(legend_dir, as.character(legend_df$filename), sep = "/")
    time_list = as.POSIXct(unique(legend_df$timestamp))
    band_list = as.character(unique(legend_df$band))

    time_cols = matrix(nrow = length(band_list), ncol = length(time_list))
    num_files = dim(legend_df)[1]
    for(tifs in 1:num_files)
    {
      time_cols[legend_df$band_index[tifs], legend_df$time_index[tifs]] = paste(legend_dir, as.character(legend_df$filename[tifs]), sep = "/")
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
    # Fixing temporal dimensions - might need to find better solutions!
    attr(stars_nD, "dimensions")[["time"]]$to = as.numeric(dim(stars_nD)["time"])
    attr(stars_nD, "dimensions")[["time"]]$delta = attr(stars_nD, "dimensions")[["time"]]$delta / (as.numeric(dim(stars_nD)["time"]) - 1)
  } else if(dimensionality[5] == 0) #If vector
  {
    # create a stars object named `stars_nD` from the vector files...
  } else if(is.na(dimensionality[5])) #If neither raster or vector (e.g. pure time series of values)
  {
    # Create a consistent data structure (maybe stars object too?) for applying UDFs on
  }
  stars_nD
}


#' @title Run an UDF on a `stars` object created from generic GeoTIFF files
#'
#' @description Reads generic files from disk by looking up a legend file, runs the User Defined Function (UDF) specified by the user and
#' writes back the results to disk as a multi-band GeoTIFF file named `out.tif` in the directory specified by the user.
#'
#' @param legend_name Name of the legend file as a string (default: "legend.csv")
#' @param function_name Name of the User-Defined Function (UDF) the user defined in his own script
#' @param drop_dim Numeric value (or vector) representing the dimension number of the dimension to be dropped. (1,2 = Space, 3 = Band, 4 = Time, 5 = Whether raster)
#' @param out_dir Path of the directory where the resulting files are to be written to disk (default: "results")
#' @param in_dim Dimensionality of the incoming Collection object (default: `c(1,1,1,1,1)` representing spatial multi-band, multi-temporal raster)
#'
#' @details The semantics of the written multi-band GeoTIFF depends on the argument `drop_dim`. For example, if `drop_dim = 4`
#' the bands in the output file represent bands while if `drop_dim = 3`, the bands in the output file represent time.
#' This modification of the dimensionality (`drop_dim`) as well as the dimensionality of the incoming Collection object
#' (`in_dim`) would be controlled by the backend as parameters passed on to the UDF server as metadata files along with
#' the legend file (`legend_name`).
#' @export
#'
run_UDF = function(legend_name = "legend.csv", function_name, drop_dim, in_dim = c(1,1,1,1,1), out_dir = "results")
{
    # drop_dim and in_dim could be passed on from the backend as metadata in the form of files

    #In the future in_dim has to read in from disk (from what was written by the backend)
    #drop_dim is currently a numeric value corresponding to one of the indices of in_dim, but could also be a vector
    #For space (x,y), band (b), time (t) and whether raster? (r; 1 = raster, 0 = vector, NA = neither) for now
    all_dims = 1:4 #Keeping only x,y,b,t
    if(file.exists(legend_name)) #Check if legend file exists
    stars_obj = read_generics(legend_file = legend_name, dimensionality = in_dim)  else
    stop("Legend file is not accessible or does not exist!")
    #Need to keep check that drop_dim is consistent with the boolean typechecking framework suggested to Florian as
    #an extra layer of armour against inconsistent UDFs from the user
    result = st_apply(stars_obj, FUN = function_name, MARGIN = all_dims[-c(drop_dim)])
    out_dirpath = paste(dirname(legend_name), out_dir, sep = "/")

    if (!dir.exists(out_dirpath)) {
      dir.create(out_dirpath)
    }

    new_dim = in_dim
    new_dim[drop_dim] = 0

    if(new_dim[5] == 1) #If UDF result = raster
    {
      in_legend = read_legend(legend_name)
      num_band = dim(result)["band"]
      if(is.na(num_band) || num_band < 1) num_band = 1 #Since rasters must have at least 1 band in it!

      out_path = paste(out_dirpath, "t_", sep = "/")
      if(new_dim[4] == 1) #If UDF result = raster + temporal
      {
        num_time = as.numeric(dim(result)["time"])

        if(!is.na(num_time))
        {
          out_legend = matrix(ncol = 10, nrow = num_time * num_band)
          colnames(out_legend) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
          out_legend = as.data.frame(out_legend)
          band_list = as.character(unique(in_legend$band))
          for(time_num in 1:num_time)
          {
            #time_num: iterator for time indices
            #num_time: total number of time observations present
            dir.create(path = paste(out_path, time_num, sep = ""))

            time_index = time_num
            timestamp = attr(result, "dimensions")$time$offset
            timestamp = as.POSIXct(head(in_legend$timestamp[in_legend$time_index == time_num], 1)) #Timestamp of the 1st band for the corresponding time_index in in_legend

            for(band_num in 1:num_band)
            {
              #band_num: iterator for band indices
              #num_band: total number of bands present
              tmp_stars_obj = result[,,, time_num] #time dimension inconsistent with (a) dim() and attr(<obj>, "dimensions") - need to look when reading TIFFs to stars objects!
              # tmp_raster_obj = as(tmp_stars_obj, "Raster")
              st_write(obj = tmp_stars_obj, dsn = paste(out_path, time_num, "/b_", band_num, ".tif",  sep = ""))
              # writeRaster(x = tmp_raster_obj, filename = paste(out_path, time_num, "/", "b_", band_num, ".tif",  sep = ""))

              out_legend$xmin[(time_num - 1) * num_band + band_num] = attr(result, "dimensions")$x$offset + attr(result, "dimensions")$x$from - 1
              out_legend$xmax[(time_num - 1) * num_band + band_num] = attr(result, "dimensions")$x$offset + attr(result, "dimensions")$x$to - 1
              out_legend$ymin[(time_num - 1) * num_band + band_num] = attr(result, "dimensions")$y$offset + attr(result, "dimensions")$y$from - 1
              out_legend$ymax[(time_num - 1) * num_band + band_num] = attr(result, "dimensions")$y$offset + attr(result, "dimensions")$y$to - 1

              out_legend$filename[(time_num - 1) * num_band + band_num] = paste("t_", time_num, "/", "b_", band_num, ".tif",  sep = "")

              out_legend$band_index[(time_num - 1) * num_band + band_num] = band_num
              out_legend$band[(time_num - 1) * num_band + band_num] = band_list[band_num]

              out_legend$time_index[(time_num - 1) * num_band + band_num] = time_index
              out_legend$timestamp[(time_num - 1) * num_band + band_num] = as.character.Date(timestamp)

              out_legend$whether_raster = 1
            }
          }
          write.csv(x = out_legend, file = paste(out_dirpath, "out_legend.csv", sep = "/"))

          # #Need to have separate write methods for resultant objects which have different dimensionality - say c(0,0,0,0,1) (a time-series only).
          # #Resultant dimensionality can be calculated from the dimensionality of the Collection and the UDF (as suggested to Florian)
          # st_write(obj = result, dsn = paste(out_dir, "out.tif", sep = "/"))
        }
      } else #If UDF result is raster but not temporal
      {
        out_legend = matrix(ncol = 10, nrow = num_band)
        colnames(out_legend) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
        out_legend = as.data.frame(out_legend)
        dir.create(paste(out_path, NA, sep = ""))

        band_list = as.character(unique(in_legend$band))
        num_band = dim(result)["band"]
        time_index = NA
        timestamp = NA

        for(band_number in 1:num_band)
        {
          #band_number: iterator for band indices
          #num_band: total number of bands present
          st_write(obj = result[,,,band_number], dsn = paste(out_path, "NA/", "b_", band_number, ".tif",  sep = ""))

          out_legend$xmin[band_number] = attr(result, "dimensions")$x$offset + attr(result, "dimensions")$x$from - 1
          out_legend$xmax[band_number] = attr(result, "dimensions")$x$offset + attr(result, "dimensions")$x$to - 1
          out_legend$ymin[band_number] = attr(result, "dimensions")$y$offset + attr(result, "dimensions")$y$from - 1
          out_legend$ymax[band_number] = attr(result, "dimensions")$y$offset + attr(result, "dimensions")$y$to - 1

          out_legend$filename[band_number] = paste("t_NA/", "b_", band_number, ".tif",  sep = "")

          out_legend$band_index[band_number] = band_number
          out_legend$band[band_number] = band_list[band_number]

          out_legend$time_index[band_number] = time_index
          out_legend$timestamp[band_number] = timestamp

          out_legend$whether_raster = 1
        }
        write.csv(x = out_legend, file = paste(out_dirpath, "out_legend.csv", sep = "/"))
      }
    } else #If UDf result is not a raster
    {
      #Do things accordingly if the feature/timeseries is temporal, layered etc.
    }
}