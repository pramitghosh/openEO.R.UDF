# OpenEO.R.UDF

This package reads generic (currently GeoTIFF only; but will support non-raster data (e.g. feature data) in the near future as well) files to a stars object, applies users' custom function on it, and writes the resultant files back to disk. This package acts as a tool for users to parse the files written to disk by the backend so that their (users') custom functions (UDFs) could be applied. In the same way, the results from the UDF are written back to disk in a consistent format parsable by the backend.

### General strategy
This package has to be loaded from the user's R script file (which should also contain the UDF definition) and the UDF to be applied has to be called as an argument to the function `run_UDF()` defined in this package which will apply it on a `stars` object created from generic files on disk. Metadata regarding these files (e.g. path, band, time etc.) are looked up from an ASCII "legend" file in CSV format which is written to disk along with the actual data by the [`write_generics()` function](https://github.com/pramitghosh/openeo-r-backend/blob/b7da77f87a90ba49d79cafd17a634f6117dccc2f/R/prepare_UDF.R#L13) in the backend.

Once the UDF has been applied on the data, the result is written back to disk in the form of generic files - e.g. GeoTIFF for rasters. A corresponding "legend" file is also generated containing metadata regarding the output files, which could be then used to read the files back in the backend. The way the output is written to disk (e.g. format, directory structure etc.) are determined by the dimensionality of the UDF result and will be reflected by the corresponding "legend" file which acts as a look-up table with metadata.

## Dependencies
This R package needs the package `stars` which is not on CRAN yet. The `stars` package is available here: <https://github.com/r-spatial/stars>. Therefore, this dependency needs to be installed in the environment first by:

```
library(devtools)
install_github("r-spatial/stars")
```

## Installation
This package can be currently installed by

```
install_github("pramitghosh/openeo.R.UDF")
```

## Usage
**Please note that this section is likely to change during the development of this package. The documentation currently associated with this package too are outdated but acts as a general reference for the idea**

This package loads GeoTIFF files from the disk by looking up an legend file provided in a CSV format containing metadata regarding the files into a `stars` object. It applies a function defined and specified by the user and writes the results back to disk in a directory specified by the user.

```
run_UDF(legend_name = <Name of the legend file with path>, function_name = <Name of the UDF defined by the user>, drop_dim = <Dimension index of the dimension to be dropped>, in_dim = <Dimensionality of the incoming Collection object>, out_dir = <Name of the new directory where resultant file(s) are to be written>)
```
More details can be found in the function documentation.
