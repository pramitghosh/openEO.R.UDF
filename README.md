# OpenEO.R.UDF

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
This package loads GeoTIFF files from the disk by looking up an legend file provided in a CSV format containing metadata regarding the files into a `stars` object. It applies a function defined and specified by the user and writes the results back to disk in a directory specified by the user.

```
run_UDF(legend_name = <Name of the legend file with path>, function_name = <Name of the UDF defined by the user>, drop_dim = <Dimension index of the dimension to be dropped>, out_dir = <Name of the new directory where resultant file(s) are to be written>)
```
More details can be found in the function documentation.
