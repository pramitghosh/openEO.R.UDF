# openEO.R.UDF

## Introduction
This repository contains a R package for implementing the concept of User-Defined Functions for processing Earth Observation (EO) data in cloud backends. This will allow users to run their custom code written in R to be executed on EO data such as satellite imageries with the help of processing backends conforming to the openEO API. The openEO API is being developed as part of the project ["openEO"](https://github.com/Open-EO).

### Background
This repository is meant to be part of the H2020 funded project [openEO](http://openeo.org). The objective of this project is to develop an uniform API to allow processing of Earth Observation (EO) data in cloud-based processing backends from various client nodes. In this API framework, User-Defined Functions (UDFs) is a concept that would allow users to run their own scripts on EO data in these cloud backends.

### User-Defined Functions
The UDFs are implemented by developing an UDF API which work hand-in-hand with the openEO core API. The main idea is that there are UDF (web-) services which could be used by the backends as required. The typical workflow is:

1. The user uploads his/her script from the client nodes to the backends along with the process graph
2. The backend executes the process graph and encounters the UDF in the process graph
3. The backend seeks the services of the UDF service to execute the user's script and sends the script and intermediate data to the service through appropriate means (file-based service, RESTful web-service etc.)
4. The UDF service executes the script on the data and sends the result back to the backend.
5. The backend receives the data and continues executing the process graph until the final result is obtained.
6. The backend sends the completed result to the user's client node.

These UDF service is being developed for two different languages - Python and R. This repository concerns with the implementation using R.

### Architecture

![openEO UDF Architecture](https://github.com/pramitghosh/openEO.R.UDF/blob/master/data/openeo_github.png)

In the openEO API, the different clients interact with the different backends through the openEO API which acts as a common language understood by both the clients and the backends. The UDF service is not accessible to the clients directly but only through the backends and hence the UDF service's internal operations are abstracted to the user.

## Installation

### Dependencies
This R package has the following dependencies
 * stars
 * jsonlite
 * plumber
 * raster
 * base64enc
 * zip

These can be installed by running the following:

```r
install.packages(c("stars", "jsonlite", "plumber", "raster", "base64enc", "zip"), dependencies = TRUE)
```

### Installing `openEO.R.UDF`
This package can then be installed using

```r
install_github("pramitghosh/openEO.R.UDF")
```

### Using Docker
Docker provides a virtual containerized environment for running software. In order to install this R package in a Docker environment, please follow these steps:

1. Install Docker on your machine. The installation instructions vary according to the Operating System. Detailed instructions for all common Operating Systems may be found here: <https://docs.docker.com/install/>.
2. Make sure that Docker has been installed correctly using the following command. Details on containers and Docker version will be shown.
```bash
docker info
```
3. Test whether installation using Docker is working correctly. A hello message should be printed on screen. 
```bash
docker run hello-world
```
4. Run it using the following command. If this image is not present on the local machine, it will be pulled from [Docker Hub](https://hub.docker.com/r/pramitghosh/openeo.r.udf/).
```bash
docker run -p 5384:8010 pramitghosh/openeo.r.udf
```
In the above command, `-p` re-routes the port where the service will be available. In the Docker image itself, this is available on port 8010. It can be mapped to any port of choice on the host (here it is mapped to port 5384). Instead of running, the image can be pulled from Docker Hub using
```bash
docker pull pramitghosh/openeo.r.udf
```
#### For using base64 encoded string
One of the strategies used involve transmission of data to and from the backend through HTTP POST requests in the form of base64 encoded strings representing a ZIP file containing generic GeoTIFFs embedded in a JSON. Please use the Docker container `pramitghosh/openeo.r.udf:wbin` on Docker Hub to use it. Currently, it is not possible to run it manually without encountering segmentation fault due to a long-standing issue in one of the dependencies of this package.

## Usage
This package is intended to be used as part of the openEO API. The package works along with the different backends and are not supposed to accessible directly by the client. However, for testing, please refer to the the Wiki pages of this repository [here](https://github.com/pramitghosh/openEO.R.UDF/wiki).

### Releases
This repository has two versions. The first pre-release version v0.0.1 contains a proof-of-concept implementation of a file-based service. The subsequent tagged version, v0.0.2, implements a RESTful web service to run user-defined functions on EO data. Please note, that the Docker image is meant to be used for this web service only.
