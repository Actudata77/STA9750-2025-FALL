library(sf)
library(sf, type = "binary")


# Create dummy folder
dir.create("data/mp03", showWarnings = FALSE)

# Download and read a small sample shapefile
url <- "https://data.cityofnewyork.us/download/k2i6-zkzx/application%2Fzip"
dest <- "data/mp03/test_shape.zip"
download.file(url, dest)
unzip(dest, exdir = "data/mp03/test_shape")
test_sf <- st_read("data/mp03/test_shape", quiet = TRUE)
head(test_sf)



library(sf)

# Built-in shapefile shipped with sf (North Carolina counties)
nc_path <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(nc_path, quiet = TRUE)
print(nc[1:3, c("NAME")])
st_crs(nc)
