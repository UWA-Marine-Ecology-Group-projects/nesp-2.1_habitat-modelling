## setting up the covariate data for multinomial modelling
## spatial raster for Aus benthos
library(terra)
tif_paths <- list.files("data/raster/",full.names = TRUE)
lapply(tif_paths,rast)
dep <- rast(tif_paths[2])
asp <- rast(tif_paths[1])
dep2 <- crop(dep,asp)
dep3 <- mask(dep2,asp)
writeRaster(dep3,filename = "data/raster/depth.tif")

## load in cropped and masked rasters
tif_paths2 <- list.files("data/raster/",full.names = TRUE)[-2]
r <- rast(tif_paths2)
plot(r)
names(r)[2] <- "depth"

## set extent and crop
e <- c(114.5,115.25,-34.25,-33.5)
r2 <- crop(r,e)

## extract covars at sites
xy <- habitat[,4:3]
covars <- terra::extract(r2,xy,xy=FALSE)

covars_out <- cbind(habitat[,1:4,],covars)
head(covars_out)
write.csv(covars_out,"data/tidy/nesp-2.1_WA_habitat_covariates.csv")
