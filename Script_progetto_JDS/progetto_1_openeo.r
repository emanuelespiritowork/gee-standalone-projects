#         2024-12-06 
#progetto di R per creare gli indici 

#######################################
#             LIBRARIES
#######################################
library(terra)

rm(list = ls())

#######################################
#             GET FILES
#######################################

#raster path where we have the rasters
raster_path <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo/"

#list.files to create the name list files
list_of_files <- list.files(path = raster_path, pattern = ".tif$", full.names = TRUE, all.files = FALSE)

#######################################
#             COMPUTE VEGETATION INDEX AND FLOODING INDEX
#######################################
counter <- 1

number_of_files <- length(list_of_files)

#raster_list <- vector(mode="list", length=number_of_files)

#using sentinel 2 https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel-2/evi/
evi <- function(red,nir,blue){
  2.5*(nir-red)/(nir+6*red-7.5*blue+10000) #1 becomes 10000 to scale the surface reflectance values
}

#using 645nm, 2130nm as NDFI2 in https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0088741
ndfi2 <- function(red,SWIR2){
  (red-SWIR2)/(red+SWIR2)
}

bands_names <- c( "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")

while(counter <= number_of_files)
{
  
  #save the rasters in a list
  datoS2 <- rast(list_of_files[counter])
  print(list_of_files[counter])
  
  names(datoS2) <- bands_names
  
  #index computation
  datoS2_evi <- evi(datoS2[["B4"]],datoS2[["B8"]],datoS2[["B2"]])
  datoS2_ndfi2 <- ndfi2(datoS2[["B4"]],datoS2[["B12"]])
  
  names(datoS2_evi) <- paste0(substring(basename(list_of_files[counter]),8,17))
  names(datoS2_ndfi2) <- paste0(substring(basename(list_of_files[counter]),8,17))
  
  writeRaster(x= datoS2_evi,
              paste0(raster_path,"Indices/EVI/",
                     "EVI_",basename(list_of_files[counter])), overwrite=TRUE, filetype = "GTiff" )

  writeRaster(x= datoS2_ndfi2,
              paste0(raster_path,"Indices/NDFI2/",
                  "NDFI_",basename(list_of_files[counter])), overwrite=TRUE, filetype = "GTiff" )
  
  #update the counter
  counter <- counter + 1
}


























