########################################################
#               LIBRARIES
########################################################
library(reshape2)
library(ggplot2)
library(terra)

rm(list = ls())

########################################################
#               DIRECTORIES VECTOR
########################################################
path_vector <- 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/vector'

path_ndfi <- 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo/Indices/NDFI2'
path_plot_ndfi <- 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo/NDFI2'

path_evi <- 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo/Indices/EVI'
path_plot_evi <- 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo/EVI'

########################################################
#               LOAD RASTERS
########################################################
raster_names_evi <- list.files(path = path_evi, pattern = '.tif', full.names = TRUE)
raster_stack_evi <- terra::rast(raster_names_evi)

raster_names_ndfi <- list.files(path = path_ndfi, pattern = '.tif', full.names = TRUE)
raster_stack_ndfi <- terra::rast(raster_names_ndfi)

########################################################
#               COMPUTE MULTI-TEMPORAL RASTER
########################################################
vector_shapefile <- vect(path_vector)

terra_extract_evi <- terra::extract(
  x = raster_stack_evi,
  y = vector_shapefile,
  fun = mean,
  method = 'bilinear',
  cells = TRUE,
  touches = TRUE
)

terra_extract_ndfi <- terra::extract(
  x = raster_stack_ndfi,
  y = vector_shapefile,
  fun = mean,
  method = 'bilinear',
  cells = TRUE,
  touches = TRUE
)

terra_extract_evi$ID <- vector_shapefile$id_geom
terra_extract_ndfi$ID <- vector_shapefile$id_geom

database_serie_temporali_EVI <- as.data.frame(t(terra_extract_evi))
database_serie_temporali_NDFI <- as.data.frame(t(terra_extract_ndfi))

#View(database_serie_temporali_EVI)
#View(database_serie_temporali_NDFI)

names(database_serie_temporali_EVI) <- paste0('n',database_serie_temporali_EVI[1,])
database_serie_temporali_EVI <- database_serie_temporali_EVI[c(2:nrow(database_serie_temporali_EVI)),]
names(database_serie_temporali_NDFI) <- paste0('n',database_serie_temporali_NDFI[1,])
database_serie_temporali_NDFI <- database_serie_temporali_NDFI[c(2:nrow(database_serie_temporali_NDFI)),]

#terra::plot(database_serie_temporali_EVI$n0008, main = 'n0008')
#terra::plot(database_serie_temporali_NDFI$n0008, main = 'n0008')

dates <- as.Date(row.names(database_serie_temporali_EVI), tryFormats = '%Y-%m-%d')
new_dataframe_evi <- data.frame(cbind.data.frame(dates,database_serie_temporali_EVI))
names(new_dataframe_evi) <- c('dates',names(database_serie_temporali_EVI))
new_dataframe_ndfi <- data.frame(cbind.data.frame(dates,database_serie_temporali_NDFI))
names(new_dataframe_ndfi) <- c('dates',names(database_serie_temporali_NDFI))

########################################################
#               PRINT MULTI-TEMPORAL RASTER 
########################################################

for(i in names(database_serie_temporali_EVI)){
  print(i)
  
  #saving evi plots
  new_dataframe_evi[,i] <- as.numeric(new_dataframe_evi[,i])
  
  ggplot(data=new_dataframe_evi,aes(x=dates,y=new_dataframe_evi[,i])) +
    geom_point() +
    xlab('dates') + 
    ylab('EVI') + 
    ggtitle(i) + 
    ylim(c(-1,1))
  
  ggsave(filename = paste0(path_plot_evi,'/plot_',i,".png"),
         plot = last_plot(),
         height = 3.5,
         width = 7)
  
  #saving ndfi2 plots
  new_dataframe_ndfi[,i] <- as.numeric(new_dataframe_ndfi[,i])
  
  ggplot(data=new_dataframe_ndfi,aes(x=dates,y=new_dataframe_ndfi[,i])) +
    geom_point() +
    xlab('dates') + 
    ylab('NDFI') + 
    ggtitle(i) + 
    ylim(c(-1,1))
  
  ggsave(filename = paste0(path_plot_ndfi,'/plot_',i,".png"),
         plot = last_plot(),
         height = 3.5,
         width = 7)
  
}








