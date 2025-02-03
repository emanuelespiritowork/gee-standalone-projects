
################################################
#              LIBRARIES
################################################
library(reshape2)
library(ggplot2)
library(terra)

rm(list = ls())

################################################
#              PATH
################################################
path_EVI <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo/Indices/EVI/"
path_NDFI <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo/Indices/NDFI2/"
path_SCL <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo_SCL/"
path_plot_evi_masked <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo_masked/EVI"
path_plot_ndfi_masked <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo_masked/NDFI2"
path_plot_evi_ndfi_masked <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo_masked/EVI_and_NDFI2"

################################################
#              GET FILES
################################################
r_list_EVI <- list.files(path = path_EVI, pattern = ".tif$", full.names = TRUE, all.files = FALSE)

EVI_stack <- terra::rast(r_list_EVI)

r_list_NDFI <- list.files(path = path_NDFI, pattern = ".tif$", full.names = TRUE, all.files = FALSE)

NDFI_stack <- terra::rast(r_list_NDFI)

r_list_SCL <- list.files(path = path_SCL, pattern = ".tif$", full.names = TRUE, all.files = FALSE)

SCL_stack <- terra::rast(r_list_SCL)

################################################
#              MASK
################################################
masks <- SCL_stack %in% c(3,8,9,10,11)

EVI_stack_masked <- mask(EVI_stack, masks, maskvalue = TRUE)

NDFI_stack_masked <- mask(NDFI_stack, masks, maskvalue = TRUE)

################################################
#              GET DATES
################################################

dates <- paste0(substring(basename(r_list_EVI),12,21))

dates <- as.Date(dates, "%Y-%m-%d")

################################################
#              VECTOR FILE
################################################
path_vector <- "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/vector"

vector_shapefile <- vect(path_vector)

########################################################
#               COMPUTE MULTI-TEMPORAL RASTER
########################################################
terra_extract_evi <- terra::extract(EVI_stack_masked, vector_shapefile, 
                                           fun = mean, 
                                           na.rm = T,
                                           method = 'bilinear',
                                           cells = TRUE,
                                           touches = TRUE)

terra_extract_ndfi <- terra::extract(NDFI_stack_masked, vector_shapefile, 
                                    fun = mean, 
                                    na.rm = T,
                                    method = 'bilinear',
                                    cells = TRUE,
                                    touches = TRUE)


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
    geom_point(color = 'blue') +
    geom_smooth(method = "loess",
                span = 0.2, se = TRUE, color = "blue") +
    xlab('dates') + 
    ylab('EVI') + 
    ggtitle(i) + 
    ylim(c(-1,1))
  
  ggsave(filename = paste0(path_plot_evi_masked,'/plot_',i,".png"),
         plot = last_plot(),
         height = 3.5,
         width = 7)
  
  #saving ndfi2 plots
  new_dataframe_ndfi[,i] <- as.numeric(new_dataframe_ndfi[,i])
  
  ggplot(data=new_dataframe_ndfi,aes(x=dates,y=new_dataframe_ndfi[,i])) +
    geom_point(color = 'red') +
    geom_smooth(method = "loess",
                span = 0.2, se = TRUE, color = "red") +
    xlab('dates') + 
    ylab('NDFI') + 
    ggtitle(i) + 
    ylim(c(-1,1))
  
  ggsave(filename = paste0(path_plot_ndfi_masked,'/plot_',i,".png"),
         plot = last_plot(),
         height = 3.5,
         width = 7)
  
  ggplot() +
    geom_point(data=new_dataframe_ndfi,aes(x=dates,y=new_dataframe_ndfi[,i]),color = 'red') +
    geom_point(data=new_dataframe_evi,aes(x=dates,y=new_dataframe_evi[,i]),color = 'blue') +
    geom_smooth(data=new_dataframe_evi,aes(x=dates,y=new_dataframe_evi[,i]),method = "loess",
                span = 0.2, se = TRUE, color = "blue") +
    xlab('dates') +
    ylab('EVI2 and NDFI2 value') +
    ggtitle(i) + 
    ylim(c(-1,1))
  
  ggsave(filename = paste0(path_plot_evi_ndfi_masked,'/plot_',i,".png"),
         plot = last_plot(),
         height = 3.5,
         width = 7)
  
  
}

########################################################
#               PRINT GRID MULTI-TEMPORAL RASTERS 
########################################################

dB_melt_EVI <- melt(new_dataframe_evi,
                id.vars = names(new_dataframe_evi)[c(1)])

ggplot(aes(y = value, x = dates),
       data = dB_melt_EVI) +
  geom_point() +
  facet_wrap(~ variable) +
  geom_line() + 
  geom_smooth(method = "loess",
              span = 0.2, se = TRUE, color = "blue")

ggsave(file = paste0(path_plot_evi_masked,"/all_plots.png"),
       dpi = 150, width = 100, height = 100, units= 'cm')

dB_melt_NDFI <- melt(new_dataframe_ndfi,
                     id.vars = names(new_dataframe_ndfi)[c(1)])

ggplot(aes(y = value, x = dates),
       data = dB_melt_NDFI) +
  geom_point() +
  facet_wrap(~ variable)+
  geom_line() + 
  geom_smooth(method = "loess",
              span = 0.2, se = TRUE, color = "red")

ggsave(file = paste0(path_plot_ndfi_masked,"/all_plots.png"),
       dpi = 150, width = 100, height = 100, units= 'cm')

dB_melt <- dB_melt_EVI

names(dB_melt) <- c('dates','variable','EVI')

dB_melt$NDFI <- dB_melt_NDFI$value


ggplot() + 
  geom_point(mapping = aes(y = EVI,x = dates), data = dB_melt, color = 'blue') +
  geom_point(mapping = aes(y = NDFI,x = dates), data = dB_melt, color = 'red') +
  geom_smooth(mapping = aes(y = EVI,x = dates), data = dB_melt, 
              method = 'loess', span = 0.2, se = TRUE, color = 'blue') +
  #geom_smooth(data = dB_melt, mapping = aes(y = NDFI, x = dates),
  #          method = 'loess', span = 0.2, se = TRUE, color = 'red')+
  facet_wrap(~ variable, scales = 'free')

ggsave(file = "C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/Plots_openeo_masked/all_plots.png",
       dpi = 150, width = 100, height = 100, units= 'cm')
