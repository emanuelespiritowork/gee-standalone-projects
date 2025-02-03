#written over the script of F. Nutini (CNR-IREA) 
library(openeo)
image_repository = connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")

login()


p = processes()


datacube = p$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent=list(west = 11.87754, south = 44.82105, east = 12.03337, north = 44.90196),
  temporal_extent=c("2023-10-01", "2024-11-28"),
  #bands= c( "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12") #, "SCL", "WVP", "AOT", "sunAzimuthAngles", "sunZenithAngles", "viewAzimuthMean", "viewZenithMean") #"SCL", "WVP", "AOT",
  bands = c("SCL")
  )


res <- p$save_result(data = datacube, format = "GTiff")

job = create_job(graph=res,title = "Jolanda_Download_double_image")

start_job(job = job)

jobs = list_jobs()
jobs # printed as a tibble or data.frame, but the object is a list

#check here job status
# https://openeo.dataspace.copernicus.eu/
  
# download all the files into a folder on the file system
#download_results(job = job, folder = 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo')
download_results(job = job, folder = 'C:/Users/emast/Desktop/Remote_sensing_AGR/24-11-28_Progetto/TIF_openeo_SCL')

