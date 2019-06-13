library(ncdf4)
library(raster)

apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

sfsreg <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))

paths <- c(
  # 'D:/TERNSoils/CoVariates/Relief/relief_dems_3s_mosaic1.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_elev_focalrange300m_3s.tif',
  # 'D:/TERNSoils/CoVariates/Relief/ContributingArea_3s1.tif',
  # 'D:/TERNSoils/CoVariates/Relief/dem_foc2.tif',
  # 'D:/TERNSoils/CoVariates/Relief/rel_foc.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_apsect.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_elev_focalrange1000m_3s.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_mrvbf_3s_mosaic.tif',
  # 'D:/TERNSoils/CoVariates/Relief/Relief_mrrtf_3s.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_slope_perc.tif',
  # 'D:/TERNSoils/CoVariates/Relief/relief_twi_3s.tif',
  # 'D:/TERNSoils/CoVariates/Location/LOC_distance_to_coast.tif',
  # 'D:/TERNSoils/CoVariates/Parent_Material/PM_Radiometrics_Modelled_U.tif',
  # 'D:/TERNSoils/CoVariates/Parent_Material/PM_Radiometrics_Modelled_K.tif',
  # 'D:/TERNSoils/CoVariates/Parent_Material/PM_Radiometrics_Modelled_TH.tif',
  # 'D:/TERNSoils/CoVariates/Parent_Material/PM_Silica.tif',
  # 'D:/TERNSoils/CoVariates/Parent_Material/PM_Weathering_Index.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/AWC/AWC_000_005_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/AWC/AWC_030_060_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/AWC/AWC_060_100_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/Clay/CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/Clay/CLY_030_060_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/Clay/CLY_060_100_EV_N_P_AU_NAT_C_20140801.tif',
  # 'D:/TERNSoils/National_digital_soil_property_maps/Depth_of_Soil/DES_000_200_EV_N_P_AU_NAT_C_20140801.tif',
  'D:/Projects/DSM_Extras/AWCPedo/AWC100.tif',
  'C:/Projects/GenericSoilGroups/soil_forestclassifier_most_likely.tif'
)

outDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/Covariates/'

for (i in 1:length(paths)) {
  
  print(i)
  path <- paths[i]
  r<- raster(path)
  
  cr <- crop(r, sfsreg)
  plot(cr)
  rcr <- resample(cr, sfsreg, filename = paste0(outDir, basename(path)))
  
  
}


