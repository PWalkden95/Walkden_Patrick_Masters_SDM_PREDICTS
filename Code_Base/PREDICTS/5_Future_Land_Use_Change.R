rm(list = ls())
require(raster)
setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS")

load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_North_Africa_Mask.RData")


Future_Land_Use_Change <- function(Land_Use, Year){

Land_Use_Change <- function(Land_Use, Year){

Europe_Extent <- extent(-10, 36, 30, 75) #


   ### Extract LU change rasters and upsample them so that they are at the same resolution as Climate change projections
  RCP_2.6 <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW/2.6_",Land_Use,"-",sep = ""),".tif", sep = Year)) ### Careful to mention that this does not convey any more information
  RCP_4.5 <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW/4.5_",Land_Use,"-",sep = ""),".tif", sep = Year))
  RCP_6.0 <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW/6.0_",Land_Use,"-",sep = ""),".tif", sep = Year))
  RCP_8.5 <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW/8.5_",Land_Use,"-",sep = ""),".tif", sep = Year))
  RCP_2.6 <- raster::mask(RCP_2.6, mask = North_Africa_Mask, inverse = TRUE)
  RCP_4.5 <- raster::mask(RCP_4.5, mask = North_Africa_Mask, inverse = TRUE)
  RCP_6.0 <- raster::mask(RCP_6.0, mask = North_Africa_Mask, inverse = TRUE)
  RCP_8.5 <- raster::mask(RCP_8.5, mask = North_Africa_Mask, inverse = TRUE)
  Stack <- stack(RCP_2.6,RCP_4.5,RCP_6.0,RCP_8.5)  ## Stack each of the RCP scenarios together               ## Stack each of the RCP scenarios together             
}

LU_2015 <- Land_Use_Change(Land_Use, "2015")
Future_LU <- Land_Use_Change(Land_Use, Year)

colours <- c("darkgreen", "forestgreen","green", "greenyellow","darkolivegreen1", "darkseagreen1","lightblue", "coral1", "khaki1","lightgoldenrod", "gold","yellow", "orange", "goldenrod4")

Future_LUC <- Future_LU - LU_2015
plot(Future_LUC, zlim = c(-1, 1 , 0.1), col = colours)
Future_LUC
}

Primary_2050 <- Future_Land_Use_Change("primary", "2050")
Primary_2070 <- Future_Land_Use_Change("primary", "2070")

Secondary_2050 <- Future_Land_Use_Change("secondary", "2050")
Secondary_2070 <- Future_Land_Use_Change("secondary", "2070")

Crop_Light_2050 <- Future_Land_Use_Change("cropland_light", "2050")
Crop_Light_2070 <- Future_Land_Use_Change("cropland_light", "2070")

Crop_minimal_2050 <- Future_Land_Use_Change("cropland_minimal", "2050")
Crop_minimal_2070 <- Future_Land_Use_Change("cropland_minimal", "2070")

Crop_Intense_2050 <- Future_Land_Use_Change("cropland_intense", "2050")
Crop_Intense_2070 <- Future_Land_Use_Change("cropland_intense", "2070")

Pasture_2050 <- Future_Land_Use_Change("pasture", "2050")
Pasture_2070 <- Future_Land_Use_Change("pasture", "2070")

Urban_2050 <- Future_Land_Use_Change("urban", "2050")
Urban_2070 <- Future_Land_Use_Change("urban", "2070")

HPD_2050 <- Future_Land_Use_Change("HPD", "2050")
HPD_2070 <- Future_Land_Use_Change("HPD", "2070")

