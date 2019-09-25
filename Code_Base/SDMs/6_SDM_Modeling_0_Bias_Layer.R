rm(list = ls())
library(raster) # spatial data manipulation
library(MASS) # for 2D kernel density function
library(magrittr) # for piping functionality, i.e., %>%
library(maptools) # reading shapefiles

### THIS BIAS LAYER CAN BE USED FOR MAXENT SDM MODELLING HOWEVER BIOMOD2 DIDNT HAVE THE FUNTIONALITY TO BE ABLE TO INCOROPORATE IT INTO THE MODELLING

load("RData_European_Bee_Species_Thinned_Data_5km.RData")

Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Bioclim_Stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15) ## C


Bee_data <- c()
for( i in 1:length(Thinned_Data_5km)){
  Bee_data <- rbind(Bee_data, Thinned_Data_5km[[i]][c("species","decimallongitude","decimallatitude")])
}

Europe_Extent <- extent(-25, 50, 30, 75)
Europe_Map <- crop(Bioclim_Stack,Europe_Extent)


Europe <- reclassify(subset(Europe_Map,1), c(-Inf,Inf,0))


occur.ras <- rasterize(Bee_data[,c(2:3)], Bioclim_Stack, 1)
Europe_ras <- crop(occur.ras, Europe_Extent)
Europe_ras <- reclassify(Europe_ras,c(NA,NA,0))


Europe_occ <- Europe + Europe_ras
plot(Europe_occ)

presences <- which(values(Europe_occ) == 1)
pres.locs <- coordinates(Europe_occ)[presences, ]


dens <- kde2d(pres.locs[,1], pres.locs[,2], n = c(nrow(Europe_occ), ncol(Europe_occ)))
dens.ras <- raster(dens)
plot(dens.ras)


writeRaster(dens.ras, "European_Bee_Species_Bias_Layer.tif")
