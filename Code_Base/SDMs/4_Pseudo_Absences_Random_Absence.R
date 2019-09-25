rm(list = ls())
require(dplyr)
require(tidyr)
require(biomod2)
require(magrittr)
require(rgdal)
require(sp)
require(dismo)
require(mopa)
require(raster)
require(doParallel)

load("RData_European_Bee_Species_Thinned_Data_5km.RData")
Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")
Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")
Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")
Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")   

Bioclim_Stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15)


registerDoParallel(cores = 4)

Random_Absence_Data <- foreach(i = 1:length(Thinned_Data_5km),
                               .packages = c("mopa","dplyr","tidyr","sp","raster","magrittr")) %dopar% {
spp_xy <- Thinned_Data_5km[[i]][c("decimallongitude","decimallatitude")]           ### extract the coordinates for each species
xmin <- floor(min(spp_xy$decimallongitude))                            ## min long ---- Possibly add buffer to the extent that pseudo absences are chosen from.
xmax <- ceiling(max(spp_xy$decimallongitude))                          ## max long
ymin <- floor(min(spp_xy$decimallatitude))                             ## min lat
ymax <- ceiling(max(spp_xy$decimallatitude))                           ## max at

ex <- extent(xmin, xmax, ymin, ymax)                                   ## create and extent raster
bg <- crop(x = Bioclim_Stack, y = ex)                                  ## crop bioclimatic variable raster stack to the extent size

background <- backgroundGrid(bg)                                       ### create a background grid to define the area for pseudoabsences to be chosen from 
unsuitable_bg <- OCSVMprofiling(xy = spp_xy, varstack = Bioclim_Stack,background = background$xy)  ## define a area that is climatically suitable for the species and exlude these areas from being chosen as absences 

pseudo_abs_10 <- pseudoAbsences(xy= spp_xy, background = unsuitable_bg$absence,              ### choose pseudo-absences points from the background grid (excluding the areas highlighted) 
                             prevalence = -4, kmeans = FALSE,exclusion.buffer = 0.0415)   ## prevalence - proportion of points that are presence point -- exclusion buffer set a 5km - so pseudoabsence points are at least 5km away from a presence point so grid cells will not contain bouth an absence and a presence point.
pseudo_abs_10 <- pseudo_abs_10[["species1"]][["PA01"]][[1]] %>% filter( v == 0)                ## filter out just the pseudo-absences generated 


pseudo_abs_1 <-pseudoAbsences(xy = spp_xy, background = unsuitable_bg$absence,
                              prevalence = 0.5, kmeans = FALSE, exclusion.buffer = 0.0415)
pseudo_abs_1 <- pseudo_abs_1[["species1"]][["PA01"]][[1]] %>% filter( v == 0) 

Random_Absence_Data <- list(pseudo_abs_10,pseudo_abs_1)
names(Random_Absence_Data)[1:2] <- c("Random_Absence_10x","Random_Absence_1x")
Random_Absence_Data
}                                                                          

for(i in 1:length(Random_Absence_Data)){
  names(Random_Absence_Data)[i] <- paste(Thinned_Data_5km[[i]][["species"]][1])
}

save(file = "RData_European_Bee_Species_Random_Absence_Data.RData", Random_Absence_Data)


