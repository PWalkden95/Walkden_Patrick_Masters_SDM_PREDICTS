rm(list= ls())
require(biomod2)    ### to make the projections
require(raster)      ## working with rasters 
require(dplyr)       ## data wrangling
require(magrittr)     ## piping
require(sp)
require(REdaS)
require(dplyr)
require(rgdal)
setwd("../")
setwd("./SDMs")

load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/RData_European_Bee_LU_SSI.RData")
load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_North_Africa_Mask.RData")

load("RData_European_Bee_Species_OLE_Coord_Limits.RData") 
load("RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData")

raster <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")
Europe_Extent <- extent(-10, 36, 30, 75) 
alphaMap <- reclassify(subset(raster,1), c(-Inf,Inf,0))
alphaMap <- crop(alphaMap, Europe_Extent)


species <- c()
for(i in 1:nrow(European_Bee_SSI)){
  species[i] <- sub(" ", ".", European_Bee_SSI[i,1])
}
species <- as.data.frame(species)

species$OLE_name <- sub("[.]"," ",species$species)

SSI_OLE_Coord_Limits <- OLE_Coord_Limits %>%
  filter(Spp_name %in% species$OLE_name)



get_climate_SSI <- function(species){

Climate_SSI_calc <- function(Date, RCP){
  Current <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Unbound_Current_2/","/2_Raster_Tif.tif", sep = Model_Outs[[i]]@sp.name))
  Current <- raster::mask(Current, mask = North_Africa_Mask, inverse = TRUE)
  Current <- crop(Current, Europe_Extent)
  Current_Occ <- alphaMap + Current
  AM_Occ <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_",paste("/",Model_Outs[[i]]@sp.name,"/2_Raster_Tif.tif",sep = ""),sep = paste(Date,RCP, sep = "_")))
  AM_Occ <- alphaMap + AM_Occ
  AM_Occ <- raster::mask(AM_Occ, mask = North_Africa_Mask, inverse = TRUE)
  SSI_AM <- log(sum(AM_Occ@data@values, na.rm = TRUE)/sum(Current_Occ@data@values, na.rm = TRUE)) 

  
  if(OLE_Coord_Limits[i,2] < -25 | OLE_Coord_Limits[i,2] == "NaN" ){
    xmin <- -25
  } else {
    xmin <- OLE_Coord_Limits[i, 2]
  }
  if(OLE_Coord_Limits[i,4] > 50 | OLE_Coord_Limits[i,4] == "NaN"){
    xmax <- 50
  } else {
    xmax <- OLE_Coord_Limits[i, 4]
  }
  if(OLE_Coord_Limits[i,6] < 30 | OLE_Coord_Limits[i,6] == "NaN"){
    ymin <- 30
  } else {
    ymin <- OLE_Coord_Limits[i, 6]
  }
  if(OLE_Coord_Limits[i,8] > 75 | OLE_Coord_Limits[i,8] == "NaN"){
    ymax <- 75
  } else {
    ymax <- OLE_Coord_Limits[i, 8]
  }
  
  No_Dispersal_extent <- extent(xmin, xmax, ymin, ymax)
  
  ND_Occ <- crop(AM_Occ, No_Dispersal_extent)
  ND_Occ <- (ND_Occ + Current)
  ND_Occ <- extend(ND_Occ, Europe_Extent)
  ND_Occ <- reclassify(ND_Occ, c(NA,NA,0))
  ND_Occ <- alphaMap + ND_Occ == 2
  ND_Occ <- raster::mask(ND_Occ, mask = North_Africa_Mask, inverse = TRUE)
  SSI_ND <- log(sum(ND_Occ@data@values, na.rm = TRUE)/sum(Current_Occ@data@values, na.rm = TRUE))
  
  if(Date == "2050"){
  Limited_Dispesal_extent <- extent(xmin-(350/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))), 
                                    xmax + (350/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))),
                                    ymin - 350/110.574,
                                    ymax + 350/110.574)
  } else {
    Limited_Dispesal_extent <- extent(xmin-(550/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))), 
                                      xmax + (550/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))),
                                      ymin - 550/110.574,
                                      ymax + 550/110.574)
  }
  
  LD_Occ <- crop(AM_Occ, Limited_Dispesal_extent)
  LD_Occ <- extend(LD_Occ, Europe_Extent)
  LD_Occ <- reclassify(LD_Occ, c(NA,NA,0))
  LD_Occ <- alphaMap + LD_Occ
  LD_Occ <- raster::mask(LD_Occ, mask = North_Africa_Mask, inverse = TRUE)
  SSI_LD <- log(sum(LD_Occ@data@values, na.rm = TRUE)/sum(Current_Occ@data@values, na.rm = TRUE))
  
  SSI <- data.frame(Scientific_Name = species,SSI_AM = SSI_AM, SSI_LD = SSI_LD, SSI_ND = SSI_ND)
}

SSI_2050_2.6 <- Climate_SSI_calc("2050", "2.6")
colnames(SSI_2050_2.6)[2:4] <- c("SSI_AM_2050_2.6","SSI_LD_2050_2.6","SSI_ND_2050_2.6")
SSI_2050_4.5 <- Climate_SSI_calc("2050", "4.5")
colnames(SSI_2050_4.5)[2:4] <- c("SSI_AM_2050_4.5","SSI_LD_2050_4.5","SSI_ND_2050_4.5")
SSI_2050_6.0 <- Climate_SSI_calc("2050", "6.0")
colnames(SSI_2050_6.0)[2:4] <- c("SSI_AM_2050_6.0","SSI_LD_2050_6.0","SSI_ND_2050_6.0")
SSI_2050_8.5 <- Climate_SSI_calc("2050", "8.5")
colnames(SSI_2050_8.5)[2:4] <- c("SSI_AM_2050_8.5","SSI_LD_2050_8.5","SSI_ND_2050_8.5")

SSI_2070_2.6 <- Climate_SSI_calc("2070", "2.6")
colnames(SSI_2070_2.6)[2:4] <- c("SSI_AM_2070_2.6","SSI_LD_2070_2.6","SSI_ND_2070_2.6")
SSI_2070_4.5 <- Climate_SSI_calc("2070", "4.5")
colnames(SSI_2070_4.5)[2:4] <- c("SSI_AM_2070_4.5","SSI_LD_2070_4.5","SSI_ND_2070_4.5")
SSI_2070_6.0 <- Climate_SSI_calc("2070", "6.0")
colnames(SSI_2070_6.0)[2:4] <- c("SSI_AM_2070_6.0","SSI_LD_2070_6.0","SSI_ND_2070_6.0")
SSI_2070_8.5 <- Climate_SSI_calc("2070", "8.5")
colnames(SSI_2070_8.5)[2:4] <- c("SSI_AM_2070_8.5","SSI_LD_2070_8.5","SSI_ND_2070_8.5")


SSI_calc <- cbind(SSI_2050_2.6, SSI_2050_4.5[,2:4],SSI_2050_6.0[,2:4],SSI_2050_8.5[,2:4],
                  SSI_2070_2.6[2:4], SSI_2070_4.5[,2:4],SSI_2070_6.0[,2:4],SSI_2070_8.5[,2:4])
}


Climate_SSI <- c()      
for(i in 1:length(Model_Outs)){
  SSI <- get_climate_SSI(Model_Outs[[i]]@sp.name)
  Climate_SSI <- rbind(Climate_SSI,SSI)
}

plot(Climate_SSI$SSI_LD_2070_2.6)

save(Climate_SSI, file = "C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_European_All_Bee_CC_SSI.RData")
