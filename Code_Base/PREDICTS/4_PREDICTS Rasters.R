rm(list = ls())
require(lme4)
require(raster)
require(ncdf4)
require(RColorBrewer)
require(tidyr)
require(rgdal)
setwd("../")
setwd("./PREDICTS")

load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_North_Africa_Mask.RData")

Europe_Extent <- extent(-10, 36, 30, 75) 

Year <- c("2015","2050","2070")


for( i in 1:length(Year)){   ### Extract LU change rasters and upsample them so that they are at the same resolution as Climate change projections
RCP_2.6 <- raster(paste("LUH2_rasters_PW2/two_PREDICTS_RCP2.6-",".tif", sep = Year[i])) ### Careful to mention that this does not convey any more information
RCP_4.5 <- raster(paste("LUH2_rasters_PW2/two_PREDICTS_RCP4.5-",".tif", sep = Year[i]))
RCP_6.0 <- raster(paste("LUH2_rasters_PW2/two_PREDICTS_RCP6.0-",".tif", sep = Year[i]))
RCP_8.5 <- raster(paste("LUH2_rasters_PW2/two_PREDICTS_RCP8.5-",".tif", sep = Year[i]))
RCP_2.6 <- disaggregate(RCP_2.6, fact = 0.25/ 0.04166667)
RCP_2.6 <- mask(RCP_2.6, mask = North_Africa_Mask, inverse = TRUE)
RCP_4.5 <- disaggregate(RCP_4.5, fact = 0.25/ 0.04166667)
RCP_4.5 <- mask(RCP_4.5, mask = North_Africa_Mask, inverse = TRUE)
RCP_6.0 <- disaggregate(RCP_6.0, fact = 0.25/ 0.04166667)
RCP_6.0 <- mask(RCP_6.0, mask = North_Africa_Mask, inverse = TRUE)
RCP_8.5 <- disaggregate(RCP_8.5, fact = 0.25/ 0.04166667)
RCP_8.5 <- mask(RCP_8.5, mask = North_Africa_Mask, inverse = TRUE)
assign(paste("PREDICTS_Stack",Year[i], sep = "_"), crop(stack(RCP_2.6,RCP_4.5,RCP_6.0,RCP_8.5),Europe_Extent))  ## Stack each of the RCP scenarios together             
}


plot(PREDICTS_Stack_2015)
plot(PREDICTS_Stack_2050)
plot(PREDICTS_Stack_2070)




LU_2050 <- log(PREDICTS_Stack_2050/PREDICTS_Stack_2015) #### get the log response ratio of relative species richness for each RCP scenario
names(LU_2050) <- c("Log_Response_Ratio_2050_RCP2.6",
                    "Log_Response_Ratio_2050_RCP4.5",
                    "Log_Response_Ratio_2050_RCP6.0",
                    "Log_Response_Ratio_2050_RCP8.5")
plot(LU_2050,col = rev(terrain.colors(10)), zlim=c(min(LU_2050@data@min)-0.1,max(LU_2050@data@max)+0.1))



LU_2070 <- log(PREDICTS_Stack_2070/PREDICTS_Stack_2015)  ## same for 2070
names(LU_2070) <- c("Log_Response_Ratio_2070_RCP2.6",
                    "Log_Response_Ratio_2070_RCP4.5",
                    "Log_Response_Ratio_2070_RCP6.0",
                    "Log_Response_Ratio_2070_RCP8.5")
plot(LU_2070,col = rev(terrain.colors(10)),zlim=c(min(LU_2070@data@min)-0.1,max(LU_2070@data@max)+0.1))


LU_2050_frame <- as.data.frame(LU_2050)     ### extract the values into a data frame for each time period
LU_2070_frame <- as.data.frame(LU_2070)

save(LU_2050_frame, file = "C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_PREDICTS_Land_Use_Response_Ratio_2050_2.RData")   ### save as RData for later to be able to combine with climate change estimates
save(LU_2070_frame, file = "C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_PREDICTS_Land_Use_Response_Ratio_2070_2.RData")



#### Now just going to create binary maps to indicate whether it is predicted that species richness will either go up or go down for a given grid cell


bin_LU_2050 <- reclassify(LU_2050, c(0, 1, 1))
bin_LU_2050 <- reclassify(bin_LU_2050, c(-1.5, 0, -1))
bin_LU_2050 <- reclassify(bin_LU_2050, c(0,0,0))

names(bin_LU_2050) <- c("Binary_Log_Response_Ratio_2050_RCP2.6",
                        "Binary_Log_Response_Ratio_2050_RCP4.5",
                        "Binary_Log_Response_Ratio_2050_RCP6.0",
                        "Binary_Log_Response_Ratio_2050_RCP8.5")

plot(bin_LU_2050, zlim=c(-1.3,1.3))


writeRaster(stack(bin_LU_2050), filename="C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/Binary_Land_Use_2050_Tif_2.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

bin_LU_2070 <- reclassify(LU_2070, c(0, 1, 1))
bin_LU_2070 <- reclassify(bin_LU_2070, c(-1.5, 0, -1))
bin_LU_2070 <- reclassify(bin_LU_2070, c(0,0,0))

names(bin_LU_2070) <- c("Binary_Log_Response_Ratio_2070_RCP2.6",
                        "Binary_Log_Response_Ratio_2070_RCP4.5",
                        "Binary_Log_Response_Ratio_2070_RCP6.0",
                        "Binary_Log_Response_Ratio_2070_RCP8.5")


plot(bin_LU_2070, zlim=c(-1.3,1.3))

writeRaster(stack(bin_LU_2070), filename="C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/Binary_Land_Use_2070_Tif_2.tif", options="INTERLEAVE=BAND", overwrite=TRUE)



