rm(list = ls())
require(raster)
setwd("../")
setwd("./SDMs")


Europe_Extent <- extent(-10, 36, 30, 75) 

Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Current_Bioclim_stack <- crop(stack(Bioclim_01,
                       Bioclim_04,
                       Bioclim_12,
                       Bioclim_15),Europe_Extent)

Future_Climate_Change <- function(date){

Get_future <- function(date, RCP){
  
  Mean_future <- function(date, RCP, Bioclim_Var){
    NASA <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/NASA_GISS-E2-R/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "gs"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
    MET <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_Met_Office_HadGEM2-ES/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "he"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
    NCCSR <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_National_Center_For_Climate_System_Research_CCSM4/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "cc"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
    UOT <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_University_Of_Tokyo_MIROC5/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "mc"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
    Stack<- crop(stack(NASA,MET,NCCSR,UOT), Europe_Extent)
    Mean <- mean(Stack)
  }
  
  Bioclim_1 <- Mean_future(date, RCP, "1")
  Bioclim_2 <- Mean_future(date, RCP, "4")
  Bioclim_3 <- Mean_future(date, RCP, "12")
  Bioclim_4 <- Mean_future(date, RCP, "15")
  Bioclim_Stack <- stack(Bioclim_1/10, Bioclim_2/10, Bioclim_3, Bioclim_4)
}

Future_CC_Change_2.6 <- Get_future(date, "2.6")
Future_CC_Change_4.5 <- Get_future(date, "4.5")
Future_CC_Change_6.0 <- Get_future(date, "6.0")
Future_CC_Change_8.5 <- Get_future(date, "8.5")


Future_CC_Change_Ann_Temp <- stack(Future_CC_Change_2.6[[1]],
                                   Future_CC_Change_4.5[[1]],
                                   Future_CC_Change_6.0[[1]],
                                   Future_CC_Change_8.5[[1]])


Future_CC_Change_Seas_Temp <- stack(Future_CC_Change_2.6[[2]],
                              Future_CC_Change_4.5[[2]],
                              Future_CC_Change_6.0[[2]],
                              Future_CC_Change_8.5[[2]])

Future_CC_Change_Ann_Precip <- stack(Future_CC_Change_2.6[[3]],
                                           Future_CC_Change_4.5[[3]],
                                           Future_CC_Change_6.0[[3]],
                                           Future_CC_Change_8.5[[3]])


Future_CC_Change_Seas_Precip <- stack(Future_CC_Change_2.6[[4]],
                                              Future_CC_Change_4.5[[4]],
                                              Future_CC_Change_6.0[[4]],
                                              Future_CC_Change_8.5[[4]])
 


Future_Climate_ATC <- Future_CC_Change_Ann_Temp - Current_Bioclim_stack[[1]]
names(Future_Climate_ATC) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")
Future_Climate_TSC <- Future_CC_Change_Seas_Temp - Current_Bioclim_stack[[2]]
names(Future_Climate_TSC) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")
Future_Climate_APC <- Future_CC_Change_Ann_Precip - Current_Bioclim_stack[[3]]
names(Future_Climate_APC) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")
Future_Climate_PSC <- Future_CC_Change_Seas_Temp - Current_Bioclim_stack[[4]]
names(Future_Climate_PSC) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

CC_list <- list(Future_Climate_ATC, Future_Climate_TSC, Future_Climate_APC, Future_Climate_PSC)
names(CC_list) <- c("Future_Climate_Annual_Temperature_Change",
                    "Future_Climate_Temperature_Seasonality_Change",
                    "Future_Climate_Annual_Precipitation_Change",
                    "Future_Climate_Precipitation_Seasonality_Change")

CC_list
}


Bioclim_Change_2050 <- Future_Climate_Change("2050")
Bioclim_Change_2070 <- Future_Climate_Change("2070")


plot(Bioclim_Change_2050[[1]], zlim = c(0,10,1))
plot(Bioclim_Change_2070[[1]])

plot(Bioclim_Change_2050[[2]])
plot(Bioclim_Change_2070[[2]])

summary(Bioclim_Change_2050[[3]])
plot(Bioclim_Change_2050[[3]], zlim = c(-100,0))
plot(Bioclim_Change_2070[[3]])

plot(Bioclim_Change_2050[[4]])
plot(Bioclim_Change_2070[[4]])


RCP8.5 <- stack(Bioclim_Change_2070[[1]][[4]],
                Bioclim_Change_2070[[2]][[4]],
                Bioclim_Change_2070[[3]][[4]],
                Bioclim_Change_2070[[4]][[4]])

RCP2.6 <- stack(Bioclim_Change_2070[[1]][[1]],
                Bioclim_Change_2070[[2]][[1]],
                Bioclim_Change_2070[[3]][[1]],
                Bioclim_Change_2070[[4]][[1]])


plot(crop(Bioclim_01, Europe_Extent))
plot(RCP8.5[[1]])

plot(crop(Bioclim_04, Europe_Extent))
plot(reclassify(RCP8.5[[2]], c(-Inf, 0, NA)))

plot(crop(Bioclim_12,Europe_Extent))
plot(reclassify(RCP2.6[[3]], c(0, Inf, NA)))

plot(crop(Bioclim_15,Europe_Extent))
plot(RCP8.5[[4]])

current <- raster("Projections/Current_2/All_bee_Raster_Tif.tif")
Current_2 <- raster("Projections/Unbound_Current_2/All_bees_Raster_Tif.tif")
lala <- raster("Projections/Future_2070_2.6/All_bee_Raster_LD_Tif.tif")
lalala <- raster("Projections/Future_2070_4.5/All_bee_Raster_LD_Tif.tif")
lalalala <- raster("Projections/Future_2070_6.0/All_bee_Raster_LD_Tif.tif")
lalalalala <- raster("Projections/Future_2070_8.5/All_bee_Raster_LD_Tif.tif")
plot(current)
plot(reclassify(lalalalala -current,c(0, Inf, NA)))
plot(lala)
plot(lalala)
plot(lalalala)
plot(lalalalala)
