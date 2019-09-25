rm(list = ls())
require(rgdal)
require(raster)
require(dplyr)
setwd("../")
setwd("./Combine")

load("RData_North_Africa_Mask.RData")

Land_Use_Binary_2050 <- stack("Binary_Land_Use_2050_Tif_2.tif")
names(Land_Use_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_LD_Binary_2050 <- stack("Binary_Climate_Change_2050_LD_Tif.tif")
names(Climate_Change_LD_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
Richness_Change <- Land_Use_Binary_2050[[RCP[i]]] + Climate_Change_LD_Binary_2050[[RCP[i]]]
Both_Positive <- Richness_Change == 2
Both_Negative <- Richness_Change == -2
Differ <- Richness_Change == 0 


Climate_Change_Negative <-((Land_Use_Binary_2050[[RCP[i]]] == 1) - Both_Positive)
Land_Use_Negative <- ((Climate_Change_LD_Binary_2050[[RCP[i]]] == 1) - Both_Positive)


Both_Positive <- (Both_Positive*4)
Both_Negative <- (Both_Negative*-4)
Land_Use_Negative <- (Land_Use_Negative*-2)
Climate_Change_Negative <- (Climate_Change_Negative*2)



colours <- c("darkblue","forestgreen","orange" ,"lightblue")


Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
plot(Map, col = colours, zlim = c(-5,5,1))
assign(paste("Threat_Map_2050_LD",RCP[i],sep = "_"),Map)
writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2050_LD_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}



Land_Use_Binary_2070 <- stack("Binary_Land_Use_2070_Tif_2.tif")
names(Land_Use_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_LD_Binary_2070 <- stack("Binary_Climate_Change_2070_LD_Tif.tif")
names(Climate_Change_LD_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
  Richness_Change <- Land_Use_Binary_2070[[RCP[i]]] + Climate_Change_LD_Binary_2070[[RCP[i]]]
  Both_Positive <- Richness_Change == 2
  Both_Negative <- Richness_Change == -2
  Differ <- Richness_Change == 0 
  
  
  Climate_Change_Negative <-((Land_Use_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  Land_Use_Negative <- ((Climate_Change_LD_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  
  
  Both_Positive <- (Both_Positive*4)
  Both_Negative <- (Both_Negative*-4)
  Land_Use_Negative <- (Land_Use_Negative*-2)
  Climate_Change_Negative <- (Climate_Change_Negative*2)
  
  
  
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  
  
  Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
  plot(Map, col = colours, zlim = c(-5,5,1))
  assign(paste("Threat_Map_2070_LD",RCP[i],sep = "_"),Map)
  writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2070_LD_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}





###### No Dispersal Scenarios

Land_Use_Binary_2050 <- stack("Binary_Land_Use_2050_Tif_2.tif")
names(Land_Use_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_ND_Binary_2050 <- stack("Binary_Climate_Change_2050_ND_Tif.tif")
names(Climate_Change_ND_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
  Richness_Change <- Land_Use_Binary_2050[[RCP[i]]] + Climate_Change_ND_Binary_2050[[RCP[i]]]
  Both_Positive <- Richness_Change == 2
  Both_Negative <- Richness_Change == -2
  Differ <- Richness_Change == 0 
  
  
  Climate_Change_Negative <-((Land_Use_Binary_2050[[RCP[i]]] == 1) - Both_Positive)
  Land_Use_Negative <- ((Climate_Change_ND_Binary_2050[[RCP[i]]] == 1) - Both_Positive)
  
  
  Both_Positive <- (Both_Positive*4)
  Both_Negative <- (Both_Negative*-4)
  Land_Use_Negative <- (Land_Use_Negative*-2)
  Climate_Change_Negative <- (Climate_Change_Negative*2)
  
  
  
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  
  
  Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
  plot(Map, col = colours, zlim = c(-5,5,1))
  assign(paste("Threat_Map_2050_ND",RCP[i],sep = "_"),Map)
  writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2050_ND_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}



Land_Use_Binary_2070 <- stack("Binary_Land_Use_2070_Tif_2.tif")
names(Land_Use_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_ND_Binary_2070 <- stack("Binary_Climate_Change_2070_ND_Tif.tif")
names(Climate_Change_ND_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
  Richness_Change <- Land_Use_Binary_2070[[RCP[i]]] + Climate_Change_ND_Binary_2070[[RCP[i]]]
  Both_Positive <- Richness_Change == 2
  Both_Negative <- Richness_Change == -2
  Differ <- Richness_Change == 0 
  
  
  Climate_Change_Negative <-((Land_Use_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  Land_Use_Negative <- ((Climate_Change_ND_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  
  
  Both_Positive <- (Both_Positive*4)
  Both_Negative <- (Both_Negative*-4)
  Land_Use_Negative <- (Land_Use_Negative*-2)
  Climate_Change_Negative <- (Climate_Change_Negative*2)
  
  
  
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  
  
  Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
  plot(Map, col = colours, zlim = c(-5,5,1))
  assign(paste("Threat_Map_2070_ND",RCP[i],sep = "_"),Map)
  writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2070_ND_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}




#### Assisted Migration scenarios

Land_Use_Binary_2050 <- stack("Binary_Land_Use_2050_Tif_2.tif")
names(Land_Use_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_AM_Binary_2050 <- stack("Binary_Climate_Change_2050_AM_Tif.tif")
names(Climate_Change_AM_Binary_2050) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
  Richness_Change <- Land_Use_Binary_2050[[RCP[i]]] + Climate_Change_AM_Binary_2050[[RCP[i]]]
  Both_Positive <- Richness_Change == 2
  Both_Negative <- Richness_Change == -2
  Differ <- Richness_Change == 0 
  
  
  Climate_Change_Negative <-((Land_Use_Binary_2050[[RCP[i]]] == 1) - Both_Positive)
  Land_Use_Negative <- ((Climate_Change_AM_Binary_2050[[RCP[i]]] == 1) - Both_Positive)
  
  
  Both_Positive <- (Both_Positive*4)
  Both_Negative <- (Both_Negative*-4)
  Land_Use_Negative <- (Land_Use_Negative*-2)
  Climate_Change_Negative <- (Climate_Change_Negative*2)
  
  
  
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  
  
  Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
  plot(Map, col = colours, zlim = c(-5,5,1))
  assign(paste("Threat_Map_2050_AM",RCP[i],sep = "_"),Map)
  writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2050_AM_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}



Land_Use_Binary_2070 <- stack("Binary_Land_Use_2070_Tif_2.tif")
names(Land_Use_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

Climate_Change_AM_Binary_2070 <- stack("Binary_Climate_Change_2070_AM_Tif.tif")
names(Climate_Change_AM_Binary_2070) <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

RCP <-c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")

for(i in 1:length(RCP)){
  Richness_Change <- Land_Use_Binary_2070[[RCP[i]]] + Climate_Change_AM_Binary_2070[[RCP[i]]]
  Both_Positive <- Richness_Change == 2
  Both_Negative <- Richness_Change == -2
  Differ <- Richness_Change == 0 
  
  
  Climate_Change_Negative <-((Land_Use_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  Land_Use_Negative <- ((Climate_Change_AM_Binary_2070[[RCP[i]]] == 1) - Both_Positive)
  
  
  Both_Positive <- (Both_Positive*4)
  Both_Negative <- (Both_Negative*-4)
  Land_Use_Negative <- (Land_Use_Negative*-2)
  Climate_Change_Negative <- (Climate_Change_Negative*2)
  
  
  
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  
  
  Map <- Both_Positive + Both_Negative + Land_Use_Negative + Climate_Change_Negative
  plot(Map, col = colours, zlim = c(-5,5,1))
  assign(paste("Threat_Map_2070_AM",RCP[i],sep = "_"),Map)
  writeRaster(Map, filename = paste("Threat_Maps_2/Threat_Map_2070_AM_",".tif", sep = RCP[i]),format = "GTiff", overwrite = TRUE)
}




Save_Threat_Map <- function(Date, Dispersal, RCP){ 
  colours <- c("darkblue","forestgreen","orange" ,"lightblue")
  Map <- raster(paste(paste("Threat_Maps_2/Threat_Map",Date,Dispersal,RCP, sep = "_"),".tif", sep = ""))
  Map <- raster::mask(Map, mask = North_Africa_Mask, inverse = TRUE)
  png(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/Threat_Maps_2/Threat_Map_",paste(Date,Dispersal,RCP,sep = "_"),".png",sep = ""), width=4, height=4, units="in", res=300)
  plot(Map, zlim = c(-5, 5), axes = FALSE, legend = FALSE, box = FALSE, col = colours)
  dev.off()
  plot(Map, zlim = c(-5, 5), axes = FALSE, legend = FALSE, box = FALSE, col = colours)
}


#### 4 = both positive
#### -4 = both Negative 
#### Land-Use Negative (Climate change positive) = -2
#### Climate change negative ( Land-Use Chane postive) = 2


Save_Threat_Map("2050", "LD", "RCP_2.6")
Save_Threat_Map("2050", "LD", "RCP_4.5")
Save_Threat_Map("2050", "LD", "RCP_6.0")
Save_Threat_Map("2050", "LD", "RCP_8.5")

Save_Threat_Map("2070", "LD", "RCP_2.6")
Save_Threat_Map("2070", "LD", "RCP_4.5")
Save_Threat_Map("2070", "LD", "RCP_6.0")
Save_Threat_Map("2070", "LD", "RCP_8.5")

Save_Threat_Map("2050", "ND", "RCP_2.6")
Save_Threat_Map("2050", "ND", "RCP_4.5")
Save_Threat_Map("2050", "ND", "RCP_6.0")
Save_Threat_Map("2050", "ND", "RCP_8.5")

Save_Threat_Map("2070", "ND", "RCP_2.6")
Save_Threat_Map("2070", "ND", "RCP_4.5")
Save_Threat_Map("2070", "ND", "RCP_6.0")
Save_Threat_Map("2070", "ND", "RCP_8.5")

Save_Threat_Map("2050", "AM", "RCP_2.6")
Save_Threat_Map("2050", "AM", "RCP_4.5")
Save_Threat_Map("2050", "AM", "RCP_6.0")
Save_Threat_Map("2050", "AM", "RCP_8.5")

Save_Threat_Map("2070", "AM", "RCP_2.6")
Save_Threat_Map("2070", "AM", "RCP_4.5")
Save_Threat_Map("2070", "AM", "RCP_6.0")
Save_Threat_Map("2070", "AM", "RCP_8.5")

