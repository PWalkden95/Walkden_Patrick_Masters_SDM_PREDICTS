rm(list= ls())
require(biomod2)    ### to make the projections
require(raster)      ## working with rasters 
require(dplyr)       ## data wrangling
require(magrittr)     ## piping
require(sp)          ## working with spatial data 
require(doParallel)
#dir.create("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections")

setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs")

load("RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData") #### Load the chosen models for each species
load("RData_European_Bee_Species_OLE_Coord_Limits.RData")            ### Load the OLE projection bounds 




Get_projection <- function(date, RCP){

Get_future <- function(date, RCP){

Mean_future <- function(date, RCP, Bioclim_Var){
NASA <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/NASA_GISS-E2-R/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "gs"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
MET <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_Met_Office_HadGEM2-ES/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "he"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
NCCSR <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_National_Center_For_Climate_System_Research_CCSM4/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "cc"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
UOT <- raster(paste(paste(paste(paste("Raw_Data/Bioclimatic_Variables/Future/","/The_University_Of_Tokyo_MIROC5/", sep = paste(date,RCP,sep = "/RCP_")),sub("[.]","",RCP),sep = "mc"),sub("20","",date),sep = "bi"),".tif",sep = as.character(Bioclim_Var)))
Stack<- stack(NASA,MET,NCCSR,UOT)
Mean <- mean(Stack)
}

Bioclim_1 <- Mean_future(date, RCP, "1")
Bioclim_2 <- Mean_future(date, RCP, "4")
Bioclim_3 <- Mean_future(date, RCP, "12")
Bioclim_4 <- Mean_future(date, RCP, "15")
Bioclim_Stack <- stack(Bioclim_1/10, Bioclim_2/10, Bioclim_3, Bioclim_4)
}


setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs")
future_Bioclim <- Get_future(date, RCP)
load("RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData") 
dir.create(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future",date,RCP, sep = "_"))


Europe_Extent <- extent(-25, 50, 30, 75) 

setwd("D:/SDM_2/SDM")

alphaMap <- reclassify(subset(future_Bioclim,1), c(-Inf,Inf,0))
alphaMap <- crop(alphaMap, Europe_Extent)



registerDoParallel(cores = 4)

foreach(i = 1:length(Model_Outs),
.packages = c("biomod2","raster","dplyr","magrittr","sp")) %dopar% {
  MyBiomodEnsemble <- BIOMOD_EnsembleModeling(modeling.output =  Model_Outs[[i]],
                                              chosen.models = c(grep("RF", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("GBM", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("FDA", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("GAM", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("MARS", get_built_models(Model_Outs[[i]]), value = TRUE)),
                                              eval.metric = c("ROC"),
                                              eval.metric.quality.threshold = c(0.8),
                                              em.by = "all",
                                              prob.mean = T,
                                              prob.median = T,
                                              committee.averaging = T,
                                              prob.mean.weight = T,
                                              prob.mean.weight.decay = 'proportional')
  
  Europe_future <- stack(crop(future_Bioclim,Europe_Extent))
  
  
  myBiomodProj <- BIOMOD_Projection(
    modeling.output = Model_Outs[[i]],
    new.env = Europe_future,
    proj.name = paste("future",date,RCP,sep = "_"),  
    selected.models = c(grep("RF", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("GBM", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("FDA", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("GAM", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("MARS", get_built_models(Model_Outs[[i]]), value = TRUE)),
    binary.meth = 'TSS',
    compress = 'xz',
    clamping.mask = T,
    output.format = '.grd')
  
  
  myBiomodEF <- BIOMOD_EnsembleForecasting(
    EM.output = MyBiomodEnsemble,
    projection.output = myBiomodProj)
  
  Spp_Name <- Model_Outs[[i]]@sp.name
  
  proj <- raster(paste(paste(paste(Spp_Name,paste(paste(paste("proj_future",date,RCP, sep = "_"),"/proj_future", sep = ""),date,RCP,sep = "_"),sep = "/"),Spp_Name,sep = "_"),"_TSSbin.grd",sep = ""))
  
  
  spp_alphaMap <- alphaMap + 
    subset(stack(proj),1)
  
  dir.create(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future",date,RCP,sep = "_"), Spp_Name, sep = "/"))
  png(paste(paste(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future",date,RCP,sep = "_"), Spp_Name, sep = "/")),"/1_Raster_Image.png",sep = ""), width=4, height=4, units="in", res=300)
  plot(spp_alphaMap)
  dev.off()
  
  writeRaster(spp_alphaMap,filename = paste(paste(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future",date,RCP,sep = "_"), Spp_Name, sep = "/")),"/2_Raster_Tif",sep = ""),format = "GTiff", overwrite = TRUE)
  }
registerDoSEQ()

}



Get_projection("2050", "2.6")
Get_projection("2050", "4.5")
Get_projection("2050", "6.0")
Get_projection("2050", "8.5")

Get_projection("2070", "2.6")
Get_projection("2070", "4.5")
Get_projection("2070", "6.0")
Get_projection("2070", "8.5")


