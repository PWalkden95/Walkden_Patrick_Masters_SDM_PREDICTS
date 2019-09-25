rm(list= ls())
require(biomod2)
require(dismo)
require(raster)
require(magrittr)
require(foreach)
require(doParallel)
require(rJava)
require(ff)

load("RData_European_Bee_Species_Thinned_Data_5km.RData")     ### Load Presence and Absence Data
load("RData_European_Bee_Species_Occ_Absence_Data.RData")
load("RData_European_Bee_Species_Random_Absence_Data.RData")


Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")  ## Load Bioclimatic variable Data

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Bioclim_Stack <- stack(Bioclim_01, Bioclim_04, Bioclim_12, Bioclim_15)           ## Combine Bioclimatic variables into a stack 


setwd("D:/SDM")
                                

## This script will take a long time to do as it will have to do all three models for all 367 species so I think I will look into running this in parallel
## in the future but I will have to see how I would do that.

registerDoParallel(cores = 4)


   
SDM_Evaluations <- foreach(i = 1:length(Thinned_Data_5km),
        .packages = c("biomod2", "dismo", "raster", "rJava", "dplyr", "magrittr"),
        .combine = "rbind") %dopar% {          
Spp_name <- Thinned_Data_5km[[i]][["species"]][1]                               ## For the thinned data extract : species Name
Spp_Presence <- Thinned_Data_5km[[i]][c("decimallongitude","decimallatitude")]    ### Longitude and latitude
Spp_Presence$presence <- 1                                                      ### Code Presences as 1

Rnm_Spp_Absence_10x <- Random_Absence_Data[[Spp_name]][[1]]                                   ### Extract absence data where absences are 10x greater than presences 
colnames(Rnm_Spp_Absence_10x)[1:3] <- c("decimallongitude","decimallatitude", "presence")   ## Rename columns to match presence set

Occ_Spp_Absence <- Occ_Absence_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"Occ_Absence_Data", sep = "_")]][c(1:2,4)]                                   ### Do the same for the occurrence absences

Spp_Data_10x <- rbind(Spp_Presence,Rnm_Spp_Absence_10x,Occ_Spp_Absence)                    ### Bind all three datasets together to have the presence/absence dataset
RespVar_10x <- as.numeric(Spp_Data_10x$presence)                                       ## Seperate Response variable (Presence/absence)
RespVarCoord_10x <- Spp_Data_10x[1:2]                                                  ## And Coordinates

xmin_10x <- floor(min(RespVarCoord_10x$decimallongitude))                             #### Get the extent of the SDMs 
xmax_10x <- ceiling(max(RespVarCoord_10x$decimallongitude))
ymin_10x <- floor(min(RespVarCoord_10x$decimallatitude))
ymax_10x <- ceiling(max(RespVarCoord_10x$decimallatitude)) 

extent_10x <- extent(xmin_10x,xmax_10x,ymin_10x,ymax_10x)
ExplVar_10x<- stack(crop(Bioclim_Stack, extent_10x))                                   ### Crop Bioclimatic Stack to the right extent




###### Make another Dataset for the equl number of absences as presences


Rnm_Spp_Absence_1x <- Random_Absence_Data[[Spp_name]][[2]]                                   ### Extract absence data where absences are 10x greater than presences 
colnames(Rnm_Spp_Absence_1x)[1:3] <- c("decimallongitude","decimallatitude", "presence")   ## Rename columns to match presence set


Spp_Data_1x <- rbind(Spp_Presence,Rnm_Spp_Absence_1x,Occ_Spp_Absence)                    ### Bind all three datasets together to have the presence/absence dataset
RespVar_1x <- as.numeric(Spp_Data_1x$presence)                                       ## Seperate Response variable (Presence/absence)
RespVarCoord_1x <- Spp_Data_1x[1:2]                                                  ## And Coordinates

xmin_1x <- floor(min(RespVarCoord_1x$decimallongitude))                             #### Get the extent of the SDMs 
xmax_1x <- ceiling(max(RespVarCoord_1x$decimallongitude))
ymin_1x <- floor(min(RespVarCoord_1x$decimallatitude))
ymax_1x <- ceiling(max(RespVarCoord_1x$decimallatitude)) 

extent_1x <- extent(xmin_1x,xmax_1x,ymin_1x,ymax_1x)
ExplVar_1x<- stack(crop(Bioclim_Stack, extent_1x))                                   ### Crop Bioclimatic Stack to the right extent


### Now have two datasets Spp_Data_1x and the rest and Spp_Data_10x and the rest to make comparisons.

  
BiomodData_10x <- BIOMOD_FormatingData(resp.var = RespVar_10x,                       ### Biomod2 will format the data so that it is suitable to run SDMs with 
                                   expl.var = ExplVar_10x,                     ## This is done by basically creating two matricies one of the response variable and sampling unit
                                   resp.xy = RespVarCoord_10x,                  ## the other being explanatory variable and sampling unit 
                                   resp.name = Spp_name)

BiomodData_1x <- BIOMOD_FormatingData(resp.var = RespVar_1x,                       
                                       expl.var = ExplVar_1x,                     
                                       resp.xy = RespVarCoord_1x,                 
                                       resp.name = Spp_name)


myBiomodOptions_1x <- BIOMOD_ModelingOptions(
  MAXENT.Phillips = list(path_to_maxent.jar = getwd(),
                         background_data_dir = paste(getwd(),"Bioclim_asc",sep = "/"),
                         maximumbackground = (nrow(Spp_Presence))))

myBiomodOptions_10x <- BIOMOD_ModelingOptions(
  MAXENT.Phillips = list(path_to_maxent.jar = getwd(),
                         background_data_dir = paste(getwd(),"Bioclim_asc",sep = "/"),
                         maximumbackground = (10*nrow(Spp_Presence))))

##### ENvironment "background_data_dir" route to environmental raster from which maxent choses backgrond points.  

                                                   ## Biomod modelling options can be changed but I havent looked into doing this yet so options have been left on default

myBiomodModelOut_10x <- BIOMOD_Modeling(BiomodData_10x,                            
                                    models = c("MAXENT.Phillips"),        ### The modelling approaches that are available with Biomod2 - will maybe look into doing HMSC as well.
                                    models.options = myBiomodOptions_10x,
                                    NbRunEval = 3,                         ## Three fold cross validation ( 3 - k fold)
                                    DataSplit = 80,                       ## using an 80 - 20 split for calibration and evaluation of the model
                                    Prevalence = 0.5,                     ## Presence and absences are equalling weighted whereby the sum of the weights of the presences equals the sum of the weights of the absences
                                    VarImport = 3,                        ## Number of permutations to estimate variable importance
                                    models.eval.meth = c("TSS","ROC","ACCURACY","BIAS"),   ### Names of Evaluation metric used - True skill statistic (ACCURACY), Relative Operating Characteristic (ROC/AUC)(DISCRIMINATION), Accuracy (ACCURACY), KAPPA (Cohen's K) - Accuracy of the forecast relative to random chance.  
                                    SaveObj = TRUE,                            ### Probability of Detection ( Our measure of calibration) -- ACCURACY can then be used to also calculate precision and it is by these four measire of predictive performance we will choose our
                                    rescal.all.models = TRUE,                  ### Ensemble of SDMs.
                                    do.full.models = FALSE,
                                    modeling.id = paste(Spp_name,"FirstModeling_10x",sep = " "))     ## File to save the outputs of these models


#### Model for equal number of absences again

myBiomodModelOut_1x <- BIOMOD_Modeling(BiomodData_1x,                            
                                        models = c("MAXENT.Phillips"),       
                                        models.options = myBiomodOptions_1x,
                                        NbRunEval = 3,                        
                                        DataSplit = 80,                       
                                        Prevalence = 0.5,                     
                                        VarImport = 3,                        
                                        models.eval.meth = c("TSS","ROC","ACCURACY","BIAS"),     
                                        SaveObj = TRUE,                            
                                        rescal.all.models = TRUE,                  
                                        do.full.models = FALSE,
                                        modeling.id = paste(Spp_name,"FirstModeling_1x",sep = " "))


myBiomodModelEval_10x <- get_evaluations(myBiomodModelOut_10x)## Extract the evaluation statistics from the modelling output
myBiomodModelEval_1x <- get_evaluations(myBiomodModelOut_1x)


get_mean_eval <- function(data,x,y){                             ## This is a function that will get the mean of the three fold cross validation evaluation for each of the statistics
  eval <- as.data.frame(data[y,"Testing.data",x,,])
  mean_eval <- mean(eval[,1], na.rm = TRUE)
}


myBiomod_Prediction_10x <- cbind(get_formal_data(myBiomodModelOut_10x,"resp.var"),
                                                 as.data.frame(get_predictions(myBiomodModelOut_10x)))

myBiomod_Prediction_1x <- cbind(get_formal_data(myBiomodModelOut_1x,"resp.var"),
                                 as.data.frame(get_predictions(myBiomodModelOut_1x)))

get_precision_eval <- function(data, x){
  Precision <- data %>%
    select(paste(x,".RUN1.AllData",sep = ""),
           paste(x,".RUN2.AllData",sep = ""),
           paste(x,".RUN3.AllData",sep = ""))
  mean <- mean(mean(Precision[,1]/1000),mean(Precision[,2]/1000),mean(Precision[,3]/1000))  
  Precision <- sqrt(mean*(1-mean))  
}


 
ROC_MAXENT_10x <- get_mean_eval(myBiomodModelEval_10x,"MAXENT.Phillips","ROC")
TSS_MAXENT_10x <- get_mean_eval(myBiomodModelEval_10x,"MAXENT.Phillips","TSS")
ACCURACY_MAXENT_10x <- get_mean_eval(myBiomodModelEval_10x,"MAXENT.Phillips","ACCURACY")
BIAS_MAXENT_10x <- get_mean_eval(myBiomodModelEval_10x,"MAXENT.Phillips","BIAS")
PRECISION_MAXENT_10x <- get_precision_eval(myBiomod_Prediction_10x, "MAXENT.Phillips")

### Again for the equal presences and absences



ROC_MAXENT_1x <- get_mean_eval(myBiomodModelEval_1x,"MAXENT.Phillips","ROC")
TSS_MAXENT_1x <- get_mean_eval(myBiomodModelEval_1x,"MAXENT.Phillips","TSS")
ACCURACY_MAXENT_1x <- get_mean_eval(myBiomodModelEval_1x,"MAXENT.Phillips","ACCURACY")
BIAS_MAXENT_1x <- get_mean_eval(myBiomodModelEval_1x,"MAXENT.Phillips","BIAS")
PRECISION_MAXENT_1x  <- get_precision_eval(myBiomod_Prediction_1x, "MAXENT.Phillips")

Evaluations<- data.frame(Spp_name,nrow(Spp_Presence), (nrow(Spp_Data_10x)-nrow(Spp_Presence)), (nrow(Spp_Data_1x)-nrow(Spp_Presence)),                                              ## Collate into a data frame
                         ROC_MAXENT_10x, TSS_MAXENT_10x,BIAS_MAXENT_10x, ACCURACY_MAXENT_10x, PRECISION_MAXENT_10x,
                         ROC_MAXENT_1x, TSS_MAXENT_1x,BIAS_MAXENT_1x, ACCURACY_MAXENT_1x, PRECISION_MAXENT_1x)

colnames(Evaluations)[c(1:4)] <- c("Scientific_Name","Presences","Pseudo-Absences_10x", "Pseudo_Absences_1x")         ## Name the columns
Evaluations <-data.frame(Evaluations)                                 ## For some reason rbinding the output from each species won't work for the loop so I collated the outputs together into a list and then extracted them subsequently
}


MAXENT_SDM_Evaluations <- SDM_Evaluations

setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs") ## Set working dircetor back to the project
save(file = "RData_European_Bee_Species_MAXENT_SDM_Evaluations.RData", MAXENT_SDM_Evaluations)                               ## save. 
ff::file.move("C:/Users/patri/Desktop/SDM_Eval","C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/SDM_Eval") ## Move SDM_Eval folder from Desktop to SDMs folder

