rm(list = ls())
require(dplyr) ### for wrangling
require(ggplot2) ### for graphs 
require(tidyr)  ## for wrangling
require(magrittr) ### for piping
require(extrafont)
loadfonts(device="win")
load("RData_European_Bee_Species_SDM_Evaluations.RData") # Load SDM Evaluation Data
#dir.create("Model_Evaluations")
#Models <- c("GLM", "GAM", "MARS", "FDA", "CTA", "ANN", "GBM", "SRE", "MAXENT.Phillips", "RF")

#for( i in 1:length(Models)){
#  dir.create(paste("Model_Evaluations", Models[i],sep = "/"))
#}
setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs")


SDM_Evaluations <- SDM_Evaluations %>% gather(Model, Measure, -c("Scientific_Name","Presences", "Pseudo-Absences_10x","Pseudo-Absences_1x"), na.rm = TRUE) ### Gather every measure is on a single row 


for(i in 1:nrow(SDM_Evaluations)){                               ##### this is all just maing sure that the dataframe is ordered and labelled correctly
if(grepl(x = SDM_Evaluations[i,5], pattern = "ROC")){
  SDM_Evaluations$Evaluation_Statistic[i] <- "ROC"
} else { 
  if (grepl(x = SDM_Evaluations$Model[i], pattern = "TSS")){
  SDM_Evaluations$Evaluation_Statistic[i] <- "TSS"
  } else {
  if (grepl(x = SDM_Evaluations$Model[i], pattern = "POD")){
      SDM_Evaluations$Evaluation_Statistic[i] <- "POD"
  } else {
  if (grepl(x = SDM_Evaluations$Model[i], pattern = "PRECISION")){
      SDM_Evaluations$Evaluation_Statistic[i] <- "PRECISION"  
  } else {
    if (grepl(x = SDM_Evaluations$Model[i], pattern = "BIAS")){
      SDM_Evaluations$Evaluation_Statistic[i] <- "BIAS"
    } else {
      if (grepl(x = SDM_Evaluations$Model[i], pattern = "ACCURACY")){
        SDM_Evaluations$Evaluation_Statistic[i] <- "ACCURACY"
        }}}}}}} 
  
for(i in 1:nrow(SDM_Evaluations)){
  if(grepl(x = SDM_Evaluations[i,5], pattern = "10x")){
    SDM_Evaluations$Absence_Proportion[i] <- "10x"
  } else { 
    if (grepl(x = SDM_Evaluations$Model[i], pattern = "1x")){
      SDM_Evaluations$Absence_Proportion[i] <- "1x"
    }}}

for(i in 1:nrow(SDM_Evaluations)){
  if(grepl(x = SDM_Evaluations[i,5], pattern = "GLM")){
    SDM_Evaluations[i,5] <- "GLM"
  } else { 
    if (grepl(x = SDM_Evaluations$Model[i], pattern = "GAM")){
      SDM_Evaluations[i,5] <- "GAM"
    } else {
      if (grepl(x = SDM_Evaluations$Model[i], pattern = "MARS")){
        SDM_Evaluations[i,5] <- "MARS"
      } else {
        if (grepl(x = SDM_Evaluations$Model[i], pattern = "RF")){
          SDM_Evaluations[i,5] <- "RF"  
        } else {
          if (grepl(x = SDM_Evaluations$Model[i], pattern = "SRE")){
            SDM_Evaluations[i,5] <- "SRE" 
          } else {
            if (grepl(x = SDM_Evaluations$Model[i], pattern = "GBM")){
              SDM_Evaluations[i,5] <- "GBM"
            } else {
              if (grepl(x = SDM_Evaluations$Model[i], pattern = "CTA")){
                SDM_Evaluations[i,5] <- "CTA"
              } else {
                if (grepl(x = SDM_Evaluations$Model[i], pattern = "ANN")){
                  SDM_Evaluations[i,5] <- "ANN"
                } else {
                  if (grepl(x = SDM_Evaluations$Model[i], pattern = "FDA")){
                    SDM_Evaluations[i,5] <- "FDA"
                  } else {
                    if (grepl(x = SDM_Evaluations$Model[i], pattern = "MAXENT")){
                      SDM_Evaluations[i,5] <- "MAXENT"
        }}}}}}}}}}}
  

SDM_Evaluations <- SDM_Evaluations[,c(1:4,8,5,7,6)]  ### order columns 


SDM_Evaluations <- SDM_Evaluations %>% 
  filter(Evaluation_Statistic != "TSS") %>%
  group_by(Model,Evaluation_Statistic,Absence_Proportion) %>%    ### Calculate mean and standard deviations for each model, evaluation statictic, and proportion of PAs
  mutate( Mean = mean(Measure), sd = sd(Measure)) %>%
  ungroup()                             


### GLM

GLM_Eval <- SDM_Evaluations %>% filter(Model == "GLM" )    #### Get measures for just GLMs

GLM_Eval_Table <- GLM_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


GLM_PA_Eval_1 <- ggplot(data = GLM_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +     ### Create a bar graph getting the mean of each evaluation statistic and compare between proportion of PAs
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "GLM")


GLM_Eval_1x <- GLM_Eval %>% filter(Absence_Proportion == "1x")                   ### Split Eval data into Absence proportion groups 
GLM_Eval_10x <- GLM_Eval %>% filter(Absence_Proportion == "10x")

GLM_PA_Eval_2 <- ggplot() +                                                                                                        ## Compare the two lines for each of the evaluation statistics
  geom_smooth(data = GLM_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +  ## if the lines cross it shows that a different proportion of PAs is better depending on the number of presences recorded for the species 
  geom_smooth(data = GLM_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "GLM")

ggsave("Model_Evaluations/GLM/GLM_PA_Eval_1.png",GLM_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/GLM/GLM_PA_Eval_2.png",GLM_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.

###### GAM                   ## Repeat this for each of the models 

GAM_Eval <- SDM_Evaluations %>% filter(Model == "GAM" )

GAM_Eval_Table <- GAM_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))

GAM_PA_Eval_1 <- ggplot(data = GAM_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "GAM")

GAM_Eval_1x <- GAM_Eval %>% filter(Absence_Proportion == "1x")
GAM_Eval_10x <- GAM_Eval %>% filter(Absence_Proportion == "10x")

GAM_PA_Eval_2 <- ggplot() +
  geom_smooth(data = GAM_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = GAM_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "GAM")

ggsave("Model_Evaluations/GAM/GAM_PA_Eval_1.png",GAM_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/GAM/GAM_PA_Eval_2.png",GAM_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


### MARS

MARS_Eval <- SDM_Evaluations %>% filter(Model == "MARS" )

MARS_Eval_Table <- MARS_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


MARS_PA_Eval_1 <- ggplot(data = MARS_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "MARS")

MARS_Eval_1x <- MARS_Eval %>% filter(Absence_Proportion == "1x")
MARS_Eval_10x <- MARS_Eval %>% filter(Absence_Proportion == "10x")

MARS_PA_Eval_2 <- ggplot() +
  geom_smooth(data = MARS_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = MARS_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "MARS") 


ggsave("Model_Evaluations/MARS/MARS_PA_Eval_1.png",MARS_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/MARS/MARS_PA_Eval_2.png",MARS_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


### RF

RF_Eval <- SDM_Evaluations %>% filter(Model == "RF" )

RF_Eval_Table <- RF_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


RF_PA_Eval_1 <- ggplot(data = RF_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "RF")

RF_Eval_1x <- RF_Eval %>% filter(Absence_Proportion == "1x")
RF_Eval_10x <- RF_Eval %>% filter(Absence_Proportion == "10x")

RF_PA_Eval_2 <- ggplot() +
  geom_smooth(data = RF_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = RF_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "RF")


ggsave("Model_Evaluations/RF/RF_PA_Eval_1.png",RF_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/RF/RF_PA_Eval_2.png",RF_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


### SRE

SRE_Eval <- SDM_Evaluations %>% filter(Model == "SRE" )

SRE_Eval_Table <- SRE_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


SRE_PA_Eval_1 <- ggplot(data = SRE_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title = "SRE")

SRE_Eval_1x <- SRE_Eval %>% filter(Absence_Proportion == "1x")
SRE_Eval_10x <- SRE_Eval %>% filter(Absence_Proportion == "10x")

SRE_PA_Eval_2 <- ggplot() +
  geom_smooth(data = SRE_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = SRE_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "SRE")

ggsave("Model_Evaluations/SRE/SRE_PA_Eval_1.png",SRE_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/SRE/SRE_PA_Eval_2.png",SRE_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


## GBM


GBM_Eval <- SDM_Evaluations %>% filter(Model == "GBM" )

GBM_Eval_Table <- GBM_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


GBM_PA_Eval_1 <- ggplot(data = GBM_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "GBM")

GBM_Eval_1x <- GBM_Eval %>% filter(Absence_Proportion == "1x")
GBM_Eval_10x <- GBM_Eval %>% filter(Absence_Proportion == "10x")

GBM_PA_Eval_2 <- ggplot() +
  geom_smooth(data = GBM_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = GBM_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "GBM")

ggsave("Model_Evaluations/GBM/GBM_PA_Eval_1.png",GBM_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/GBM/GBM_PA_Eval_2.png",GBM_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


## CTA


CTA_Eval <- SDM_Evaluations %>% filter(Model == "CTA" )

CTA_Eval_Table <- CTA_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


CTA_PA_Eval_1 <- ggplot(data = CTA_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "CTA")

CTA_Eval_1x <- CTA_Eval %>% filter(Absence_Proportion == "1x")
CTA_Eval_10x <- CTA_Eval %>% filter(Absence_Proportion == "10x")

CTA_PA_Eval_2 <- ggplot() +
  geom_smooth(data = CTA_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = CTA_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "CTA")

ggsave("Model_Evaluations/CTA/CTA_PA_Eval_1.png",CTA_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/CTA/CTA_PA_Eval_2.png",CTA_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


## ANN


ANN_Eval <- SDM_Evaluations %>% filter(Model == "ANN" )

ANN_Eval_Table <- ANN_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


ANN_PA_Eval_1 <- ggplot(data = ANN_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "ANN")

ANN_Eval_1x <- ANN_Eval %>% filter(Absence_Proportion == "1x")
ANN_Eval_10x <- ANN_Eval %>% filter(Absence_Proportion == "10x")

ANN_PA_Eval_2 <- ggplot() +
  geom_smooth(data = ANN_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = ANN_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic)+
  labs(title = "ANN")

ggsave("Model_Evaluations/ANN/ANN_PA_Eval_1.png",ANN_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/ANN/ANN_PA_Eval_2.png",ANN_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


## FDA


FDA_Eval <- SDM_Evaluations %>% filter(Model == "FDA" )

FDA_Eval_Table <- FDA_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


FDA_PA_Eval_1 <- ggplot(data = FDA_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "FDA")

FDA_Eval_1x <- FDA_Eval %>% filter(Absence_Proportion == "1x")
FDA_Eval_10x <- FDA_Eval %>% filter(Absence_Proportion == "10x")

FDA_PA_Eval_2 <- ggplot() +
  geom_smooth(data = FDA_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = FDA_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "FDA")

ggsave("Model_Evaluations/FDA/FDA_PA_Eval_1.png",FDA_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/FDA/FDA_PA_Eval_2.png",FDA_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.


## MAXENT


MAXENT_Eval <- SDM_Evaluations %>% filter(Model == "MAXENT" )

MAXENT_Eval_Table <- MAXENT_Eval %>% group_by(Model, Evaluation_Statistic,Absence_Proportion) %>%
  summarise(Mean = mean(Measure), Standard_Deviation = sd(Measure))


MAXENT_PA_Eval_1 <- ggplot(data = MAXENT_Eval, aes(y = Mean, x = Evaluation_Statistic, fill = Absence_Proportion)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "MAXENT")

MAXENT_Eval_1x <- MAXENT_Eval %>% filter(Absence_Proportion == "1x")
MAXENT_Eval_10x <- MAXENT_Eval %>% filter(Absence_Proportion == "10x")

MAXENT_PA_Eval_2 <- ggplot() +
  geom_smooth(data = MAXENT_Eval_1x, aes(x = Presences, y = Measure), colour = "blue", method = "lm", se = FALSE) +
  geom_smooth(data = MAXENT_Eval_10x, aes(x = Presences, y = Measure), colour = "red", method = "lm", se = FALSE) +
  facet_grid(~Evaluation_Statistic, scales = "free_y")+
  labs(title = "MAXENT")

ggsave("Model_Evaluations/MAXENT.Phillips/MAXENT_PA_Eval_1.png",MAXENT_PA_Eval_1,device = "png", height = 10, width = 15,dpi=300)  ### Save.
ggsave("Model_Evaluations/MAXENT.Phillips/MAXENT_PA_Eval_2.png",MAXENT_PA_Eval_2,device = "png", height = 10, width = 15,dpi=300)  ### Save.



#### Looking at the graphs you can deduce what proportion of PAs should be used for the methodology being implemented and whether 
### for some methodologies a different proportion of PAs is better for fewer or greater number of presences.

#### 



Model_Evals_Table <- rbind(GLM_Eval_Table,GAM_Eval_Table,MARS_Eval_Table,
                           RF_Eval_Table,SRE_Eval_Table,ANN_Eval_Table,
                           CTA_Eval_Table,FDA_Eval_Table, MAXENT_Eval_Table,
                           GBM_Eval_Table)



GLM_Final <- GLM_Eval_10x                                  ### GLM 10x PA was always best 

GAM_Final <- GAM_Eval_10x

MARS_Final <- MARS_Eval_10x

RF_Final <- RF_Eval_10x

SRE_Final <- SRE_Eval_1x

GBM_Final <- GBM_Eval_10x

CTA_Final <- CTA_Eval_10x

ANN_Final <- ANN_Eval_10x

FDA_Final <- FDA_Eval_10x

MAXENT_Final <- MAXENT_Eval_10x

Final_Evaluations <- rbind(GLM_Final,                      #### Bind together the final dataset so that each evaluation statistic is derived from only one of the PA proportions 
                           GAM_Final,
                           MARS_Final,
                           RF_Final,
                           SRE_Final,
                           GBM_Final,
                           CTA_Final,
                           ANN_Final,
                           FDA_Final,
                           MAXENT_Final)

Final_Evaluations <- Final_Evaluations %>% group_by(Model,Evaluation_Statistic) %>%   ### Recalculate the mean and standard deviation of the evaluation statistics for each of the models 
  summarise(Mean = mean(Measure), sd = sd(Measure)) %>%
  ungroup()


FinalModel_Evals <- ggplot(data = Final_Evaluations, aes(y = Mean, x = Model, fill = Evaluation_Statistic)) +      ## Bar graph comparing the evaluation statistics for each of the models 
  facet_grid(~Evaluation_Statistic)+                                                  ## For my ensemble of SDMs I want a selection of models that captures a different indicy of predictive performance 
  geom_bar(position="dodge", stat = "identity") +                                     ## this will reduce the uncertainties in the models and projections.
  geom_errorbar(aes(ymin= Mean, ymax=Mean + sd), width=.2,
                position=position_dodge(1)) +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title = "Evaluation_Statistics") +
  theme_classic() +
  theme(legend.position = "none",
        text=element_text(family = "Times New Roman", size = 20), 
        axis.text.x = element_text(size = 20, angle = 90, hjust = 1), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_fill_hue(l=50, c=50) 
  


ggsave("Model_Evaluations/FinalModel_Evals.png",FinalModel_Evals,device = "png", height = 10, width = 25,dpi=300)  ### Save.

plot(FinalModel_Evals)


