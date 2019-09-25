rm(list = ls())
require(dplyr)
require(tidyr)
require(magrittr)
require(lme4)
require(yarg)
require(roquefort)
require(Hmisc)
require(arm)
require(RColorBrewer)
require(mgcv)
require(influence.ME)
require(spdep)
require(R2admb)
require(glmmADMB)
require(plotrix)
require(raster)
require(usdm)
require(ggResidpanel)
require(car)

setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/")

Bee_Families <- c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae","Melittidae","Stenotritidae")
taxa <- readRDS("Raw_Data/taxa-2019-07-17-02-32-17.rds")
sites <- readRDS("Raw_Data/sites-2019-07-17-02-31-45.rds")
diversity <- readRDS("Raw_Data/diversity-2019-05-09-02-35-39.rds")



European_Bee_Diversity <- diversity %>%
  filter(UN_region == "Europe" & Family %in% Bee_Families)


  
European_Bee_Diversity <- CorrectSamplingEffort(European_Bee_Diversity)


European_Bee_Diversity <- MergeSites(European_Bee_Diversity, match.extra = "Wilderness_area")

sites <- SiteMetrics(diversity = European_Bee_Diversity, 
                     extra.cols = c("SSB", "SSBS", "Biome")) 


hpd <- raster("Raw_Data/Human_Population_Density/gpw_v4_population_density_rev11_2015_2pt5_min.tif")
Europe_Extent <- extent(-25, 50, 30, 75)
hpd <- crop(hpd, Europe_Extent)

plot(hpd)

                                                          # get the coordinates from the sites dataset
point_coords <- dplyr::select(sites, Longitude, Latitude)
                                                          # extract the HPD values for these coordinates
hpd_value <- raster::extract(hpd, point_coords)
                                                          # take a look at the data
head(hpd_value)

sites <- cbind(sites, hpd_value)
sites$logHPD <- log(sites$hpd_value + 1)


# make a new column using the mutate function called LandUse out of the Predominant_habitat column
sites$LandUse <- paste(sites$Predominant_habitat)
# if the land use is either primary forest or primary non-forest, change the value of LandUse to Primary
sites$LandUse[sites$LandUse %in% c("Primary forest", 
                                   "Primary non-forest")] <- "Primary"
# when the value of land use is Cannot decide, change to NA
sites$LandUse[sites$LandUse == "Cannot decide"] <- NA
# do the same for use intensity
sites$Use_intensity[sites$Use_intensity == "Cannot decide"] <- NA
# indeterminate age isn't really a proper category, so let's turn this into NA
sites$LandUse[sites$LandUse == "Secondary vegetation (indeterminate age)"] <- NA
# turn LandUse and UseIntensity to factors with the appropriate first level
sites$LandUse <- factor(sites$LandUse)
sites$LandUse <- relevel(sites$LandUse, ref = "Primary")
sites$Use_intensity <- factor(sites$Use_intensity)
sites$Use_intensity <- relevel(sites$Use_intensity, ref = "Minimal use")
# take a look at the LandUse/Use intensity split
table(sites$LandUse, sites$Use_intensity)


source("https://highstat.com/Books/Book2/HighstatLibV10.R")
corvif(sites[,c("LandUse","logHPD","Use_intensity")])

model_data <- sites[complete.cases(sites[ , c("Species_richness", "LandUse", "Use_intensity", "logHPD")]), ]
model_data$Use_intensity <- sub( " ", "_", model_data$Use_intensity) 

model_data <- model_data %>%
  mutate(LandUse_Use_intensity = ifelse(LandUse == "Primary",
                                        "Primary",
                                        paste(LandUse, Use_intensity, sep = "_")),
         LandUse_Use_intensity =  ifelse(grepl("secondary", tolower(LandUse)),
                                                "Secondary_vegetation",
                                                  paste(LandUse_Use_intensity)),
         LandUse_Use_intensity = ifelse(grepl("plantation forest", tolower(LandUse)),
                                        "Cropland_Light_use",
                                        paste(LandUse_Use_intensity)),
         LandUse_Use_intensity = ifelse(grepl("urban", tolower(LandUse)),
                                        "Urban",
                                        paste(LandUse_Use_intensity)),
         LandUse_Use_intensity = ifelse(grepl("pasture", tolower(LandUse)),
                                        "Pasture",
                                        paste(LandUse_Use_intensity)),
         LandUse_Use_intensity = factor(LandUse_Use_intensity),
         LandUse_Use_intensity = relevel(LandUse_Use_intensity, ref = "Primary"))
                                        

summary(model_data$LandUse_Use_intensity)

SR_Model_1 <- glmer(Species_richness ~ LandUse_Use_intensity + logHPD + logHPD:LandUse_Use_intensity +
                      (1 + logHPD|SS) + (1|SSB), 
                    data = model_data ,family = "poisson", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_1)


SR_Model_2 <- glmer(Species_richness ~ LandUse_Use_intensity + logHPD + logHPD:LandUse_Use_intensity +
                      (1 + logHPD|SS) + (1|SSB)+ (1|SSBS), 
                    data = model_data ,family = "poisson", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_2)


SR_Model_3 <- glmer(Species_richness ~ LandUse_Use_intensity + logHPD + logHPD:LandUse_Use_intensity +
                       (1|SS) + (1|SSB), 
                     data = model_data ,family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_3)

SR_Model_4 <- glmer(Species_richness ~ LandUse_Use_intensity + logHPD + logHPD:LandUse_Use_intensity +
                       (1|SS) + (1|SSB) + (1|SSBS), 
                     data = model_data ,family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_4)



SR_Model_5 <- glmer(Species_richness ~ LandUse_Use_intensity + poly(logHPD,2) + poly(logHPD,2):LandUse_Use_intensity +
                      (1 + poly(logHPD,2)|SS) + (1|SSB), data = model_data, family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_5)


SR_Model_6 <- glmer(Species_richness ~ LandUse_Use_intensity + poly(logHPD,2) + poly(logHPD,2):LandUse_Use_intensity +
                       (1 + poly(logHPD,2)|SS) + (1|SSB)+ (1|SSBS), 
                     data = model_data, family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_6)



SR_Model_7 <- glmer(Species_richness ~ LandUse_Use_intensity + poly(logHPD,2) + poly(logHPD,2):LandUse_Use_intensity +
                       (1|SS) + (1|SSB), data = model_data, family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_7)


SR_Model_8 <- glmer(Species_richness ~ LandUse_Use_intensity + poly(logHPD,2) + poly(logHPD,2):LandUse_Use_intensity +
                       (1 |SS) + (1|SSB) + (1|SSBS), data = model_data, family = "poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

resid_panel(SR_Model_8)


AIC(SR_Model_1,SR_Model_2,SR_Model_3,SR_Model_4,
    SR_Model_5,SR_Model_6,SR_Model_7,SR_Model_8)


OverdispersionTest(SR_Model_3)


Anova(SR_Model_4)
summary(SR_Model_4)
anova(SR_Model_4)

resid_panel(SR_Model_4)

Anova(SR_Model_4, type = "III")

summary(SR_Model_4)

saveRDS(SR_Model_4, file = "RData_PREDICTS_Model.rds")

Anova(SR_Model_1)
Anova(SR_Model_2)
Anova(SR_Model_3)
Anova(SR_Model_4)
Anova(SR_Model_5)
Anova(SR_Model_6)
