setwd("./PREDICTS")
require(usdm)
require(raster)


Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Bioclim_stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15)

vifstep(Bioclim_stack, th = 3)
usdm::vifcor(Bioclim_stack, th = 0.85)
