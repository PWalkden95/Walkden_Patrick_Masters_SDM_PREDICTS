#!/usr/bin/env python

import time
import fiona
import multiprocessing
from rasterio.plot import show
import math
import os
import click
#import matlibplot.pyplot as plt
import numpy as np
import numpy.ma as ma
import rasterio
from rasterio.plot import show, show_hist
import pandas

from projections.rasterset import RasterSet, Raster
from projections.simpleexpr import SimpleExpr
import projections.predicts as predicts
import projections.r2py.modelr as modelr
import projections.utils as utils


mod = modelr.load('RData_PREDICTS_Model.rds')
predicts.predictify(mod)

years = [2015, 2050, 2070]

for year in years:

## EITHER THIS WAY
        rasters = predicts.rasterset('luh2',"ssp1_rcp2.6_image", year)
        rs = RasterSet(rasters)

        rs['LandUse_Use_intensityCropland_Intense_use'] = SimpleExpr('LandUse_Use_intensityCropland_Intense_use','cropland_intense')
        rs['LandUse_Use_intensityCropland_Light_use'] = SimpleExpr('LandUse_Use_intensityCropland_Light_use','cropland_light')
        rs['LandUse_Use_intensityCropland_Minimal_use'] = SimpleExpr('LandUse_Use_intensityCropland_Minimal_use','cropland_minimal')
        rs['LandUse_Use_intensityPasture'] = SimpleExpr('LandUse_Use_intensityPasture','pasture')
        rs['LandUse_Use_intensityPlantation_Forest'] = SimpleExpr('LandUse_Use_intensityPlantation_Forest','perennial')
        rs['LandUse_Use_intensitySecondary_vegetation'] = SimpleExpr('LandUse_Use_intensitySecondary_vegetation','secondary')
        rs['LandUse_Use_intensityUrban'] = SimpleExpr('LandUse_Use_intensityUrban','urban')
        rs['logHPD'] = SimpleExpr('logHPD',
                              'log(hpd)')

        rs[mod.output] = mod        
        rs['output'] = SimpleExpr('output', '(exp(%s) / exp(%f))' % (mod.output, mod.intercept))
        rs.write('output', 'D:/LUH2_rasters_PW/PREDICTS_RCP2.6-%d.tif' % year)

        rasters = predicts.rasterset('luh2',"ssp2_rcp4.5_message-globiom", year)
        rs = RasterSet(rasters)

        rs['LandUse_Use_intensityCropland_Intense_use'] = SimpleExpr('LandUse_Use_intensityCropland_Intense_use','cropland_intense')
        rs['LandUse_Use_intensityCropland_Light_use'] = SimpleExpr('LandUse_Use_intensityCropland_Light_use','cropland_light')
        rs['LandUse_Use_intensityCropland_Minimal_use'] = SimpleExpr('LandUse_Use_intensityCropland_Minimal_use','cropland_minimal')
        rs['LandUse_Use_intensityPasture'] = SimpleExpr('LandUse_Use_intensityPasture','pasture')
        rs['LandUse_Use_intensityPlantation_Forest'] = SimpleExpr('LandUse_Use_intensityPlantation_Forest','perennial')
        rs['LandUse_Use_intensitySecondary_vegetation'] = SimpleExpr('LandUse_Use_intensitySecondary_vegetation','secondary')
        rs['LandUse_Use_intensityUrban'] = SimpleExpr('LandUse_Use_intensityUrban','urban')
        rs['logHPD'] = SimpleExpr('logHPD',
                              'log(hpd)')

        rs[mod.output] = mod        
        rs['output'] = SimpleExpr('output', '(exp(%s) / exp(%f))' % (mod.output, mod.intercept))
        rs.write('output', 'D:/LUH2_rasters_PW/PREDICTS_RCP4.5-%d.tif' % year)


        rasters = predicts.rasterset('luh2',"ssp4_rcp6.0_gcam", year)
        rs = RasterSet(rasters)

        rs['LandUse_Use_intensityCropland_Intense_use'] = SimpleExpr('LandUse_Use_intensityCropland_Intense_use','cropland_intense')
        rs['LandUse_Use_intensityCropland_Light_use'] = SimpleExpr('LandUse_Use_intensityCropland_Light_use','cropland_light')
        rs['LandUse_Use_intensityCropland_Minimal_use'] = SimpleExpr('LandUse_Use_intensityCropland_Minimal_use','cropland_minimal')
        rs['LandUse_Use_intensityPasture'] = SimpleExpr('LandUse_Use_intensityPasture','pasture')
        rs['LandUse_Use_intensityPlantation_Forest'] = SimpleExpr('LandUse_Use_intensityPlantation_Forest','perennial')
        rs['LandUse_Use_intensitySecondary_vegetation'] = SimpleExpr('LandUse_Use_intensitySecondary_vegetation','secondary')
        rs['LandUse_Use_intensityUrban'] = SimpleExpr('LandUse_Use_intensityUrban','urban')
        rs['logHPD'] = SimpleExpr('logHPD',
                              'log(hpd)')

        rs[mod.output] = mod        
        rs['output'] = SimpleExpr('output', '(exp(%s) / exp(%f))' % (mod.output, mod.intercept))
        rs.write('output', 'D:/LUH2_rasters_PW/PREDICTS_RCP6.0-%d.tif' % year)



        rasters = predicts.rasterset('luh2',"ssp5_rcp8.5_remind-magpie", year)
        rs = RasterSet(rasters)

        rs['LandUse_Use_intensityCropland_Intense_use'] = SimpleExpr('LandUse_Use_intensityCropland_Intense_use','cropland_intense')
        rs['LandUse_Use_intensityCropland_Light_use'] = SimpleExpr('LandUse_Use_intensityCropland_Light_use','cropland_light')
        rs['LandUse_Use_intensityCropland_Minimal_use'] = SimpleExpr('LandUse_Use_intensityCropland_Minimal_use','cropland_minimal')
        rs['LandUse_Use_intensityPasture'] = SimpleExpr('LandUse_Use_intensityPasture','pasture')
        rs['LandUse_Use_intensityPlantation_Forest'] = SimpleExpr('LandUse_Use_intensityPlantation_Forest','perennial')
        rs['LandUse_Use_intensitySecondary_vegetation'] = SimpleExpr('LandUse_Use_intensitySecondary_vegetation','secondary')
        rs['LandUse_Use_intensityUrban'] = SimpleExpr('LandUse_Use_intensityUrban','urban')
        rs['logHPD'] = SimpleExpr('logHPD',
                              'log(hpd)')

        rs[mod.output] = mod        
        rs['output'] = SimpleExpr('output', '(exp(%s) / exp(%f))' % (mod.output, mod.intercept))
        rs.write('output', 'D:/LUH2_rasters_PW/PREDICTS_RCP8.5-%d.tif' % year)


    





        
    





