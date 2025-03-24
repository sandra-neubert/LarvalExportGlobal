# -*- coding: utf-8 -*-
"""
Created on Thu Jan  5 22:35:39 2023

@author: Sandra Neubert

Create file with particle release location information based on OFES data
"""

import os
import numpy as np
import xarray as xr
import geopandas as gp
import pandas as pd

from parcels import FieldSet, Field, ParticleSet, Variable, JITParticle
from parcels import AdvectionRK4, plotTrajectoriesFile, ErrorCode

# REQUIRES OFES DATA (e.g. from https://apdrc.soest.hawaii.edu/datadoc/ofes/ofes.php)

df = pd.read_csv('PATH_TO_dfOFESOCEAN.csv').dropna().reset_index(drop=True)
df["lon"] = df.longitude
df["lat"] = df.latitude
df = df[['lon','lat']]
df

StartLocations = df.reset_index(drop=True)       
for i in np.arange(0, len(StartLocations)):
    lonVals = np.linspace(StartLocations.lon[i]-0.05, StartLocations.lon[i]+0.05, 1) #to get even steps between released particles within and between cells (released every 0.1°)
    latVals = np.linspace(StartLocations.lat[i]-0.05, StartLocations.lat[i]+0.05, 1)
    print(i)
    
    for j in np.arange(0, len(lonVals)):
        lonRep = np.repeat(lonVals[j], 1)
        partRelease = pd.concat([pd.DataFrame({"lon": lonRep}), pd.DataFrame({"lat":latVals})], axis=1)
        if i == 0 and j == 0:
            StartLoc = partRelease
        else:
            StartLoc = pd.concat([StartLoc, partRelease])

StartLoc = StartLoc[StartLoc.lat != -75.05]
#StartLoc2 = StartLoc[StartLoc.lat != 74.95]

subset = df[np.round(df.lat, decimals = 1) == 74.8]
subsetStart = subset
subsetStart["lat"] = subsetStart.lat +0.05

StartLoc2 = pd.concat([StartLoc , subsetStart])
           
StartLoc2.to_csv('YOUR_PATH\\Analysis\\Movement\\Data\\dfOFESStartLocationsGlobal2.csv')
