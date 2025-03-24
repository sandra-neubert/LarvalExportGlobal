# -*- coding: utf-8 -*-
"""
Created on Tue Feb 14 14:41:07 2023

@author: Sandra Neubert
Transform OFES data from cm/s to m/s (done on HPC) otherwise Parcels has problems
"""
import os
import numpy as np
import xarray as xr
import geopandas as gp
import pandas as pd

#This needs to be changed
fileU = 'OfESncep01globalmmeanu.nc'
fileV = 'OfESncep01globalmmeanv.nc'
path = 'YOUR_PATH\\Analysis\\Movement\\TracerDataAndOutput\\OFES\\'

#should work from here
ds = xr.open_dataset(path+fileU)
ds.uvel.values = (ds.uvel.values)/100

ds.to_netcdf(path+"OfESncep01globalmmeanuMS.nc")

ds = xr.open_dataset(path+fileV)
ds.vvel.values = (ds.vvel.values)/100

ds.to_netcdf(path+"OfESncep01globalmmeanvMS.nc")