# -*- coding: utf-8 -*-
"""
Created on Tue Jan 10 15:30:52 2023

@author: Sandra Neubert and Dan Hewitt (HPC parts)
Particle tracking script for HPC
"""
#Make sure OFES data is in m/s (not cm/s)

import os
import numpy as np
import xarray as xr
#import geopandas as gp
import pandas as pd

from parcels import FieldSet, Field, ParticleSet, Variable, JITParticle
from parcels import AdvectionRK4, ErrorCode

import math
from datetime import timedelta as delta
from datetime import datetime 
from operator import attrgetter

# import the index variable from the .pbs script
index = int(os.environ['PBS_ARRAY_INDEX']) # a value between 0-827 (69 years * 12 months = 828 jobs)

# set up the array of years and months
years = [year for year in range(1950, 2020)]
months = [month for month in range(1, 13)]

# all possible combinations of the two vectors
years = np.repeat(years, len(months))
months = np.repeat(months, 70)



dataPath = "/srv/scratch/z9902002/OFES/" # jases scratch
#ufiles = dataPath + "OfESncep01globalmmeanu20152019MS.nc"
ufiles = dataPath + str(years[index]) + "_uvel.nc"
#vfiles = dataPath + "OfESncep01globalmmeanv20152019MS.nc"
vfiles = dataPath + str(years[index]) + "_vvel.nc"

filenames = {'U': ufiles,
             'V': vfiles}

variables = {'U': 'uvel', # had to change 'u' here to 'uvel' which is the name of the variable in the netcdf
             'V': 'vvel'}

dimensions = {'lat': 'latitude',
              'lon': 'longitude',
              'time': 'time'}

StartLocations = pd.read_csv("/srv/scratch/z5278054/particle-tracking-sandra/Data/dfOFESStartLocationsGlobal2.csv")
StartLocations = StartLocations[['lon','lat']]


fieldset = FieldSet.from_netcdf(filenames, variables, dimensions)

fieldset.add_constant('maxage', 3.*86400) #get rid of particles after 3 days

fieldset = FieldSet.from_netcdf(filenames, variables, dimensions, deferred_load = False) # deferred load = False for on the fly transformation of OFES

fieldset.U.data = fieldset.U.data/100
fieldset.V.data = fieldset.V.data/100

fieldset.add_constant('maxage', 2.*86400) #get rid of particles after 3 days

fieldset.add_constant('maxage', 2.*86400) #get rid of particles after 3 days

fieldset.add_periodic_halo(zonal=True) #to not get artifacts around prime meridian (linked to kernel further down)

lon_array = StartLocations.lon
lat_array = StartLocations.lat

npart = 1 #how many particles are released at each location (every time)
lon = np.repeat(lon_array, npart)
lat = np.repeat(lat_array, npart)

# How often to release the particles; 
#Probelm: if I release particles over a long period of time, setting the repeatdt to 30 days leads to particles being released on a different day each month and it gets worse with time
#if I set repeatdt at 30.4375, release dates stay around the same
#repeatdt = delta(days = 30.4375) # release from the same set of locations every months

start_time = datetime(years[index], months[index], 15) #year, month, day,
end_time = datetime(years[index], months[index], 18) # 3 days so all particles can 'die'

runtime = end_time-start_time + delta(days = 5) #add some days at the end to make sure tracking can be done for 5 days from the last start location onwards if release date is not exactly on 15th

time = 0 #np.arange(0, npart) * delta(days = 30.4375).total_seconds() 

class SampleParticle(JITParticle):         # Define a new particle class
        sampled = Variable('sampled', dtype = np.float32, initial = 0, to_write=False)
        age = Variable('age', dtype=np.float32, initial=0.) # initialise age
        distance = Variable('distance', initial=0., dtype=np.float32)  # the distance travelled
        prev_lon = Variable('prev_lon', dtype=np.float32, to_write=False,
                            initial=0)  # the previous longitude
        prev_lat = Variable('prev_lat', dtype=np.float32, to_write=False,
                            initial=0)  # the previous latitude
        #beached = Variable('beached', dtype = np.float32, initial = 0)
    
def DeleteParticle(particle, fieldset, time): #needed to avoid error mesasage of Particle out of bounds
    particle.delete()
    
# Define all the sampling kernels
def SampleDistance(particle, fieldset, time):
    # Calculate the distance in latitudinal direction (using 1.11e2 kilometer per degree latitude)
    lat_dist = (particle.lat - particle.prev_lat) * 1.11e2
    # Calculate the distance in longitudinal direction, using cosine(latitude) - spherical earth
    lon_dist = (particle.lon - particle.prev_lon) * 1.11e2 * math.cos(particle.lat * math.pi / 180)
    # Calculate the total Euclidean distance travelled by the particle
    particle.distance += math.sqrt(math.pow(lon_dist, 2) + math.pow(lat_dist, 2))
    particle.prev_lon = particle.lon  # Set the stored values for next iteration.
    particle.prev_lat = particle.lat

def SampleAge(particle, fieldset, time):
    particle.age = particle.age + math.fabs(particle.dt)
    if particle.age >= fieldset.maxage: #if not >= : get one more particle tracking point after maxage
           particle.delete()
            
def periodicBC(particle, fieldset, time):
    if particle.lon < 0:
        particle.lon += 360 - 0
    elif particle.lon > 359.9:
        particle.lon -= 360 - 0
    
def SampleInitial(particle, fieldset, time): # do we have to add particle.age and particle.ageRise
        if particle.sampled == 0:
            particle.distance = particle.distance
            particle.prev_lon = particle.lon
            particle.prev_lat = particle.lat
            #particle.beached = particle.beached
            particle.sampled = 1
               
pset = ParticleSet.from_list(fieldset, 
                             pclass=SampleParticle, 
                             time=time, 
                             lon=lon, 
                             lat=lat)#,
                            # repeatdt=repeatdt)

kernels = SampleInitial + pset.Kernel(AdvectionRK4) + periodicBC + SampleAge + SampleDistance

output_nc_dist = 'NearGlobalParticleTrackingOFES.zarr'
try:
    os.remove(output_nc_dist)
except OSError:
    pass

file_dist = pset.ParticleFile(name=output_nc_dist, 
                                outputdt=delta(hours=6)) #save location every 6 hours

pset.execute(kernels,  
             runtime=runtime,
             dt=delta(minutes=10), #to reduce computational load
             output_file=file_dist,
             recovery={ErrorCode.ErrorOutOfBounds: DeleteParticle})

parcels_dist = xr.open_dataset(output_nc_dist)

# where to save the data on the HPC
localPath = "/srv/scratch/z5278054/particle-tracking-sandra/Output/"


dfParcels = parcels_dist.to_dataframe()
dfParcels.to_csv(localPath + 'dfParcelsGlobal.csv') #local path for HPC 


parcels_dist.to_netcdf(localPath + str(years[index]) + '-' + str(months[index]) + '-ParcelsOutput.nc')


#dfParcels = parcels_dist.to_dataframe()
#dfParcels.to_csv(localPath + 'dfParcelsGlobal.csv') #local path for HPC 

for i in range(1, 13):
    monthlyData = parcels_dist.where(
    parcels_dist['time.month'] == i, drop=True)
    monthlyData.to_netcdf(localPath + 'ParcelsGlobalYear' + str(YEAR) + 'Month' + str(i) + '.nc') #ADD year variable
    print(i)


#parcels_dist.to_netcdf(localPath + 'ParcelsGlobal.nc')


#dfParcels = parcels_dist.to_dataframe()
#dfParcels.to_csv(localPath + 'dfParcelsGlobal.csv') #local path for HPC 