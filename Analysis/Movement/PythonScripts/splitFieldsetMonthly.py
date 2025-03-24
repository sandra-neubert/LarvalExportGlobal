# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 08:36:26 2023

@author: Dan Hewitt

Split Fieldset data to run monthly instead of yearly
"""

import os
from datetime import datetime
import numpy as np
from parcels import FieldSet

# import environment variables - these are defined in the .pbs script (which I'm working on figuring out)
year = os.environ.get('VAR1') # range 0-68
month = os.environ.get('VAR2') # range 0-11

# define your original FieldSet here, assuming it is called 'fieldset'
filedset = fieldset = FieldSet.from_netcdf(filenames, variables, dimensions)

# list the years OFES runs for (+1 bc of zero-indexing)
years = list(range(1950, 2019 + 1))

# list of months
months = list(range(1, 12 + 1))

# define the start for the current month
start_time = datetime(years[year], months[month], 1)

# define the end time for the current month
end_time = datetime(years[year], months[month] + 1, 1) if month < 12 else datetime(years[year] + 1, 1, 1) # might cause an issue for the Dec 2019 data (bc no 2020 data exists)
    
# get the indices of the velocity fields that fall within the current time period
indices = np.where((fieldset.U.grid.time >= start_time) & (fieldset.U.grid.time < end_time))[0]
    
# create a new fieldset containing only the velocity fields for the current time period
new_fieldset = fieldset.split(fieldset.U.grid.time, indices)