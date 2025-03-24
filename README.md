# Larval Export from MPAs

This repository provides the code to the study **"Larval export mitigates the cost of conservation to fisheries in over-exploited waters"**. Briefly, we couple Lagrangian particle tracking to simulate passive larval movement with the global, spatially-resolved bioeconomic trophic size-spectrum model BOATS to estimate fish biomass and harvest. We then explore two 30✕30 MPA scenarios that maximise economic and biodiversity benefit respectively to test how much incorporating larval movement in a marine ecosystem model could offset the projected loss to fisheries upon MPA implementation. 
<br>
The code can be divided into four main sections:
<br>
* **Particle Tracking:** Preprocessing scripts for particle tracking can be found in `Analysis/Notebooks and Analysis/PythonScripts`. Near-global particle tracking is done in Python with [Parcels](https://oceanparcels.org/#whatisparcels) by [Delandmeter and van Sebille (2019)](https://www.geosci-model-dev.net/12/3571/2019/gmd-12-3571-2019.html), performed with `Analysis/Movement/PythonScripts/HPCParticleTrackingScript.py` and requires a high perfomance computer. We also provide the PBS file that was used to run the particle tracking on Katana at UNSW (Analysis/Movement/PythonScripts/HPCParticleTrackingScript.pbs). Running particle tracking simulations also requires hydrodynamic model data. We used OFES data (see manuscript for more detail) which can be obtained from multiple sources (e.g. https://apdrc.soest.hawaii.edu/datadoc/ofes/ofes.php), but other hydrodynamic model data also works, as long as the scripts for calculating the particle starting locations is updated. 
* **Calculating the transition matrix with movement probabilities:** The output of the particle tracking was processed to monthly transition matrices in R using the scripts in `Analysis/Movement/RScripts/ProbabilityMatrix`, specifically the script `Calculate_ProbMatPTGlobalCompile25Y.R`. This analysis also requires a high performance computer and we provide the PBS files used to calculate the transition matrices on Katana at UNSW (`Analysis/Movement/RScripts/ProbabilityMatrix/StartCompile25Y.pbs`).
* **Global bioeconomic modelling simulations:** We used the publicly available BiOeconomic mArine Trophic Size-spectrum model ([BOATS](https://github.com/obeg-boats)) by Carozza et al. ([2016](https://earthsystemdynamics.org/wp-content/uploads/2018/05/Carozza-GMD-2016-BOATS.pdf), [2017](https://earthsystemdynamics.org/wp-content/uploads/2018/05/Carozza-PLoS-2017-BOATS.pdf)) to simulate whether egg/larval movement can offset lost fishing following MPA implementation. The version of BOATS including egg/larval movement can be found in `Analysis/BOATS/sneubert-boats_v1` with instructions on how to run the general model in the doc folder. The model requires forcing input data, data on the MPA matrices to use, as well as the transition matrices created in the first two steps of this repository. We used the standard ecological forcings for BOATS that are described in Carozza et al. ([2016](https://earthsystemdynamics.org/wp-content/uploads/2018/05/Carozza-GMD-2016-BOATS.pdf) that can be obtained from https://github.com/obeg-boats. We used the static economic forcings set as default in the `Analysis/BOATS/sneubert-boats_v1/preprcess.mat` script. We only adapted the effective effort target and soecietal enforcement strength forcing to reflect no-take MPAs (within an MPA, effort target was set to 0 and societal enforcement to 1). These parameters are described in detail in [Scherrer and Galbraith (2022)](https://doi.org/10.1093/icesjms/fsaa109). Locations of MPAs were taken from [Sala et al. (2021)](https://www.nature.com/articles/s41586-021-03371-z) and [Waldron et al. (2020)](https://www.conservation.cam.ac.uk/files/waldron_report_30_by_30_publish.pdf) and are publicly available. Processing of MPA matrices to preapre them as an input in BOATS can be found in `Analysis/BOATS/sneubert-boats_v1/preprocessingMPAsRegulation`. Before running the model, the script `Analysis/BOATS/sneubert-boats_v1/prepare_monthlyEggMovementMatrices.mat` needs to be run ONCE initially to prepare the R outputs of the transition matrices for being integrated into BOATS. The remaining steps to run the model are just as described in `Analysis/BOATS/sneubert-boats_v1/doc`. Running BOATS as available here also requires a high performance computer because we ran the model in daily time steps compared to the common monthly BOATS time steps to capture larval movement. We used the Bunya HPC at the University of Queensland and provide an example Shell script that was used to run one of the iterations of the model (`Analysis/BOATS/BOATS_HPC.sh`).
* **Analysis of simulations:** BOATS outputs were first processed in MATLAB to prepare them for further analysis in R (see `Analysis/BOATS/sneubert-boats_v1/postprocess`). The output was then visualised in R and further analysed in R with the scripts in `Analysis/Movement/RScripts/BOATSOutputEnsemble` using a Generalised Linear Model (`GLM` folder).
