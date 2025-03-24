%%%%%%%% Prepare MATLAB Spatial Outputs to load into R for downstream anaylsis %%%%%%%
load("geo_time.mat") % from general repo

lat     = repmat(geo_time.lat(16:165,:),1,360);
geo_time.nlat = 150;
lon     = geo_time.lon';
lon     = repmat(lon,150,1);
mask    = geo_time.mask_land_2d(16:165,:);
surface = geo_time.surf(16:165,:);

% Ensembles to load 
ensemble= [6290,6363,6920,8874,9459];

sim_time = 250; %86; % define length of simulation in years (29 for 150 Tg, 16 for 5-47 Tg)
n_group  = 3; % define number of size groups for size grouped output
n_ens    = length(ensemble);


% Pre-allocate output summed over size groups
Biom = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
Harv = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
Eff = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));

% Pre-allocate the time for regulation onset array
Time_reg_ons = NaN(geo_time.nlat, geo_time.nlon, n_group, length(ensemble));

sim_name_vec = ["Boats_LEx_d250_m0_oa_mpa0", "Boats_LEx_d250_m0_oa_mpa1", "Boats_LEx_d250_m0_oa_mpa2", ...
    "Boats_LEx_d250_m1_oa_mpa0", "Boats_LEx_d250_m1_oa_mpa1", "Boats_LEx_d250_m1_oa_mpa2", ...
    "Boats_LEx_d250_m0_msy_mpa0", "Boats_LEx_d250_m0_msy_mpa1", "Boats_LEx_d250_m0_msy_mpa2", ...
    "Boats_LEx_d250_m1_msy_mpa0", "Boats_LEx_d250_m1_msy_mpa1", "Boats_LEx_d250_m1_msy_mpa2"];

sim_name_vec = ["Boats_LEx_d250_m0_msy_mpa2"];

% Load
for name=1:length(sim_name_vec)
    for ens=1:length(ensemble)
        
        % Load ensemble output
        %ESM = 'IPSL';
        sim_name = sim_name_vec(name);
        load (['YOUR_PATH\OutputFiles\' num2str(sim_name) '_' num2str(ens) '_h_ind_' num2str(ensemble(ens)) '.mat'])
    
        disp(ens)
        
        % Integrated size groups [time, lat, lon, ens]
        Biom(:,:,:,ens)=boats.output.annual.fish_t_out(:,:,:);              % [gwB m-2]
        Harv(:,:,:,ens)=boats.output.annual.harvest_t_out(:,:,:);           % [gwB m-2 s-1]
        Eff(:,:,:,ens)=boats.output.annual.effort_t_out(:,:,:);             % [W m-2]
        
    end
    save(['YOUR_PATH\OutputFiles\outputFiles\SpatiallyExplicit\H_' num2str(sim_name) '_allEns.mat'],'Harv')

    
end
