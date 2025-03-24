%%%%%%%% Prepare MATLAB Timeseries Outputs to load into R for downstream anaylsis %%%%%%%
clear all

% Ensembles to load 
ensemble= [6290,6363,6920,8874,9459];

sim_time = 250; %86; % define length of simulation in years 
n_group  = 3; % define number of size groups for size grouped output
n_ens    = length(ensemble);

% Pre-allocate output globally integrated 
Biom_gi_t = zeros(sim_time,length(ensemble));
Harv_gi_t = zeros(sim_time,length(ensemble));
Eff_gi_t  = zeros(sim_time,length(ensemble));

sim_name_vec = ["Boats_LEx_d250_m0_oa_mpa0", "Boats_LEx_d250_m0_oa_mpa1", "Boats_LEx_d250_m0_oa_mpa2", ...
    "Boats_LEx_d250_m1_oa_mpa0", "Boats_LEx_d250_m1_oa_mpa1", "Boats_LEx_d250_m1_oa_mpa2", ...
    "Boats_LEx_d250_m0_msy_mpa0", "Boats_LEx_d250_m0_msy_mpa1", "Boats_LEx_d250_m0_msy_mpa2", ...
    "Boats_LEx_d250_m1_msy_mpa0", "Boats_LEx_d250_m1_msy_mpa1", "Boats_LEx_d250_m1_msy_mpa2"];

% Load
for name=1:length(sim_name_vec)
    for ens=1:length(ensemble)
        
        % Load ensemble output
        sim_name = sim_name_vec(name);
        load (['YOUR_PATH\OutputFiles\' num2str(sim_name) '_' num2str(ens) '_h_ind_' num2str(ensemble(ens)) '.mat'])
    
        % Create arrays with all ensembles in last dimension
    
        % Global integral [time, ens]
        Biom_gi_t(:,ens)=boats.output.annual.fish_gi_t(:);                  % [gwB]
        Harv_gi_t(:,ens)=boats.output.annual.harvest_gi_t(:);               % [gwB s-1]
        Eff_gi_t(:,ens)=boats.output.annual.effort_gi_t(:);                 % [W]
        
        price          = boats.forcing_used.price(1);          % ONLY OK IF PRICE IS CONSTANT!
        cost_effort    = boats.forcing_used.cost_effort(1);    % ONLY OK IF COST per EFFORT IS CONSTANT!
    
        Rev_gi_t(:,ens)   = Harv_gi_t(:,ens) * price; % / mmolC_2_wetB
        Cost_gi_t(:,ens)   = Eff_gi_t(:,ens) * cost_effort;
    
    end

    H = Harv_gi_t*3600*24*360*1e-12; % [Mt wB yr-1]
    B = Biom_gi_t*1e-12;             % [Mt]
    E = Eff_gi_t*1e-9;               % [GW]
    
    writematrix(H,['outputFiles/H_' num2str(sim_name) '.csv'])
    writematrix(B,['outputFiles/B_' num2str(sim_name) '.csv'])
    writematrix(E,['outputFiles/E_' num2str(sim_name) '.csv'])
    disp(name)
end
