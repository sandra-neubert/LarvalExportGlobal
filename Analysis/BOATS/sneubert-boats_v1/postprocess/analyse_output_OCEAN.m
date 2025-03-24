% FAST ANALYSIS OF OUTPUT FROM ALL ENSEMBLES TO USE ON EARTH
% ed. K SCHERRER (05-2019)

% LEGEND FOR DIMENSION OF OUTPUT
% -------------------------------------------------------------------------
% Original units:
% all variables are given per size group
            % Biomass spectrum  [g wetB m-2 g-1]
            % Harvest spectrum  [g wetB m-2 s-1 g-1]
            % Effort            [W m-2]
          
% Alternative processing types:
% si		size integral                               (removes g-1)
% gi		global integral                             (sum over area)	
% 2di		2D spatial integral                         (removes m-2)
% 2di_si	2D spatial integral with size integration	(both removes m-2 and sum of groups)

% Output units:
% fish_g_out 		si          [gwB m-2] 		per group
% harvest_g_out 	si          [gwB m-2 s-1] 	per group
% effort_g_out		none		[W m-2] 		per group
% 
% fish_t_out            		[gwB m-2]		group sum
% harvest_t_out 				[gwB m-2 s-1]	group sum
% effort_t_out					[W m-2]         group sum
% 
% fish_gi_g         gi_si		[gwB] 			per group global integral
% harvest_gi_g 		gi_si		[gwB s-1] 		per group global integral
% effort_gi_g		gi          [W] 			per group global integral
% 
% fish_gi_t                 	[gwB] 			group sum global integral
% harvest_gi_t              	[gwB s-1] 		group sum global integral
% effort_gi_t               	[W] 			group sum global integral

% All the outputs are annual averages if mode 'annual' is chosen
% Conversion factors even out for: tonne/km^-2 = g/m^-2
% -------------------------------------------------------------------------


% Coordinates GLOBAL simulation
load("geo_time.mat") % OCEAN
% load('/Users/kimscherrer/Documents/PhD/BOATS/BOATS_VB1/input/Global/Bioforcing/geo_time.mat')

lat     = repmat(geo_time.lat,1,360);
lon     = geo_time.lon';
lon     = repmat(lon,180,1);
mask    = geo_time.mask_land_2d;
surface = geo_time.surf;

% Ensembles to load 
ensemble= 1;%[6290,6363,6920,8874,9459];

sim_time = 250; %86; % define length of simulation in years (29 for 150 Tg, 16 for 5-47 Tg)
n_group  = 3; % define number of size groups for size grouped output
n_ens    = length(ensemble);

% Pre-allocate output by size group
Biom_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
Harv_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
Eff_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));

% Pre-allocate output summed over size groups
Biom = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
Harv = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
Eff = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));

% Pre-allocate output globally integrated per group
Biom_gi_g = zeros(sim_time, n_group, length(ensemble));
Harv_gi_g = zeros(sim_time, n_group, length(ensemble));
Eff_gi_g  = zeros(sim_time, n_group, length(ensemble));

% Pre-allocate output globally integrated 
Biom_gi_t = zeros(sim_time,length(ensemble));
Harv_gi_t = zeros(sim_time,length(ensemble));
Eff_gi_t  = zeros(sim_time,length(ensemble));

% Pre-allocate the time for regulation onset array
Time_reg_ons = NaN(geo_time.nlat, geo_time.nlon, n_group, length(ensemble));

qcatch = NaN(sim_time*12, length(ensemble));


% Load
for ens=1:length(ensemble)
    
    % Load ensemble output
    ESM = 'IPSL';
    sim_name = ['MPA_TNC_PROJ_' num2str(ESM) '_SSP5_scen0'];
    load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/Boats_VB1_MPA_TNC_' num2str(ESM) '_h_ind_' num2str(ensemble(ens)) '.mat'])
%     load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/Global_regBoats_VB1_h_ind_' num2str(ensemble(ens)) '.mat'])
%     load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER_peaks_adjusted_IRONLIM/' num2str(sim_name) '/Global_regBoats_VB1_nuclear_h_ind_' num2str(ensemble(ens)) '.mat'])
%     load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/Global_regBoats_VB1_h_ind_' num2str(ensemble(ens)) '.mat'])
%     load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER/' num2str(sim_name) '/Global_regBoats_VB1_nuclear_h_ind_' num2str(ensemble(ens)) '.mat'])
%     load (['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/Global_reg/iron_lim/' num2str(sim_name) '/Boats_VB1_ironlim_h_ind_' num2str(ensemble(ens)) '.mat'])
 
    % Create arrays with all ensembles in last dimension
    % Per size group [time, lat, lon, group, ens]
    Biom_g(:,:,:,:,ens)=boats.output.annual.fish_g_out(:,:,:,:);        % [gwB m-2]
    Harv_g(:,:,:,:,ens)=boats.output.annual.harvest_g_out(:,:,:,:);     % [gwB m-2 s-1]
    Eff_g(:,:,:,:,ens)=boats.output.annual.effort_g_out(:,:,:,:);       % [W m-2]
    
    % Integrated size groups [time, lat, lon, ens]
    Biom(:,:,:,ens)=boats.output.annual.fish_t_out(:,:,:);              % [gwB m-2]
    Harv(:,:,:,ens)=boats.output.annual.harvest_t_out(:,:,:);           % [gwB m-2 s-1]
    Eff(:,:,:,ens)=boats.output.annual.effort_t_out(:,:,:);             % [W m-2]
    
    % Global integral per group [time, group, ens]
    Biom_gi_g(:,:,ens)=boats.output.annual.fish_gi_g(:,:);              % [gwB]
    Harv_gi_g(:,:,ens)=boats.output.annual.harvest_gi_g(:,:);           % [gwB s-1]
    Eff_gi_g(:,:,ens)=boats.output.annual.effort_gi_g(:,:);             % [W]
    
    % Global integral [time, ens]
    Biom_gi_t(:,ens)=boats.output.annual.fish_gi_t(:);                  % [gwB]
    Harv_gi_t(:,ens)=boats.output.annual.harvest_gi_t(:);               % [gwB s-1]
    Eff_gi_t(:,ens)=boats.output.annual.effort_gi_t(:);                 % [W]
    
%     Time_reg_ons(:,:,:,ens) = boats.output.stats.indt_at_reg_onset(:,:,:);
    
    qcatch(:,ens) = boats.forcing_used.catchability;

end

% Price, cost and profit [$ m-2 s-1]
price          = boats.forcing_used.price(1);          % ONLY OK IF PRICE IS CONSTANT!
cost_effort    = boats.forcing_used.cost_effort(1);    % ONLY OK IF COST per EFFORT IS CONSTANT!
% mmolC_2_wetB   = 12*10/1000;

Rev_g    = Harv_g * price; % / mmolC_2_wetB
Cost_g   = Eff_g * cost_effort;
Prof_g   = Rev_g - Cost_g;

Rev    = Harv * price; %/ mmolC_2_wetB
Cost   = Eff * cost_effort;
Prof   = Rev - Cost;

Rev_gi_g    = Harv_gi_g * price; % / mmolC_2_wetB
Cost_gi_g   = Eff_gi_g * cost_effort;
Prof_gi_g   = Rev_gi_g - Cost_gi_g;

Rev_gi_t    = Harv_gi_t * price; % / mmolC_2_wetB
Cost_gi_t   = Eff_gi_t * cost_effort;
Prof_gi_t   = Rev_gi_t - Cost_gi_t;

peak_yr = 140;
xmin = peak_yr-45;
xmax = xmin+150; % To show simulation from 1950-2100

% Unit conversion
H = Harv_gi_t*3600*24*360*1e-12; % [Mt wB yr-1]
B = Biom_gi_t*1e-12;             % [Mt]
E = Eff_gi_t*1e-9;               % [GW]
P = Prof_gi_t*3600*24*360*1e-9;  % [B$ yr-1]

% Harv_USec_gr2 = squeeze(Harv_g(:,131,291,2,:));

% Ensemble mean
Biom_avg = squeeze(mean(Biom,4,'omitnan'));
Eff_avg  = squeeze(mean(Eff,4,'omitnan'));
Harv_avg = squeeze(mean(Harv,4,'omitnan'));
Prof_avg = squeeze(mean(Prof,4,'omitnan'));


save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_gi_t_' num2str(sim_name) '.mat'],'Biom_gi_t','Harv_gi_t','Eff_gi_t','Prof_gi_t') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'
save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_comp_w_Ryan_t_' num2str(sim_name) '.mat'],'Harv_g','Eff_g','Prof_g') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_g_' num2str(sim_name) '.mat'],'Eff_g','Harv_g','Biom_g', 'qcatch', '-v7.3') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_t_' num2str(sim_name) '.mat'],'Eff','Harv','Biom', 'qcatch', '-v7.3') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'

% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER_peaks_adjusted_IRONLIM/' num2str(sim_name) '/output_gi_t_CESMnucl_' num2str(sim_name) '.mat'],'Biom_gi_t','Harv_avg','Eff_avg') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER_peaks_adjusted_IRONLIM/' num2str(sim_name) '/output_g_CESMnucl_' num2str(sim_name) '.mat'],'Biom','Harv') %,'Harv_avg','Biom_avg','Eff_avg','Prof_avg'

% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER/' num2str(sim_name) '/oustput_CESMnucl_all.mat'],'Harv_g','Biom_g','Eff_g')
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/NUCLEAR_WINTER/' num2str(sim_name) '/output_gi_t_CESMnucl.mat'],'Harv_gi_t','Biom_gi_t','Eff_gi_t','Prof_gi_t')
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_MPA_map.mat'],'Biom_g')
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_' num2str(sim_name) '.mat'],'Harv_avg','Biom_avg','Eff_avg','Prof_avg')
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_' num2str(sim_name) '_all.mat'],'Harv_g','Biom_g','Eff_g','Prof_g','Time_reg_ons','qcatch')
% save(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_' num2str(sim_name) '_gi_t.mat'],'Harv_gi_t','Biom_gi_t','Eff_gi_t','Prof_gi_t')


%% Plot
figure
h1 = subplot(2,2,1);
plot(H, 'Linewidth',2)
xlim([xmin xmax])
xticks([105 145 185 225])
set(h1,'XTickLabel',{'1960','2000','2040','2080'})
hold on
plot(mean(H,2),'Linewidth',4,'color','black')
set(gca,'FontSize',15,'Linewidth',1)
xlabel(h1,'Year')
ylabel(h1,'Harvest (Mt wB)')

subplot(2,2,2)
h2 = plot(E, 'Linewidth',2);
xlim([xmin xmax])
xticks([105 145 185 225])
set(h2,'XTickLabel',{'1960','2000','2040','2080'})
hold on
plot(mean(E,2),'Linewidth',4,'color','black')
set(gca,'FontSize',15,'Linewidth',1)
xlabel('Year')
ylabel('Effort (GW)')

subplot(2,2,3)
h3 = plot(B, 'Linewidth',2);
xlim([xmin xmax])
xticks([105 145 185 225])
set(h3,'XTickLabel',{'1960','2000','2040','2080'})
hold on
plot(mean(B,2),'Linewidth',4,'color','black')
set(gca,'FontSize',15,'Linewidth',1)
xlabel('Year')
ylabel('Biomass (Mt wB)')

subplot(2,2,4)
h4 = plot(P, 'Linewidth',2);
xlim([xmin xmax])
xticks([105 145 185 225])
set(h4,'XTickLabel',{'1960','2000','2040','2080'})
hold on
plot(mean(P,2),'Linewidth',4,'color','black')
set(gca,'FontSize',15,'Linewidth',1)
xlabel('Year')
ylabel('Profit (B$)')


% % savefig(['/data/results/kscherrer/BOATS/BOATS_VB1_regsim/' num2str(sim_name) '/output_EARTH.fig'])
% 
% %% LEFTOVERS
% 
% % % Pre-allocate output by size group
% % Biom_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
% % Harv_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
% % Eff_g = zeros(sim_time, geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
% 
% % % Pre-allocate output summed over size groups
% % Biom = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
% % Harv = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
% % Eff = zeros(sim_time, geo_time.nlat, geo_time.nlon, length(ensemble));
% 
% % % Pre-allocate regulation stats
% % Reg_onset = zeros(geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
% % EffEtarg = zeros(geo_time.nlat, geo_time.nlon, n_group, length(ensemble));
% 
% 
% %     % Per size group [time, lat, lon, group, ens]
% %     Biom_g(:,:,:,:,ens)=boats.output.annual.fish_g_out(:,:,:,:);        % [gwB m-2]
% %     Harv_g(:,:,:,:,ens)=boats.output.annual.harvest_g_out(:,:,:,:);     % [gwB m-2 s-1 OR yr-1?????]
% %     Eff_g(:,:,:,:,ens)=boats.output.annual.effort_g_out(:,:,:,:);       % [W m-2]
% %     
% %     % Integrated size groups [time, lat, lon, ens]
% %     Biom(:,:,:,ens)=boats.output.annual.fish_t_out(:,:,:);              % [gwB m-2]
% %     Harv(:,:,:,ens)=boats.output.annual.harvest_t_out(:,:,:);           % [gwB m-2 s-1]
% %     Eff(:,:,:,ens)=boats.output.annual.effort_t_out(:,:,:);             % [W m-2]
% %     
%             
%     % Regulation stats
% %     Reg_onset(:,:,:,ens)=boats.output.stats.regulation_onset(:,:,:);
% %     EffEtarg(:,:,:,ens)=boats.forcing.effEtarg(:,:,:);
% 
% % % Integrated size group [time, lat, lon, ens]
% % Rev         = Harv * price / mmolC_2_wetB;
% % Cost        = Eff * cost_effort;
% % Prof        = Rev - Cost;
% % % Per size group [time, lat, lon, group, ens]
% % Rev_g       = Harv_g * price / mmolC_2_wetB;
% % Cost_g      = Eff_g * cost_effort;
% % Prof_g      = Rev_g - Cost_g;
% % Global integral [time, group, ens]