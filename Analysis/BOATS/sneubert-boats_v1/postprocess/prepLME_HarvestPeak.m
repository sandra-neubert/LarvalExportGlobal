%Save global map data with cells accounting for the actual surface size of
%cells to create time series harvest data for specific LMEs in R
%Sandra Neubert

clear all

Folder = cd;
mainFolder = fullfile(Folder, '..');

load(fullfile(mainFolder, "geo_time.mat"))
load(fullfile(mainFolder, 'output_thesis/Boats_ThesisSN_reg0_mvmt1_d250_h.mat'))

addpath("C:\Users\sandr\Documents\MME\MA\m_map")

surf = repmat(boats.forcing.surf,1,1,250);

harvest = boats.output.annual.harvest_t_out;
harvestLME = permute(harvest, [2,3,1]);

harvestLME = (harvestLME .* surf)*3600*24*360*1e-9;
mask = geo_time.mask_LME(16:165,:);
repmask = repmat(mask,1,1,250);

%harvestLME = permute(harvest, [2,3,1]);
harvestLME(repmask) = nan;

harvestExample = squeeze(harvestLME(:,:,160));

LT     = repmat(geo_time.lat(16:165,:),1,360);
LG     = geo_time.lon';
LG     = repmat(LG,150,1);
% 

%Pmin = -5e-07; Pmax = 5e-07;
figure
m_proj('robinson','lon',[0 360], 'lat', [-75 75]); %to get cutout of map give smaller window in [ ]
m_pcolor(LG,LT,harvestExample ); 
m_coast('patch',[.8 .8 .8],'edgecolor','black');
m_grid('tickdir','out','fontsize',7);
colormap(m_colmap('divergent','step',6));
h=colorbar('eastoutside');%,'Ticks');

%Get oceans cells lon/lat from BOATS (already centroids) for tracer start locations
maskLength = size(harvestLME, 1)*size(harvestLME, 2)*size(harvestLME, 3);
load(fullfile(mainFolder, 'Ecological.mat'))

%init files
BOATSLME = zeros(maskLength,4);
counter = 1;

%Bring lon, lat and ocean cell info together
for year = 1:size(harvestLME, 3)
    for i = 1:size(harvestLME, 1)
        for j = 1:size(harvestLME, 2)
        BOATSLME(counter, 1) = Ecological.lon(i,j);
        BOATSLME(counter, 2) = Ecological.lat(i,j);
        BOATSLME(counter, 3) = harvestLME(i,j,year); %land/ocean info
        BOATSLME(counter, 4) = year;
        counter = counter + 1;
        end
    end
end

writematrix(BOATSLME,'BOATSLMEHarvestm1.csv')
