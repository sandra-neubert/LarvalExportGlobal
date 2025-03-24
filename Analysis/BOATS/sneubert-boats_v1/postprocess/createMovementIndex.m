%create movement index to use as predictor in GLM (R)
%Movement index = 1 - Probability to stay in a cell, so a proxy for current
%speed no matter the direction
%Sandra Neubert

Folder = cd;
Folder = fullfile(Folder, '..');

load(fullfile(Folder, 'ProbMat.mat')) %use old ProbMatLand for thesis in backup

meanProb = squeeze(ProbMat(:,1,:,:));
meanProb  = squeeze(mean(meanProb,1));

invStayProb = 1- meanProb; 

load(fullfile(Folder, 'Ecological.mat'))
mask = Ecological.mask;

invStayProb(find(mask == 1)) = 0;

%%plot

addpath("C:\Users\sandr\Documents\MME\MA\m_map")
load(fullfile(Folder, "geo_time.mat"))

LT     = repmat(geo_time.lat(16:165,:),1,360);
LG     = geo_time.lon';
LG     = repmat(LG,150,1);

t = tiledlayout(1,1,'TileSpacing','Compact','Padding','Compact');
nexttile
m_proj('robinson','lon',[0 360], 'lat', [-75 75]); %to get cutout of map give smaller window in [ ]
m_pcolor(LG,LT,invStayProb );
m_coast('patch',[.8 .8 .8],'edgecolor','black');
m_grid('tickdir','out','fontsize',20);%('xticklabels',['','','','','',''],'yticklabels',['','','','','',''],'tickdir','out','linewi',1);
%colormap(m_colmap('step',6));
h=colorbar('eastoutside');%,'Ticks');
h.Label.String = ('Probability'); % gwB m^-^2
h.FontSize = 20;
h.Label.FontSize = 20;
%title(h,'gwB/m2','fontsize',15);
title('Movement index','fontsize',20, 'Position', [0 1.6 0]); % ' (' num2str(round(Biom07_2010*1e-15,0)) ' Gt)'
% set(h,'tickdir','out')
set(gcf, 'PaperUnits', 'centimeters');
set(gcf, 'PaperPosition', [0 0 25 18]); %x_width=10cm y_width=15cm
exportgraphics(t,fullfile(Folder, 'figures/MovementIndex.png'),"Resolution",400)


maskLength = size(invStayProb, 1)*size(invStayProb, 2);

%init files
MoveInd = zeros(maskLength,3);
counter = 1;

%Bring lon, lat and ocean cell info together

    for i = 1:size(invStayProb, 1)
        for j = 1:size(invStayProb, 2)
        MoveInd(counter, 1) = Ecological.lon(i,j);
        MoveInd(counter, 2) = Ecological.lat(i,j);
        MoveInd(counter, 3) = invStayProb(i,j); %land/ocean info
        counter = counter + 1;
        end
    end

writematrix(MoveInd,'MoveInd.csv')