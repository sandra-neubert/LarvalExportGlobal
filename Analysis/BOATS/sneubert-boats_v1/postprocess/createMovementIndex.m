%create movement index to use as predictor in GLM (R)
%Movement index = 1 - Probability to stay in a cell, so a proxy for current
%speed no matter the direction
%Sandra Neubert

Folder = cd;
Folder = fullfile(Folder, '..');

load(fullfile(Folder, 'ProbMat.mat')) 

meanProb = squeeze(ProbMat(:,1,:,:));
meanProb  = squeeze(mean(meanProb,1));

invStayProb = 1- meanProb; 

load(fullfile(Folder, 'Ecological.mat'))
mask = Ecological.mask;

invStayProb(find(mask == 1)) = 0;

writematrix(MoveInd,'MoveInd.csv')