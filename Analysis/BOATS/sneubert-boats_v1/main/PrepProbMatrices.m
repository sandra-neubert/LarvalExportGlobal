%%%% LOAD AND PREP PROB MATRICES
addpath('main')
addpath('general')

%later: create additional dimension to save all 12 prob matrices:
%ProbMatrix = zeros(dimLat, dimLon, 9, months)

data_path = "C:\Users\sandr\Documents\Github\ThesisSandra\Analysis\Movement\Data";

probMatrixR = table2array(readtable(fullfile(data_path,'ProbMatrixTest.csv')));

dimLon = 360;
dimLat = 180;

ProbMatrix = zeros(dimLat, dimLon, 9);

for cellNum = 1:size(probMatrixR,1)
    lon = round(probMatrixR(cellNum, 10));
    lat = round(probMatrixR(cellNum, 11));
    for i = 1:9
        ProbMatrix(lat, lon, i) = probMatrixR(cellNum, i);
    end
end

save('ProbMatTest.mat','ProbMatrix','-v7.3')

