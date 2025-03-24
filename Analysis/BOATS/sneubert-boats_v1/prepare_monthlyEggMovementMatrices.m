%%%% LOAD AND PREP PROB MONTHLY MATRICES
%%% In this script the 12 (monthly) transition matrices, will be combined
%%% to a single structure that can be fed into BOATS.
%Sandra Neubert

addpath('main')
addpath('general')
clear all

%%%% LOAD DATA
data_path = "YOUR_PATH\Analysis\Movement\Data"; %Path to transition matrices that are the outcomes of the R script to generate transition matrices. 

filePattern = fullfile(data_path, 'ProbMatrixGlobal_*.csv'); 
theFiles = dir(filePattern);

for k = 1 : length(theFiles)
  baseFileName = theFiles(k).name;
  fullFileName = fullfile(data_path , baseFileName);
  fprintf(1, 'Now reading %s\n', fullFileName);
  RawMonthsStruct(:,:,k) = table2array(readtable((fullFileName)));

end

clear filePattern k baseFileName fullFileName %clean up

%%%%% initiate output movement matrices

dimLon = 360; %is still in -180; 180 format from R!!!!!
dimLat = 180;

ProbMatrix = zeros(length(theFiles), 9, dimLat, dimLon); %shape: dimLat, dimLon, possible directions, months

for month = 1:length(theFiles) %loop through months
    probMatrixR = RawMonthsStruct(:,:, month);
    for cellNum = 1:size(probMatrixR,1) %loop through BOATS cells
        lon = probMatrixR(cellNum, 10);
        if lon < 0
            lon = lon + 360; 
        end
        lon = round(lon); 
        disp(lon)
        lat = round(probMatrixR(cellNum, 11)+90);
        for i = 1:9 %get probabilities (order: Stay, N, S, W, E, NW, NE, SW, SE)
            ProbMatrix(month, i, lat, lon) = probMatrixR(cellNum, i);
        end
    end
end
ProbMat = ProbMatrix;

save('ProbMat.mat','ProbMat','-v7.3')
