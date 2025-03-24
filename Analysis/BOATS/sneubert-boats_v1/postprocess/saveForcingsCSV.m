%save Forcings in format readable in R to get predictors for GLM
%Sandra Neubert

Folder = cd;
mainFolder = fullfile(Folder, '..');

load(fullfile(mainFolder, "Ecological.mat"))

npp = Ecological.npp;
temp = Ecological.temperature;

meanNPP  = squeeze(mean(npp,3));
meanTemp = squeeze(mean(temp,3));

maskLength = size(meanNPP , 1)*size(meanNPP , 2);

%init files
NPPTempMean = zeros(maskLength,4);
counter = 1;

%Bring lon, lat and ocean cell info together

    for i = 1:size(meanNPP , 1)
        for j = 1:size(meanNPP , 2)
        NPPTempMean(counter, 1) = Ecological.lon(i,j);
        NPPTempMean(counter, 2) = Ecological.lat(i,j);
        NPPTempMean(counter, 3) = meanNPP(i,j); %npp
        %disp(meanNPP(i,j))
        NPPTempMean(counter, 4) = meanTemp(i,j); %temp
        %disp(meanTemp(i,j))
        counter = counter + 1;
        end
    end

writematrix(NPPTempMean,'NPPTempMeanNew.csv')