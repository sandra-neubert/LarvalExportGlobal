%%%%%%%% Prepare MATLAB Spatial Outputs to load into R for downstream anaylsis %%%%%%%
% Takes a snapshot 10 years after 30x30
% Requires outputs from prepOutputMaps.mat

clear all

%oa
H.H_m0_oa_mpa0 = squeeze(load('H_Boats_LEx_d250_m0_oa_mpa0_allEns.mat').Harv(164,:,:,:))*3600*24*360; %ten years after all MPAs
H.H_m1_oa_mpa0 = squeeze(load('H_Boats_LEx_d250_m1_oa_mpa0_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m0_oa_mpa1 = squeeze(load('H_Boats_LEx_d250_m0_oa_mpa1_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m1_oa_mpa1 = squeeze(load('H_Boats_LEx_d250_m1_oa_mpa1_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m0_oa_mpa2 = squeeze(load('H_Boats_LEx_d250_m0_oa_mpa2_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m1_oa_mpa2 = squeeze(load('H_Boats_LEx_d250_m1_oa_mpa2_allEns.mat').Harv(164,:,:,:))*3600*24*360;

%msy
H.H_m0_msy_mpa0 = squeeze(load('H_Boats_LEx_d250_m0_msy_mpa0_allEns.mat').Harv(164,:,:,:))*3600*24*360; %ten years after all MPAs
H.H_m1_msy_mpa0 = squeeze(load('H_Boats_LEx_d250_m1_msy_mpa0_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m0_msy_mpa1 = squeeze(load('H_Boats_LEx_d250_m0_msy_mpa1_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m1_msy_mpa1 = squeeze(load('H_Boats_LEx_d250_m1_msy_mpa1_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m0_msy_mpa2 = squeeze(load('H_Boats_LEx_d250_m0_msy_mpa2_allEns.mat').Harv(164,:,:,:))*3600*24*360;
H.H_m1_msy_mpa2 = squeeze(load('H_Boats_LEx_d250_m1_msy_mpa2_allEns.mat').Harv(164,:,:,:))*3600*24*360;

%prep for GLM
HDiff_oa_m1 = H.H_m1_oa_mpa1 - H.H_m1_oa_mpa0;
HDiff_oa_m0 = H.H_m0_oa_mpa1 - H.H_m0_oa_mpa0;
HDiff_oa_mpa1 = HDiff_oa_m1 - HDiff_oa_m0;
clear HDiff_oa_m1 HDiff_oa_m0

HDiff_oa_m1 = H.H_m1_oa_mpa2 - H.H_m1_oa_mpa0;
HDiff_oa_m0 = H.H_m0_oa_mpa2 - H.H_m0_oa_mpa0;
HDiff_oa_mpa2 = HDiff_oa_m1 - HDiff_oa_m0;
clear HDiff_oa_m1 HDiff_oa_m0

HDiff_msy_m1 = H.H_m1_msy_mpa1 - H.H_m1_msy_mpa0;
HDiff_msy_m0 = H.H_m0_msy_mpa1 - H.H_m0_msy_mpa0;
HDiff_msy_mpa1 = HDiff_msy_m1 - HDiff_msy_m0;
clear HDiff_msy_m1 HDiff_msy_m0

HDiff_msy_m1 = H.H_m1_msy_mpa2 - H.H_m1_msy_mpa0;
HDiff_msy_m0 = H.H_m0_msy_mpa2 - H.H_m0_msy_mpa0;
HDiff_msy_mpa2 = HDiff_msy_m1 - HDiff_msy_m0;
clear HDiff_msy_m1 HDiff_msy_m0

HDiff.HDiff_oa_mpa1 = HDiff_oa_mpa1;
HDiff.HDiff_oa_mpa2 = HDiff_oa_mpa2;
HDiff.HDiff_msy_mpa1 = HDiff_msy_mpa1;
HDiff.HDiff_msy_mpa2 = HDiff_msy_mpa2;

fileNames = ["HDiff_oa_mpa1", "HDiff_oa_mpa2", "HDiff_msy_mpa1", "HDiff_msy_mpa2"];
load("Ecological.mat")

npp = Ecological.npp;
npp_ed = Ecological.npp_ed;
temp = Ecological.temperature;

meanNPP  = squeeze(mean(npp,3));
meanTemp = squeeze(mean(temp,3));
meanNPP_ed  = squeeze(mean(npp_ed,3));

for i = 1:length(fileNames)
        for j = 1:size(HDiff_oa_mpa1, 3)

        currFile = squeeze(HDiff.(fileNames{i})(:,:,j)); %get right file and right ensemble
        maskLength = size(currFile, 1)*size(currFile, 2);
        
        HarvAdd = zeros(maskLength,6);
        counter = 1;
            
        
        for k = 1:size(currFile, 1)
                for m = 1:size(currFile, 2)
                HarvAdd(counter, 1) = Ecological.lon(k,m);
                HarvAdd(counter, 2) = Ecological.lat(k,m);
                HarvAdd(counter, 3) = currFile(k,m); %land/ocean info
                HarvAdd(counter, 4) = meanNPP(k,m);
                HarvAdd(counter, 5) = meanNPP_ed(k,m);
                HarvAdd(counter, 6) = meanTemp(k,m);
                counter = counter + 1;
                end
        end
        
        lon =  HarvAdd(:,1);
        lat =  HarvAdd(:,2);
        val =  HarvAdd(:,3);
        npp = HarvAdd(:,4);
        npp_ed = HarvAdd(:,5);
        temp = HarvAdd(:,6);

        level = wildcardPattern + "_";
        pat = asManyOfPattern(level);
        mpa = extractAfter(fileNames{i},pat);
        mpa = repelem([{mpa}], [length(val)])';
        clear level pat
        
        reg = extractBetween(fileNames{i},'HDiff_','_mpa');
        reg = repelem([{reg}], [length(val)])';

        ens = repelem([j], [length(val)])';

        if i == 1 && j == 1
            %disp("Now")
            GLMDat = table(lon, lat, val, ens, mpa, reg, npp, npp_ed, temp);

        else
            %disp("Else")
            GLMDatNew = table(lon, lat, val, ens, mpa, reg, npp, npp_ed, temp);
            GLMDat = [GLMDat;GLMDatNew];

        end
        end
end

writetable(GLMDat,'YOUR_PATH\Analysis\Movement\RScripts\BOATSOutputEnsemble\FilesSpatial\GLMDat_y164.csv')

%prep  for plotting
fileNamesH = ["H_m0_oa_mpa1", "H_m1_oa_mpa1", "H_m0_msy_mpa1", "H_m1_msy_mpa1",...
    "H_m0_oa_mpa2", "H_m1_oa_mpa2", "H_m0_msy_mpa2", "H_m1_msy_mpa2"];

for i = 1:length(fileNamesH)
        for j = 1:size(H.H_m0_oa_mpa1, 3)

        currFile = squeeze(H.(fileNamesH{i})(:,:,j)); %get right file and right ensemble
        maskLength = size(currFile, 1)*size(currFile, 2);
        
        HarvAdd = zeros(maskLength,3);
        counter = 1;
            
        
        for k = 1:size(currFile, 1)
                for m = 1:size(currFile, 2)
                HarvAdd(counter, 1) = Ecological.lon(k,m);
                HarvAdd(counter, 2) = Ecological.lat(k,m);
                HarvAdd(counter, 3) = currFile(k,m); %land/ocean info
                counter = counter + 1;
                end
        end
        
        
        lon =  HarvAdd(:,1);
        lat =  HarvAdd(:,2);
        val =  HarvAdd(:,3);

        level = wildcardPattern + "_";
        pat = asManyOfPattern(level);
        mpa = extractAfter(fileNamesH{i},pat);
        mpa = repelem([{mpa}], [length(val)])';
        clear level pat
        
        reg = extractBetween(fileNamesH{i},'H_','_mpa');
        reg = repelem([{reg}], [length(val)])';

        ens = repelem([j], [length(val)])';

        if i == 1 && j == 1
            %disp("Now")
            Harv = table(lon, lat, val, ens, mpa, reg);

        else
            %disp("Else")
            HarvNew = table(lon, lat, val, ens, mpa, reg);
            Harv = [Harv;HarvNew];

        end
        end
end

writetable(Harv,'YOUR_PATH\Analysis\Movement\RScripts\BOATSOutputEnsemble\FilesSpatial\HarvestSpatial_y164.csv')

%%%
%%% Same but means
%%% GLMDatMean
fileNames = ["HDiff_oa_mpa1", "HDiff_oa_mpa2", "HDiff_msy_mpa1", "HDiff_msy_mpa2"];

for i = 1:length(fileNames)
        %for j = 1:size(HDiff_oa_mpa1, 3)

        currFile = squeeze(mean(HDiff.(fileNames{i}),3,'omitnan')); %get right file and right ensemble
        maskLength = size(currFile, 1)*size(currFile, 2);
        
        HarvAddMean = zeros(maskLength,6);
        counter = 1;
            
        
        for k = 1:size(currFile, 1)
                for m = 1:size(currFile, 2)
                HarvAddMean(counter, 1) = Ecological.lon(k,m);
                HarvAddMean(counter, 2) = Ecological.lat(k,m);
                HarvAddMean(counter, 3) = currFile(k,m); %land/ocean info
                HarvAddMean(counter, 4) = meanNPP(k,m);
                HarvAddMean(counter, 5) = meanNPP_ed(k,m);
                HarvAddMean(counter, 6) = meanTemp(k,m);
                counter = counter + 1;
                end
        end
        
        lon =  HarvAddMean(:,1);
        lat =  HarvAddMean(:,2);
        val =  HarvAddMean(:,3);
        npp = HarvAddMean(:,4);
        npp_ed = HarvAddMean(:,5);
        temp = HarvAddMean(:,6);

        level = wildcardPattern + "_";
        pat = asManyOfPattern(level);
        mpa = extractAfter(fileNames{i},pat);
        mpa = repelem([{mpa}], [length(val)])';
        clear level pat
        
        reg = extractBetween(fileNames{i},'HDiff_','_mpa');
        reg = repelem([{reg}], [length(val)])';


        if i == 1 
            %disp("Now")
            GLMDatEnsMean = table(lon, lat, val, npp, npp_ed, temp, mpa, reg);

        else
            %disp("Else")
            GLMDatEnsMeanNew = table(lon, lat, val, npp, npp_ed, temp, mpa, reg);
            GLMDatEnsMean = [GLMDatEnsMean;GLMDatEnsMeanNew];

        end
       
end

writetable(GLMDatEnsMean,'YOUR_PATH\Analysis\Movement\RScripts\BOATSOutputEnsemble\FilesSpatial\GLMDatEnsMean_y164.csv')

%%%
%prep  for plotting
fileNamesH = ["H_m0_oa_mpa1", "H_m1_oa_mpa1", "H_m0_msy_mpa1", "H_m1_msy_mpa1",...
    "H_m0_oa_mpa2", "H_m1_oa_mpa2", "H_m0_msy_mpa2", "H_m1_msy_mpa2"];

for i = 1:length(fileNamesH)
        %for j = 1:size(H.H_m0_oa_mpa1, 3)

        currFile = squeeze(mean(H.(fileNamesH{i}),3,'omitnan')); %get right file and right ensemble
        maskLength = size(currFile, 1)*size(currFile, 2);
        
        HarvAdd = zeros(maskLength,3);
        counter = 1;
            
        
        for k = 1:size(currFile, 1)
                for m = 1:size(currFile, 2)
                HarvAdd(counter, 1) = Ecological.lon(k,m);
                HarvAdd(counter, 2) = Ecological.lat(k,m);
                HarvAdd(counter, 3) = currFile(k,m); %land/ocean info
                counter = counter + 1;
                end
        end
        
        
        lon =  HarvAdd(:,1);
        lat =  HarvAdd(:,2);
        val =  HarvAdd(:,3);

        level = wildcardPattern + "_";
        pat = asManyOfPattern(level);
        mpa = extractAfter(fileNamesH{i},pat);
        mpa = repelem([{mpa}], [length(val)])';
        clear level pat
        
        reg = extractBetween(fileNamesH{i},'H_','_mpa');
        reg = repelem([{reg}], [length(val)])';


        if i == 1
            %disp("Now")
            HarvEnsMean = table(lon, lat, val, mpa, reg);

        else
            %disp("Else")
            HarvNew = table(lon, lat, val, mpa, reg);
            HarvEnsMean = [HarvEnsMean;HarvNew];

        end
        %end
end

writetable(HarvEnsMean,'YOUR_PATH\Analysis\Movement\RScripts\BOATSOutputEnsemble\FilesSpatial\HarvestSpatialEnsMean_y164.csv')


%%% 
%%MPAs
mpa1 = squeeze(load("MPA1.mat").MPA_onset(:,:,3));
mpa2 = squeeze(load("MPA2.mat").MPA_onset(:,:,3));

maskLength = size(mpa1, 1)*size(mpa1, 2);

MPA1 = zeros(maskLength,3);
counter = 1;
    
for i = 1:size(mpa1, 1)
        for j = 1:size(mpa1, 2)
        MPA1(counter, 1) = Ecological.lon(i,j);
        MPA1(counter, 2) = Ecological.lat(i,j);
        MPA1(counter, 3) = mpa1(i,j); %land/ocean info
        counter = counter + 1;
        end
end
writematrix(MPA1,'MPA1.csv')

MPA2= zeros(maskLength,3);
counter = 1;
    
for i = 1:size(mpa2, 1)
        for j = 1:size(mpa2, 2)
        MPA2(counter, 1) = Ecological.lon(i,j);
        MPA2(counter, 2) = Ecological.lat(i,j);
        MPA2(counter, 3) = mpa2(i,j); %land/ocean info
        counter = counter + 1;
        end
end

writematrix(MPA2,'MPA2.csv')