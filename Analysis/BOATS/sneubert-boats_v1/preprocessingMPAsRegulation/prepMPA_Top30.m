%%%%%%%%%% Script to prepare MPA implementation without current MPAs %%%%%%%
%%% from Boats_VB1_reg0_mvmt0_monthly250_h.mat: maximum fishing in year 147
%%% (=1995) %UPDATE AFTER NO MPA FOR OA and MSY FOR No-Movement
%%% create a 5D file (lat, lon, nfish, nensemble, time)
%%% Here: time = 0:250; (0:193 no MPAs; 193:223 current MPAs; 223:228 half MPAs; 228:250: all MPAs)

clear all

input_path = "YOUR_PATH\Analysis\BOATS\sneubert-boats_v1\input"; 
mpaPath = fullfile(input_path, "mpaWaldron", "mpa_scenarios.csv");
mpasWaldron = readmatrix(mpaPath); %order: "Current_MPAs","Cheapest_Half","Cheapest_Full","Top30_Half","Top30_Full"

v = (1:1:64800)';
MPAMatrix = horzcat(mpasWaldron, v);
%ind1 = (mpasWaldron (:,2) < 75) & (mpasWaldron (:,2) > -75);
%MPAMatrix = mpasWaldron(ind1,:);

%% 1. Prep MPAs
%% 1.1 Cheapest
fullSum = sum(MPAMatrix(:,7), "omitnan");

FullMPAsT = MPAMatrix(:, [1,2,7, 8]);
ind2 = FullMPAsT(:,3) == 1;
MPAFullT = FullMPAsT(ind2,:);

%randomly get 1/3 and 2/3 of protected cells
%get 2/3 and then select half of that for 1/3
TwoThirds = round(size(MPAFullT,1)/3)*2;
selected2Thirds = MPAFullT(randperm(fullSum, TwoThirds), :);
selected1Third = selected2Thirds(randperm(TwoThirds, TwoThirds/2), :);

%get cells that have not been selected in one third and two thirds but in
%full and set to zero
ind1Third = ~ismember(MPAFullT(:,4), selected1Third(:,4));
noMPA1Third = MPAFullT(ind1Third,:);
noMPA1Third(find(noMPA1Third(:,3) == 1), 3) = 0;

ind2Thirds = ~ismember(MPAFullT(:,4), selected2Thirds(:,4));
noMPA2Thirds = MPAFullT(ind2Thirds,:);
noMPA2Thirds(find(noMPA2Thirds(:,3) == 1), 3) = 0;

%get cells that are zero or NaN even in full MPA
neverMPA = FullMPAsT(:,3) ~= 1;
neverMPAC = FullMPAsT(neverMPA,:);

%combine all this info (MPA, not MPA in 1/3 or 2/3, never MPA);
C1Third = sortrows(vertcat(selected1Third, noMPA1Third, neverMPAC),4);
C2Thirds = sortrows(vertcat(selected2Thirds, noMPA2Thirds, neverMPAC),4);

%stack 1/3, 2/3, Full in new table
CombinedCMPAs = horzcat(C1Third(:, [1:3]), C2Thirds(:, [3]), FullMPAsT(:, [3]));

%Convert to matrix
MPAMatrixT = zeros(180, 360, 3); 

for cellNum = 1:size(CombinedCMPAs,1) %loop through BOATS cells
        lon = round(CombinedCMPAs(cellNum, 1));
        lat = round(CombinedCMPAs(cellNum, 2)+90);
        for i = 3:5 
            MPAMatrixT(lat, lon, i-2) = CombinedCMPAs(cellNum, i);
        end
end

%crop Matrix 
latStart = -75; 
latEnd = 75; 
MPAMatrixT = MPAMatrixT((latStart+1:latEnd)+90,:,:);
Folder = cd;
save(fullfile(Folder, 'preprocessingMPAsRegulation/MPAMatrixRandomThirdsTop30.mat'),'MPAMatrixT','-v7.3')
