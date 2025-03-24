%% MSY Regulation Prep

%% 1.2 Regulation
simulationLength = 250;
MaxFishing = 119;
oneThirdFishing = 20;
twoThirdsFishing = 15;

%load('Economical_MSY_NoTime.mat') %from BackupForcingsGlobalmonthly
%load('MPAMatrixRandomThirdsCheapest.mat')
MPA_onset = MPAMatrixT;
%save('MPA_onset.mat','MPA_onset','-v7.3')
MPAs1Third = MPA_onset(:,:,1);
MPAs2Thirds = MPA_onset(:,:,2);
MPAsFull = MPA_onset(:,:,3); 

effEtargMSY = Economical_MSY_NoTime.effEtarg;

%one third
effEtargMSY1 = effEtargMSY;
MPAs1ThirdnoNan = MPAs1Third;
MPAs1ThirdnoNan(isnan(MPAs1ThirdnoNan)) =0;
revMPAs1Third = double(~MPAs1ThirdnoNan);
revMPAs1Third(isnan(MPAs1Third)) = NaN; % create matrix where MPAs are zero so we can multiply it with the effort target and the target in MPAs becomes 0

effEtargMSY1 = effEtargMSY1 .*revMPAs1Third;

%two thirds
effEtargMSY2 = effEtargMSY;
MPAs2ThirdsnoNan = MPAs2Thirds;
MPAs2ThirdsnoNan(isnan(MPAs2ThirdsnoNan)) =0;
revMPAs2Thirds = double(~MPAs2ThirdsnoNan);
revMPAs2Thirds(isnan(MPAs2Thirds)) = NaN; % create matrix where MPAs are zero so we can multiply it with the effort target and the target in MPAs becomes 0

effEtargMSY2 = effEtargMSY2 .*revMPAs2Thirds;

%full MPAs
effEtargMSYFull = effEtargMSY;
MPAsFullnoNan = MPAsFull;
MPAsFullnoNan(isnan(MPAsFullnoNan)) =0;
revMPAsFull = double(~MPAsFullnoNan);
revMPAsFull(isnan(MPAsFull)) = NaN; % create matrix where MPAs are zero so we can multiply it with the effort target and the target in MPAs becomes 0

effEtargMSYFull = effEtargMSYFull .*revMPAsFull;

%also put NaNs in no MPA one
effEtargMSY(isnan(MPAsFull)) = NaN;

%now repmat based on onset times
EffNone = repmat(effEtargMSY,1,1,1,1,MaxFishing);%*timestep);
Eff1 = repmat(effEtargMSY1,1,1,1,1,oneThirdFishing);%*timestep);
Eff2 = repmat(effEtargMSY2,1,1,1,1,twoThirdsFishing);%*timestep);
EffFull = repmat(effEtargMSYFull,1,1,1,1,(simulationLength-MaxFishing-oneThirdFishing-twoThirdsFishing));%*timestep);

%stack and permute back to right order
effEtarg = permute([permute(EffNone, [5 1 2 3 4]); permute(Eff1, [5 1 2 3 4]); permute(Eff2, [5 1 2 3 4]); permute(EffFull, [5 1 2 3 4]) ], [2 3 1 4 5]);
effEtarg(isnan(effEtarg)) = 0; %NAs need to be zero to get rid of artefacts in harvest output (mismatch between MPA land and BOATS land)
%societenf always 100 everyhwere for MSY

Economical.effEtarg = effEtarg;%effEtarget;
save('Economical.mat','Economical','-v7.3')