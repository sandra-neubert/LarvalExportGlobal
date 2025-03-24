%% OA Regulation Prep

simulationLength = 250;
MaxFishing = 119;
oneThirdFishing = 20;
twoThirdsFishing = 15;

%% Cheapest
load('MPAMatrixRandomThirdsCheapest.mat');

MPAs1Third = MPAMatrixC(:,:, 1);
MPAs2Thirds = MPAMatrixC(:,:, 2);
MPAsFull = MPAMatrixC(:,:, 3);

noMPAs = MPAMatrixC(:,:,1);
none = find(noMPAs>0);
noMPAs(none) = 0;

%effort target
effNone = noMPAs;
effNone(find(effNone == 0)) = 100;

eff1Third = MPAs1Third;
eff1Third(find(eff1Third == 0)) = 100;
eff1Third(find(eff1Third == 1)) = 0;

eff2Thirds = MPAs2Thirds;
eff2Thirds(find(eff2Thirds == 0)) = 100;
eff2Thirds(find(eff2Thirds == 1)) = 0;

effFull = MPAsFull;
effFull(find(effFull == 0)) = 100;
effFull(find(effFull == 1)) = 0;

EffNone = repmat(effNone,1,1,MaxFishing);
Eff1Third = repmat(eff1Third,1,1,oneThirdFishing);
Eff2Thirds = repmat(eff2Thirds,1,1,twoThirdsFishing);
EffFull = repmat(effFull,1,1,(simulationLength-MaxFishing-oneThirdFishing-twoThirdsFishing));


effEtarget = permute([permute(EffNone, [3 1 2]); permute(Eff1Third, [3 1 2]); permute(Eff2Thirds, [3 1 2]); permute(EffFull, [3 1 2]) ], [2 3 1]);
effEtarget = repmat(effEtarget,1,1,1,3); %all groups treated the same way, no ensemble so far

%ensemble
effEtarg_Cheapest = repmat(effEtarget,1,1,1,1,5);
save('effEtarg_oa_Cheapest.mat','effEtarg_Cheapest','-v7.3') 


%societal enforcement
socNone = noMPAs;

soc1Third = MPAs1Third;
soc1Third(find(soc1Third == 1)) = 100;

soc2Thirds = MPAs2Thirds;
soc2Thirds(find(soc2Thirds == 1)) = 100;

socFull= MPAsFull;
socFull(find(socFull == 1)) = 100;

SocNone = repmat(socNone,1,1,MaxFishing);
Soc1Third = repmat(soc1Third,1,1,oneThirdFishing);
Soc2Thirds = repmat(soc2Thirds,1,1,twoThirdsFishing);
SocFull = repmat(socFull,1,1,(simulationLength-MaxFishing-oneThirdFishing-twoThirdsFishing));

societenf_Cheapest = permute([permute(SocNone, [3 1 2]); permute(Soc1Third, [3 1 2]); permute(Soc2Thirds, [3 1 2]); permute(SocFull, [3 1 2]) ], [2 3 1]);
save('societenf_oa_Cheapest.mat','societenf_Cheapest','-v7.3') 
%% Top 30
load('MPAMatrixRandomThirdsTop30.mat');

MPAs1Third = MPAMatrixT(:,:, 1);
MPAs2Thirds = MPAMatrixT(:,:, 2);
MPAsFull = MPAMatrixT(:,:, 3);

noMPAs = MPAMatrixT(:,:,1);
none = find(noMPAs>0);
noMPAs(none) = 0;

%effort target
effNone = noMPAs;
effNone(find(effNone == 0)) = 100;

eff1Third = MPAs1Third;
eff1Third(find(eff1Third == 0)) = 100;
eff1Third(find(eff1Third == 1)) = 0;

eff2Thirds = MPAs2Thirds;
eff2Thirds(find(eff2Thirds == 0)) = 100;
eff2Thirds(find(eff2Thirds == 1)) = 0;

effFull = MPAsFull;
effFull(find(effFull == 0)) = 100;
effFull(find(effFull == 1)) = 0;

EffNone = repmat(effNone,1,1,MaxFishing);
Eff1Third = repmat(eff1Third,1,1,oneThirdFishing);
Eff2Thirds = repmat(eff2Thirds,1,1,twoThirdsFishing);
EffFull = repmat(effFull,1,1,(simulationLength-MaxFishing-oneThirdFishing-twoThirdsFishing));


effEtarget = permute([permute(EffNone, [3 1 2]); permute(Eff1Third, [3 1 2]); permute(Eff2Thirds, [3 1 2]); permute(EffFull, [3 1 2]) ], [2 3 1]);
effEtarget = repmat(effEtarget,1,1,1,3); %all groups treated the same way, no ensemble so far

%ensemble
effEtarg_Top30 = repmat(effEtarget,1,1,1,1,5);
save('effEtarg_oa_Top30.mat','effEtarg_Top30','-v7.3') 

%societal enforcement
socNone = noMPAs;

soc1Third = MPAs1Third;
soc1Third(find(soc1Third == 1)) = 100;

soc2Thirds = MPAs2Thirds;
soc2Thirds(find(soc2Thirds == 1)) = 100;

socFull= MPAsFull;
socFull(find(socFull == 1)) = 100;

SocNone = repmat(socNone,1,1,MaxFishing);
Soc1Third = repmat(soc1Third,1,1,oneThirdFishing);
Soc2Thirds = repmat(soc2Thirds,1,1,twoThirdsFishing);
SocFull = repmat(socFull,1,1,(simulationLength-MaxFishing-oneThirdFishing-twoThirdsFishing));

societenf_Top30 = permute([permute(SocNone, [3 1 2]); permute(Soc1Third, [3 1 2]); permute(Soc2Thirds, [3 1 2]); permute(SocFull, [3 1 2]) ], [2 3 1]);
save('societenf_oa_Top30.mat','societenf_Top30','-v7.3') 