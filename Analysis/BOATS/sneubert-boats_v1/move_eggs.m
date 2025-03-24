function MovedEggs = move_eggs(flux_in_num_eggs, ProbMatrix, Time)
    %Sandra Neubert

    %inputs: 
    % flux_in_num_eggs: produced eggs before movement (shape:
    % nan(FORC.nlat,FORC.nlon,ECOL.nfish))
    % ProbMatrix (later: 12 different ones; shape: (nlat, nlon,
    % direction) with direction = 9 (Stay, N, S, W, E, NW, NE, SW, SE));
    %WHEN NOT GLOBAL: set a buffer of zeros around the probability matrix
    %to avoid wrapping data around?
    % Time: depends on dimension 4 of movement matrices (e.g. monthly
    % matrices: 12 options; if interpolated daily: 360 options) and where
    % BOATS is time-wise in the simulation

    %outputs:
    % MovedEggs: eggs after movement (shape = shape(flux_in_num_eggs))
        
    %init matrix
    EggsM = flux_in_num_eggs; %init each new time step with biomass from previous time step (which is then changed)
   
    ProbMatTime = squeeze(ProbMatrix(Time, :, :, :));
    %calculate Biomass based on Probs
    StayM = EggsM.*squeeze(ProbMatTime(1, :, :));

    NM = EggsM.*squeeze(ProbMatTime(2, :, :));
    NM(isnan(NM))=0;
    NorthM = circshift(NM,[-1 0]); %shifts all cells on up (ie North position) (wraps around but doesnt matter because the prob to go north from last cell is 0) --> see comment in function description for ProbMat

    SM = EggsM.*squeeze(ProbMatTime(3, :, :));
    SM(isnan(SM))=0;
    SouthM = circshift(SM,[1 0]);

    WM = EggsM.*squeeze(ProbMatTime(4, :, :));
    WM(isnan(WM))=0;
    WestM = circshift(WM,[0 -1]);

    EM = EggsM.*squeeze(ProbMatTime(5, :, :));
    EM(isnan(EM))=0;
    EastM = circshift(EM,[0 1]);

    NWM = EggsM.*squeeze(ProbMatTime(6, :, :));
    NWM(isnan(NWM))=0;
    NorthWestM = circshift(NWM,[-1 -1]);

    NEM = EggsM.*squeeze(ProbMatTime(7, :, :));
    NEM(isnan(NEM))=0;
    NorthEastM = circshift(NEM,[-1 1]);

    SWM = EggsM.*squeeze(ProbMatTime(8, :, :));
    SWM(isnan(SWM))=0;
    SouthWestM = circshift(SWM,[1 -1]);

    SEM = EggsM.*squeeze(ProbMatTime(9, :, :));
    SEM(isnan(SEM))=0;
    SouthEastM = circshift(SEM,[1 1]);

    MovedEggs = StayM + NorthM + SouthM + WestM + EastM + NorthWestM + NorthEastM + SouthWestM + SouthEastM; 
