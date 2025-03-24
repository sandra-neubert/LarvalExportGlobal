function croppedM = crop_spatially(matrixToBeCropped, lonStart, lonEnd, latStart, latEnd, maxLon, maxLat) 
    %inputs: 
    % matrixToBeCropped: structure with matrices you want to crop 
    % lonStart: start longitude of cropping. Starts at 0 for 0.5°
    % lonEnd: end longitude
    % latStart: start latitude of cropping. Starts at 0 for 0.5°
    % latEnd: end latitude
    % maxLon: eg 360
    % maxLat: eg 180

    %outputs:
    % croppedM: structure cropped to dimensions you put in

    %maxLon = size(matrixToBeCropped.lon,1);
    %maxLat = size(matrixToBeCropped.lat,1);
    
    croppedM = matrixToBeCropped;
    DataField = fieldnames(croppedM);
    
    %crop rest
    %DataField = fieldnames(croppedM);
    %DataField(strcmp(DataField, 'lon')) = [];
    %DataField(strcmp(DataField, 'lat')) = [];
    
    for i= 1:numel(DataField)
       aField = DataField{i};
       if ismember(maxLon, size(croppedM.(aField)))
    
           whichDim = find(size(croppedM.(aField)) == maxLon); 
           croppedDim = lonStart+1:lonEnd;                
           tempInd = repmat({':'},1,ndims(croppedM.(aField)));
           tempInd(whichDim) = {croppedDim};  
    
           croppedM.(aField) = croppedM.(aField)(tempInd{:});
       end
    
        if ismember(maxLat, size(croppedM.(aField))) %need to separate if statements or will be skipped in ifelse
           
           whichDim = find(size(croppedM.(aField)) == maxLat); 
           croppedDim = (latStart+1:latEnd)+90;   % + 90 because otherwise I'd try to index with negative values             
           tempInd = repmat({':'},1,ndims(croppedM.(aField)));
           tempInd(whichDim) = {croppedDim};  
    
           croppedM.(aField) = croppedM.(aField)(tempInd{:});  
    
       end 
    end