function intM = interpolate_temporally(MatrixToBeInterpolated, currentTimeStep, newTimeStep, IntMethod) 

    %inputs: 
    % MatrixToBeInterpolated: structure with matrices you want to temporally interpolate 
    % currentTimeStep: should usually be 12 (monthly BOATS timesteps)
    % newTimeStep: when going from monthly timesteps, how many steps in a
    % month, e.g. for daily BOATS time steps, newTimeStep = 30; 
    % IntMethod: here: only use 'linear' or 'cubic' or 'v5cubic' - 'pchip', 'spline', or 'makima' have problems with NAN values in data - for
    % additional info, see https://de.mathworks.com/help/matlab/ref/interp1.html#btwp6lt-1-method

    %outputs:
    % intM: structure interpolated to specified time steps
    
    %Display disclaimer
    disp(sprintf("<strong>Disclaimer:</strong> \nThis automated interpolation script is based on the input data the writer of this script had when running BOATS. \n" + ...
        "If the input data structure is different, some fields in the structure will have to be altered manually. \nDouble-check interpolated input structures " + ...
        "if they are as expected."))

    %shorten Names
    cTime = currentTimeStep;
    nTime = newTimeStep;
    intM = MatrixToBeInterpolated;

    %Interpolate
    if isstruct(intM)

        DataField = fieldnames(intM);
        
        for fieldNum = 1:numel(DataField)
           aField = DataField{fieldNum};
           
           if aField == "mpery"
                 intM.(aField) = cTime *nTime; %steps in a year
           end
    
           if ismember(cTime, size(intM.(aField)))
             if aField == "time"
                 intM.(aField) = (1:(cTime *nTime))'; %% ASSUMES THAT WE NOW HAVE daily BOATS TIME STEPS
                 if cTime ~= 12 || nTime ~= 30 % sanity check for this field
                     disp("Check '.time' field manually if output is as expected.")
                 end
             else
                whichDim = find(size(intM.(aField)) == cTime); 
                %disp(whichDim)
                
                if whichDim ~= 1
                    intM.(aField) = permute(intM.(aField),[whichDim 1 2]);
                end

                intM.(aField) = [intM.(aField); intM.(aField)(1,:,:,:)]; %interpolate with January in December
                for i= 1:cTime
                     if i == 1
                        steps = linspace(i, i+1, nTime+1);
                        steps(:,nTime+1) = [];
                        intSteps = steps';
                
                    else 
                        steps = linspace(i, i+1, nTime+1);
                        steps(:,nTime+1) = [];
                        intSteps = cat(1, intSteps, steps');
                
                    end
                 end
                nonIntSteps = 1:(cTime+1);
                intSteps = intSteps';
    
                intM.(aField) = interp1(nonIntSteps,intM.(aField),intSteps, IntMethod);

                if whichDim ~= 1 
                    intM.(aField) = permute(intM.(aField),[2 3 1]);
                end

             end
    
           end
        end

    else
        intM = [intM; intM(1,:,:,:)]; 

        for i= 1:cTime
                     if i == 1
                        steps = linspace(i, i+1, nTime+1);
                        steps(:,nTime+1) = [];
                        intSteps = steps';
                
                    else 
                        steps = linspace(i, i+1, nTime+1);
                        steps(:,nTime+1) = [];
                        intSteps = cat(1, intSteps, steps');
                
                     end
        end
        nonIntSteps = 1:(cTime+1);
        intSteps = intSteps';
    
        intM = interp1(nonIntSteps,intM,intSteps, IntMethod);
    end
