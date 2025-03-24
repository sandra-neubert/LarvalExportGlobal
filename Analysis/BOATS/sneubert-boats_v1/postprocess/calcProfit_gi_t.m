function Profit = calcProfit_gi_t(harvest_gi_t, effort_gi_t, price, cost_effort)

%calculate profit using gi_t values based on script
%postprocess/analyse_output_Ocean.m

Rev_gi_t    = harvest_gi_t * price; % / mmolC_2_wetB
Cost_gi_t   = effort_gi_t * cost_effort;
Prof_gi_t   = Rev_gi_t - Cost_gi_t;
Profit = Prof_gi_t*3600*24*360*1e-9;  % [B$ yr-1]