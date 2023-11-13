function S = EquitySimulation(S0,forward_rates,sigma,T,RD)
% function which simulate the equity using the GBM model 
%
% INPUTS:
% S0:               Equity at time t=0
% sigma:            Volatility
% forward_rates:    Forward rates from the EOPIA 
% T:                Maturity
%
% OUTPUT: 
% SimEquity:        vector of the simulated equity per year


%% Generate the B.M

N = 1e6;
dt = 1;
g = randn(N,T);  


%% Inizialitation

S = zeros(N,T+1);
S(:,1) = S0;
factor_RD = 1-RD;


%% Compute Equity using GBM via a monte carlo simulation

for t=2:T
    S(:,t) = S(:,t-1).*exp( (forward_rates(t-1)-0.5*sigma^2)*dt+sigma*sqrt(dt)*g(:,t-1) );
    S(:,t) = factor_RD*S(:,t);
end

S = mean(S,1)';


end
