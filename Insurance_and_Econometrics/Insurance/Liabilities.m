function [Liabilities, Duration, Cf, Bel_Lapse, Bel_Death, Bel_Expen, Bel_Commissions] = Liabilities(S0, S, rates, times, lx, qx, comm_if_benefit, expenses, RD, COMM)
% function which computes the value of the liabilities, the duration and
% cash flows
%
% INPUTS:
% S0:               initial value of the stock
% S:                value of the stock simulated
% rates:            risk-free rates
% times:            vector of times
% lx:               vector of probability of lapse for each year
% qx:               vector of probability of death for each year
% comm_if_benefit:  commission of the benefit is paid
% expenses:         expenses each year
% RD:               Regulatory capital
% COMM:             commission to the distribution channel
%
% OUTPUTS:
% Liabilities:      value of the Liabilities
% Duration:         maculay duration
% Cf:               vector of future cash flows
% Bel_Lapse:        liabilities of lapse
% Bel_Death:        liabilities of death
% Bel_Expen:        liabilities of expenses
% Bel_Commissions:  liabilities of commissions
%

% Initializations
T = times(end);
F = S;

% factor of the regulatory capital
factor_RD = 1-RD;

%Survival Probability
P_surv = cumprod([1;(1 - qx(1:end-1)).*(1 - lx(1:end-1)); 0]); %prob di rimanere nel contratto fino a anno i

%BEL components
Lapse_bel = zeros(T,1);
Death_bel = zeros(T,1);
Expen_bel = zeros(T,1);
Commissions_bel = zeros(T,1);

% initialization
C = zeros(T,1);
for i = 1:T
    
    % Bel computation
    Lapse_bel(i) = ((F(i+1) - comm_if_benefit) * lx(i) * (1-qx(i))) * P_surv(i);
    Death_bel(i) = ((max(1.1*S0,F(i+1)) - comm_if_benefit) * qx(i)) * P_surv(i);
    Expen_bel(i) = expenses(i) * P_surv(i);
    Commissions_bel(i) = (F(i+1)/factor_RD*COMM) * P_surv(i);
    
    % computation of the costs each year
    C(i) = Lapse_bel(i) + Death_bel(i) + Expen_bel(i) + Commissions_bel(i);
    
end

% Discounts computation
discounts = exp(-rates.*times);

% Computation of Liabilities
Cf = C.*discounts;
Liabilities = sum(Cf)';

% Duration computation
Num = sum(Cf.*times)';
Duration = Num/Liabilities;

%BEL components
Bel_Lapse = sum(Lapse_bel.*discounts);
Bel_Death = sum(Death_bel.*discounts);
Bel_Expen = sum(Expen_bel.*discounts);
Bel_Commissions = sum(Commissions_bel.*discounts);


end