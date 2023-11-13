%GROUP 22 SII project

close all;
clearvars;
clc
rng(1)


%%  Data

% Asset data
C0 = 100000;            % Insured capital 
F0 = 100000;            % The value of the fund at t_0
S0 = F0;                % Equity price at t_0 (all equity)
sigma = 0.25;           % Volatility 
delta_BOF = 0;          % Dividend yield
T = 50;                 % Maturity (in years)

% extra data
lx = 0.15;              % Fixed lapse rate
inflation = 0.02;       % Inflation rate
expenses_t0 = 50;       % Expenses per year (growing with inflation rate)
RD = 0.02;              % Regular deduction
COMM = 0.014;           % Commission to the distribution channel
comm_if_benefit = 20;   % Commission if benefit is paid

% Maturitues
times = (1:1:T)';


%% Rates from EIOPA IT with no VA 31.03.22

% Spot rates
rates = xlsread('EIOPA_RFR_20220331_Term_Structures', 'RFR_spot_no_VA', 'S11:S60'); 

% Rates shock UP
r_up = xlsread('EIOPA_RFR_20220331_Term_Structures', 'Spot_NO_VA_shock_UP', 'S11:S60');

% Rates shock DOWN
r_down = xlsread('EIOPA_RFR_20220331_Term_Structures', 'Spot_NO_VA_shock_DOWN', 'S11:S60');


%% Computing The Forward Rates

% Discount
B = exp(-rates.*times);
B_up = exp(-r_up.*times);
B_down = exp(-r_down.*times);

% computing forward discounts
fwd_B = B(2:end)./B(1:end-1);
fwd_up_B = B_up(2:end)./B_up(1:end-1);
fwd_down_B = B_down(2:end)./B_down(1:end-1);

% computing forward rates
fwd_rates = [rates(1); -log(fwd_B)];
fwd_rates_up = [r_up(1); -log(fwd_up_B)];
fwd_rates_down = [r_down(1); -log(fwd_down_B)];


%% Computing The Probabilities

% Probability of survival (per thousand) ISTAT 2021
life_table = readmatrix('Life_table_male_2021');
px = life_table(64:114,2);

% computing the probability of death
qx = 1-px(2:end)./px(1:end-1); 

% mortality shock
qx_mor = min((1 + 15/100)*qx(1:end),1);             

% mortality cat
qx_cat = [(qx(1) + 1.5/1000); qx(2:end)]; 

% Expenses
expenses = [expenses_t0; expenses_t0 * (1+inflation).^times(1:end-1)];

% Expenses shock
expenses_t0_bumped = expenses_t0*1.1;
expenses_bumped = [expenses_t0_bumped; expenses_t0_bumped * (1+(inflation+0.01)).^times(1:end-1)];

% equity shock
e_shock=0.61*S0;                           

% lapse up
lx_up = min(1.5*lx,1) .* ones(T,1);

% lapse down
lx_down = max(0.5*lx, lx-0.2) .* ones(T,1);  

% lapse mass
lx_mass = [lx + 0.4; lx .* ones(T-1,1)]; 

% lapse spot
lx = lx .* ones(T,1);


%% BASE SCENARIO 

% simulation
S = EquitySimulation(S0,fwd_rates,sigma,T,RD);

% Liabilities, Durantion and cash-flows computations
[Liabilities_1, Duration, Cf, Bel_Lapse, Bel_Death, Bel_Expen, Bel_Commissions] = Liabilities(S0, S, rates, times, lx, qx, comm_if_benefit, expenses,RD, COMM);

% Sum of all the BEL
sum_bel = Bel_Lapse + Bel_Death + Bel_Expen + Bel_Commissions;

% Basic Own Fund
Basic_Fund = S0 - Liabilities_1;

% print the output
disp('BEL:')
disp(sum_bel)
disp('Liabilities:')
disp(Liabilities_1)
disp('Duration:')
disp(Duration)


%% Interest Rate Up Risk

S_IR_up = EquitySimulation(S0, fwd_rates_up, sigma, T,RD);

[Liabilities_IR_up, Duration_IR_up, Cf_IR_up] = Liabilities(S0, S_IR_up, r_up, times, lx, qx, comm_if_benefit, expenses, RD, COMM);

Basic_Fund_IR_Up = S0 - Liabilities_IR_up;
delta_BOF_IR_Up = max(Basic_Fund - Basic_Fund_IR_Up,0);


%% Interest Rate Down Risk

S_IR_down = EquitySimulation(S0, fwd_rates_down, sigma, T, RD);

[Liabilities_IR_down, Duration_IR_down, Cf_IR_down] = Liabilities(S0, S_IR_down, r_down, times, lx, qx, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_IR_Down = S0 - Liabilities_IR_down;
delta_BOF_IR_Down = max(Basic_Fund - Basic_Fund_IR_Down,0);


%% Equity Risk

S_Eq_R = EquitySimulation(e_shock,fwd_rates,sigma,T,RD);

[Liabilities_Eq_R, Duration_Eq_R, Cf_Eq_R] = Liabilities(S0, S_Eq_R, rates, times, lx, qx, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_Eq_R = e_shock - Liabilities_Eq_R;
delta_BOF_Eq_R = max(Basic_Fund - Basic_Fund_Eq_R,0);
 

%% Mortality Risk

S_Mor_R = S;

[Liabilities_Mor_R, Duration_Mor_R, Cf_Mor_R] = Liabilities(S0, S_Mor_R, rates, times, lx, qx_mor, comm_if_benefit, expenses, RD, COMM);

Basic_Fund_Mor_R = S0 - Liabilities_Mor_R;
delta_BOF_Mor_R = max(Basic_Fund - Basic_Fund_Mor_R,0);
 

%% Lapse Down Risk

S_L_down = S_Mor_R;

[Liabilities_L_down, Duration_L_down, Cf_L_down] = Liabilities(S0, S_L_down, rates, times, lx_down, qx, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_L_down = S0 - Liabilities_L_down;
delta_BOF_L_down = max(Basic_Fund - Basic_Fund_L_down,0);
 

%% Lapse Up Risk

S_L_up = S_Mor_R;

[Liabilities_L_up, Duration_L_up, Cf_L_up] = Liabilities(S0, S_L_up, rates, times, lx_up, qx, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_L_up = S0 - Liabilities_L_up;
delta_BOF_L_up = max(Basic_Fund - Basic_Fund_L_up,0);


%% Lapse Mass Risk

S_L_mass = S_Mor_R;

[Liabilities_L_mass, Duration_L_mass, Cf_L_mass] = Liabilities(S0, S_L_mass, rates, times, lx_mass, qx, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_L_mass = S0 - Liabilities_L_mass;
delta_BOF_L_mass = max(Basic_Fund - Basic_Fund_L_mass,0);


%% Catastrophe Risk

S_Cat = S_Mor_R;

[Liabilities_Cat, Duration_Cat, Cf_Cat] = Liabilities(S0, S_Cat, rates, times, lx, qx_cat, comm_if_benefit, expenses,RD, COMM);

Basic_Fund_Cat = S0 - Liabilities_Cat;
delta_BOF_Cat = max(Basic_Fund - Basic_Fund_Cat,0);


%% Expenses Risk

S_Exp = S_Mor_R;

[Liabilities_Exp, Duration_Exp, Cf_Exp] = Liabilities(S0, S_Exp, rates, times, lx, qx, comm_if_benefit, expenses_bumped, RD, COMM);

Basic_Fund_Exp = S0 - Liabilities_Exp;
delta_BOF_Exp = max(Basic_Fund - Basic_Fund_Exp,0);


%% Computation of BSCR

% Computation of SCR: Market Risk

SCR_IR_up = max(delta_BOF_IR_Up, 0);
SCR_IR_down= max(delta_BOF_IR_Down, 0);
SCR_IR = max(SCR_IR_up, SCR_IR_down);

SCR_Eq_R = max(delta_BOF_Eq_R, 0);

MRKT = [SCR_IR SCR_Eq_R]';


if SCR_IR == SCR_IR_up
    CORR = [1 0 ; 0 1 ];
else
    CORR = [1 0.5 ; 0.5 1 ];
end

% 
SCR_MRKT = sqrt(MRKT'*CORR*MRKT);

% Computation of SCR: Life Risk
SCR_L_down = max(delta_BOF_L_down, 0);
SCR_L_mass = max(delta_BOF_L_mass, 0);
SCR_L_up = max(delta_BOF_L_up, 0);
SCR_Cat = max(delta_BOF_Cat, 0);
SCR_Mor_R = max(delta_BOF_Mor_R, 0);
SCR_Exp = max(delta_BOF_Exp, 0);

SCR_L_A = max([SCR_L_down SCR_L_mass SCR_L_up]); %max change lapse

LIFE = [SCR_Mor_R SCR_L_A SCR_Exp SCR_Cat]';
COR_L = [1 0 0.25 0.25; 0 1 0.5 0.25; 0.25 0.5 1 0.25 ; 0.25 0.25 0.25 1];
SCR_LIFE = sqrt(LIFE'*COR_L*LIFE);

% Computation of BSCR
SCR = [SCR_MRKT SCR_LIFE];
COR = [1 0.25; 0.25 1];
BSCR = sqrt(SCR*COR*SCR')


 
