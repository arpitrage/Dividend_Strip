%% This file loads the data used in the AP model
% The data file itself contains more details on data construction
mdata      = xlsread('../../../Data/APdata.xlsx','VAR','a2:ay294'); 

Tstart     = 109; % start sample in 1974.Q1 (1=1947.Q1)
Tend       = 292; % end sample in 2019.Q4 
T          = Tend-Tstart+1;

date       = mdata(Tstart:Tend,1);   
rpcgdpgr   = mdata(Tstart:Tend,5);  % real per capita gdp growth (in logs)
inflation  = mdata(Tstart:Tend,6);  % growth in GDP price deflator (in logs)
ynom1      = mdata(Tstart:Tend,8)/400;  % nominal yield on a 3-month Treasury bill, expressed per quarter
ynom4      = mdata(Tstart:Tend,9)/400;  % nominal yield on a 1-year Treasury bill, expressed per quarter
ynom8      = mdata(Tstart:Tend,10)/400; % nominal yield on a 2-year Treasury note, expressed per quarter
ynom20     = mdata(Tstart:Tend,11)/400; % nominal yield on a 5-year Treasury note, expressed per quarter
ynom40     = mdata(Tstart:Tend,12)/400; % nominal yield on a 10-year Treasury bond, expressed per quarter
ynom120    = mdata(Tstart:Tend,13)/400; % nominal yield on a 30-year Treasury bond, expressed per quarter
pdm        = mdata(Tstart:Tend,14);  % log price-dividend ratio on CRSP vw stock index; dividend is quarterly and seasonally adjusted
divgrm     = mdata(Tstart:Tend,15);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdreit     = mdata(Tstart:Tend,16);  % log price-dividend ratio on CRSP vw stock index; dividend is quarterly and seasonally adjusted
divgrreit  = mdata(Tstart:Tend,17);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdinfra    = mdata(Tstart:Tend,18);  % log price-dividend ratio on CRSP vw stock index; dividend is quarterly and seasonally adjusted
divgrinfra = mdata(Tstart:Tend,19);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
PDm_strip_2q_data=mdata(Tstart:Tend,20); % PD ratio on next 2 quarters of dividend strips
PDm_strip_4q_data=mdata(Tstart:Tend,21); % PD ratio on next 2 quarters of dividend strips
PDm_strip_6q_data=mdata(Tstart:Tend,22); % PD ratio on next 2 quarters of dividend strips
PDm_strip_8q_data=mdata(Tstart:Tend,23); % PD ratio on next 2 quarters of dividend strips
sharestrip_2q_data=mdata(Tstart:Tend,24); % PD ratio on next 2 quarters of dividend strips
sharestrip_4q_data=mdata(Tstart:Tend,25); % PD ratio on next 2 quarters of dividend strips
sharestrip_6q_data=mdata(Tstart:Tend,26); % PD ratio on next 2 quarters of dividend strips
sharestrip_8q_data=mdata(Tstart:Tend,27); % PD ratio on next 2 quarters of dividend strips
ynom80    = mdata(Tstart:Tend,28)/400; % nominal yield on a 20-year Treasury bond, expressed in percent per year, so divide by 400
pdMEQ1       = mdata(Tstart:Tend,29);  % log price-dividend ratio, annualized 
divgrMEQ1    = mdata(Tstart:Tend,30);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdMEQ5       = mdata(Tstart:Tend,31);  % log price-dividend ratio, annualized 
divgrMEQ5    = mdata(Tstart:Tend,32);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdBMQ1       = mdata(Tstart:Tend,33);  % log price-dividend ratio, annualized 
divgrBMQ1    = mdata(Tstart:Tend,34);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdBMQ5       = mdata(Tstart:Tend,35);  % log price-dividend ratio, annualized 
divgrBMQ5    = mdata(Tstart:Tend,36);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdINVQ1      = mdata(Tstart:Tend,37);  % log price-dividend ratio, annualized 
divgrINVQ1   = mdata(Tstart:Tend,38);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdINVQ5      = mdata(Tstart:Tend,39);  % log price-dividend ratio, annualized 
divgrINVQ5   = mdata(Tstart:Tend,40);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdOPQ1       = mdata(Tstart:Tend,41);  % log price-dividend ratio, annualized 
divgrOPQ1    = mdata(Tstart:Tend,42);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdOPQ5       = mdata(Tstart:Tend,43);  % log price-dividend ratio, annualized 
divgrOPQ5    = mdata(Tstart:Tend,44);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
pdNR         = mdata(Tstart:Tend,45);  % log price-dividend ratio, annualized 
divgrNR      = mdata(Tstart:Tend,46);  % nominal log dividend growth; dividend is quarterly and seasonally adjusted
yreal20      = mdata(Tstart:Tend,47)/400; % real yield on a 5-year TIIS, expressed per quarter
yreal28      = mdata(Tstart:Tend,48)/400; % real yield on a 7-year TIIS, expressed per quarter
yreal40      = mdata(Tstart:Tend,49)/400; % real yield on a 10-year TIIS, expressed per quarter
yreal80      = mdata(Tstart:Tend,50)/400; % real yield on a 20-year TIIS, expressed per quarter
yreal120     = mdata(Tstart:Tend,51)/400; % real yield on a 30-year TIIS, expressed per quarter


% Yields in the model are log yields
ynom1   = log(1+ynom1);
ynom4   = log(1+ynom4);
ynom8   = log(1+ynom8);
ynom20  = log(1+ynom20);
ynom40  = log(1+ynom40);
ynom80  = log(1+ynom80);
ynom120 = log(1+ynom120);

yreal20  = log(1+yreal20);
yreal28  = log(1+yreal28);
yreal40  = log(1+yreal40);
yreal80  = log(1+yreal80);
yreal120 = log(1+yreal120);

yieldspr  = ynom20-ynom1; % spread between 5-yr yield and 1-month
y0nom_1   = mean(ynom1);
y0nom_20  = mean(ynom20);
yspr0     = mean(yieldspr);
pi0       = mean(inflation);
x0        = mean(rpcgdpgr);

% Calculate mean real devidend growth rates
mu_m         = nanmean(divgrm-inflation);
mu_reit      = nanmean(divgrreit-inflation);
mu_infra     = nanmean(divgrinfra-inflation);
mu_small     = nanmean(divgrMEQ1-inflation);
mu_large     = nanmean(divgrMEQ5-inflation);
mu_growth    = nanmean(divgrBMQ1-inflation);
mu_value     = nanmean(divgrBMQ5-inflation);
mu_invQ1     = nanmean(divgrINVQ1-inflation);
mu_invQ5     = nanmean(divgrINVQ5-inflation);
mu_opq1      = nanmean(divgrOPQ1-inflation);
mu_opq5      = nanmean(divgrOPQ5-inflation);
mu_NR        = nanmean(divgrNR-inflation);

% log pd ratio on stock market
A0m         = nanmean(pdm); 
k1m         = exp(A0m)/(exp(A0m)+1);
k0m         = log(exp(A0m)+1)-k1m*A0m;
r0_m        = mu_m + (log(exp(A0m)+1)-A0m); 

% log pd ratio on reits
A0_reit     = nanmean(pdreit);
k1_reit     = exp(A0_reit)/(exp(A0_reit)+1);
k0_reit     = log(exp(A0_reit)+1)-k1_reit*A0_reit;
r0_reit     = mu_reit + (k0_reit + A0_reit*(k1_reit-1));

% log pd ratio on infrastructure
A0_infra    = nanmean(pdinfra);
k1_infra    = exp(A0_infra)/(exp(A0_infra)+1);
k0_infra    = log(exp(A0_infra)+1)-k1_infra*A0_infra;
r0_infra    = mu_infra + (k0_infra + A0_infra*(k1_infra-1));

% log pd ratio on meq1 portfolios
A0_small    = nanmean(pdMEQ1 ); 
k1_small    = exp(A0_small)/(exp(A0_small)+1);
k0_small    = log(exp(A0_small)+1)-k1_small*A0_small;
r0_small    = mu_small + (log(exp(A0_small)+1)-A0_small); 

% log pd ratio on bmq1 portfolio
A0_growth   = nanmean(pdBMQ1); 
k1_growth   = exp(A0_growth)/(exp(A0_growth)+1);
k0_growth   = log(exp(A0_growth)+1)-k1_growth*A0_growth;
r0_growth   = mu_growth + (log(exp(A0_growth)+1)-A0_growth); 

% log pd ratio on NR portfolio
A0_NR       = nanmean(pdNR); 
k1_NR       = exp(A0_NR)/(exp(A0_NR)+1);
k0_NR       = log(exp(A0_NR)+1)-k1_NR*A0_NR;
r0_NR       = mu_NR + (log(exp(A0_NR)+1)-A0_NR); 

% log pd ratio on bmq5 portfolio
A0_value   = nanmean(pdBMQ5); 
k1_value   = exp(A0_value)/(exp(A0_value)+1);
k0_value   = log(exp(A0_value)+1)-k1_value*A0_value;
r0_value   = mu_value + (log(exp(A0_value)+1)-A0_value); 


% select yield maturities for the term structure model
tau = [1,4,8,20,40,80,120]; 
yielddata  = [ynom1,ynom4,ynom8,ynom20,ynom40,ynom80,ynom120];

taureal = [20,28,40,80,120]; 
yielddatareal  = [yreal20,yreal28,yreal40,yreal80,yreal120];

%% VAR elements 
infl         = inflation    - pi0;
x            = rpcgdpgr     - x0;
yield1       = ynom1        - y0nom_1;
nomyspr      = yieldspr     - yspr0;
pdm          = pdm          - A0m;
divgr_m      = divgrm-inflation - mu_m;
pd_reit      = pdreit - A0_reit;
divgr_reit   = divgrreit-inflation - mu_reit;
pd_infra     = pdinfra     - A0_infra;
divgr_infra  = divgrinfra-inflation  - mu_infra;
pd_small     = pdMEQ1     - A0_small;
divgr_small  = divgrMEQ1-inflation  - mu_small;
pd_growth    = pdBMQ1     - A0_growth;
divgr_growth = divgrBMQ1-inflation  - mu_growth;
pd_NR        = pdNR     - A0_NR;
divgr_NR     = divgrNR-inflation  - mu_NR;
pd_value     = pdBMQ5    - A0_value;
divgr_value  = divgrBMQ5-inflation  - mu_value;

%% define the ordering in the VAR
inflpos          = 1;
gdppos           = 2;
y1pos            = 3;
ysprpos          = 4;
pdmpos           = 5;
divgrmpos        = 6;

X2(:,inflpos)         = infl;     
X2(:,gdppos)          = x;
X2(:,y1pos)           = yield1;  
X2(:,ysprpos)         = nomyspr; 
X2(:,pdmpos)          = pdm;
X2(:,divgrmpos)       = divgr_m;  

N             = cols(X2);
I             = eye(N); 
I_pi          = I(:,inflpos);   
I_gdp         = I(:,gdppos); 
I_y1          = I(:,y1pos);     
I_yspr        = I(:,ysprpos); 
I_pdm         = I(:,pdmpos);      
I_divgrm      = I(:,divgrmpos);

z_var     = X2(2:end,:);
z_varlag  = X2(1:end-1,:);
Y         = z_var;
X         = z_varlag;