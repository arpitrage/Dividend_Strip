%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AP Model 
%
% This code is the first step in a series of files to run to produce good
% starting values for the full market price of risk estimation. This file
% uses only moments on bonds to pin down a subset of MPR coefficients. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all; clc

%% Load data
APloaddata

%% First stage OLS estimation of VAR
Psi = zeros(N,N);
Psi_se = zeros(N,N);
for i = 1:N
    regr         = ols(Y(:,i),[ones(T-1,1),X]);
    c(i)         = regr.beta(1);
    c_se(i)      = regr.bstd(1);
    Psi(i,:)    = regr.beta(2:end)';
    Psi_se(i,:) = regr.bstd(2:end)';
    R2(i)        = regr.rsqr;
    R2bar(i)     = regr.rbar;
    eps(:,i)     = Y(:,i) - c(i) - X*Psi(i,:)';
    tstat(:,i)   = regr.tstat';
    clear regr;
end
select = abs(Psi./Psi_se)>2.2;
for iii = 1:5
    Psi = zeros(N,N);
    Psi_se = zeros(N,N);
    for i = 1:N
        regr         = ols(Y(:,i),[ones(T-1,1),X(:,select(i,:))]);
        c(i)         = regr.beta(1);
        c_se(i)      = regr.bstd(1);
        Psi(i,select(i,:))    = regr.beta(2:end)';
        Psi_se(i,select(i,:)) = regr.bstd(2:end)';
        R2(i)        = regr.rsqr;
        R2bar(i)     = regr.rbar;
        eps(:,i)     = Y(:,i) - c(i) - X(:,select(i,:))*Psi(i,select(i,:))';
        clear regr;
    end
    select1 = abs(Psi./Psi_se)>2.2;
    select1(5,1) = 0;
    select1(8,8) = 1;
    select1(10,10) = 1;
    select = select1;
end
Tstatt=abs(Psi./Psi_se);
R_psi = ~isnan(Tstatt);
save Psimatrix Psi R_psi eps Tstatt

Sigma = cov(eps); 
Sig   = chol(Sigma,'lower'); % Sig is a standard deviation matrix, Sigma is a variance matrix
eps2  = eps;
Omega = cov(X2);

%% Second stage estimation of market prices of risk
striphorizon = 2000;

% x = zeros(13,1);
load x_guess_1 x
x_init = x;

global counts
counts = 0;
options = optimset('DiffMinChange',.01,'TolX',0.001,'TolFun',0.001,'MaxIter',1000,'MaxFunEval',1000);
[x,fval,exitflag,output] = fminsearch('solvenul_startval1',x_init,options,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,I_pd_reit,I_divgr_reit,I_pd_infra,I_divgr_infra,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,A0_reit,k1_reit,r0_reit,mu_reit,A0_infra,k1_infra,r0_infra,mu_infra,PDm_strip_2q_data,PDm_strip_4q_data,PDm_strip_6q_data,PDm_strip_8q_data,sharestrip_2q_data,sharestrip_4q_data,sharestrip_6q_data,sharestrip_8q_data,taureal,yielddatareal);

save  x_guess_1 x

L0 = zeros(N,1);
L1 = zeros(N,N);

i=1;
% constant MPRs (7 params, 1-4)
L0(I_pi==1)  = x(i);   i = i+1;
L0(I_gdp==1) = x(i);   i = i+1;
L0(I_y1==1)  = x(i);   i = i+1;
L0(I_yspr==1) = x(i);   i = i+1;

% TV MPR
% bond block (5-12)
L1(I_pi==1,I_pi==1)         = x(i);   i = i+1;
L1(I_gdp==1,I_gdp==1)       = x(i);   i = i+1;
L1(I_y1==1,I_y1==1)         = x(i);   i = i+1;
L1(I_y1==1,I_yspr==1)       = x(i);   i = i+1;
L1(I_yspr==1,I_pi==1)       = x(i);   i = i+1;
L1(I_yspr==1,I_gdp==1)      = x(i);   i = i+1;
L1(I_yspr==1,I_y1==1)       = x(i);   i = i+1;
L1(I_yspr==1,I_yspr==1)     = x(i);   i = i+1;
L1(I_yspr==1,I_pdm==1)     = x(i);   i = i+1;


%% arbitrage-free definition of real risk-free rate
y0_1   = y0nom_1 - pi0 - .5*I_pi'*(Sig*Sig')*I_pi + I_pi'*Sig*L0; 
yts_1  = y0_1 + (I_y1'-I_pi'*Psi+I_pi'*Sig*L1)*X2'; % time series of real risk-free rate

%% No arbitrage restrictions on bond yields
A       = zeros(striphorizon,1); 
B       = zeros(N,striphorizon);
Api     = zeros(striphorizon,1); 
Bpi     = zeros(N,striphorizon);
Am      = zeros(striphorizon,1); 
Bm      = zeros(N,striphorizon);
A_reit  = zeros(striphorizon,1); 
B_reit  = zeros(N,striphorizon);
A_infra = zeros(striphorizon,1); 
B_infra = zeros(N,striphorizon);

A(1)    = - y0_1 ;  
B(:,1)  = -(I_y1'-I_pi'*Psi + I_pi'*Sig*L1)';

Api(1)  = -y0nom_1; 
Bpi(:,1)= -I_y1';

Am(1)   =  mu_m - y0_1     + .5*(I_divgrm)'*Sig*Sig'*(I_divgrm) - (I_divgrm)'*Sig*(L0-Sig'*I_pi); 
Bm(:,1) = ((I_divgrm+I_pi)'*Psi - I_y1'  - (I_divgrm+I_pi)'*Sig*L1)';
PDm_model=exp(Am(1)+Bm(:,1)'*X2')';

A_reit(1)   =  mu_reit - y0_1     + .5*(I_divgr_reit)'*Sig*Sig'*(I_divgr_reit) - (I_divgr_reit)'*Sig*(L0-Sig'*I_pi); 
B_reit(:,1) = ((I_divgr_reit+I_pi)'*Psi - I_y1'  - (I_divgr_reit+I_pi)'*Sig*L1)';
PD_reit_model=exp(A_reit(1)+B_reit(:,1)'*X2')';

A_infra(1)   =  mu_infra - y0_1     + .5*(I_divgr_infra)'*Sig*Sig'*(I_divgr_infra) - (I_divgr_infra)'*Sig*(L0-Sig'*I_pi); 
B_infra(:,1) = ((I_divgr_infra+I_pi)'*Psi  - I_y1'  - (I_divgr_infra+I_pi)'*Sig*L1)';
PD_infra_model=exp(A_infra(1)+B_infra(:,1)'*X2')';

Psi_j = zeros(N);

for j = 1:striphorizon-1
    Api(j+1)  = - y0nom_1 + Api(j) + .5*Bpi(:,j)'*(Sig*Sig')*Bpi(:,j)- Bpi(:,j)'*Sig*L0;
    Bpi(:,j+1)= (Bpi(:,j)'*Psi- I_y1'- Bpi(:,j)'*Sig*L1)';    

    A(j+1)    = - y0_1    + A(j)   + .5*B(:,j)'*(Sig*Sig')*B(:,j)    - B(:,j)'*Sig*(L0-Sig'*I_pi);
    B(:,j+1)  = ((I_pi + B(:,j))'*Psi - I_y1' - (I_pi+B(:,j))'*Sig*L1)';  
    
    Am(j+1)   = Am(j) + mu_m - y0_1 + .5*(I_divgrm +Bm(:,j))'*Sig*Sig'*(I_divgrm+Bm(:,j)) -(I_divgrm+Bm(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bm(:,j+1) = ((I_divgrm+I_pi+Bm(:,j))'*Psi - I_y1'  - (I_divgrm+I_pi+Bm(:,j))'*Sig*L1)';
    PDm_model = PDm_model+exp(Am(j+1)+Bm(:,j+1)'*X2')';
    
    A_reit(j+1)   = A_reit(j) + mu_reit - y0_1 + .5*(I_divgr_reit+B_reit(:,j))'*Sig*Sig'*(I_divgr_reit +B_reit(:,j)) -(I_divgr_reit+B_reit(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_reit(:,j+1) = ((I_divgr_reit+I_pi+B_reit(:,j))'*Psi  - I_y1'  - (I_divgr_reit+I_pi+B_reit(:,j))'*Sig*L1)';
    PD_reit_model = PD_reit_model+exp(A_reit(j+1)+B_reit(:,j+1)'*X2')';

    A_infra(j+1)   = A_infra(j) + mu_infra - y0_1 + .5*(I_divgr_infra +B_infra(:,j))'*Sig*Sig'*(I_divgr_infra+B_infra(:,j)) -(I_divgr_infra+B_infra(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_infra(:,j+1) = ((I_divgr_infra+I_pi+B_infra(:,j))'*Psi  - I_y1'  - (I_divgr_infra+I_pi+B_infra(:,j))'*Sig*L1)';
    PD_infra_model = PD_infra_model+exp(A_infra(j+1)+B_infra(:,j+1)'*X2')';

    nombondriskprem(j) = 400*Bpi(:,j)'*Sig*L0;
    equitydivstripriskprem(j) = 400*(I_divgrm+Bm(:,j))'*Sig*L0;
    reitdivstripriskprem(j) = 400*(I_divgr_reit+B_reit(:,j))'*Sig*L0;
    infradivstripriskprem(j) = 400*(I_divgr_infra+B_infra(:,j))'*Sig*L0;

    if j<61
        % These are expected returns [in levels, not logs] on strips of horizon ttt
        Psi_j = Psi_j+Psi^j;
        stripexpret_bond(j,:)   = exp(-Api(j)-Bpi(:,j)'*X2')-1;
        stripexpret_stock(j,:)  = exp(j*mu_m     - Am(j)     +I_divgrm'*Psi_j*X2'      - Bm(:,j)'*X2'      + 0.5*j*I_divgrm'*Sig*Sig'*I_divgrm)-1;
        stripexpret_reit(j,:)   = exp(j*mu_reit  - A_reit(j) +I_divgr_reit'*Psi_j*X2'  - B_reit(:,j)'*X2'  + 0.5*j*I_divgr_reit'*Sig*Sig'*I_divgr_reit)-1;
        stripexpret_infra(j,:)  = exp(j*mu_infra - A_infra(j)+I_divgr_infra'*Psi_j*X2' - B_infra(:,j)'*X2' + 0.5*j*I_divgr_infra'*Sig*Sig'*I_divgr_infra)-1;
    end

end

stripexpret_bond = stripexpret_bond'; % turn into 175 (time series, quarterly 1974.Q1-2017.Q4) by 60 horizons (quarters)
stripexpret_stock = stripexpret_stock';
stripexpret_reit = stripexpret_reit';
stripexpret_infra = stripexpret_infra';


PDm_strip_2q = exp(Am(1)+Bm(:,1)'*X2')'+exp(Am(2)+Bm(:,2)'*X2')';
PDm_strip_4q = PDm_strip_2q+ exp(Am(3)+Bm(:,3)'*X2')'+exp(Am(4)+Bm(:,4)'*X2')';
PDm_strip_6q = PDm_strip_4q+ exp(Am(5)+Bm(:,5)'*X2')'+exp(Am(6)+Bm(:,6)'*X2')';
PDm_strip_8q = PDm_strip_6q+ exp(Am(7)+Bm(:,7)'*X2')'+exp(Am(8)+Bm(:,8)'*X2')';
sharestrip_2q = PDm_strip_2q./PDm_model;
sharestrip_4q = PDm_strip_4q./PDm_model;
sharestrip_6q = PDm_strip_6q./PDm_model;
sharestrip_8q = PDm_strip_8q./PDm_model;

%% Evaluate moments
aa = (I_divgrm+k1m*I_pdm+I_pi)'*Sig;
cc = ((I_divgrm+k1m*I_pdm+I_pi)'*Psi -I_pdm' -I_y1');
riskprem_m              = r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)';
equityriskpremTV_model  = 400*(aa*L0+aa*L1*X2'); 
equityriskpremTV_data   = 400*(riskprem_m + cc*X2'); 
FF_vw = 400*(riskprem_m - aa*L0); % pricing error is in percent per year
disp('Average equity premium: data, model, pricing error, all in percent per year')
[mean(equityriskpremTV_data), mean(equityriskpremTV_model), FF_vw]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(equityriskpremTV_data),  std(equityriskpremTV_model), (std(equityriskpremTV_data)-std(equityriskpremTV_model)) ]

aa_reit = (I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Sig;
cc_reit = ((I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Psi -I_pd_reit' -I_y1');
riskprem_reit           = r0_reit + pi0 - y0nom_1 +0.5*(aa_reit)*(aa_reit)';
reit_riskpremTV_model   = 400*(aa_reit*L0 + aa_reit*L1*X2'); 
reit_riskpremTV_data    = 400*(riskprem_reit + cc_reit*X2'); 
FF_reit = 400*(riskprem_reit - aa_reit*L0); % pricing error is in percent per year
disp('Average REIT premium: data, model, pricing error, all in percent per year')
[mean(reit_riskpremTV_data), mean(reit_riskpremTV_model), FF_reit]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(reit_riskpremTV_data),  std(reit_riskpremTV_model), (std(reit_riskpremTV_data)-std(reit_riskpremTV_model)) ]

aa_infra = (I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Sig;
cc_infra = ((I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Psi -I_pd_infra' -I_y1');
riskprem_infra            = r0_infra + pi0 - y0nom_1 + 0.5*(aa_infra)*(aa_infra)';
infra_riskpremTV_model    = 400*(aa_infra*L0+aa_infra*L1*X2'); 
infra_riskpremTV_data     = 400*(riskprem_infra + cc_infra*X2'); 
FF_infra = 400*(riskprem_infra - aa_infra*L0); % pricing error is in percent per year
disp('Average infrastructure premium: data, model, pricing error, all in percent per year')
[mean(infra_riskpremTV_data), mean(infra_riskpremTV_model), FF_infra]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(infra_riskpremTV_data),  std(infra_riskpremTV_model), (std(infra_riskpremTV_data)-std(infra_riskpremTV_model)) ]


% Matching average intermediate yields
yielderror(1,:) = (-Api(1)'./1 -Bpi(:,1)'./1*X2')'         -ynom1; 
yielderror(2,:) = (-Api(4)'./4 -Bpi(:,4)'./4*X2')'         -ynom4; 
yielderror(3,:) = (-Api(8)'./8 -Bpi(:,8)'./8*X2')'         -ynom8; 
yielderror(4,:) = (-Api(20)'./20 -Bpi(:,20)'./20*X2')'     -ynom20;
yielderror(5,:) = (-Api(40)'./40 -Bpi(:,40)'./40*X2')'     -ynom40;
yielderror(6,:) = (-Api(80)'./80 -Bpi(:,80)'./80*X2')'     -ynom80;
yielderror(7,:) = (-Api(120)'./120 -Bpi(:,120)'./120*X2')' -ynom120;


disp(['Average nominal yield curve for maturities used in estimation (in quarters): ',num2str(tau)])
disp('data, model all in percent per year')
avgyields_model = nanmean(kron(ones(length(X2),1),-Api(tau)'./tau) -((Bpi(:,tau)'./kron(tau',ones(1,N)))*X2')');
avgyields_data  = avgyields_model - nanmean(yielderror');
[400*avgyields_data;400*avgyields_model]
disp('bond pricing error: mean, stdev, RMSE')
[400*nanmean(yielderror'); sqrt(4)*100*nanstd(yielderror');400*sqrt(nanmean(yielderror'.^2))]
disp(['Average real yield curve for maturities used in estimation (in quarters): ',num2str(tau)])
disp(' model (not used in estimation), all in percent per year')
[-400*A(tau)'./tau]

% Dynamics of 5-yr yield
ynom20_model = (-Api(20)/20 - Bpi(:,20)'/20*X2')';
yreal20_model= (-A(20)/20 - B(:,20)'/20*X2')';
disp('Standard deviation of nominal 5-yr yield in data, model, difference, all in percent per year')
[sqrt(4)*100*std(ynom20), sqrt(4)*100*std(ynom20_model), sqrt(4)*100*(std(ynom20)-std(ynom20_model))]
disp('Standard deviation of real 5-yr yield in model in percent per year')
sqrt(4)*100*std(yreal20_model)
% Expected inflation over the next 5 years, in quarterly terms
expinfl20 = (pi0 + (I_pi'*(I-Psi^20)*inv(I-Psi)*X2')/20)';
disp(' Expected inflation over the next 5 years, in percent per year')
400*mean(expinfl20)

% Nominal bond risk premia on 5-year bond
bondriskpremTV_model    = (Api(1)-Api(20)/20) + (-Bpi(:,20)'/20 + Bpi(:,1)'/20 *(I-Psi^20)*inv(I-Psi))*X2';
bondriskpremTV_data     = (y0nom_20-y0nom_1) + (I_y1'+I_yspr'-I_y1'/20*(I-Psi^20)*inv(I-Psi))*X2';
disp('Average bond risk premium on 5-yr bond (in percent per year):')
disp('And the standard deviations of the 5-yr risk premium')
disp('data, model, pricing error, all in percent per year')
[400*mean(bondriskpremTV_data),  400*mean(bondriskpremTV_model) 400*mean(bondriskpremTV_data)-400*mean(bondriskpremTV_model)]
[sqrt(4)*100*std(bondriskpremTV_data),  sqrt(4)*100*std(bondriskpremTV_model), sqrt(4)*100*(std(bondriskpremTV_data)-std(bondriskpremTV_model)) ]

FF=[];
FF_newa    = 40000*abs(-Api(20)/20 - y0nom_20);
FF        = [FF FF_newa];
FF_newb    = 4000*abs(-Bpi(1,20)/20 - 0);
FF        = [FF FF_newb];
FF_newc    = 4000*abs(-Bpi(2,20)/20 - 0);
FF        = [FF FF_newc];
FF_newd    = 4000*abs(-Bpi(3,20)/20 - 1);
FF        = [FF FF_newd];
FF_newe    = 4000*abs(-Bpi(4,20)/20 - 1);
FF        = [FF FF_newe];
FF_newf    = 4000*abs(-Bpi(5,20)/20 - 0);
FF        = [FF FF_newf];
FF_newg    = 4000*abs(-Bpi(6,20)/20 - 0);
FF        = [FF FF_newg];
FF_newh    = 4000*abs(-Bpi(7,20)/20 - 0);
FF        = [FF FF_newh];
FF_newi    = 4000*abs(-Bpi(8,20)/20 - 0);
FF        = [FF FF_newi];
FF_newj    = 4000*abs(-Bpi(9,20)/20 - 0);
FF        = [FF FF_newj];
FF_newk    = 4000*abs(-Bpi(10,20)/20 - 0);
FF        = [FF FF_newk];
FF_newl    = 4000*abs(-Bpi(11,20)/20 - 0);
FF        = [FF FF_newl];
FF_newm    = 4000*abs(-Bpi(12,20)/20 - 0);
FF        = [FF FF_newm];
FF_newn    = 4000*abs(-Bpi(13,20)/20 - 0);
FF        = [FF FF_newn];
FF_newo    = 4000*abs(-Bpi(14,20)/20 - 0);
FF        = [FF FF_newo];
FF_newp    = 4000*abs(-Bpi(15,20)/20 - 0);
FF        = [FF FF_newp];
FF_newq    = 4000*abs(-Bpi(16,20)/20 - 0);
FF        = [FF FF_newq];
FF_newr    = 4000*abs(-Bpi(17,20)/20 - 0);
FF        = [FF FF_newr];
FF_news    = 4000*abs(-Bpi(18,20)/20 - 0);
FF        = [FF FF_news];
disp(['20-qtr bond yield violation:  ',num2str(FF_newa+FF_newb+FF_newc+FF_newd+FF_newe+FF_newf+FF_newg+FF_newh+FF_newi+FF_newj+FF_newk+FF_newl+FF_newm+FF_newn+FF_newo+FF_newp+FF_newq+FF_newr+FF_news)])
FF


% Term structure of bond risk premia
for ttt = 1:400 
    nombrp(ttt,:)         = ((Api(1)-Api(ttt)/ttt) + (-Bpi(:,ttt)'/ttt + Bpi(:,1)'/ttt *(I-Psi^ttt)*inv(I-Psi))*X2');
    realbrp(ttt,:)        = ((A(1)-A(ttt)/ttt) + (-B(:,ttt)'/ttt + B(:,1)'/ttt *(I-Psi^ttt)*inv(I-Psi))*X2');
    inflbrp(ttt,:)        = nombrp(ttt,:) - realbrp(ttt,:) ;
    ts_brp_mean(ttt)      = mean(nombrp(ttt,:));
    ts_brp_std(ttt)       = std(nombrp(ttt,:));
    ts_brp_real_mean(ttt) = mean(realbrp(ttt,:));
    ts_brp_real_std(ttt)  = std(realbrp(ttt,:));
    ts_brp_infl_mean(ttt) = mean(inflbrp(ttt,:));
    ts_brp_infl_std(ttt)  = std(inflbrp(ttt,:));
end

% Inflation risk premium over 5-years (Ang-Bekaert or KvHVN definition), in quarterly terms
inflrp20_model = ynom20_model - yreal20_model - expinfl20;
disp(' Inflation risk premium over the next 5 years, in percent per year')
400*mean(inflrp20_model)

%% Conditional MPR
nomshortrate = y0nom_1 + I_y1'*X2'; % 1 by T
L = kron(L0,ones(1,T)) + L1*X2';    % N by T (10 by 324)
eps_orig = Sig\eps2';               % eps2 are VAR resids, they have covariance Sigma, eps_orig are theoretical shocks with covariance matrix Identity 
eps_orig = eps_orig';
eps_orig = [zeros(1,N);eps_orig];   % N by T; shocks in period 1 set to zero
mnom(1)  = -nomshortrate(1) - .5*L(:,1)'*L(:,1)-L(:,1)'*eps_orig(1,:)';
mreal(1) = mnom(1) +  pi0 + I_pi'*X2(1,:)';
mnom_constantmpr(1) = -nomshortrate(1) - .5*L0'*L0 -L0'*eps_orig(1,:)';

divgrowth_stock = mu_m     + X2(:,divgrmpos);
divgrowth_reit  = mu_reit  + X2(:,divgr_reitpos);
divgrowth_infra = mu_infra + X2(:,divgr_infrapos);

expdivgr_stock = mu_m     + I_divgrm'*Psi*X2';
expdivgr_reit  = mu_reit  + I_divgr_reit'*Psi*X2';
expdivgr_infra = mu_infra + I_divgr_infra'*Psi*X2';

logdiv_stock(1) = 0+divgrowth_stock(1); % div normalized to 1 in Dec 1989, first obs is Jan 1990
logdiv_reit(1)  = 0+divgrowth_reit(1); % div normalized to 1 in Dec 1989, first obs is Jan 1990
logdiv_infra(1) = 0+divgrowth_infra(1); % div normalized to 1 in Dec 1989, first obs is Jan 1990

for t =2 :T   
   mnom(t)             = -nomshortrate(t-1) - .5*L(:,t-1)'*L(:,t-1) -L(:,t-1)'*eps_orig(t,:)';
   mreal(t)            =  mnom(t) + pi0 + I_pi'*X2(t,:)';   
   mnom_constantmpr(t) = -nomshortrate(t-1) - .5*L0'*L0                 - L0'*eps_orig(t,:)';
   mnom_bond(t)        = -nomshortrate(t-1) - .5*L(1:4,t-1)'*L(1:4,t-1) - L(1:4,t-1)'*eps_orig(t,1:4)';
   mnom_stockbond(t)   = -nomshortrate(t-1) - .5*L(1:6,t-1)'*L(1:6,t-1) - L(1:6,t-1)'*eps_orig(t,1:6)';
   logdiv_stock(t)     = logdiv_stock(t-1) + divgrowth_stock(t);
   logdiv_reit(t)      = logdiv_reit(t-1)  + divgrowth_reit(t);
   logdiv_infra(t)     = logdiv_infra(t-1) + divgrowth_infra(t);
end

mnom_ann(1:T-3) = mnom(1:T-3)+mnom(2:T-2)+mnom(3:T-1)+mnom(4:T); 

disp('Max implied Sharpe ratio')
viol_maxSR = 100*exp(max(std(mnom)-.80,0))^4 -1;
maxannualSR = std(mnom_ann);
[maxannualSR]



%% figures
ttime=[1974:0.25:2019+0.75];
figure;
subplot(2,3,1)
plot(ttime,[400*(-Api(1)/1 - Bpi(:,1)'/1*X2')'  400*ynom1])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nominal yield on 1-qtr bond')
subplot(2,3,2)
plot(ttime,[400*(-Api(4)/4 - Bpi(:,4)'/4*X2')'  400*ynom4])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nominal yield on 1-yr bond')
subplot(2,3,3)
plot(ttime,[400*(-Api(20)/20 - Bpi(:,20)'/20*X2')' 400*ynom20])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nominal yield on 5-year bond')
subplot(2,3,4)
plot(ttime,[400*(-Api(40)'./40 -Bpi(:,40)'./40*X2')',  400*ynom40])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
title('Nominal yield on 10-year bond') 
subplot(2,3,5)
plot(ttime,[400*(-Api(80)'./80 -Bpi(:,80)'./80*X2')',  400*ynom80])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
title('Nominal yield on 20-year bond') 
subplot(2,3,6)
plot(ttime,[400*(-Api(120)'./120 -Bpi(:,120)'./120*X2')',  400*ynom120])
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
title('Nominal yield on 30-year bond') 

maxmat = striphorizon;
figure;
subplot(2,2,1)
plot([1:1:maxmat],-400*Api(1:maxmat)./[1:1:maxmat]')
xlim([0,maxmat])
ylim([0,8])
title('Average Nominal Yield Curve model')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(2,2,2)
plot([1:1:maxmat],-400*A(1:maxmat)./[1:1:maxmat]')
xlim([0,maxmat])
ylim([0,8])
title('Average Real Yield Curve model')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(2,2,3)
plot(ttime,[400*bondriskpremTV_model' 400*bondriskpremTV_data'])
legend('model','data','Location','NorthEast')
xlim([1973,2018]);ylim([-2,6]);
ylabel('% per year')
title('Risk Premium on 5-yr Nominal Bond')
subplot(2,2,4)
plot(ttime,[400*ynom20_model 400*yreal20_model 400*expinfl20 400*inflbrp(20,:)']) 
legend('ynom','yreal','exp infl','IRP','Location','NorthEast')
xlim([1973,2018]);ylim([-3,15]);
title('Decomposing Nominal Yield on 5-Year Bond')


figure;
subplot(2,3,1)
plot(ttime,[400*(-A(20)/20 - B(:,20)'/20*X2')'  400*yreal20])
xlim([2000,2020]);ylim([-3,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield on 5-yr TIIS')
subplot(2,3,2)
plot(ttime,[400*(-A(28)/28 - B(:,28)'/28*X2')'  400*yreal28])
xlim([2000,2020]);ylim([-3,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield on 7-yr TIIS')
subplot(2,3,3)
plot(ttime,[400*(-A(40)/40 - B(:,40)'/40*X2')'  400*yreal40])
xlim([2000,2020]);ylim([-3,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield on 10-yr TIIS')
subplot(2,3,4)
plot(ttime,[400*(-A(80)'./80 -B(:,80)'./80*X2')',  400*yreal80])
xlim([2000,2020]);ylim([-3,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield on 20-yr TIIS')
subplot(2,3,5)
plot(ttime,[400*(-A(120)'./120 -B(:,120)'./120*X2')',  400*yreal120])
xlim([2000,2020]);ylim([-3,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield on 30-yr TIIS')