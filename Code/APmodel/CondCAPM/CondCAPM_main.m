%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Model that estimates the prices of risk 
% 
%
% KN model has only stocks and bonds
%
% SVN:  April 2020
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all; clc
printfigs=1; % flag if you want to save output

%% Load data
CondCAPM_loaddata

%% First stage OLS estimation of VAR
load Psimatrix_CondCAPM Psi R_psi eps Tstatt

Sigma = cov(eps); 
Sig   = chol(Sigma,'lower'); % Sig is a standard deviation matrix, Sigma is a variance matrix
eps2  = eps;
Omega = cov(X2);

%% Second stage estimation of market prices of risk
striphorizon = 3600;

global counts
counts = 0;

% %% If this is the first time running this file, unomment this block
% L0 = zeros(N,1);
% L1 = zeros(N,N);
% % mkt dividend growth row
% aa = (I_divgrm+k1m*I_pdm+I_pi)'*Sig;
% cc = ((I_divgrm+k1m*I_pdm+I_pi)'*Psi -I_pdm' -I_y1');
% L0(find(I_divgrm==1)) = (((r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)')-aa(1:5)*L0(1:5))/aa(6));
% x_init(1) = L0(find(I_divgrm==1));
% L1(find(I_divgrm==1),:)=((cc-aa(1:5)*L1(1:5,:))/aa(6));
% num_m=length(find(L1(find(I_divgrm==1),:)));
% x_init(2:5) = L1(find(I_divgrm==1),find(L1(find(I_divgrm==1),:)));

% If this is the second or more time running, uncomment the next two lines
load x_guess_CondCAPM x
x_init = x;


%% Estimation of Market Price of Risk parameters
options = optimset('DiffMinChange',.01,'TolX',0.01,'TolFun',0.01,'MaxIter',2000,'MaxFunEval',2000);
[x,fval,exitflag,output] = fminsearch('solvenul_CondCAPM',x_init,options,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,PDm_strip_8q_data,sharestrip_8q_data,taureal,yielddatareal);
save x_guess_CondCAPM x


%% Processing estimation results
L0 = zeros(N,1);
L1 = zeros(N,N);

i=1;
L0(I_divgrm==1)                     = x(i);i=i+1;
L1(I_divgrm==1,I_pi==1)             = x(i); i = i+1;
L1(I_divgrm==1,I_y1==1)             = x(i);  i = i+1;
L1(I_divgrm==1,I_pdm==1)            = x(i);  i = i+1;
L1(I_divgrm==1,I_divgrm==1)         = x(i);  i = i+1;


%% arbitrage-free definition of real risk-free rate
y0_1   = y0nom_1 - pi0 - .5*I_pi'*(Sig*Sig')*I_pi + I_pi'*Sig*L0; 
yts_1  = y0_1 + (I_y1'-I_pi'*Psi+I_pi'*Sig*L1)*X2'; % time series of real risk-free rate

%% No arbitrage restrictions on bond yields
A           = zeros(striphorizon,1); 
B           = zeros(N,striphorizon);
Api         = zeros(striphorizon,1); 
Bpi         = zeros(N,striphorizon);
Am          = zeros(striphorizon,1); 
Bm          = zeros(N,striphorizon);


A(1)    = - y0_1 ;  
B(:,1)  = -(I_y1'-I_pi'*Psi + I_pi'*Sig*L1)';

Api(1)  = -y0nom_1; 
Bpi(:,1)= -I_y1';

Am(1)         =  mu_m - y0_1  + .5*(I_divgrm)'*Sig*Sig'*(I_divgrm) - (I_divgrm)'*Sig*(L0-Sig'*I_pi); 
Bm(:,1)       = ((I_divgrm+I_pi)'*Psi - I_y1'  - (I_divgrm+I_pi)'*Sig*L1)';
PDm_model     = exp(Am(1)+Bm(:,1)'*X2')';

  
Psi_j = zeros(N);
varcapgain1 = zeros(N);
varcapgain2 = zeros(N);

adj_stock_j  = zeros(1,T);

for j = 1:striphorizon-1
    Api(j+1)  = - y0nom_1 + Api(j) + .5*Bpi(:,j)'*(Sig*Sig')*Bpi(:,j)- Bpi(:,j)'*Sig*L0;
    Bpi(:,j+1)= (Bpi(:,j)'*Psi- I_y1'- Bpi(:,j)'*Sig*L1)';    

    A(j+1)    = - y0_1    + A(j)   + .5*B(:,j)'*(Sig*Sig')*B(:,j)    - B(:,j)'*Sig*(L0-Sig'*I_pi);
    B(:,j+1)  = ((I_pi + B(:,j))'*Psi - I_y1' - (I_pi+B(:,j))'*Sig*L1)';  
    
    Am(j+1)        = Am(j) + mu_m - y0_1 + .5*(I_divgrm +Bm(:,j))'*Sig*Sig'*(I_divgrm+Bm(:,j)) -(I_divgrm+Bm(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bm(:,j+1)      = ((I_divgrm+I_pi+Bm(:,j))'*Psi - I_y1'  - (I_divgrm+I_pi+Bm(:,j))'*Sig*L1)';
    PDm_model      = PDm_model+exp(Am(j+1)+Bm(:,j+1)'*X2')';
      
    nombondriskprem(j)            = 400*Bpi(:,j)'*Sig*L0;
    realbondriskprem(j)           = 400*B(:,j)'*Sig*L0;
    equitydivstripriskprem(j)     = 400*(I_divgrm+I_pi+Bm(:,j))'*Sig*L0;
    
    if j<61
        % These are expected returns [in levels, not logs] on strips of horizon ttt
        Psi_j        = Psi_j+Psi^j;
        varcapgain1   = Psi*(varcapgain1)*Psi' + Sigma;
        varcapgain2   = varcapgain2 + varcapgain1;
        adj_stock_j  = adj_stock_j + exp(Am(j)+Bm(:,j)'*X2')./exp(A0m+I_pdm'*X2');
        
        stripexpret_bond(j,:)       = exp(-Api(j)-Bpi(:,j)'*X2')-1;
        stripexpret_stock(j,:)      = exp(j*(mu_m+pi0)     - Am(j)     +(I_divgrm+I_pi)'*Psi_j*X2'      - Bm(:,j)'*X2'      + 0.5*j*(I_divgrm+I_pi)'*Sig*Sig'*(I_divgrm+I_pi))-1;
        stripexpret_capgain_stock(j,:) = (exp((I_pdm'*(Psi^j-eye(N))      +(I_divgrm+I_pi)'*Psi_j)*X2'+ j*(mu_m+pi0)     + 0.5*I_pdm'*varcapgain1*I_pdm             + 0.5*(I_divgrm+I_pi)'*varcapgain2*(I_divgrm+I_pi))./(1-adj_stock_j)) -1;
    end
    
    nombrp(j,:)         = ((Api(1)-Api(j)/j) + (-Bpi(:,j)'/j + Bpi(:,1)'/j *(I-Psi^j)*inv(I-Psi))*X2');
    realbrp(j,:)        = ((A(1)-A(j)/j) + (-B(:,j)'/j + B(:,1)'/j *(I-Psi^j)*inv(I-Psi))*X2');
    inflbrp(j,:)        = nombrp(j,:) - realbrp(j,:) ;
    ts_brp_mean(j)      = mean(nombrp(j,:));
    ts_brp_std(j)       = std(nombrp(j,:));
    ts_brp_real_mean(j) = mean(realbrp(j,:));
    ts_brp_real_std(j)  = std(realbrp(j,:));
    ts_brp_infl_mean(j) = mean(inflbrp(j,:));
    ts_brp_infl_std(j)  = std(inflbrp(j,:));

end


stripexpret_bond   = stripexpret_bond'; % turn into 175 (time series, quarterly 1974.Q1-2017.Q4) by 60 horizons (quarters)
stripexpret_stock  = stripexpret_stock';
stripexpret_capgain_stock  = stripexpret_capgain_stock';

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


% Comparison risk premium dynamics
disp('Equity risk premium diagnostics')
[L0(find(I_divgrm==1)), (((r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)')-aa(1:5)*L0(1:5))/aa(6))]
[L1(find(I_divgrm==1),:)',((cc-aa(1:5)*L1(1:5,:))/aa(6))']

% Matching average intermediate yields
yielderror(1,:) = (-Api(1)'./1 -Bpi(:,1)'./1*X2')'       -ynom1; 
yielderror(2,:) = (-Api(4)'./4 -Bpi(:,4)'./4*X2')'       -ynom4; 
yielderror(3,:) = (-Api(8)'./8 -Bpi(:,8)'./8*X2')'       -ynom8; 
yielderror(4,:) = (-Api(20)'./20 -Bpi(:,20)'./20*X2')'   -ynom20;
yielderror(5,:) = (-Api(40)'./40 -Bpi(:,40)'./40*X2')'   -ynom40;
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


% Inflation risk premium over 5-years (Ang-Bekaert or KvHVN definition), in quarterly terms
inflrp20_model = ynom20_model - yreal20_model - expinfl20;
disp(' Inflation risk premium over the next 5 years, in percent per year')
400*mean(inflrp20_model)

% Risk premia on dividend futures
divfuturereturn = zeros(striphorizon,T);
divfuturereturn(2:end,2:end) = exp(Am(1:end-1)-Api(1:end-1) + (Bm(:,1:end-1)-Bpi(:,1:end-1))'*X2(2:end,:)' ...
    -Am(2:end)+Api(2:end) - (Bm(:,2:end)-Bpi(:,2:end))'*X2(1:end-1,:)'...
    +mu_m+pi0 + (I_divgrm+I_pi)'*X2(2:end,:)') -1 ;
portfdivfuturereturn_model = 400*mean(mean(divfuturereturn(2:29,117:158),2)); % avg of first 28 quarterly strip returns, avg from 2003.Q1-2014.Q2 
portfdivfuturereturn_data = (0.0041+0.0059+0.0067+0.0072+0.0084+0.0090+0.0095)*1200/7; % from Binsbergen and Koijen, JFE 2017, Table 1, US panel, average of the 1- through 7-year dividend future returns
disp('Dividend futures avg realized excess return (avg 1-7 years), in percent per year')
[portfdivfuturereturn_data portfdivfuturereturn_model]

divfutureriskprem_model = 400*(Am(1:end-1)-Api(1:end-1)-Am(2:end)+Api(2:end) +mu_m+pi0...
    +((Bm(:,1:end-1)-Bpi(:,1:end-1)+I_divgrm+I_pi)'*Psi -(Bm(:,2:end)-Bpi(:,2:end))')*X2(2:end,:)');
divfutureriskprem_model_uncond = 400*(Am(1:end-1)-Api(1:end-1)-Am(2:end)+Api(2:end) +mu_m+pi0);
divfutureriskprem_model_short = mean(400*(Am(1:end-1)-Api(1:end-1)-Am(2:end)+Api(2:end) +mu_m+pi0...
    +((Bm(:,1:end-1)-Bpi(:,1:end-1)+I_divgrm+I_pi)'*Psi -(Bm(:,2:end)-Bpi(:,2:end))')*X2(117:158,:)'),2);

%% Conditional MPR
nomshortrate = y0nom_1 + I_y1'*X2'; % 1 by T
L = kron(L0,ones(1,T)) + L1*X2';    % N by T (10 by 324)
eps_struct = Sig\eps2';               % eps2 are VAR resids, they have covariance Sigma, eps_orig are theoretical shocks with covariance matrix Identity 
eps_struct = eps_struct';
eps_struct = [zeros(1,N);eps_struct];   % N by T; shocks in period 1 set to zero
mnom(1)  = -nomshortrate(1) - .5*L(:,1)'*L(:,1)-L(:,1)'*eps_struct(1,:)';
mreal(1) = mnom(1) +  pi0 + I_pi'*X2(1,:)';
mnom_constantmpr(1) = -nomshortrate(1) - .5*L0'*L0 -L0'*eps_struct(1,:)';

% Nominal realized dividend growth
divgrowth_stock = mu_m    +pi0 + (I_divgrm+I_pi)'*X2';
gdpgrowth       = x0      +pi0 + (I_gdp+I_pi)'*X2';
divgrowth_stockreal = mu_m     + (I_divgrm)'*X2';
gdpgrowthreal   = x0      + (I_gdp)'*X2';
cpigrowth       = pi0     + (I_pi)'*X2';

% Nominal expected dividend growth
expdivgr_stock = mu_m     +pi0 + (I_divgrm+I_pi)'*Psi*X2';

logdiv_stock(1) = 0+divgrowth_stock(1); % div normalized to 1 in 1974.Q1
loggdp(1)       = 0+gdpgrowth(1);            
logdiv_stockreal(1) = 0+divgrowth_stockreal(1); % div normalized to 1 in 1974.Q1
loggdpreal(1)   = 0+gdpgrowthreal(1);            
logcpi(1)       = 0+cpigrowth(1);            


for t =2 :T   
   mnom(t)             = -nomshortrate(t-1) - .5*L(:,t-1)'*L(:,t-1) -L(:,t-1)'*eps_struct(t,:)';
   mreal(t)            =  mnom(t) + pi0 + I_pi'*X2(t,:)';   
   mnom_constantmpr(t) = -nomshortrate(t-1) - .5*L0'*L0                 - L0'*eps_struct(t,:)';
   mnom_bond(t)        = -nomshortrate(t-1) - .5*L(1:4,t-1)'*L(1:4,t-1) - L(1:4,t-1)'*eps_struct(t,1:4)';
   mnom_stockbond(t)   = -nomshortrate(t-1) - .5*L(1:6,t-1)'*L(1:6,t-1) - L(1:6,t-1)'*eps_struct(t,1:6)';
   
   logdiv_stock(t)     = logdiv_stock(t-1) + divgrowth_stock(t);
   loggdp(t)           = loggdp(t-1) + gdpgrowth(t);
   
   logdiv_stockreal(t) = logdiv_stockreal(t-1) + divgrowth_stockreal(t);
   loggdpreal(t)       = loggdpreal(t-1) + gdpgrowthreal(t);
   logcpi(t)           = logcpi(t-1) + cpigrowth(t);
   
end


mnom_ann(1:T-3) = mnom(1:T-3)+mnom(2:T-2)+mnom(3:T-1)+mnom(4:T); 

disp('Max implied Sharpe ratio')
viol_maxSR = 100*exp(max(std(mnom)-.80,0))^4 -1;
maxannualSR = std(mnom_ann);
[maxannualSR]

zcbprices = exp(kron((Api(1:60)')',ones(1,T)) +Bpi(:,1:60)'*X2')';
divstripprices_stock = exp(kron((Am(1:60)')',ones(1,T))+Bm(:,1:60)'*X2')';

FF_new    = nansum((exp(A0m+I_pdm'*X2'-log(4))' - PDm_model/4).^2);
disp(['PDm violation:  ',num2str(FF_new)])

%% Plot figures
ttime=[1974:0.25:2019+0.75];

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(2,2,1)
plot(ttime,[400*(-Api(1)/1 - Bpi(:,1)'/1*X2')'  400*ynom1],'LineWidth',1.4)
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 1-qtr bond')
subplot(2,2,2)
plot(ttime,[400*(-Api(4)/4 - Bpi(:,4)'/4*X2')'  400*ynom4],'LineWidth',1.4)
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 1-yr bond')
subplot(2,2,3)
plot(ttime,[400*(-Api(20)/20 - Bpi(:,20)'/20*X2')' 400*ynom20],'LineWidth',1.4)
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 5-year bond')
subplot(2,2,4)
plot(ttime,[400*(-Api(40)'./40 -Bpi(:,40)'./40*X2')',  400*ynom40],'LineWidth',1.4)
xlim([1973,2018]);ylim([0,15]);
legend('model','data','Location','NorthEast')
title('Nom. yield 10-year bond') 
set(findall(gcf,'-property','FontSize'),'FontSize',24)


darkgreen=[0 0.5 0];
maxmat = 400;
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(2,2,1)
plot([1:1:maxmat],-400*Api(1:maxmat)./[1:1:maxmat]','black','LineWidth',1.6)
xlim([0,maxmat]); ylim([0,8]);
title('Avg. nom. yield')
ylabel('percent per year')
xlabel('maturity in quarters')
subplot(2,2,2)
plot([1:1:maxmat],-400*A(1:maxmat)./[1:1:maxmat]','r','LineWidth',1.6)
xlim([0,maxmat]); ylim([0,5])
title('Avg. real yield')
ylabel('percent per year')
xlabel('maturity in quarters')
subplot(2,2,3)
plot(ttime,[400*bondriskpremTV_model' 400*bondriskpremTV_data'],'LineWidth',1.6)
xlim([1973,2018]);ylim([-1.5,5.5]);
legend('model','data','Location','NorthWest')
ylabel('% per year')
title('Risk Premium 5-yr nom. bond')
subplot(2,2,4)
plot(ttime,400*ynom20_model,'Color','black','LineWidth',1.6)
hold on
plot(ttime, 400*yreal20_model,'--','Color','r','LineWidth',1.6)
hold on
plot(ttime,  400*expinfl20,'-.', 'Color', darkgreen,'LineWidth',1.6)
hold on
plot(ttime, 400*inflbrp(20,:)',':','Color','c','LineWidth',1.6)
legend('ynom','yreal','exp infl','IRP')
ylabel('% per year')
xlim([1973,2018]);ylim([-3.5,15.5]);
title('Decomposing 5-yr nom. bond yield')
set(findall(gcf,'-property','FontSize'),'FontSize',24)


figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot(ttime,[equityriskpremTV_model' equityriskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Equity risk premium')
subplot(1,2,2)
plot(ttime,[PDm_model/4,exp(A0m+I_pdm'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,90]);
legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Equity ') 

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot(ttime,[PDm_strip_8q PDm_strip_8q_data],'LineWidth',1.5)
xlim([1973,2018]);ylim([5,10]);
title('8-quarter Cum. Dividend Strip PD')
legend('model','data','Location','SouthWest')
subplot(1,2,2)
plot(ttime,[sharestrip_8q sharestrip_8q_data],'LineWidth',1.5)
xlim([1973,2018]);ylim([0,.15]);
title('8-quarter Cum. Dividend Strip Share')
legend('model','data','Location','NorthEast')
set(findall(gcf,'-property','FontSize'),'FontSize',24)

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot(ttime(4:end),(mnom_ann))
xlim([1974 2020]);
title('Annual log SDF m');
subplot(1,2,2)
plot(ttime(4:end),exp(mnom_ann))
xlim([1974 2020]);
title('Annual SDF M');
set(findall(gcf,'-property','FontSize'),'FontSize',24)


% Save ZCB prices and dividend strip prices (PD ratios) for maturities 1 to 180 months
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot(ttime,zcbprices(:,[4,20,40]),'LineWidth',1.2);legend('One-year','Five-year','Ten-year','Location','SouthEast');title('Zero-coupon Bond Prices')
xlim([1973,2020]);ylim([0,1.6]);
subplot(1,2,2)
plot(ttime,divstripprices_stock(:,[4,20,40]),'LineWidth',1.2);legend('One-year','Five-year','Ten-year','Location','SouthEast');title('Dividend Strip Prices - Stock Market')
xlim([1973,2020]);ylim([0,1.6]);
set(findall(gcf,'-property','FontSize'),'FontSize',24)


maxmat = 60;
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot([1:1:maxmat],nombondriskprem(1:maxmat),'LineWidth',2,'color','black')
xlim([0,maxmat])
ylim([0,12])
title('Average zero-coupon bond risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(1,2,2)
plot([1:1:maxmat],[equitydivstripriskprem(1:maxmat)',divfutureriskprem_model_uncond(1:maxmat),divfutureriskprem_model_short(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,12])
title('Average Market div strip risk premium')
legend('Spot risk premium','Futures risk premium','Futures risk premium,2003.Q1-2014.Q2','location','NorthEast') 
ylabel('percent per annum')
xlabel('maturity in quarters')
set(findall(gcf,'-property','FontSize'),'FontSize',24)


%% Save output to be used in MC analysis
disp('Saving results to be used in MC analysis')
if printfigs==1    
    save APoutputMCinput_CondCAPM N Psi Sigma Sig eps2 Omega L0 L1 y0nom_1 I_y1 mu_m pi0 I_divgrm I_pi A0m I_pdm Api Bpi Am Bm 
end