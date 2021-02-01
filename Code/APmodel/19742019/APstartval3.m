%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AP Model 
%
% This code is the third step in a series of files to run to produce good
% starting values for the full market price of risk estimation. This file
% uses only moments on real estate and infrastructure stocks to pin down a  
% subset of MPR coefficients. It takes as given the the MPR coefficients 
% estimated form the first and second steps.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all; clc

%% Load data
APloaddata

%% First stage OLS estimation of VAR
load Psimatrix Psi R_psi eps Tstatt

Sigma = cov(eps); 
Sig   = chol(Sigma,'lower'); % Sig is a standard deviation matrix, Sigma is a variance matrix
eps2  = eps;
Omega = cov(X2);

%% Second stage estimation of market prices of risk
striphorizon = 3600;

global LL LL1 counts
counts = 0;

% Come up with good initial guess
load x_guess_1 x
LL = x;

load x_guess_2 x
LL1 = x;

L0 = zeros(N,1);
L1 = zeros(N,N);
L0(I_pi==1)                 = LL(1);
L0(I_gdp==1)                = LL(2);
L0(I_y1==1)                 = LL(3);
L0(I_yspr==1)               = LL(4);
L1(I_pi==1,I_pi==1)         = LL(5);
L1(I_gdp==1,I_gdp==1)       = LL(6);
L1(I_y1==1,I_y1==1)         = LL(7);
L1(I_y1==1,I_yspr==1)       = LL(8);
L1(I_yspr==1,I_pi==1)       = LL(9);
L1(I_yspr==1,I_gdp==1)      = LL(10);
L1(I_yspr==1,I_y1==1)       = LL(11);
L1(I_yspr==1,I_yspr==1)     = LL(12);
L1(I_yspr==1,I_pdm==1)      = LL(13);

L0(I_pdm==1)           = LL1(1);
L0(I_divgrm==1)        = LL1(2);
L1(I_pdm==1,I_pi==1)        = LL1(3); 
L1(I_pdm==1,I_gdp==1)       = LL1(4); 
L1(I_pdm==1,I_y1==1)        = LL1(5); 
L1(I_pdm==1,I_yspr==1)      = LL1(6); 
L1(I_pdm==1,I_pdm==1)       = LL1(7); 
L1(I_pdm==1,I_divgrm==1)    = LL1(8); 
L1(I_divgrm==1,I_pi==1)     = LL1(9);
L1(I_divgrm==1,I_gdp==1)    = LL1(10);
L1(I_divgrm==1,I_y1==1)     = LL1(11);
L1(I_divgrm==1,I_yspr==1)   = LL1(12);
L1(I_divgrm==1,I_pdm==1)    = LL1(13);
L1(I_divgrm==1,I_divgrm==1) = LL1(14);
L1(I_divgrm==1,7)           = LL1(15);
L1(I_divgrm==1,11)          = LL1(16);

% % If this is the first time running this file uncomment this block
% % reit div growth row
% aa_reit = (I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Sig;
% cc_reit = ((I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Psi -I_pd_reit' -I_y1');
% L0(find(I_divgr_reit==1)) = (((r0_reit + pi0 - y0nom_1 +0.5*(aa_reit)*(aa_reit)')-aa_reit(1:7)*L0(1:7))/aa_reit(8));    
% x_init(1) = L0(find(I_divgr_reit==1));
% L1(find(I_divgr_reit==1),:) =((cc_reit-aa_reit(1:7)*L1(1:7,:))/aa_reit(8));
% num_reit=length(find(L1(find(I_divgr_reit==1),:)));
% x_init(3:3+num_reit-1) = L1(find(I_divgr_reit==1),find(L1(find(I_divgr_reit==1),:)));
% % infra div growth
% aa_infra = (I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Sig;
% cc_infra = ((I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Psi -I_pd_infra' -I_y1');
% L0(find(I_divgr_infra==1))=(((r0_infra + pi0 - y0nom_1 +0.5*(aa_infra)*(aa_infra)')-aa_infra(1:9)*L0(1:9))/aa_infra(10));
% x_init(2) = L0(find(I_divgr_infra==1));
% L1(find(I_divgr_infra==1),:)=((cc_infra-aa_infra(1:9)*L1(1:9,:))/aa_infra(10));
% num_infra=length(find(L1(find(I_divgr_infra==1),:)));
% x_init(3+num_reit:3+num_reit+num_infra-1) = L1(find(I_divgr_infra==1),find(L1(find(I_divgr_infra==1),:)));

% If this is the second or higher time running this file, uncomment the
% next two lines
load  x_guess_3 x
x_init = x;

options = optimset('DiffMinChange',.1,'TolX',0.01,'TolFun',0.01,'MaxIter',1000,'MaxFunEval',1000);
[x,fval,exitflag,output] = fminsearch('solvenul_startval3',x_init,options,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,I_pd_reit,I_divgr_reit,I_pd_infra,I_divgr_infra,I_pd_small,I_divgr_small,I_pd_growth,I_divgr_growth,I_pd_NR,I_divgr_NR,I_pd_value,I_divgr_value,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,A0_reit,k1_reit,r0_reit,mu_reit,A0_infra,k1_infra,r0_infra,mu_infra,A0_small,k1_small,r0_small,mu_small,A0_growth,k1_growth,r0_growth,mu_growth,PDm_strip_8q_data,sharestrip_8q_data,A0_NR,k1_NR,r0_NR,mu_NR,A0_value,k1_value,r0_value,mu_value,taureal,yielddatareal);
save x_guess_3 x

%%
L0 = zeros(N,1);
L1 = zeros(N,N);

i=1;


% constant MPRs (1-12)
L0(I_pi==1)            = LL(1);
L0(I_gdp==1)           = LL(2);
L0(I_y1==1)            = LL(3);
L0(I_yspr==1)          = LL(4);
L1(I_pi==1,I_pi==1)         = LL(5);
L1(I_gdp==1,I_gdp==1)       = LL(6);
L1(I_y1==1,I_y1==1)         = LL(7);
L1(I_y1==1,I_yspr==1)       = LL(8);
L1(I_yspr==1,I_pi==1)       = LL(9);
L1(I_yspr==1,I_gdp==1)      = LL(10);
L1(I_yspr==1,I_y1==1)       = LL(11);
L1(I_yspr==1,I_yspr==1)     = LL(12);
L1(I_yspr==1,I_pdm==1)      = LL(13);

L0(I_pdm==1)           = LL1(1);
L0(I_divgrm==1)        = LL1(2);
L1(I_pdm==1,I_pi==1)        = LL1(3); 
L1(I_pdm==1,I_gdp==1)       = LL1(4); 
L1(I_pdm==1,I_y1==1)        = LL1(5); 
L1(I_pdm==1,I_yspr==1)      = LL1(6); 
L1(I_pdm==1,I_pdm==1)       = LL1(7); 
L1(I_pdm==1,I_divgrm==1)    = LL1(8); 
L1(I_divgrm==1,I_pi==1)     = LL1(9);
L1(I_divgrm==1,I_gdp==1)    = LL1(10);
L1(I_divgrm==1,I_y1==1)     = LL1(11);
L1(I_divgrm==1,I_yspr==1)   = LL1(12);
L1(I_divgrm==1,I_pdm==1)    = LL1(13);
L1(I_divgrm==1,I_divgrm==1) = LL1(14);
L1(I_divgrm==1,7)           = LL1(15);
L1(I_divgrm==1,11)          = LL1(16);

L0(I_divgr_reit==1)            = x(i);i=i+1;
L0(I_divgr_infra==1)           = x(i);i=i+1;
L1(I_divgr_reit==1,I_pi==1)         = x(i); i = i+1;
L1(I_divgr_reit==1,I_gdp==1)        = x(i); i = i+1;
L1(I_divgr_reit==1,I_y1==1)         = x(i);  i = i+1;
L1(I_divgr_reit==1,I_yspr==1)       = x(i);  i = i+1;
L1(I_divgr_reit==1,I_pdm==1)        = x(i);  i = i+1;
L1(I_divgr_reit==1,I_divgrm==1)     = x(i);  i = i+1;
L1(I_divgr_reit==1,I_pd_reit==1)    = x(i);  i = i+1;
L1(I_divgr_reit==1,8)               = x(i);  i = i+1;
L1(I_divgr_reit==1,9)               = x(i);  i = i+1;
L1(I_divgr_reit==1,11)              = x(i);  i = i+1;
L1(I_divgr_infra==1,I_pi==1)        = x(i);  i = i+1;
L1(I_divgr_infra==1,I_gdp==1)       = x(i);  i = i+1;
L1(I_divgr_infra==1,I_y1==1)        = x(i);  i = i+1;
L1(I_divgr_infra==1,I_yspr==1)      = x(i);  i = i+1;
L1(I_divgr_infra==1,I_pdm==1)       = x(i);  i = i+1;
L1(I_divgr_infra==1,I_divgrm==1)    = x(i);  i = i+1;
L1(I_divgr_infra==1,I_pd_reit==1)   = x(i);  i = i+1;
L1(I_divgr_infra==1,8)              = x(i);  i = i+1;
L1(I_divgr_infra==1,9)              = x(i);  i = i+1;
L1(I_divgr_infra==1,10)             = x(i);  i = i+1;
L1(I_divgr_infra==1,11)             = x(i);  i = i+1;


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
A_reit      = zeros(striphorizon,1); 
B_reit      = zeros(N,striphorizon);
A_infra     = zeros(striphorizon,1); 
B_infra     = zeros(N,striphorizon);
A_small     = zeros(striphorizon,1); 
B_small     = zeros(N,striphorizon);
A_growth    = zeros(striphorizon,1); 
B_growth    = zeros(N,striphorizon);
A_NR        = zeros(striphorizon,1); 
B_NR        = zeros(N,striphorizon);
A_value     = zeros(striphorizon,1); 
B_value     = zeros(N,striphorizon);

Ax          = zeros(striphorizon,1); 
Bx          = zeros(N,striphorizon);
Amr         = zeros(striphorizon,1); 
Bmr         = zeros(N,striphorizon);
A_reitr     = zeros(striphorizon,1); 
B_reitr     = zeros(N,striphorizon);
A_infrar    = zeros(striphorizon,1); 
B_infrar    = zeros(N,striphorizon);
Axr         = zeros(striphorizon,1); 
Bxr         = zeros(N,striphorizon);
Ainfl       = zeros(striphorizon,1); 
Binfl       = zeros(N,striphorizon);
Amrsmall    = zeros(striphorizon,1); 
Bmrsmall    = zeros(N,striphorizon);
Amrgrowth     = zeros(striphorizon,1); 
Bmrgrowth     = zeros(N,striphorizon);
Am_det      = zeros(striphorizon,1); 
Bm_det      = zeros(N,striphorizon);
A_reit_det  = zeros(striphorizon,1); 
B_reit_det  = zeros(N,striphorizon);
A_infra_det = zeros(striphorizon,1); 
B_infra_det = zeros(N,striphorizon);

A(1)    = - y0_1 ;  
B(:,1)  = -(I_y1'-I_pi'*Psi + I_pi'*Sig*L1)';

Api(1)  = -y0nom_1; 
Bpi(:,1)= -I_y1';

Am(1)         =  mu_m - y0_1  + .5*(I_divgrm)'*Sig*Sig'*(I_divgrm) - (I_divgrm)'*Sig*(L0-Sig'*I_pi); 
Bm(:,1)       = ((I_divgrm+I_pi)'*Psi - I_y1'  - (I_divgrm+I_pi)'*Sig*L1)';
PDm_model     = exp(Am(1)+Bm(:,1)'*X2')';

A_reit(1)     =  mu_reit - y0_1     + .5*(I_divgr_reit)'*Sig*Sig'*(I_divgr_reit) - (I_divgr_reit)'*Sig*(L0-Sig'*I_pi); 
B_reit(:,1)   = ((I_divgr_reit+I_pi)'*Psi - I_y1'  - (I_divgr_reit+I_pi)'*Sig*L1)';
PD_reit_model = exp(A_reit(1)+B_reit(:,1)'*X2')';

A_infra(1)    =  mu_infra - y0_1     + .5*(I_divgr_infra)'*Sig*Sig'*(I_divgr_infra) - (I_divgr_infra)'*Sig*(L0-Sig'*I_pi); 
B_infra(:,1)  = ((I_divgr_infra+I_pi)'*Psi  - I_y1'  - (I_divgr_infra+I_pi)'*Sig*L1)';
PD_infra_model= exp(A_infra(1)+B_infra(:,1)'*X2')';

A_small(1)    =  mu_small - y0_1     + .5*(I_divgr_small)'*Sig*Sig'*(I_divgr_small) - (I_divgr_small)'*Sig*(L0-Sig'*I_pi); 
B_small(:,1)  = ((I_divgr_small+I_pi)'*Psi  - I_y1'  - (I_divgr_small+I_pi)'*Sig*L1)';
PD_small_model= exp(A_small(1)+B_small(:,1)'*X2')';

A_growth(1)     =  mu_growth - y0_1     + .5*(I_divgr_growth)'*Sig*Sig'*(I_divgr_growth) - (I_divgr_growth)'*Sig*(L0-Sig'*I_pi); 
B_growth(:,1)   = ((I_divgr_growth+I_pi)'*Psi  - I_y1'  - (I_divgr_growth+I_pi)'*Sig*L1)';
PD_growth_model = exp(A_growth(1)+B_growth(:,1)'*X2')';

A_NR(1)     =  mu_NR - y0_1     + .5*(I_divgr_NR)'*Sig*Sig'*(I_divgr_NR) - (I_divgr_NR)'*Sig*(L0-Sig'*I_pi); 
B_NR(:,1)   = ((I_divgr_NR+I_pi)'*Psi  - I_y1'  - (I_divgr_NR+I_pi)'*Sig*L1)';
PD_NR_model = exp(A_NR(1)+B_NR(:,1)'*X2')';

A_value(1)     =  mu_value - y0_1     + .5*(I_divgr_value)'*Sig*Sig'*(I_divgr_value) - (I_divgr_value)'*Sig*(L0-Sig'*I_pi); 
B_value(:,1)   = ((I_divgr_value+I_pi)'*Psi  - I_y1'  - (I_divgr_value+I_pi)'*Sig*L1)';
PD_value_model = exp(A_value(1)+B_value(:,1)'*X2')';

% GDP claim
Ax(1)     =  x0 - y0_1     + .5*(I_gdp)'*Sig*Sig'*(I_gdp) - (I_gdp)'*Sig*(L0-Sig'*I_pi); 
Bx(:,1)   = ((I_gdp+I_pi)'*Psi - I_y1'  - (I_gdp+I_pi)'*Sig*L1)';
PDx_model = exp(Ax(1)+Bx(:,1)'*X2')';

% Real dividend claims
Amr(1)   =  mu_m - y0_1  -pi0   + .5*(I_divgrm-I_pi)'*Sig*Sig'*(I_divgrm-I_pi) - (I_divgrm-I_pi)'*Sig*(L0-Sig'*I_pi); 
Bmr(:,1) = (I_divgrm'*Psi - I_y1'  - I_divgrm'*Sig*L1)';
PDmr_model=exp(Amr(1)+Bmr(:,1)'*X2')';

A_reitr(1)   =  mu_reit - y0_1 -pi0    + .5*(I_divgr_reit-I_pi)'*Sig*Sig'*(I_divgr_reit-I_pi) - (I_divgr_reit-I_pi)'*Sig*(L0-Sig'*I_pi); 
B_reitr(:,1) = (I_divgr_reit'*Psi - I_y1'  - I_divgr_reit'*Sig*L1)';
PD_reitr_model=exp(A_reitr(1)+B_reitr(:,1)'*X2')';

A_infrar(1)   =  mu_infra - y0_1  -pi0    + .5*(I_divgr_infra-I_pi)'*Sig*Sig'*(I_divgr_infra-I_pi) - (I_divgr_infra-I_pi)'*Sig*(L0-Sig'*I_pi); 
B_infrar(:,1) = (I_divgr_infra'*Psi  - I_y1'  - I_divgr_infra'*Sig*L1)';
PD_infrar_model=exp(A_infrar(1)+B_infrar(:,1)'*X2')';

Axr(1)    =  x0 - y0_1 -pi0   + .5*(I_gdp-I_pi)'*Sig*Sig'*(I_gdp-I_pi) - (I_gdp-I_pi)'*Sig*(L0-Sig'*I_pi); 
Bxr(:,1)  = (I_gdp'*Psi - I_y1'  - I_gdp'*Sig*L1)';
PDxr_model= exp(Axr(1)+Bxr(:,1)'*X2')';

Ainfl(1)   =   - y0_1; 
Binfl(:,1) = (I_pi'*Psi - I_y1'  - I_pi'*Sig*L1)';
PDinfl_model=exp(Ainfl(1)+Binfl(:,1)'*X2')';

Amrsmall(1)   =  mu_small - y0_1  -pi0   + .5*(I_divgr_small-I_pi)'*Sig*Sig'*(I_divgr_small-I_pi) - (I_divgr_small-I_pi)'*Sig*(L0-Sig'*I_pi); 
Bmrsmall(:,1) = (I_divgr_small'*Psi - I_y1'  - I_divgr_small'*Sig*L1)';
PDmrsmall_model=exp(Amrsmall(1)+Bmrsmall(:,1)'*X2')';

Amrgrowth(1)   =  mu_growth - y0_1  -pi0   + .5*(I_divgr_growth-I_pi)'*Sig*Sig'*(I_divgr_growth-I_pi) - (I_divgr_growth-I_pi)'*Sig*(L0-Sig'*I_pi); 
Bmrgrowth(:,1) = (I_divgr_growth'*Psi - I_y1'  - I_divgr_growth'*Sig*L1)';
PDmrgrowth_model=exp(Amrgrowth(1)+Bmrgrowth(:,1)'*X2')';

% Deterministic dividend claims
Am_det(1)   =  mu_m - y0_1     ; 
Bm_det(:,1) = ((I_pi)'*Psi - I_y1'  - (I_pi)'*Sig*L1)';
PDm_model_det=exp(Am_det(1)+Bm_det(:,1)'*X2')';

A_reit_det(1)   =  mu_reit - y0_1     ; 
B_reit_det(:,1) = ((I_pi)'*Psi - I_y1'  - (I_pi)'*Sig*L1)';
PD_reit_model_det=exp(A_reit_det(1)+B_reit_det(:,1)'*X2')';

A_infra_det(1)   =  mu_infra - y0_1     ; 
B_infra_det(:,1) = ((I_pi)'*Psi  - I_y1'  - (I_pi)'*Sig*L1)';
PD_infra_model_det=exp(A_infra_det(1)+B_infra_det(:,1)'*X2')';

   
Psi_j = zeros(N);
varcapgain1 = zeros(N);
varcapgain2 = zeros(N);

adj_stock_j  = zeros(1,T);
adj_reit_j   = zeros(1,T);
adj_infra_j  = zeros(1,T);
adj_small_j  = zeros(1,T);
adj_growth_j = zeros(1,T);
adj_value_j = zeros(1,T);
adj_NR_j = zeros(1,T);

for j = 1:striphorizon-1
    Api(j+1)  = - y0nom_1 + Api(j) + .5*Bpi(:,j)'*(Sig*Sig')*Bpi(:,j)- Bpi(:,j)'*Sig*L0;
    Bpi(:,j+1)= (Bpi(:,j)'*Psi- I_y1'- Bpi(:,j)'*Sig*L1)';    

    A(j+1)    = - y0_1    + A(j)   + .5*B(:,j)'*(Sig*Sig')*B(:,j)    - B(:,j)'*Sig*(L0-Sig'*I_pi);
    B(:,j+1)  = ((I_pi + B(:,j))'*Psi - I_y1' - (I_pi+B(:,j))'*Sig*L1)';  
    
    Am(j+1)        = Am(j) + mu_m - y0_1 + .5*(I_divgrm +Bm(:,j))'*Sig*Sig'*(I_divgrm+Bm(:,j)) -(I_divgrm+Bm(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bm(:,j+1)      = ((I_divgrm+I_pi+Bm(:,j))'*Psi - I_y1'  - (I_divgrm+I_pi+Bm(:,j))'*Sig*L1)';
    PDm_model      = PDm_model+exp(Am(j+1)+Bm(:,j+1)'*X2')';
    
    A_reit(j+1)    = A_reit(j) + mu_reit - y0_1 + .5*(I_divgr_reit+B_reit(:,j))'*Sig*Sig'*(I_divgr_reit +B_reit(:,j)) -(I_divgr_reit+B_reit(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_reit(:,j+1)  = ((I_divgr_reit+I_pi+B_reit(:,j))'*Psi  - I_y1'  - (I_divgr_reit+I_pi+B_reit(:,j))'*Sig*L1)';
    PD_reit_model  = PD_reit_model+exp(A_reit(j+1)+B_reit(:,j+1)'*X2')';

    A_infra(j+1)   = A_infra(j) + mu_infra - y0_1 + .5*(I_divgr_infra +B_infra(:,j))'*Sig*Sig'*(I_divgr_infra+B_infra(:,j)) -(I_divgr_infra+B_infra(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_infra(:,j+1) = ((I_divgr_infra+I_pi+B_infra(:,j))'*Psi  - I_y1'  - (I_divgr_infra+I_pi+B_infra(:,j))'*Sig*L1)';
    PD_infra_model = PD_infra_model+exp(A_infra(j+1)+B_infra(:,j+1)'*X2')';

    A_small(j+1)   = A_small(j) + mu_small - y0_1 + .5*(I_divgr_small +B_small(:,j))'*Sig*Sig'*(I_divgr_small+B_small(:,j)) -(I_divgr_small+B_small(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_small(:,j+1) = ((I_divgr_small+I_pi+B_small(:,j))'*Psi  - I_y1'  - (I_divgr_small+I_pi+B_small(:,j))'*Sig*L1)';
    PD_small_model = PD_small_model+exp(A_small(j+1)+B_small(:,j+1)'*X2')';

    A_growth(j+1)    = A_growth(j) + mu_growth - y0_1 + .5*(I_divgr_growth +B_growth(:,j))'*Sig*Sig'*(I_divgr_growth+B_growth(:,j)) -(I_divgr_growth+B_growth(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_growth(:,j+1)  = ((I_divgr_growth+I_pi+B_growth(:,j))'*Psi  - I_y1'  - (I_divgr_growth+I_pi+B_growth(:,j))'*Sig*L1)';
    PD_growth_model  = PD_growth_model+exp(A_growth(j+1)+B_growth(:,j+1)'*X2')';

    A_NR(j+1)    = A_NR(j) + mu_NR - y0_1 + .5*(I_divgr_NR +B_NR(:,j))'*Sig*Sig'*(I_divgr_NR+B_NR(:,j)) -(I_divgr_NR+B_NR(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_NR(:,j+1)  = ((I_divgr_NR+I_pi+B_NR(:,j))'*Psi  - I_y1'  - (I_divgr_NR+I_pi+B_NR(:,j))'*Sig*L1)';
    PD_NR_model  = PD_NR_model+exp(A_NR(j+1)+B_NR(:,j+1)'*X2')';

    A_value(j+1)    = A_value(j) + mu_value - y0_1 + .5*(I_divgr_value +B_value(:,j))'*Sig*Sig'*(I_divgr_value+B_value(:,j)) -(I_divgr_value+B_value(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_value(:,j+1)  = ((I_divgr_value+I_pi+B_value(:,j))'*Psi  - I_y1'  - (I_divgr_value+I_pi+B_value(:,j))'*Sig*L1)';
    PD_value_model  = PD_value_model+exp(A_value(j+1)+B_value(:,j+1)'*X2')';
   
    % Nominal GDP claim
    Ax(j+1)   = Ax(j) + x0 - y0_1 + .5*(I_gdp +Bx(:,j))'*Sig*Sig'*(I_gdp+Bx(:,j)) -(I_gdp+Bx(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bx(:,j+1) = ((I_gdp+I_pi+Bx(:,j))'*Psi - I_y1'  - (I_gdp+I_pi+Bx(:,j))'*Sig*L1)';
    PDx_model = PDx_model+exp(Ax(j+1)+Bx(:,j+1)'*X2')';

    % Real dividend claims
    Amr(j+1)   = Amr(j) + mu_m - pi0 - y0_1 + .5*(I_divgrm -I_pi +Bmr(:,j))'*Sig*Sig'*(I_divgrm-I_pi+Bmr(:,j)) -(I_divgrm-I_pi+Bmr(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bmr(:,j+1) = ((I_divgrm+Bmr(:,j))'*Psi - I_y1'  - (I_divgrm+Bmr(:,j))'*Sig*L1)';
    PDmr_model = PDmr_model+exp(Amr(j+1)+Bmr(:,j+1)'*X2')';
    
    A_reitr(j+1)   = A_reitr(j) + mu_reit -pi0 - y0_1 + .5*(I_divgr_reit-I_pi+B_reitr(:,j))'*Sig*Sig'*(I_divgr_reit-I_pi +B_reitr(:,j)) -(I_divgr_reit-I_pi+B_reitr(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_reitr(:,j+1) = ((I_divgr_reit+B_reitr(:,j))'*Psi  - I_y1'  - (I_divgr_reit+B_reitr(:,j))'*Sig*L1)';
    PD_reitr_model = PD_reitr_model+exp(A_reitr(j+1)+B_reitr(:,j+1)'*X2')';

    A_infrar(j+1)   = A_infrar(j) + mu_infra -pi0 - y0_1 + .5*(I_divgr_infra-I_pi +B_infrar(:,j))'*Sig*Sig'*(I_divgr_infra-I_pi+B_infrar(:,j)) -(I_divgr_infra-I_pi+B_infrar(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_infrar(:,j+1) = ((I_divgr_infra+B_infrar(:,j))'*Psi  - I_y1'  - (I_divgr_infra+B_infrar(:,j))'*Sig*L1)';
    PD_infrar_model = PD_infrar_model+exp(A_infrar(j+1)+B_infrar(:,j+1)'*X2')';

    % Real GDP claim
    Axr(j+1)   = Axr(j) + x0 - pi0 - y0_1 + .5*(I_gdp -I_pi +Bxr(:,j))'*Sig*Sig'*(I_gdp-I_pi+Bxr(:,j)) -(I_gdp-I_pi+Bxr(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bxr(:,j+1) = ((I_gdp+Bxr(:,j))'*Psi - I_y1'  - (I_gdp + Bxr(:,j))'*Sig*L1)';
    PDxr_model = PDxr_model+exp(Axr(j+1)+Bxr(:,j+1)'*X2')';
    
    % CPI claim
    Ainfl(j+1)   = Ainfl(j) - y0_1 + .5*(Binfl(:,j))'*Sig*Sig'*(Binfl(:,j)) -(Binfl(:,j))'*Sig*(L0-Sig'*I_pi); 
    Binfl(:,j+1) = ((I_pi+Binfl(:,j))'*Psi - I_y1'  - (I_pi+Binfl(:,j))'*Sig*L1)';
    PDinfl_model = PDinfl_model+exp(Ainfl(j+1)+Binfl(:,j+1)'*X2')';

    % Cross-sectional portfolios
    Amrsmall(j+1)   = Amrsmall(j) + mu_small - pi0 - y0_1 + .5*(I_divgr_small -I_pi +Bmrsmall(:,j))'*Sig*Sig'*(I_divgr_small-I_pi+Bmrsmall(:,j)) -(I_divgr_small-I_pi+Bmrsmall(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bmrsmall(:,j+1) = ((I_divgr_small+Bmrsmall(:,j))'*Psi - I_y1'  - (I_divgr_small+Bmrsmall(:,j))'*Sig*L1)';
    PDmrsmall_model = PDmrsmall_model+exp(Amrsmall(j+1)+Bmrsmall(:,j+1)'*X2')';

    Amrgrowth(j+1)   = Amrgrowth(j) + mu_growth - pi0 - y0_1 + .5*(I_divgr_growth -I_pi +Bmrgrowth(:,j))'*Sig*Sig'*(I_divgr_growth-I_pi+Bmrgrowth(:,j)) -(I_divgr_growth-I_pi+Bmrgrowth(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bmrgrowth(:,j+1) = ((I_divgr_growth+Bmrgrowth(:,j))'*Psi - I_y1'  - (I_divgr_growth+Bmrgrowth(:,j))'*Sig*L1)';
    PDmrgrowth_model = PDmrgrowth_model+exp(Amrgrowth(j+1)+Bmrgrowth(:,j+1)'*X2')';
 
    % Deterministic dividend claims
    Am_det(j+1)   = Am_det(j) + mu_m - y0_1 + .5*(Bm_det(:,j))'*Sig*Sig'*(Bm_det(:,j)) -(Bm_det(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bm_det(:,j+1) = ((I_pi+Bm(:,j))'*Psi - I_y1'  - (I_pi+Bm_det(:,j))'*Sig*L1)';
    PDm_model_det = PDm_model_det+exp(Am_det(j+1)+Bm_det(:,j+1)'*X2')';
    
    A_reit_det(j+1)   = A_reit_det(j) + mu_reit - y0_1 + .5*(B_reit_det(:,j))'*Sig*Sig'*(B_reit_det(:,j)) -(B_reit_det(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_reit_det(:,j+1) = ((I_pi+B_reit_det(:,j))'*Psi  - I_y1'  - (I_pi+B_reit_det(:,j))'*Sig*L1)';
    PD_reit_model_det = PD_reit_model_det+exp(A_reit_det(j+1)+B_reit_det(:,j+1)'*X2')';

    A_infra_det(j+1)   = A_infra_det(j) + mu_infra - y0_1 + .5*(B_infra_det(:,j))'*Sig*Sig'*(B_infra_det(:,j)) -(B_infra_det(:,j))'*Sig*(L0-Sig'*I_pi); 
    B_infra_det(:,j+1) = ((I_pi+B_infra_det(:,j))'*Psi  - I_y1'  - (I_pi+B_infra_det(:,j))'*Sig*L1)';
    PD_infra_model_det = PD_infra_model_det+exp(A_infra_det(j+1)+B_infra_det(:,j+1)'*X2')';

    nombondriskprem(j)            = 400*Bpi(:,j)'*Sig*L0;
    realbondriskprem(j)           = 400*B(:,j)'*Sig*L0;
    equitydivstripriskprem(j)     = 400*(I_divgrm+I_pi+Bm(:,j))'*Sig*L0;
    reitdivstripriskprem(j)       = 400*(I_divgr_reit+I_pi+B_reit(:,j))'*Sig*L0;
    infradivstripriskprem(j)      = 400*(I_divgr_infra+I_pi+B_infra(:,j))'*Sig*L0;
    gdpstripriskprem(j)           = 400*(I_gdp+I_pi+Bx(:,j))'*Sig*L0;
    smalldivstripriskprem(j)      = 400*(I_divgr_small+B_small(:,j))'*Sig*L0;
    growthdivstripriskprem(j)     = 400*(I_divgr_growth+B_growth(:,j))'*Sig*L0;
    NRdivstripriskprem(j)         = 400*(I_divgr_NR+B_NR(:,j))'*Sig*L0;
    valuedivstripriskprem(j)      = 400*(I_divgr_value+B_value(:,j))'*Sig*L0;

    equityrealdivstripriskprem(j) = 400*(I_divgrm+Bmr(:,j))'*Sig*L0;
    reitrealdivstripriskprem(j)   = 400*(I_divgr_reit+B_reitr(:,j))'*Sig*L0;
    infrarealdivstripriskprem(j)  = 400*(I_divgr_infra+B_infrar(:,j))'*Sig*L0;
    gdprealstripriskprem(j)       = 400*(I_gdp+Bxr(:,j))'*Sig*L0;
    inflstripriskprem(j)          = 400*(I_pi+Binfl(:,j))'*Sig*L0;
    smallrealdivstripriskprem(j)  = 400*(I_divgr_small+Bmrsmall(:,j))'*Sig*L0;
    growthrealdivstripriskprem(j) = 400*(I_divgr_growth+Bmrgrowth(:,j))'*Sig*L0;
    
    equitydivstripriskprem_det(j) = 400*(I_pi+Bm_det(:,j))'*Sig*L0;
    reitdivstripriskprem_det(j)   = 400*(I_pi+B_reit_det(:,j))'*Sig*L0;
    infradivstripriskprem_det(j)  = 400*(I_pi+B_infra_det(:,j))'*Sig*L0;
    
    if j<61
        % These are expected returns [in levels, not logs] on strips of horizon ttt
        Psi_j        = Psi_j+Psi^j;
        varcapgain1   = Psi*(varcapgain1)*Psi' + Sigma;
        varcapgain2   = varcapgain2 + varcapgain1;
        adj_stock_j  = adj_stock_j + exp(Am(j)+Bm(:,j)'*X2')./exp(A0m+I_pdm'*X2');
        adj_reit_j   = adj_reit_j  + exp(A_reit(j)+B_reit(:,j)'*X2')./exp(A0_reit+I_pd_reit'*X2'); 
        adj_infra_j  = adj_infra_j + exp(A_infra(j)+B_infra(:,j)'*X2')./exp(A0_infra+I_pd_infra'*X2');
        adj_small_j  = adj_small_j + exp(A_small(j)+B_small(:,j)'*X2')./exp(A0_small+I_pd_small'*X2');
        adj_growth_j = adj_growth_j+ exp(A_growth(j)+B_growth(:,j)'*X2')./exp(A0_growth+I_pd_growth'*X2');
        adj_NR_j     = adj_NR_j    + exp(A_NR(j)+B_NR(:,j)'*X2')./exp(A0_NR+I_pd_NR'*X2');
        adj_value_j  = adj_value_j + exp(A_value(j)+B_value(:,j)'*X2')./exp(A0_value+I_pd_value'*X2');
        
        stripexpret_bond(j,:)       = exp(-Api(j)-Bpi(:,j)'*X2')-1;
        stripexpret_stock(j,:)      = exp(j*(mu_m+pi0)     - Am(j)     +(I_divgrm+I_pi)'*Psi_j*X2'      - Bm(:,j)'*X2'      + 0.5*j*(I_divgrm+I_pi)'*Sig*Sig'*(I_divgrm+I_pi))-1;
        stripexpret_reit(j,:)       = exp(j*(mu_reit+pi0)  - A_reit(j) +(I_divgr_reit+I_pi)'*Psi_j*X2'  - B_reit(:,j)'*X2'  + 0.5*j*(I_divgr_reit+I_pi)'*Sig*Sig'*(I_divgr_reit+I_pi))-1;
        stripexpret_infra(j,:)      = exp(j*(mu_infra+pi0) - A_infra(j)+(I_divgr_infra+I_pi)'*Psi_j*X2' - B_infra(:,j)'*X2' + 0.5*j*(I_divgr_infra+I_pi)'*Sig*Sig'*(I_divgr_infra+I_pi))-1;
        stripexpret_small(j,:)      = exp(j*(mu_small+pi0) - A_small(j)+(I_divgr_small+I_pi)'*Psi_j*X2' - B_small(:,j)'*X2' + 0.5*j*(I_divgr_small+I_pi)'*Sig*Sig'*(I_divgr_small+I_pi))-1;
        stripexpret_growth(j,:)     = exp(j*(mu_growth+pi0) - A_growth(j)+(I_divgr_growth+I_pi)'*Psi_j*X2' - B_growth(:,j)'*X2' + 0.5*j*(I_divgr_growth+I_pi)'*Sig*Sig'*(I_divgr_growth+I_pi))-1;
        stripexpret_NR(j,:)         = exp(j*(mu_NR+pi0) - A_NR(j)+(I_divgr_NR+I_pi)'*Psi_j*X2' - B_NR(:,j)'*X2' + 0.5*j*(I_divgr_NR+I_pi)'*Sig*Sig'*(I_divgr_NR+I_pi))-1;
        stripexpret_value(j,:)      = exp(j*(mu_value+pi0) - A_value(j)+(I_divgr_value+I_pi)'*Psi_j*X2' - B_value(:,j)'*X2' + 0.5*j*(I_divgr_value+I_pi)'*Sig*Sig'*(I_divgr_value+I_pi))-1;
        
        stripexpret_gdp(j,:)        = exp(j*(x0+pi0)     - Ax(j)       +(I_gdp+I_pi)'*Psi_j*X2' - Bx(:,j)'*X2'       + 0.5*j*(I_gdp+I_pi)'*Sig*Sig'*(I_gdp+I_pi))-1;
        
        stripexpret_bondreal(j,:)   = exp(-A(j)-B(:,j)'*X2')-1;
        stripexpret_stockreal(j,:)  = exp(j*(mu_m)     - Amr(j)       +(I_divgrm)'*Psi_j*X2'           - Bmr(:,j)'*X2'      + 0.5*j*(I_divgrm)'*Sig*Sig'*(I_divgrm))-1;
        stripexpret_reitreal(j,:)   = exp(j*(mu_reit)  - A_reitr(j)   +(I_divgr_reit)'*Psi_j*X2'       - B_reitr(:,j)'*X2'  + 0.5*j*(I_divgr_reit)'*Sig*Sig'*(I_divgr_reit))-1;
        stripexpret_infrareal(j,:)  = exp(j*(mu_infra) - A_infrar(j)  +(I_divgr_infra)'*Psi_j*X2'      - B_infrar(:,j)'*X2' + 0.5*j*(I_divgr_infra)'*Sig*Sig'*(I_divgr_infra))-1;
        stripexpret_gdpreal(j,:)    = exp(j*(x0)       - Axr(j)       +(I_gdp)'*Psi_j*X2'              - Bxr(:,j)'*X2'      + 0.5*j*(I_gdp)'*Sig*Sig'*(I_gdp))-1;
        stripexpret_infl(j,:)       = exp(j*(pi0)      - Ainfl(j)     +(I_pi)'*Psi_j*X2'               - Binfl(:,j)'*X2'    + 0.5*j*(I_pi)'*Sig*Sig'*(I_pi))-1;
        
        stripexpret_stock_det(j,:)  = exp(j*(mu_m+pi0)     - Am_det(j)     +(I_pi)'*Psi_j*X2'   - Bm_det(:,j)'*X2'      + 0.5*j*(I_pi)'*Sig*Sig'*(I_pi))-1;
        stripexpret_reit_det(j,:)   = exp(j*(mu_reit+pi0)  - A_reit_det(j) +(I_pi)'*Psi_j*X2'   - B_reit_det(:,j)'*X2'  + 0.5*j*(I_pi)'*Sig*Sig'*(I_pi))-1;
        stripexpret_infra_det(j,:)  = exp(j*(mu_infra+pi0) - A_infra_det(j)+(I_pi)'*Psi_j*X2'   - B_infra_det(:,j)'*X2' + 0.5*j*(I_pi)'*Sig*Sig'*(I_pi))-1;
    
        stripexpret_capgain_stock(j,:) = (exp((I_pdm'*(Psi^j-eye(N))      +(I_divgrm+I_pi)'*Psi_j)*X2'+ j*(mu_m+pi0)     + 0.5*I_pdm'*varcapgain1*I_pdm             + 0.5*(I_divgrm+I_pi)'*varcapgain2*(I_divgrm+I_pi))./(1-adj_stock_j)) -1;
        stripexpret_capgain_reit(j,:)  = (exp((I_pd_reit'*(Psi^j-eye(N))  +(I_divgr_reit+I_pi)'*Psi_j)*X2'  + j*(mu_reit+pi0)  + 0.5*I_pd_reit'*varcapgain1*I_pd_reit     + 0.5*(I_divgr_reit+I_pi)'*varcapgain2*(I_divgr_reit+I_pi))./(1-adj_reit_j)) -1;
        stripexpret_capgain_infra(j,:) = (exp((I_pd_infra'*(Psi^j-eye(N)) +(I_divgr_infra+I_pi)'*Psi_j)*X2' + j*(mu_infra+pi0) + 0.5*I_pd_infra'*varcapgain1*I_pd_infra   + 0.5*(I_divgr_infra+I_pi)'*varcapgain2*(I_divgr_infra+I_pi))./(1-adj_infra_j)) -1;
        stripexpret_capgain_small(j,:) = (exp((I_pd_small'*(Psi^j-eye(N)) +(I_divgr_small+I_pi)'*Psi_j)*X2' + j*(mu_small+pi0) + 0.5*I_pd_small'*varcapgain1*I_pd_small   + 0.5*(I_divgr_small+I_pi)'*varcapgain2*(I_divgr_small+I_pi))./(1-adj_small_j)) -1;
        stripexpret_capgain_growth(j,:)= (exp((I_pd_growth'*(Psi^j-eye(N))+(I_divgr_growth+I_pi)'*Psi_j)*X2'+ j*(mu_growth+pi0)+ 0.5*I_pd_growth'*varcapgain1*I_pd_growth + 0.5*(I_divgr_growth+I_pi)'*varcapgain2*(I_divgr_growth+I_pi))./(1-adj_growth_j)) -1;
        stripexpret_capgain_NR(j,:)= (exp((I_pd_NR'*(Psi^j-eye(N))+(I_divgr_NR+I_pi)'*Psi_j)*X2'+ j*(mu_NR+pi0)+ 0.5*I_pd_NR'*varcapgain1*I_pd_NR + 0.5*(I_divgr_NR+I_pi)'*varcapgain2*(I_divgr_NR+I_pi))./(1-adj_NR_j)) -1;
        stripexpret_capgain_value(j,:)= (exp((I_pd_value'*(Psi^j-eye(N))+(I_divgr_value+I_pi)'*Psi_j)*X2'+ j*(mu_value+pi0)+ 0.5*I_pd_value'*varcapgain1*I_pd_value + 0.5*(I_divgr_value+I_pi)'*varcapgain2*(I_divgr_value+I_pi))./(1-adj_value_j)) -1;
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
stripexpret_reit   = stripexpret_reit';
stripexpret_infra  = stripexpret_infra';
stripexpret_small  = stripexpret_small';
stripexpret_growth = stripexpret_growth';
stripexpret_NR     = stripexpret_NR';
stripexpret_value  = stripexpret_value';

stripexpret_gdp    = stripexpret_gdp';

stripexpret_bondreal = stripexpret_bondreal'; % turn into 175 (time series, quarterly 1974.Q1-2017.Q4) by 60 horizons (quarters)
stripexpret_stockreal = stripexpret_stockreal';
stripexpret_reitreal = stripexpret_reitreal';
stripexpret_gdpreal = stripexpret_gdpreal';
stripexpret_infl = stripexpret_infl';

stripexpret_stock_det = stripexpret_stock_det';
stripexpret_reit_det = stripexpret_reit_det';
stripexpret_infra_det = stripexpret_infra_det';

stripexpret_capgain_stock  = stripexpret_capgain_stock';
stripexpret_capgain_reit   = stripexpret_capgain_reit';
stripexpret_capgain_infra  = stripexpret_capgain_infra';
stripexpret_capgain_small  = stripexpret_capgain_small';
stripexpret_capgain_growth = stripexpret_capgain_growth';
stripexpret_capgain_NR     = stripexpret_capgain_NR';
stripexpret_capgain_value  = stripexpret_capgain_value';

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

aa_small = (I_divgr_small+k1_small*I_pd_small+I_pi)'*Sig;
cc_small = ((I_divgr_small+k1_small*I_pd_small+I_pi)'*Psi -I_pd_small' -I_y1');
riskprem_small            = r0_small + pi0 - y0nom_1 + 0.5*(aa_small)*(aa_small)';
small_riskpremTV_model    = 400*(aa_small*L0+aa_small*L1*X2'); 
small_riskpremTV_data     = 400*(riskprem_small + cc_small*X2'); 
FF_small = 400*(riskprem_small - aa_small*L0); % pricing error is in percent per year
disp('Average small premium: data, model, pricing error, all in percent per year')
[mean(small_riskpremTV_data), mean(small_riskpremTV_model), FF_small]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(small_riskpremTV_data),  std(small_riskpremTV_model), (std(small_riskpremTV_data)-std(small_riskpremTV_model)) ]


aa_growth = (I_divgr_growth+k1_growth*I_pd_growth+I_pi)'*Sig;
cc_growth = ((I_divgr_growth+k1_growth*I_pd_growth+I_pi)'*Psi -I_pd_growth' -I_y1');
riskprem_growth            = r0_growth + pi0 - y0nom_1 + 0.5*(aa_growth)*(aa_growth)';
growth_riskpremTV_model    = 400*(aa_growth*L0+aa_growth*L1*X2'); 
growth_riskpremTV_data     = 400*(riskprem_growth + cc_growth*X2'); 
FF_growth = 400*(riskprem_growth - aa_growth*L0); % pricing error is in percent per year
disp('Average growth premium: data, model, pricing error, all in percent per year')
[mean(growth_riskpremTV_data), mean(growth_riskpremTV_model), FF_growth]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(growth_riskpremTV_data),  std(growth_riskpremTV_model), (std(growth_riskpremTV_data)-std(growth_riskpremTV_model)) ]

aa_NR = (I_divgr_NR+k1_NR*I_pd_NR+I_pi)'*Sig;
cc_NR = ((I_divgr_NR+k1_NR*I_pd_NR+I_pi)'*Psi -I_pd_NR' -I_y1');
riskprem_NR            = r0_NR + pi0 - y0nom_1 + 0.5*(aa_NR)*(aa_NR)';
NR_riskpremTV_model    = 400*(aa_NR*L0+aa_NR*L1*X2'); 
NR_riskpremTV_data     = 400*(riskprem_NR + cc_NR*X2'); 
FF_NR = 400*(riskprem_NR - aa_NR*L0); % pricing error is in percent per year
disp('Average NR premium: data, model, pricing error, all in percent per year')
[mean(NR_riskpremTV_data), mean(NR_riskpremTV_model), FF_NR]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(NR_riskpremTV_data),  std(NR_riskpremTV_model), (std(NR_riskpremTV_data)-std(NR_riskpremTV_model)) ]

aa_value = (I_divgr_value+k1_value*I_pd_value+I_pi)'*Sig;
cc_value = ((I_divgr_value+k1_value*I_pd_value+I_pi)'*Psi -I_pd_value' -I_y1');
riskprem_value            = r0_value + pi0 - y0nom_1 + 0.5*(aa_value)*(aa_value)';
value_riskpremTV_model    = 400*(aa_value*L0+aa_value*L1*X2'); 
value_riskpremTV_data     = 400*(riskprem_value + cc_value*X2'); 
FF_value = 400*(riskprem_value - aa_value*L0); % pricing error is in percent per year
disp('Average value premium: data, model, pricing error, all in percent per year')
[mean(value_riskpremTV_data), mean(value_riskpremTV_model), FF_value]
disp('And the standard deviations of the risk premium')
disp('data, model, pricing error, all in percent per year')
[std(value_riskpremTV_data),  std(value_riskpremTV_model), (std(value_riskpremTV_data)-std(value_riskpremTV_model)) ]


% Comparison risk premium dynamics
disp('Equity risk premium diagnostics')
[L0(find(I_divgrm==1)), (((r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)')-aa(1:5)*L0(1:5))/aa(6))]
[L1(find(I_divgrm==1),:)',((cc-aa(1:5)*L1(1:5,:))/aa(6))']
disp('REIT risk premium diagnostics')
[L0(find(I_divgr_reit==1)),(((r0_reit + pi0 - y0nom_1 +0.5*(aa_reit)*(aa_reit)')-aa_reit(1:7)*L0(1:7))/aa_reit(8))]    
[L1(find(I_divgr_reit==1),:)',((cc_reit-aa_reit(1:7)*L1(1:7,:))/aa_reit(8))']
disp('Infra risk premium diagnostics')
[L0(find(I_divgr_infra==1)),(((r0_infra + pi0 - y0nom_1 +0.5*(aa_infra)*(aa_infra)')-aa_infra(1:9)*L0(1:9))/aa_infra(10))]
[L1(find(I_divgr_infra==1),:)',((cc_infra-aa_infra(1:9)*L1(1:9,:))/aa_infra(10))']
disp('Small risk premium diagnostics')
[L0(find(I_divgr_small==1)),(((r0_small + pi0 - y0nom_1 +0.5*(aa_small)*(aa_small)')-aa_small(1:11)*L0(1:11))/aa_small(12))]
[L1(find(I_divgr_small==1),:)',((cc_small-aa_small(1:11)*L1(1:11,:))/aa_small(12))']
disp('growth risk premium diagnostics')
[L0(find(I_divgr_growth==1))  , (((r0_growth + pi0 - y0nom_1 +0.5*(aa_growth)*(aa_growth)')-aa_growth(1:13)*L0(1:13))/aa_growth(14))]
[L1(find(I_divgr_growth==1),:)'  ,((cc_growth-aa_growth(1:13)*L1(1:13,:))/aa_growth(14))']
disp('NR risk premium diagnostics')
[L0(find(I_divgr_NR==1))  , (((r0_NR + pi0 - y0nom_1 +0.5*(aa_NR)*(aa_NR)')-aa_NR(1:15)*L0(1:15))/aa_NR(16))]
[L1(find(I_divgr_NR==1),:)'     ,((cc_NR-aa_NR(1:15)*L1(1:15,:))/aa_NR(16))']
disp('value risk premium diagnostics')
[L0(find(I_divgr_value==1))  , (((r0_value + pi0 - y0nom_1 +0.5*(aa_value)*(aa_value)')-aa_value(1:17)*L0(1:17))/aa_value(18))]
[L1(find(I_divgr_value==1),:)'  ,((cc_value-aa_value(1:17)*L1(1:17,:))/aa_value(18))']


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
divfutureriskprem_reit_model_uncond   = 400*(A_reit(1:end-1)-Api(1:end-1)-A_reit(2:end)+Api(2:end) +mu_reit+pi0);
divfutureriskprem_infra_model_uncond  = 400*(A_infra(1:end-1)-Api(1:end-1)-A_infra(2:end)+Api(2:end) +mu_infra+pi0);
divfutureriskprem_small_model_uncond  = 400*(A_small(1:end-1)-Api(1:end-1)-A_small(2:end)+Api(2:end) +mu_small+pi0);
divfutureriskprem_growth_model_uncond = 400*(A_growth(1:end-1)-Api(1:end-1)-A_growth(2:end)+Api(2:end) +mu_growth+pi0);
divfutureriskprem_NR_model_uncond     = 400*(A_NR(1:end-1)-Api(1:end-1)-A_NR(2:end)+Api(2:end) +mu_NR+pi0);
divfutureriskprem_value_model_uncond  = 400*(A_value(1:end-1)-Api(1:end-1)-A_value(2:end)+Api(2:end) +mu_value+pi0);


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
divgrowth_reit  = mu_reit +pi0 + (I_divgr_reit+I_pi)'*X2';
divgrowth_infra = mu_infra+pi0 + (I_divgr_infra+I_pi)'*X2';
gdpgrowth       = x0      +pi0 + (I_gdp+I_pi)'*X2';
divgrowth_stockreal = mu_m     + (I_divgrm)'*X2';
divgrowth_reitreal  = mu_reit  + (I_divgr_reit)'*X2';
divgrowth_infrareal = mu_infra + (I_divgr_infra)'*X2';
gdpgrowthreal   = x0      + (I_gdp)'*X2';
cpigrowth       = pi0     + (I_pi)'*X2';

% Nominal expected dividend growth
expdivgr_stock = mu_m     +pi0 + (I_divgrm+I_pi)'*Psi*X2';
expdivgr_reit  = mu_reit  +pi0 + (I_divgr_reit+I_pi)'*Psi*X2';
expdivgr_infra = mu_infra +pi0 + (I_divgr_infra+I_pi)'*Psi*X2';

logdiv_stock(1) = 0+divgrowth_stock(1); % div normalized to 1 in 1974.Q1
logdiv_reit(1)  = 0+divgrowth_reit(1); % div normalized to 1 in 1974.Q1
logdiv_infra(1) = 0+divgrowth_infra(1); % div normalized to 1 in 1974.Q1
loggdp(1)       = 0+gdpgrowth(1);            
logdiv_stockreal(1) = 0+divgrowth_stockreal(1); % div normalized to 1 in 1974.Q1
logdiv_reitreal(1)  = 0+divgrowth_reitreal(1); % div normalized to 1 in 1974.Q1
logdiv_infrareal(1) = 0+divgrowth_infrareal(1); % div normalized to 1 in 1974.Q1
loggdpreal(1)   = 0+gdpgrowthreal(1);            
logcpi(1)       = 0+cpigrowth(1);            

logdiv_MEQ1(1) = 0+divgrMEQ1(1); % div normalized to 1 in 1974.Q1
logdiv_MEQ5(1) = 0+divgrMEQ5(1); % div normalized to 1 in 1974.Q1
logdiv_BMQ1(1) = 0+divgrBMQ1(1); % div normalized to 1 in 1974.Q1
logdiv_BMQ5(1) = 0+divgrBMQ5(1); % div normalized to 1 in 1974.Q1
logdiv_INVQ1(1) = 0+divgrINVQ1(1); % div normalized to 1 in 1974.Q1
logdiv_INVQ5(1) = 0+divgrINVQ5(1); % div normalized to 1 in 1974.Q1
logdiv_OPQ1(1) = 0+divgrOPQ1(1); % div normalized to 1 in 1974.Q1
logdiv_OPQ5(1) = 0+divgrOPQ5(1); % div normalized to 1 in 1974.Q1

logdiv_MEQ1real(1) = 0+divgrMEQ1(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_MEQ5real(1) = 0+divgrMEQ5(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_BMQ1real(1) = 0+divgrBMQ1(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_BMQ5real(1) = 0+divgrBMQ5(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_INVQ1real(1) = 0+divgrINVQ1(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_INVQ5real(1) = 0+divgrINVQ5(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_OPQ1real(1) = 0+divgrOPQ1(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1
logdiv_OPQ5real(1) = 0+divgrOPQ5(1)-cpigrowth(1); % div normalized to 1 in 1974.Q1

for t =2 :T   
   mnom(t)             = -nomshortrate(t-1) - .5*L(:,t-1)'*L(:,t-1) -L(:,t-1)'*eps_struct(t,:)';
   mreal(t)            =  mnom(t) + pi0 + I_pi'*X2(t,:)';   
   mnom_constantmpr(t) = -nomshortrate(t-1) - .5*L0'*L0                 - L0'*eps_struct(t,:)';
   mnom_bond(t)        = -nomshortrate(t-1) - .5*L(1:4,t-1)'*L(1:4,t-1) - L(1:4,t-1)'*eps_struct(t,1:4)';
   mnom_stockbond(t)   = -nomshortrate(t-1) - .5*L(1:6,t-1)'*L(1:6,t-1) - L(1:6,t-1)'*eps_struct(t,1:6)';
   
   logdiv_stock(t)     = logdiv_stock(t-1) + divgrowth_stock(t);
   logdiv_reit(t)      = logdiv_reit(t-1)  + divgrowth_reit(t);
   logdiv_infra(t)     = logdiv_infra(t-1) + divgrowth_infra(t);
   loggdp(t)           = loggdp(t-1) + gdpgrowth(t);
   
   logdiv_stockreal(t) = logdiv_stockreal(t-1) + divgrowth_stockreal(t);
   logdiv_reitreal(t)  = logdiv_reitreal(t-1)  + divgrowth_reitreal(t);
   logdiv_infrareal(t) = logdiv_infrareal(t-1) + divgrowth_infrareal(t);
   loggdpreal(t)       = loggdpreal(t-1) + gdpgrowthreal(t);
   logcpi(t)           = logcpi(t-1) + cpigrowth(t);
   
   logdiv_MEQ1(t)     = logdiv_MEQ1(t-1) + divgrMEQ1(t);
   logdiv_MEQ5(t)     = logdiv_MEQ5(t-1) + divgrMEQ5(t);
   logdiv_BMQ1(t)     = logdiv_BMQ1(t-1) + divgrBMQ1(t);
   logdiv_BMQ5(t)     = logdiv_BMQ5(t-1) + divgrBMQ5(t);
   logdiv_INVQ1(t)     = logdiv_INVQ1(t-1) + divgrINVQ1(t);
   logdiv_INVQ5(t)     = logdiv_INVQ5(t-1) + divgrINVQ5(t);
   logdiv_OPQ1(t)      = logdiv_OPQ1(t-1) + divgrOPQ1(t);
   logdiv_OPQ5(t)      = logdiv_OPQ5(t-1) + divgrOPQ5(t);

   logdiv_MEQ1real(t)     = logdiv_MEQ1real(t-1) + divgrMEQ1(t) - cpigrowth(t);
   logdiv_MEQ5real(t)     = logdiv_MEQ5real(t-1) + divgrMEQ5(t) - cpigrowth(t);
   logdiv_BMQ1real(t)     = logdiv_BMQ1real(t-1) + divgrBMQ1(t) - cpigrowth(t);
   logdiv_BMQ5real(t)     = logdiv_BMQ5real(t-1) + divgrBMQ5(t) - cpigrowth(t);
   logdiv_INVQ1real(t)    = logdiv_INVQ1real(t-1) + divgrINVQ1(t) - cpigrowth(t);
   logdiv_INVQ5real(t)    = logdiv_INVQ5real(t-1) + divgrINVQ5(t) - cpigrowth(t);
   logdiv_OPQ1real(t)     = logdiv_OPQ1real(t-1) + divgrOPQ1(t) - cpigrowth(t);
   logdiv_OPQ5real(t)     = logdiv_OPQ5real(t-1) + divgrOPQ5(t) - cpigrowth(t);

end

DIVGR = [divgrowth_stock' divgrowth_reit' divgrowth_infra' gdpgrowth' divgrMEQ1 divgrMEQ5 divgrBMQ1 divgrBMQ5 divgrINVQ1 divgrINVQ5 divgrOPQ1 divgrOPQ5];
corrcoef(DIVGR)
DIVGRreal = DIVGR-kron(cpigrowth',ones(1,cols(DIVGR)));
corrcoef([DIVGRreal cpigrowth'])

mnom_ann(1:T-3) = mnom(1:T-3)+mnom(2:T-2)+mnom(3:T-1)+mnom(4:T); 

disp('Max implied Sharpe ratio')
viol_maxSR = 100*exp(max(std(mnom)-.80,0))^4 -1;
maxannualSR = std(mnom_ann);
[maxannualSR]


zcbprices = exp(kron((Api(1:60)')',ones(1,T)) +Bpi(:,1:60)'*X2')';
divstripprices_stock = exp(kron((Am(1:60)')',ones(1,T))+Bm(:,1:60)'*X2')';
divstripprices_reit = exp(kron((A_reit(1:60)')',ones(1,T))+B_reit(:,1:60)'*X2')';
divstripprices_infra = exp(kron((A_infra(1:60)')',ones(1,T))+B_infra(:,1:60)'*X2')';
gdpstripprices = exp(kron((Ax(1:60)')',ones(1,T))+Bx(:,1:60)'*X2')';
divstripprices_small = exp(kron((A_small(1:60)')',ones(1,T))+B_small(:,1:60)'*X2')';
divstripprices_growth = exp(kron((A_growth(1:60)')',ones(1,T))+B_growth(:,1:60)'*X2')';

divstripprices_stockreal = exp(kron((Amr(1:60)')',ones(1,T))+Bmr(:,1:60)'*X2')';
divstripprices_reitreal = exp(kron((A_reitr(1:60)')',ones(1,T))+B_reitr(:,1:60)'*X2')';
divstripprices_infrareal = exp(kron((A_infrar(1:60)')',ones(1,T))+B_infrar(:,1:60)'*X2')';
gdpstrippricesreal       = exp(kron((Axr(1:60)')',ones(1,T))+Bxr(:,1:60)'*X2')';
cpistripprices           = exp(kron((Ainfl(1:60)')',ones(1,T))+Binfl(:,1:60)'*X2')';
divstripprices_smallreal = exp(kron((Amrsmall(1:60)')',ones(1,T))+Bmrsmall(:,1:60)'*X2')';
divstripprices_growthreal = exp(kron((Amrgrowth(1:60)')',ones(1,T))+Bmrgrowth(:,1:60)'*X2')';



%% figures
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

figure;
subplot(2,2,1)
plot(ttime,[reit_riskpremTV_model' reit_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
legend('model','data','Location','SouthWest')
ylabel('% per year')
title('REITs risk premium')
subplot(2,2,2)
plot(ttime,[PD_reit_model/4,exp(A0_reit+I_pd_reit'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,40]);
legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on REITs ') 
subplot(2,2,3)
plot(ttime,[infra_riskpremTV_model' infra_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Infra risk premium')
subplot(2,2,4)
plot(ttime,[PD_infra_model/4,exp(A0_infra+I_pd_infra'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,70]);
legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Infra ') 


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