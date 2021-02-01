%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Section 1: AP model

% This is the main file which estimates the prices of risk 
% There is a 5-step procedure for obtaining good starting values that is
% explained in the readme file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all; close all; clc
printfigs = 1; % flag if you want to print and save the figure files
skipestimation = 0;% flag if you want to skip estimation and just produce output

%% Load data: runs the file APloaddata which pulls in the aggregate data used in the AP model and sets up the VAR
APloaddata

%% First stage OLS estimation of VAR
load Psimatrix Psi R_psi eps Tstatt

Sigma = cov(eps); 
Sig   = chol(Sigma,'lower'); % Sig is a standard deviation matrix, Sigma is a variance matrix
eps2  = eps;
Omega = cov(X2);

%% Second stage estimation of market prices of risk
striphorizon = 3600;

global counts
counts = 0;

%% Load good starting values for the MPR parameters obtained from five initial steps

% %% If this is the first time running this file uncomment this block
% load x_guess_1 x
% LL = x;
% clear x;
% load x_guess_2 x
% LL1 = x;
% clear x;
% load  x_guess_3 x
% LL2 = x;
% clear x;
% load  x_guess_4 x
% LL3 = x;
% clear x;
% load  x_guess_5 x
% LL4 = x;
% clear x;
% 
% % constant MPRs (1-12)
% x_init(1)      = LL(1);  % L0(I_pi==1)
% x_init(2)      = LL(2);  % L0(I_gdp==1)
% x_init(3)      = LL(3);  % L0(I_y1==1)      
% x_init(4)      = LL(4);  % L0(I_yspr==1) 
% x_init(5)      = LL1(1); % L0(I_pdm==1)
% x_init(6)      = LL1(2); % L0(I_divgrm==1)
% x_init(7)      = LL2(1); % L0(I_divgr_reit==1)  
% x_init(8)      = LL2(2); % L0(I_divgr_infra==1)
% x_init(9)      = LL3(1); % L0(I_divgr_small==1)  
% x_init(10)     = LL3(2); % L0(I_divgr_growth==1)
% x_init(11)     = LL4(1); % L0(I_divgr_NR==1) 
% x_init(12)     = LL4(2); % L0(I_divgr_value==1)
% 
% x_init(13:21) = LL(5:13); 
% % The next several lines indicate which elements of L1 are non-zero in rows 1-4
% % L1(I_pi==1,I_pi==1)         = LL(5);
% % L1(I_gdp==1,I_gdp==1)       = LL(6);
% % L1(I_y1==1,I_y1==1)         = LL(7);
% % L1(I_y1==1,I_yspr==1)       = LL(8);
% % L1(I_yspr==1,I_pi==1)       = LL(9);
% % L1(I_yspr==1,I_gdp==1)      = LL(10);
% % L1(I_yspr==1,I_y1==1)       = LL(11);
% % L1(I_yspr==1,I_yspr==1)     = LL(12);
% % L1(I_yspr==1,I_pdm==1)      = LL(13);
% 
% x_init(22:35) = LL1(3:16);
% % The next several lines indicate which elements of L1 are non-zero in rows 5-6
% % L1(I_pdm==1,I_pi==1)        = LL1(3); 
% % L1(I_pdm==1,I_gdp==1)       = LL1(4); 
% % L1(I_pdm==1,I_y1==1)        = LL1(5); 
% % L1(I_pdm==1,I_yspr==1)      = LL1(6); 
% % L1(I_pdm==1,I_pdm==1)       = LL1(7); 
% % L1(I_pdm==1,I_divgrm==1)    = LL1(8); 
% % L1(I_divgrm==1,I_pi==1)     = LL1(9);
% % L1(I_divgrm==1,I_gdp==1)    = LL1(10);
% % L1(I_divgrm==1,I_y1==1)     = LL1(11);
% % L1(I_divgrm==1,I_yspr==1)   = LL1(12);
% % L1(I_divgrm==1,I_pdm==1)    = LL1(13);
% % L1(I_divgrm==1,I_divgrm==1) = LL1(14);
% % L1(I_divgrm==1,7)           = LL1(15);
% % L1(I_divgrm==1,11)          = LL1(16);
% 
% % reits & infra
% x_init(36:56) = LL2(3:23);
% % The next several lines indicate which elements of L1 are non-zero in rows 8 and 10
% % L1(I_divgr_reit==1,I_pi==1)      =LL2(3);
% % L1(I_divgr_reit==1,I_gdp==1)     =LL2(4);
% % L1(I_divgr_reit==1,I_y1==1)      =LL2(5);
% % L1(I_divgr_reit==1,I_yspr==1)    =LL2(6);
% % L1(I_divgr_reit==1,I_pdm==1)     =LL2(7);
% % L1(I_divgr_reit==1,I_divgrm==1)  =LL2(8);
% % L1(I_divgr_reit==1,I_pd_reit==1) =LL2(9);
% % L1(I_divgr_reit==1,8)            =LL2(10);
% % L1(I_divgr_reit==1,9)            =LL2(11);
% % L1(I_divgr_reit==1,11)           =LL2(12);
% % L1(I_divgr_infra==1,I_pi==1)     =LL2(13);
% % L1(I_divgr_infra==1,I_gdp==1)    =LL2(14);
% % L1(I_divgr_infra==1,I_y1==1)     =LL2(15);
% % L1(I_divgr_infra==1,I_yspr==1)   =LL2(16);
% % L1(I_divgr_infra==1,I_pdm==1)    =LL2(17);
% % L1(I_divgr_infra==1,I_divgrm==1) =LL2(18);
% % L1(I_divgr_infra==1,I_pd_reit==1)=LL2(19);
% % L1(I_divgr_infra==1,8)           =LL2(20);
% % L1(I_divgr_infra==1,9)           =LL2(21);
% % L1(I_divgr_infra==1,10)          =LL2(22);
% % L1(I_divgr_infra==1,11)          =LL2(23);
% 
% % small and growth 
% x_init(57:82) = LL3(3:28);
% % The next several lines indicate which elements of L1 are non-zero in rows 12 and 14
% % L1(I_divgr_small==1,I_pi==1)      = LL3(3);
% % L1(I_divgr_small==1,I_gdp==1)     = LL3(4);
% % L1(I_divgr_small==1,I_y1==1)      = LL3(5);
% % L1(I_divgr_small==1,I_yspr==1)    = LL3(6);
% % L1(I_divgr_small==1,I_pdm==1)     = LL3(7);
% % L1(I_divgr_small==1,I_divgrm==1)  = LL3(8);
% % L1(I_divgr_small==1,I_pd_reit==1) = LL3(9);
% % L1(I_divgr_small==1,8)            = LL3(10);
% % L1(I_divgr_small==1,9)            = LL3(11);
% % L1(I_divgr_small==1,10)           = LL3(12);
% % L1(I_divgr_small==1,11)           = LL3(13);
% % L1(I_divgr_small==1,12)           = LL3(14);
% % L1(I_divgr_growth==1,I_pi==1)     = LL3(15);
% % L1(I_divgr_growth==1,I_gdp==1)    = LL3(16);
% % L1(I_divgr_growth==1,I_y1==1)     = LL3(17);
% % L1(I_divgr_growth==1,I_yspr==1)   = LL3(18);
% % L1(I_divgr_growth==1,I_pdm==1)    = LL3(19);
% % L1(I_divgr_growth==1,I_divgrm==1) = LL3(20);
% % L1(I_divgr_growth==1,I_pd_reit==1)= LL3(21);
% % L1(I_divgr_growth==1,8)           = LL3(22);
% % L1(I_divgr_growth==1,9)           = LL3(23);
% % L1(I_divgr_growth==1,10)          = LL3(24);
% % L1(I_divgr_growth==1,11)          = LL3(25);
% % L1(I_divgr_growth==1,12)          = LL3(26);
% % L1(I_divgr_growth==1,13)          = LL3(27);
% % L1(I_divgr_growth==1,14)          = LL3(28);
% 
% % NR and value
% x_init(83:116) = LL4(3:36);
% % The next several lines indicate which elements of L1 are non-zero in rows 16 and 18
% % L1(I_divgr_NR==1,I_pi==1)          = LL4(3);
% % L1(I_divgr_NR==1,I_gdp==1)         = LL4(4);
% % L1(I_divgr_NR==1,I_y1==1)          = LL4(5);
% % L1(I_divgr_NR==1,I_yspr==1)        = LL4(6);
% % L1(I_divgr_NR==1,I_pdm==1)         = LL4(7);
% % L1(I_divgr_NR==1,I_divgrm==1)      = LL4(8);
% % L1(I_divgr_NR==1,I_pd_reit==1)     = LL4(9);
% % L1(I_divgr_NR==1,8)                = LL4(10);
% % L1(I_divgr_NR==1,9)                = LL4(11);
% % L1(I_divgr_NR==1,10)               = LL4(12);
% % L1(I_divgr_NR==1,11)               = LL4(13);
% % L1(I_divgr_NR==1,12)               = LL4(14);
% % L1(I_divgr_NR==1,13)               = LL4(15);
% % L1(I_divgr_NR==1,14)               = LL4(16);
% % L1(I_divgr_NR==1,15)               = LL4(17);
% % L1(I_divgr_NR==1,16)               = LL4(18);
% % L1(I_divgr_value==1,I_pi==1)          = LL4(19);
% % L1(I_divgr_value==1,I_gdp==1)         = LL4(20);
% % L1(I_divgr_value==1,I_y1==1)          = LL4(21);
% % L1(I_divgr_value==1,I_yspr==1)        = LL4(22);
% % L1(I_divgr_value==1,I_pdm==1)         = LL4(23);
% % L1(I_divgr_value==1,I_divgrm==1)      = LL4(24);
% % L1(I_divgr_value==1,I_pd_reit==1)     = LL4(25);
% % L1(I_divgr_value==1,8)                = LL4(26);
% % L1(I_divgr_value==1,9)                = LL4(27);
% % L1(I_divgr_value==1,10)               = LL4(28);
% % L1(I_divgr_value==1,11)               = LL4(29);
% % L1(I_divgr_value==1,12)               = LL4(30);
% % L1(I_divgr_value==1,13)               = LL4(31);
% % L1(I_divgr_value==1,14)               = LL4(32);
% % L1(I_divgr_value==1,15)               = LL4(33);
% % L1(I_divgr_value==1,16)               = LL4(34);
% % L1(I_divgr_value==1,17)               = LL4(35);
% % L1(I_divgr_value==1,18)               = LL4(36);
% x_init = x_init';

% If this is the second or higher time running this file, uncomment the
% next two lines
load x_guess_main x
x_init = x;

%% Estimate the market prices of risk
if skipestimation == 0
    disp('Starting MPR estimation')
    options = optimset('DiffMinChange',.001,'TolX',0.001,'TolFun',0.001,'MaxIter',500,'MaxFunEval',500);
    [x,fval,exitflag,output] = fminsearch('solvenul_main',x_init,options,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,I_pd_reit,I_divgr_reit,I_pd_infra,I_divgr_infra,I_pd_small,I_divgr_small,I_pd_growth,I_divgr_growth,I_pd_NR,I_divgr_NR,I_pd_value,I_divgr_value,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,A0_reit,k1_reit,r0_reit,mu_reit,A0_infra,k1_infra,r0_infra,mu_infra,A0_small,k1_small,r0_small,mu_small,A0_growth,k1_growth,r0_growth,mu_growth,PDm_strip_8q_data,sharestrip_8q_data,A0_NR,k1_NR,r0_NR,mu_NR,A0_value,k1_value,r0_value,mu_value,taureal,yielddatareal);
    save x_guess_main x
    disp('Finished Estimation - producing results')
else
    disp('Skipping estimation - producing results')
end

%% Process the ouput from the optimization
L0 = zeros(N,1);
L1 = zeros(N,N);

i=1;

% constant MPRs (1-12)
L0(I_pi==1)            = x(i);i=i+1;
L0(I_gdp==1)           = x(i);i=i+1;
L0(I_y1==1)            = x(i);i=i+1;
L0(I_yspr==1)          = x(i);i=i+1;
L0(I_pdm==1)           = x(i);i=i+1;
L0(I_divgrm==1)        = x(i);i=i+1;
L0(I_divgr_reit==1)    = x(i);i=i+1;
L0(I_divgr_infra==1)   = x(i);i=i+1;
L0(I_divgr_small==1)   = x(i);i=i+1;
L0(I_divgr_growth==1)  = x(i);i=i+1;
L0(I_divgr_NR==1)      = x(i);i=i+1;
L0(I_divgr_value==1)   = x(i);i=i+1;

% TV MPR
% bond block (13-21)
L1(I_pi==1,I_pi==1)         = x(i);i=i+1;
L1(I_gdp==1,I_gdp==1)       = x(i);i=i+1;
L1(I_y1==1,I_y1==1)         = x(i);i=i+1;
L1(I_y1==1,I_yspr==1)       = x(i);i=i+1;
L1(I_yspr==1,I_pi==1)       = x(i);i=i+1;
L1(I_yspr==1,I_gdp==1)      = x(i);i=i+1;
L1(I_yspr==1,I_y1==1)       = x(i);i=i+1;
L1(I_yspr==1,I_yspr==1)     = x(i);i=i+1;
L1(I_yspr==1,I_pdm==1)     = x(i);i=i+1;


% stocks (22-35)
L1(I_pdm==1,I_pi==1)             = x(i);i=i+1;
L1(I_pdm==1,I_gdp==1)            = x(i);i=i+1;
L1(I_pdm==1,I_y1==1)             = x(i);i=i+1;
L1(I_pdm==1,I_yspr==1)           = x(i);i=i+1;
L1(I_pdm==1,I_pdm==1)            = x(i);i=i+1;
L1(I_pdm==1,I_divgrm==1)         = x(i);i=i+1;

L1(I_divgrm==1,I_pi==1)          = x(i);i=i+1;
L1(I_divgrm==1,I_gdp==1)         = x(i);i=i+1;
L1(I_divgrm==1,I_y1==1)          = x(i);i=i+1;
L1(I_divgrm==1,I_yspr==1)        = x(i);i=i+1;
L1(I_divgrm==1,I_pdm==1)         = x(i);i=i+1;
L1(I_divgrm==1,I_divgrm==1)      = x(i);i=i+1;
L1(I_divgrm==1,I_pd_reit==1)     = x(i);i=i+1;
L1(I_divgrm==1,11)               = x(i);i=i+1;

% reits (36-45)
L1(I_divgr_reit==1,I_pi==1)      = x(i);i=i+1;
L1(I_divgr_reit==1,I_gdp==1)     = x(i);i=i+1;
L1(I_divgr_reit==1,I_y1==1)      = x(i);i=i+1;
L1(I_divgr_reit==1,I_yspr==1)    = x(i);i=i+1;
L1(I_divgr_reit==1,I_pdm==1)     = x(i);i=i+1;
L1(I_divgr_reit==1,I_divgrm==1)  = x(i);i=i+1;
L1(I_divgr_reit==1,I_pd_reit==1) = x(i);i=i+1;
L1(I_divgr_reit==1,8)            = x(i);i=i+1;
L1(I_divgr_reit==1,9)            = x(i);i=i+1;
L1(I_divgr_reit==1,11)           = x(i);i=i+1;

% infra - row10 (46-56)
L1(I_divgr_infra==1,I_pi==1)     = x(i);i=i+1;
L1(I_divgr_infra==1,I_gdp==1)    = x(i);i=i+1;
L1(I_divgr_infra==1,I_y1==1)     = x(i);i=i+1;
L1(I_divgr_infra==1,I_yspr==1)   = x(i);i=i+1;
L1(I_divgr_infra==1,I_pdm==1)    = x(i);i=i+1;
L1(I_divgr_infra==1,I_divgrm==1) = x(i);i=i+1;
L1(I_divgr_infra==1,I_pd_reit==1)= x(i);i=i+1;
L1(I_divgr_infra==1,8)           = x(i);i=i+1;
L1(I_divgr_infra==1,9)           = x(i);i=i+1;
L1(I_divgr_infra==1,10)          = x(i);i=i+1;
L1(I_divgr_infra==1,11)          = x(i);i=i+1;

% small - row12 (57-68)
L1(I_divgr_small==1,I_pi==1)          = x(i); i = i+1;
L1(I_divgr_small==1,I_gdp==1)         = x(i); i = i+1;
L1(I_divgr_small==1,I_y1==1)          = x(i);  i = i+1;
L1(I_divgr_small==1,I_yspr==1)        = x(i);  i = i+1;
L1(I_divgr_small==1,I_pdm==1)         = x(i);  i = i+1;
L1(I_divgr_small==1,I_divgrm==1)      = x(i);  i = i+1;
L1(I_divgr_small==1,I_pd_reit==1)     = x(i);  i = i+1;
L1(I_divgr_small==1,8)                = x(i);  i = i+1;
L1(I_divgr_small==1,9)                = x(i);  i = i+1;
L1(I_divgr_small==1,10)               = x(i);  i = i+1;
L1(I_divgr_small==1,11)               = x(i);  i = i+1;
L1(I_divgr_small==1,12)               = x(i);  i = i+1;

% growth - row14 (69-82)
L1(I_divgr_growth==1,I_pi==1)          = x(i); i = i+1;
L1(I_divgr_growth==1,I_gdp==1)         = x(i); i = i+1;
L1(I_divgr_growth==1,I_y1==1)          = x(i);  i = i+1;
L1(I_divgr_growth==1,I_yspr==1)        = x(i);  i = i+1;
L1(I_divgr_growth==1,I_pdm==1)         = x(i);  i = i+1;
L1(I_divgr_growth==1,I_divgrm==1)      = x(i);  i = i+1;
L1(I_divgr_growth==1,I_pd_reit==1)     = x(i);  i = i+1;
L1(I_divgr_growth==1,I_divgr_reit==1)  = x(i);  i = i+1;
L1(I_divgr_growth==1,I_pd_infra==1)    = x(i);  i = i+1;
L1(I_divgr_growth==1,I_divgr_infra==1) = x(i);  i = i+1;
L1(I_divgr_growth==1,11)               = x(i);  i = i+1;
L1(I_divgr_growth==1,12)               = x(i);  i = i+1;
L1(I_divgr_growth==1,13)               = x(i);  i = i+1;
L1(I_divgr_growth==1,14)               = x(i);  i = i+1;

% NR - row 16 (83-98)
L1(I_divgr_NR==1,I_pi==1)          = x(i);  i = i+1;
L1(I_divgr_NR==1,I_gdp==1)         = x(i);  i = i+1;
L1(I_divgr_NR==1,I_y1==1)          = x(i);  i = i+1;
L1(I_divgr_NR==1,I_yspr==1)        = x(i);  i = i+1;
L1(I_divgr_NR==1,I_pdm==1)         = x(i);  i = i+1;
L1(I_divgr_NR==1,I_divgrm==1)      = x(i);  i = i+1;
L1(I_divgr_NR==1,I_pd_reit==1)     = x(i);  i = i+1;
L1(I_divgr_NR==1,I_divgr_reit==1)  = x(i);  i = i+1;
L1(I_divgr_NR==1,I_pd_infra==1)    = x(i);  i = i+1;
L1(I_divgr_NR==1,I_divgr_infra==1) = x(i);  i = i+1;
L1(I_divgr_NR==1,11)               = x(i);  i = i+1;
L1(I_divgr_NR==1,12)               = x(i);  i = i+1;
L1(I_divgr_NR==1,13)               = x(i);  i = i+1;
L1(I_divgr_NR==1,14)               = x(i);  i = i+1;
L1(I_divgr_NR==1,15)               = x(i);  i = i+1;
L1(I_divgr_NR==1,16)               = x(i);  i = i+1;

% value - row 18 (99-116)
L1(I_divgr_value==1,I_pi==1)          = x(i);  i = i+1;
L1(I_divgr_value==1,I_gdp==1)         = x(i);  i = i+1;
L1(I_divgr_value==1,I_y1==1)          = x(i);  i = i+1;
L1(I_divgr_value==1,I_yspr==1)        = x(i);  i = i+1;
L1(I_divgr_value==1,I_pdm==1)         = x(i);  i = i+1;
L1(I_divgr_value==1,I_divgrm==1)      = x(i);  i = i+1;
L1(I_divgr_value==1,I_pd_reit==1)     = x(i);  i = i+1;
L1(I_divgr_value==1,I_divgr_reit==1)  = x(i);  i = i+1;
L1(I_divgr_value==1,I_pd_infra==1)    = x(i);  i = i+1;
L1(I_divgr_value==1,I_divgr_infra==1) = x(i);  i = i+1;
L1(I_divgr_value==1,11)               = x(i);  i = i+1;
L1(I_divgr_value==1,12)               = x(i);  i = i+1;
L1(I_divgr_value==1,13)               = x(i);  i = i+1;
L1(I_divgr_value==1,14)               = x(i);  i = i+1;
L1(I_divgr_value==1,15)               = x(i);  i = i+1;
L1(I_divgr_value==1,16)               = x(i);  i = i+1;
L1(I_divgr_value==1,17)               = x(i);  i = i+1;
L1(I_divgr_value==1,18)               = x(i); 

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

   
Psi_j = zeros(N);
varcapgain1 = zeros(N);
varcapgain2 = zeros(N);
adj_stock_j  = zeros(1,T);
adj_reit_j   = zeros(1,T);
adj_infra_j  = zeros(1,T);
adj_small_j  = zeros(1,T);
adj_growth_j = zeros(1,T);
adj_NR_j     = zeros(1,T);
adj_value_j  = zeros(1,T);

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
   
    nombondriskprem(j)            = 400*Bpi(:,j)'*Sig*L0;
    realbondriskprem(j)           = 400*B(:,j)'*Sig*L0;
    equitydivstripriskprem(j)     = 400*(I_divgrm+I_pi+Bm(:,j))'*Sig*L0;
    reitdivstripriskprem(j)       = 400*(I_divgr_reit+I_pi+B_reit(:,j))'*Sig*L0;
    infradivstripriskprem(j)      = 400*(I_divgr_infra+I_pi+B_infra(:,j))'*Sig*L0;
    smalldivstripriskprem(j)      = 400*(I_divgr_small+B_small(:,j))'*Sig*L0;
    growthdivstripriskprem(j)     = 400*(I_divgr_growth+B_growth(:,j))'*Sig*L0;
    NRdivstripriskprem(j)         = 400*(I_divgr_NR+B_NR(:,j))'*Sig*L0;
    valuedivstripriskprem(j)      = 400*(I_divgr_value+B_value(:,j))'*Sig*L0;
   
    if j<65
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


%% Report model diagnostics
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
[L1(find(I_divgr_growth==1),:)',((cc_growth-aa_growth(1:13)*L1(1:13,:))/aa_growth(14))']
disp('NR risk premium diagnostics')
[L0(find(I_divgr_NR==1))  , (((r0_NR + pi0 - y0nom_1 +0.5*(aa_NR)*(aa_NR)')-aa_NR(1:15)*L0(1:15))/aa_NR(16))]
[L1(find(I_divgr_NR==1),:)',((cc_NR-aa_NR(1:15)*L1(1:15,:))/aa_NR(16))']
disp('value risk premium diagnostics')
[L0(find(I_divgr_value==1))  , (((r0_value + pi0 - y0nom_1 +0.5*(aa_value)*(aa_value)')-aa_value(1:17)*L0(1:17))/aa_value(18))]
[L1(find(I_divgr_value==1),:)',((cc_value-aa_value(1:17)*L1(1:17,:))/aa_value(18))']


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
eps_struct = Sig\eps2';               % eps2 are VAR resids, they have covariance Sigma, eps_struct are theoretical shocks with covariance matrix Identity 
eps_struct = eps_struct';
eps_struct = [zeros(1,N);eps_struct];   % N by T; shocks in period 1 set to zero
mnom(1)  = -nomshortrate(1) - .5*L(:,1)'*L(:,1)-L(:,1)'*eps_struct(1,:)';
mreal(1) = mnom(1) +  pi0 + I_pi'*X2(1,:)';
mnom_constantmpr(1) = -nomshortrate(1) - .5*L0'*L0 -L0'*eps_struct(1,:)';

% Nominal realized dividend growth
divgrowth_stock = mu_m    +pi0 + (I_divgrm+I_pi)'*X2';
divgrowth_reit  = mu_reit +pi0 + (I_divgr_reit+I_pi)'*X2';
divgrowth_infra = mu_infra+pi0 + (I_divgr_infra+I_pi)'*X2';
divgrowth_small = mu_small+pi0 + (I_divgr_small+I_pi)'*X2';
divgrowth_growth = mu_growth+pi0 + (I_divgr_growth+I_pi)'*X2';
divgrowth_NR    = mu_NR    +pi0 + (I_divgr_NR+I_pi)'*X2';
divgrowth_value = mu_value+pi0 + (I_divgr_value+I_pi)'*X2';

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

logdiv_NR(1) = 0+divgrowth_NR(1); % div normalized to 1 in 1974.Q1

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

   logdiv_NR(t)        = logdiv_NR(t-1) + divgrowth_NR(t);

   logdiv_MEQ1real(t)     = logdiv_MEQ1real(t-1) + divgrMEQ1(t) - cpigrowth(t);
   logdiv_MEQ5real(t)     = logdiv_MEQ5real(t-1) + divgrMEQ5(t) - cpigrowth(t);
   logdiv_BMQ1real(t)     = logdiv_BMQ1real(t-1) + divgrBMQ1(t) - cpigrowth(t);
   logdiv_BMQ5real(t)     = logdiv_BMQ5real(t-1) + divgrBMQ5(t) - cpigrowth(t);
   logdiv_INVQ1real(t)    = logdiv_INVQ1real(t-1) + divgrINVQ1(t) - cpigrowth(t);
   logdiv_INVQ5real(t)    = logdiv_INVQ5real(t-1) + divgrINVQ5(t) - cpigrowth(t);
   logdiv_OPQ1real(t)     = logdiv_OPQ1real(t-1) + divgrOPQ1(t) - cpigrowth(t);
   logdiv_OPQ5real(t)     = logdiv_OPQ5real(t-1) + divgrOPQ5(t) - cpigrowth(t);

end

DIVGR = [divgrowth_stock' divgrowth_reit' divgrowth_infra' gdpgrowth' divgrMEQ1 divgrMEQ5 divgrBMQ1 divgrBMQ5 divgrINVQ1 divgrINVQ5 divgrOPQ1 divgrOPQ5 divgrNR];
% corrcoef(DIVGR)
DIVGRreal = DIVGR-kron(cpigrowth',ones(1,cols(DIVGR)));
% corrcoef([DIVGRreal cpigrowth'])

mnom_ann(1:T-3) = mnom(1:T-3)+mnom(2:T-2)+mnom(3:T-1)+mnom(4:T); 

disp('Max implied Sharpe ratio')
viol_maxSR = 100*exp(max(std(mnom)-.80,0))^4 -1;
maxannualSR = std(mnom_ann);
[maxannualSR]

StripHor = 64; 
zcbprices                 = exp(kron((Api(1:StripHor)')',ones(1,T)) +Bpi(:,1:StripHor)'*X2')';
divstripprices_stock      = exp(kron((Am(1:StripHor)')',ones(1,T))+Bm(:,1:StripHor)'*X2')';
divstripprices_reit       = exp(kron((A_reit(1:StripHor)')',ones(1,T))+B_reit(:,1:StripHor)'*X2')';
divstripprices_infra      = exp(kron((A_infra(1:StripHor)')',ones(1,T))+B_infra(:,1:StripHor)'*X2')';
divstripprices_small      = exp(kron((A_small(1:StripHor)')',ones(1,T))+B_small(:,1:StripHor)'*X2')';
divstripprices_growth     = exp(kron((A_growth(1:StripHor)')',ones(1,T))+B_growth(:,1:StripHor)'*X2')';
divstripprices_NR         = exp(kron((A_NR(1:StripHor)')',ones(1,T))+B_NR(:,1:StripHor)'*X2')';
divstripprices_value      = exp(kron((A_value(1:StripHor)')',ones(1,T))+B_value(:,1:StripHor)'*X2')';

varstock = (I_divgrm+I_pi+k1m*I_pdm)'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
varbond = Bpi(:,20)'*Sigma*Bpi(:,20);
CovM = zeros(StripHor,8);CovB = zeros(StripHor,8);
CovM(1:StripHor,1) = Bpi(:,1:StripHor)'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,2) = (I_divgrm       + I_pi + Bm(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,3) = (I_divgr_reit   + I_pi + B_reit(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,4) = (I_divgr_infra  + I_pi + B_infra(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,5) = (I_divgr_small  + I_pi + B_small(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,6) = (I_divgr_growth + I_pi + B_growth(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,7) = (I_divgr_NR     + I_pi + B_NR(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);
CovM(1:StripHor,8) = (I_divgr_value  + I_pi + B_value(:,1:StripHor))'*Sigma*(I_divgrm+I_pi+k1m*I_pdm);

CovB(1:StripHor,1) = Bpi(:,1:StripHor)'*Sigma*Bpi(:,20);
CovB(1:StripHor,2) = (I_divgrm        + I_pi + Bm(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,3) = (I_divgr_reit    + I_pi + B_reit(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,4) = (I_divgr_infra   + I_pi + B_infra(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,5) = (I_divgr_small   + I_pi + B_small(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,6) = (I_divgr_growth  + I_pi + B_growth(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,7) = (I_divgr_NR      + I_pi + B_NR(:,1:StripHor))'*Sigma*Bpi(:,20);
CovB(1:StripHor,8) = (I_divgr_value   + I_pi + B_value(:,1:StripHor))'*Sigma*Bpi(:,20);

zcbprices6080  = exp(kron((Api(60:80)')',ones(1,T)) +Bpi(:,60:80)'*X2')';
fwdrates60 = kron(zcbprices6080(:,1),ones(1,20))./zcbprices6080(:,2:21) -1;
discrates60 = zcbprices6080(:,2:21)./kron(zcbprices6080(:,1),ones(1,20)) ;

pdm       = A0m+I_pdm'*X2'; 
PDm       = exp(A0m+I_pdm'*X2')';
PD_reit   = exp(A0_reit+I_pd_reit'*X2')'; 
PD_infra  = exp(A0_infra+I_pd_infra'*X2')'; 
PD_small  = exp(A0_small+I_pd_small'*X2')'; 
PD_growth = exp(A0_growth+I_pd_growth'*X2')'; 
PD_NR     = exp(A0_NR+I_pd_NR'*X2')'; 
PD_value  = exp(A0_value+I_pd_value'*X2')'; 


%% Plot output figures
ttime=[1974:0.25:2019+0.75];

fontfig = 14; % use 24 for slides and 14 for paper

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(2,2,1)
plot(ttime,[400*(-Api(1)/1 - Bpi(:,1)'/1*X2')'  400*ynom1],'LineWidth',1.4)
xlim([1973,2020]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 1-qtr bond')
subplot(2,2,2)
plot(ttime,[400*(-Api(4)/4 - Bpi(:,4)'/4*X2')'  400*ynom4],'LineWidth',1.4)
xlim([1973,2020]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 1-yr bond')
subplot(2,2,3)
plot(ttime,[400*(-Api(20)/20 - Bpi(:,20)'/20*X2')' 400*ynom20],'LineWidth',1.4)
xlim([1973,2020]);ylim([0,15]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Nom. yield 5-yr bond')
subplot(2,2,4)
plot(ttime,[400*(-Api(40)'./40 -Bpi(:,40)'./40*X2')',  400*ynom40],'LineWidth',1.4)
xlim([1973,2020]);ylim([0,15]);
legend('model','data','Location','NorthEast')
title('Nom. yield 10-yr bond') 
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('Yields_main')
    print('Yields_main','-depsc')
    movefile('Yields_main.eps', '..\..\..\Figures');
end


figure('units','normalized','outerposition',[0 0 1 1]);
subplot(2,2,1)
plot(ttime,[400*(-A(20)/20 - B(:,20)'/20*X2')'  400*yreal20],'LineWidth',1.4)
xlim([2000,2020]);ylim([-2,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield 5-yr bond')
subplot(2,2,2)
plot(ttime,[400*(-A(28)/28 - B(:,28)'/28*X2')'  400*yreal28],'LineWidth',1.4)
xlim([2000,2020]);ylim([-2,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield 7-yr bond')
subplot(2,2,3)
plot(ttime,[400*(-A(40)/40 - B(:,40)'/40*X2')' 400*yreal40],'LineWidth',1.4)
xlim([2000,2020]);ylim([-2,5]);
legend('model','data','Location','NorthEast')
ylabel('% per year')
title('Real yield 10-yr bond')
subplot(2,2,4)
plot(ttime,[400*(-A(80)'./80 -B(:,80)'./80*X2')',  400*yreal80],'LineWidth',1.4)
xlim([2000,2020]);ylim([-2,5]);
legend('model','data','Location','NorthEast')
title('Real yield 20-yr bond') 
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('Yieldsreal_main')
    print('Yieldsreal_main','-depsc')
    movefile('Yieldsreal_main.eps', '..\..\..\Figures');
end



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
xlim([1973,2020]);ylim([-1.5,5.5]);
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
xlim([1973,2020]);ylim([-3.5,15.5]);
title('Decomposing 5-yr nom. bond yield')
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('BRPdecomp_main')
    print('BRPdecomp_main','-depsc')
    movefile('BRPdecomp_main.eps', '..\..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(4,2,1)
plot(ttime,[equityriskpremTV_model' equityriskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Equity risk premium')
subplot(4,2,2)
plot(ttime,[PDm_model/4,exp(A0m+I_pdm'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,100]);
%legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Equity ') 
subplot(4,2,3)
plot(ttime,[small_riskpremTV_model' small_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-20,50]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Small stock risk premium')
subplot(4,2,4)
plot(ttime,[PD_small_model/4,exp(A0_small+I_pd_small'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,200]);
%legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Small') 
subplot(4,2,5)
plot(ttime,[growth_riskpremTV_model' growth_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-20,40]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Growth risk premium')
subplot(4,2,6)
plot(ttime,[PD_growth_model/4,exp(A0_growth+I_pd_growth'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,150]);
%legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Growth') 
subplot(4,2,7)
plot(ttime,[value_riskpremTV_model' value_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-20,40]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Value risk premium')
subplot(4,2,8)
plot(ttime,[PD_value_model/4,exp(A0_value+I_pd_value'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,110]);
legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Value') 
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('ERPpart1_main')
    print('ERPpart1_main','-depsc')
    movefile('ERPpart1_main.eps', '..\..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(3,2,1)
plot(ttime,[reit_riskpremTV_model' reit_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('REITs risk premium')
subplot(3,2,2)
plot(ttime,[PD_reit_model/4,exp(A0_reit+I_pd_reit'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,40]);
%legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on REITs ') 
subplot(3,2,3)
plot(ttime,[infra_riskpremTV_model' infra_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-15,30]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('Infra risk premium')
subplot(3,2,4)
plot(ttime,[PD_infra_model/4,exp(A0_infra+I_pd_infra'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,60]);
%legend('model','data','Location','NorthWest')
title('Price-Dividend Ratio on Infra ') 
subplot(3,2,5)
plot(ttime,[NR_riskpremTV_model' NR_riskpremTV_data'],'LineWidth',1.2)
xlim([1973,2020]);ylim([-20,40]);
%legend('model','data','Location','SouthWest')
ylabel('% per year')
title('NR risk premium')
subplot(3,2,6)
plot(ttime,[PD_NR_model/4,exp(A0_NR+I_pd_NR'*X2'-log(4))'],'LineWidth',1.2)
xlim([1973,2020]);ylim([0,35]);
legend('model','data','Location','NorthEast')
title('Price-Dividend Ratio on NR') 
if printfigs==1
    savefig('ERPpart2_main')
    print('ERPpart2_main','-depsc')
    movefile('ERPpart2_main.eps', '..\..\..\Figures');
end

% Plot aggregate stock market dividend strip moments
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(1,2,1)
plot(ttime,[PDm_strip_8q PDm_strip_8q_data],'LineWidth',1.5)
xlim([1973,2020]);ylim([0,10]);
title('8-quarter Cum. Dividend Strip PD')
legend('model','data','Location','SouthWest')
subplot(1,2,2)
plot(ttime,[sharestrip_8q sharestrip_8q_data],'LineWidth',1.5)
xlim([1973,2020]);ylim([0,.10]);
title('8-quarter Cum. Dividend Strip Share')
legend('model','data','Location','NorthEast')
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('StripsPDshare_main')
    print('StripsPDshare_main','-depsc')
    movefile('StripsPDshare_main.eps', '..\..\..\Figures');
end

% Save ZCB prices and dividend strip prices (PD ratios) for maturities 1 to 180 months
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(4,2,1)
plot(ttime,zcbprices(:,[4,20,40]),'LineWidth',1.2);title('Zero-coupon Bond Prices')
xlim([1973,2020]);ylim([0,1.5]);
subplot(4,2,2)
plot(ttime,divstripprices_stock(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Stock Market')
xlim([1973,2020]);ylim([0,1.5]);
subplot(4,2,3)
plot(ttime,divstripprices_small(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Small')
xlim([1973,2020]);ylim([0,2]);
subplot(4,2,4)
plot(ttime,divstripprices_growth(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Growth')
xlim([1973,2020]);ylim([0,1.5]);
subplot(4,2,5)
plot(ttime,divstripprices_value(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Value')
xlim([1973,2020]);ylim([0,2]);
subplot(4,2,6)
plot(ttime,divstripprices_reit(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - REITS')
xlim([1973,2020]);ylim([0,1.5]);
subplot(4,2,7)
plot(ttime,divstripprices_infra(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Infrastructure')
xlim([1973,2020]);ylim([0,1.5]);
legend('One-year','Five-year','Ten-year','Location','NorthWest');
subplot(4,2,8)
plot(ttime,divstripprices_NR(:,[4,20,40]),'LineWidth',1.2);title('Dividend Strip Prices - Nat. Res.')
xlim([1973,2020]);ylim([0,1.5]);
set(findall(gcf,'-property','FontSize'),'FontSize',14)
if printfigs==1
    savefig('Stripprices_main')
    print('Stripprices_main','-depsc')
    movefile('Stripprices_main.eps', '..\..\..\Figures');
end

% Plot strip risk premia across maturities
maxmat = StripHor;
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(4,2,1)
plot([1:1:maxmat],nombondriskprem(1:maxmat),'LineWidth',2,'color','black')
xlim([0,maxmat])
ylim([0,10])
title('Average zero-coupon bond risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,2)
plot([1:1:maxmat],[equitydivstripriskprem(1:maxmat)',divfutureriskprem_model_uncond(1:maxmat),divfutureriskprem_model_short(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,15])
title('Average Market div strip risk premium')
legend('Spot rp','Futures rp','Futures rp 2003-2014','location','SouthEast') 
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,3)
plot([1:1:maxmat],[smalldivstripriskprem(1:maxmat)', divfutureriskprem_small_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([-10,15])
title('Average Small div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,4)
plot([1:1:maxmat],[growthdivstripriskprem(1:maxmat)', divfutureriskprem_growth_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,15])
title('Average Growth div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,5)
plot([1:1:maxmat],[valuedivstripriskprem(1:maxmat)', divfutureriskprem_value_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([-5,20])
title('Average Value div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,6)
plot([1:1:maxmat],[reitdivstripriskprem(1:maxmat)', divfutureriskprem_reit_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,15])
title('Average REIT div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,7)
plot([1:1:maxmat],[infradivstripriskprem(1:maxmat)', divfutureriskprem_infra_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,15])
title('Average Infra div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
subplot(4,2,8)
plot([1:1:maxmat],[NRdivstripriskprem(1:maxmat)', divfutureriskprem_NR_model_uncond(1:maxmat)],'LineWidth',2)
xlim([0,maxmat])
ylim([0,15])
title('Average NR div strip risk premium')
ylabel('percent per annum')
xlabel('maturity in quarters')
legend('Spot risk premium','Futures risk premium','Location','SouthEast')
set(findall(gcf,'-property','FontSize'),'FontSize',11)
if printfigs==1
    savefig('Stripriskpremia_main')
    print('Stripriskpremia_main','-depsc')
    movefile('Stripriskpremia_main.eps', '..\..\..\Figures');
end

% Plot strip expected returns over time at 5-year maturity
figure('units','normalized','outerposition',[0 0 1 1]);
subplot(4,2,1);
plot(ttime, 400*((1+stripexpret_bond(:,20)).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([0,15]);
title('Exp ret 5-yr Bond')
subplot(4,2,2);
plot(ttime, 400*((1+[stripexpret_stock(:,20) stripexpret_capgain_stock(:,20)]).^(1/20) -1),'LineWidth',2)
legend('5yr div strip','5yr gain strip','location','west')
xlim([1973 2020]);ylim([-5,25]);
title('Exp ret Stock Mkt Strip')
subplot(4,2,3);
plot(ttime, 400*((1+[stripexpret_small(:,20) stripexpret_capgain_small(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([-10,30]);
title('Exp ret Small Strip')
subplot(4,2,4);
plot(ttime, 400*((1+[stripexpret_growth(:,20) stripexpret_capgain_growth(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([-10,30]);
title('Exp ret Growth Strip')
subplot(4,2,5);
plot(ttime, 400*((1+[stripexpret_value(:,20) stripexpret_capgain_value(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([-10,30]);
title('Exp ret Value Strip')
subplot(4,2,6);
plot(ttime, 400*((1+[stripexpret_reit(:,20) stripexpret_capgain_reit(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([-10,30]);
title('Exp ret REIT Strip')
subplot(4,2,7);
plot(ttime, 400*((1+[stripexpret_infra(:,20) stripexpret_capgain_infra(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([0,25]);
title('Exp ret Infrastructure Strip')
subplot(4,2,8);
plot(ttime, 400*((1+[stripexpret_NR(:,20) stripexpret_capgain_NR(:,20)]).^(1/20) -1),'LineWidth',2)
xlim([1973 2020]);ylim([0,25]);
title('Exp ret NR Strip')
set(findall(gcf,'-property','FontSize'),'FontSize',18)
if printfigs==1
    savefig('Expretstrips_main')
    print('Expretstrips_main','-depsc')
    movefile('Expretstrips_main.eps', '..\..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
subplot(6,2,1)
plot(ttime,L(1,:));
title('MPR for inflation')
subplot(6,2,2)
plot(ttime,L(2,:));
title('MPR for GDP')
subplot(6,2,3)
plot(ttime,L(3,:));
title('MPR for short rate')
subplot(6,2,4)
plot(ttime,L(4,:));
title('MPR for slope')
subplot(6,2,5)
plot(ttime,L(5,:));
title('MPR for pdm')
subplot(6,2,6)
plot(ttime,L(6,:));
title('MPR for divgrm')
subplot(6,2,7)
plot(ttime,L(8,:));
title('MPR for divgr reit')
subplot(6,2,8)
plot(ttime,L(10,:));
title('MPR for divgr infra')
subplot(6,2,9)
plot(ttime,L(12,:));
title('MPR for divgr small')
subplot(6,2,10)
plot(ttime,L(14,:));
title('MPR for divgr growth')
subplot(6,2,11)
plot(ttime,L(16,:));
title('MPR for divgr NR')
subplot(6,2,12)
plot(ttime,L(18,:));
title('MPR for divgr value')
set(findall(gcf,'-property','FontSize'),'FontSize',12)
if printfigs==1
    savefig('L_main')
    print('L_main','-depsc')
    movefile('L_main.eps', '..\..\..\Figures');
end

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
if printfigs==1
    savefig('Mm_main')
    print('Mm_main','-depsc')
    movefile('Mm_main.eps', '..\..\..\Figures');
end


% save some AP model outputs to be read directly into the latex paper file
if printfigs==1
    matrix2latex(Psi,'Psimat_main.tex', 'alignment', 'c', 'format', '%-3.2f','size','footnotesize');
    movefile('Psimat_main.tex', '..\..\..\Figures');
    matrix2latex(100*Sig,'Sigmat_main.tex', 'alignment', 'c', 'format', '%-3.2f','size','footnotesize');
    movefile('Sigmat_main.tex', '..\..\..\Figures');
    matrix2latex(L0','L0mat_main.tex', 'alignment', 'c', 'format', '%-3.2f','size','footnotesize');
    movefile('L0mat_main.tex', '..\..\..\Figures');
    matrix2latex(L1,'L1mat_main.tex', 'alignment', 'c', 'format', '%-3.1f','size','footnotesize');
    movefile('L1mat_main.tex', '..\..\..\Figures');

    fID = fopen('../../../Figures/TableLambda_main.tex','w'); % create the tex file, and write on it 
    fprintf(fID,'\\newcommand{\\LCa}{%4.2f}\n',L0(1))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCb}{%4.2f}\n',L0(2))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCc}{%4.2f}\n',L0(3))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCd}{%4.2f}\n',L0(4))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCe}{%4.2f}\n',L0(5))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCf}{%4.2f}\n',L0(6))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCg}{%4.2f}\n',L0(8))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCh}{%4.2f}\n',L0(10))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCi}{%4.2f}\n',L0(12))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LCj}{%4.2f}\n',L0(14))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LTaa}{%4.2f}\n',L1(1,1))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LTbb}{%4.2f}\n',L1(2,2))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LTcc}{%4.2f}\n',L1(3,3))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\LTcd}{%4.2f}\n',L1(3,4))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Fiveyrnomavg}{%4.1f}\n',400*mean(ynom20_model))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Fiveyrrealavg}{%4.1f}\n',400*mean(yreal20_model))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Fiveyrexpinfavg}{%4.1f}\n',400*mean(expinfl20))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Fiveyrinflrpavg}{%4.1f}\n',400*mean(inflrp20_model))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Divfutureretmodel}{%4.1f}\n',portfdivfuturereturn_model)  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Divfutureretdata}{%4.1f}\n',portfdivfuturereturn_data)  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Divfutexpretunc}{%4.1f}\n',mean(divfutureriskprem_model_uncond(1:28)))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Divfutexpretshort}{%4.1f}\n',mean(divfutureriskprem_model_short(1:28)))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\PDstripmodel}{%4.2f}\n',mean(PDm_strip_8q(isfinite(sharestrip_8q_data))))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\PDstripdata}{%4.2f}\n',mean(PDm_strip_8q_data(isfinite(sharestrip_8q_data))))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Sharestripmodel}{%4.1f}\n',100*mean(sharestrip_8q(isfinite(sharestrip_8q_data))))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fprintf(fID,'\\newcommand{\\Sharestripdata}{%4.1f}\n',100*mean(sharestrip_8q_data(isfinite(sharestrip_8q_data))))  ; % write the latex syntax for defining a variable/command \Plambda in latex, and give it the value of lambda in the matlab code, where the latter is defined as a floating point number with 2 digit precision
    fclose(fID); % close the file, so matlab can return to its other operations
end


%% Constructs cohort dividend growth rates for PE analysis
T = length(logdiv_stock);
H = 64;

% Build a sequence of overlapping REIT PE funds
Div_cohort_stock  = zeros(T,H)*NaN;
Div_cohort_reit   = zeros(T,H)*NaN;
Div_cohort_infra  = zeros(T,H)*NaN;
Div_cohort_small  = zeros(T,H)*NaN;
Div_cohort_large  = zeros(T,H)*NaN;
Div_cohort_growth = zeros(T,H)*NaN;
Div_cohort_value  = zeros(T,H)*NaN;
Div_cohort_NR     = zeros(T,H)*NaN;

Div_Pricestrip_cohort_stock  = zeros(T,H)*NaN;
Div_Pricestrip_cohort_reit   = zeros(T,H)*NaN;
Div_Pricestrip_cohort_infra  = zeros(T,H)*NaN;
Div_Pricestrip_cohort_small  = zeros(T,H)*NaN;
Div_Pricestrip_cohort_growth = zeros(T,H)*NaN;
Div_Pricestrip_cohort_NR     = zeros(T,H)*NaN;
Div_Pricestrip_cohort_value  = zeros(T,H)*NaN;

Price_Pricestrip_cohort_stock  = zeros(T,H)*NaN;
Price_Pricestrip_cohort_reit   = zeros(T,H)*NaN;
Price_Pricestrip_cohort_infra  = zeros(T,H)*NaN;
Price_Pricestrip_cohort_small  = zeros(T,H)*NaN;
Price_Pricestrip_cohort_growth = zeros(T,H)*NaN;
Price_Pricestrip_cohort_NR     = zeros(T,H)*NaN;
Price_Pricestrip_cohort_value  = zeros(T,H)*NaN;

DIV_STACK = [];
DIV_STACKgain = [];

for t = 1:T-H % this loop deals with the complete vintages of H quarters
    
    % These are the cash flows on the dividend strips
    Div_cohort_stock(t,:) = exp(logdiv_stock(t+1:t+H))./exp(logdiv_stock(t));
    Div_cohort_reit(t,:)  = exp(logdiv_reit(t+1:t+H))./exp(logdiv_reit(t));
    Div_cohort_infra(t,:) = exp(logdiv_infra(t+1:t+H))./exp(logdiv_infra(t));
    Div_cohort_small(t,:) = exp(logdiv_MEQ1(t+1:t+H))./exp(logdiv_MEQ1(t));
    Div_cohort_large(t,:) = exp(logdiv_MEQ5(t+1:t+H))./exp(logdiv_MEQ5(t));
    Div_cohort_growth(t,:)= exp(logdiv_BMQ1(t+1:t+H))./exp(logdiv_BMQ1(t));
    Div_cohort_value(t,:) = exp(logdiv_BMQ5(t+1:t+H))./exp(logdiv_BMQ5(t));
    Div_cohort_NR(t,:)    = exp(logdiv_NR(t+1:t+H))./exp(logdiv_NR(t));   

    % These are the cash flows on the gain strips
    Div_Pricestrip_cohort_stock(t,:)   = PDm(t+1:t+H)'.*exp(logdiv_stock(t+1:t+H))./(PDm(t)*exp(logdiv_stock(t)));
    Div_Pricestrip_cohort_reit(t,:)    = PD_reit(t+1:t+H)'.*exp(logdiv_reit(t+1:t+H))./(PD_reit(t)*exp(logdiv_reit(t)));
    Div_Pricestrip_cohort_infra(t,:)   = PD_infra(t+1:t+H)'.*exp(logdiv_infra(t+1:t+H))./(PD_infra(t)*exp(logdiv_infra(t)));
    Div_Pricestrip_cohort_small(t,:)   = PD_small(t+1:t+H)'.*exp(logdiv_MEQ1(t+1:t+H))./(PD_small(t)*exp(logdiv_MEQ1(t)));
    Div_Pricestrip_cohort_growth(t,:)  = PD_growth(t+1:t+H)'.*exp(logdiv_BMQ1(t+1:t+H))./(PD_growth(t)*exp(logdiv_BMQ1(t)));
    Div_Pricestrip_cohort_NR(t,:)      = PD_NR(t+1:t+H)'.*exp(logdiv_NR(t+1:t+H))./(PD_NR(t)*exp(logdiv_NR(t)));
    Div_Pricestrip_cohort_value(t,:)   = PD_value(t+1:t+H)'.*exp(logdiv_BMQ5(t+1:t+H))./(PD_value(t)*exp(logdiv_BMQ5(t)));
    
    % These are the time-t prices of the gain strips
    Price_Pricestrip_cohort_stock(t,:)  = (ones(1,H)*PDm(t)      -cumsum(divstripprices_stock(t,1:H)))./(PDm(t));
    Price_Pricestrip_cohort_reit(t,:)   = (ones(1,H)*PD_reit(t)  -cumsum(divstripprices_reit(t,1:H)))./(PD_reit(t));
    Price_Pricestrip_cohort_infra(t,:)  = (ones(1,H)*PD_infra(t) -cumsum(divstripprices_infra(t,1:H)))./(PD_infra(t));
    Price_Pricestrip_cohort_small(t,:)  = (ones(1,H)*PD_small(t) -cumsum(divstripprices_small(t,1:H)))./(PD_small(t));
    Price_Pricestrip_cohort_growth(t,:) = (ones(1,H)*PD_growth(t)-cumsum(divstripprices_growth(t,1:H)))./(PD_growth(t));
    Price_Pricestrip_cohort_NR(t,:)     = (ones(1,H)*PD_NR(t)    -cumsum(divstripprices_NR(t,1:H)))./(PD_NR(t));
    Price_Pricestrip_cohort_value(t,:)  = (ones(1,H)*PD_value(t) -cumsum(divstripprices_value(t,1:H)))./(PD_value(t));
    
    aux = [Div_cohort_stock(t,:)' Div_cohort_reit(t,:)' Div_cohort_infra(t,:)' Div_cohort_small(t,:)' Div_cohort_large(t,:)' Div_cohort_growth(t,:)' Div_cohort_value(t,:)' Div_cohort_NR(t,:)'];
    DIV_COH_CORR(t,:,:) = corrcoef(aux);
    DIV_STACK = [DIV_STACK;aux];
    
    auxgain = [Div_cohort_stock(t,:)' Div_cohort_reit(t,:)' Div_cohort_infra(t,:)' Div_cohort_small(t,:)' Div_cohort_growth(t,:)' Div_Pricestrip_cohort_stock(t,:)' Div_Pricestrip_cohort_reit(t,:)' Div_Pricestrip_cohort_infra(t,:)' Div_Pricestrip_cohort_small(t,:)' Div_Pricestrip_cohort_growth(t,:)' Div_Pricestrip_cohort_NR(t,:)'];
    DIV_COH_CORRgain(t,:,:) = corrcoef(auxgain);
    DIV_STACKgain = [DIV_STACKgain;auxgain];
end

for t=T-H+1:T % this deals with the incomplete vintages (fewer than H quarters of cash flows) at the end of the time-series
    HH = T-t;
    
    % These are the cash flows on the dividend strips
    Div_cohort_stock(t,1:HH)    = exp(logdiv_stock(t+1:t+HH))./exp(logdiv_stock(t));
    Div_cohort_reit(t,1:HH)     = exp(logdiv_reit(t+1:t+HH))./exp(logdiv_reit(t));
    Div_cohort_infra(t,1:HH)    = exp(logdiv_infra(t+1:t+HH))./exp(logdiv_infra(t));
    Div_cohort_small(t,1:HH)    = exp(logdiv_MEQ1(t+1:t+HH))./exp(logdiv_MEQ1(t));
    Div_cohort_large(t,1:HH)    = exp(logdiv_MEQ5(t+1:t+HH))./exp(logdiv_MEQ5(t));
    Div_cohort_growth(t,1:HH)   = exp(logdiv_BMQ1(t+1:t+HH))./exp(logdiv_BMQ1(t));
    Div_cohort_value(t,1:HH)    = exp(logdiv_BMQ5(t+1:t+HH))./exp(logdiv_BMQ5(t));
    Div_cohort_NR(t,1:HH)       = exp(logdiv_NR(t+1:t+HH))./exp(logdiv_NR(t));
       
    % These are the cash flows on the gain strips
    Div_Pricestrip_cohort_stock(t,1:HH)  = PDm(t+1:t+HH)'.*exp(logdiv_stock(t+1:t+HH))./(PDm(t)*exp(logdiv_stock(t)));
    Div_Pricestrip_cohort_reit(t,1:HH)   = PD_reit(t+1:t+HH)'.*exp(logdiv_reit(t+1:t+HH))./(PD_reit(t)*exp(logdiv_reit(t)));
    Div_Pricestrip_cohort_infra(t,1:HH)  = PD_infra(t+1:t+HH)'.*exp(logdiv_infra(t+1:t+HH))./(PD_infra(t)*exp(logdiv_infra(t)));
    Div_Pricestrip_cohort_small(t,1:HH)  = PD_small(t+1:t+HH)'.*exp(logdiv_MEQ1(t+1:t+HH))./(PD_small(t)*exp(logdiv_MEQ1(t)));
    Div_Pricestrip_cohort_growth(t,1:HH) = PD_growth(t+1:t+HH)'.*exp(logdiv_BMQ1(t+1:t+HH))./(PD_growth(t)*exp(logdiv_BMQ1(t)));
    Div_Pricestrip_cohort_NR(t,1:HH)     = PD_NR(t+1:t+HH)'.*exp(logdiv_NR(t+1:t+HH))./(PD_NR(t)*exp(logdiv_NR(t)));
    Div_Pricestrip_cohort_value(t,1:HH)  = PD_value(t+1:t+HH)'.*exp(logdiv_BMQ5(t+1:t+HH))./(PD_value(t)*exp(logdiv_BMQ5(t)));
    
    % These are the time-t prices of the gain strips
    Price_Pricestrip_cohort_stock(t,1:HH)  = (ones(1,HH)*PDm(t)-cumsum(divstripprices_stock(t,1:HH)))./(PDm(t));
    Price_Pricestrip_cohort_reit(t,1:HH)   = (ones(1,HH)*PD_reit(t)-cumsum(divstripprices_reit(t,1:HH)))./(PD_reit(t));
    Price_Pricestrip_cohort_infra(t,1:HH)  = (ones(1,HH)*PD_infra(t)-cumsum(divstripprices_infra(t,1:HH)))./(PD_infra(t));
    Price_Pricestrip_cohort_small(t,1:HH)  = (ones(1,HH)*PD_small(t)-cumsum(divstripprices_small(t,1:HH)))./(PD_small(t));
    Price_Pricestrip_cohort_growth(t,1:HH) = (ones(1,HH)*PD_growth(t)-cumsum(divstripprices_growth(t,1:HH)))./(PD_growth(t));
    Price_Pricestrip_cohort_NR(t,1:HH)     = (ones(1,HH)*PD_NR(t)-cumsum(divstripprices_NR(t,1:HH)))./(PD_NR(t));
    Price_Pricestrip_cohort_value(t,1:HH)  = (ones(1,HH)*PD_value(t)-cumsum(divstripprices_value(t,1:HH)))./(PD_value(t));

    aux = [Div_cohort_stock(t,1:HH)' Div_cohort_reit(t,1:HH)' Div_cohort_infra(t,1:HH)' Div_cohort_small(t,1:HH)' Div_cohort_large(t,1:HH)' Div_cohort_growth(t,1:HH)' Div_cohort_value(t,1:HH)' Div_cohort_NR(t,1:HH)'];
    DIV_COH_CORR(t,:,:) = corrcoef(aux);
    DIV_STACK = [DIV_STACK;aux];

    auxgain = [Div_cohort_stock(t,1:HH)' Div_cohort_reit(t,1:HH)' Div_cohort_infra(t,1:HH)' Div_cohort_small(t,1:HH)' Div_cohort_growth(t,1:HH)' Div_Pricestrip_cohort_stock(t,1:HH)' Div_Pricestrip_cohort_reit(t,1:HH)' Div_Pricestrip_cohort_infra(t,1:HH)' Div_Pricestrip_cohort_small(t,1:HH)' Div_Pricestrip_cohort_growth(t,1:HH)' Div_Pricestrip_cohort_NR(t,1:HH)'];
    DIV_COH_CORRgain(t,:,:) = corrcoef(auxgain);
    DIV_STACKgain = [DIV_STACKgain;auxgain];
end



%% Save output to be used in PE and MC analysis
disp('Saving results to be used in PE & MC analysis')
if printfigs==1
    save APoutputPEinput zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
            Div_cohort_stock Div_cohort_reit Div_cohort_infra Div_cohort_small Div_cohort_large Div_cohort_growth Div_cohort_value Div_cohort_NR... 
            Div_Pricestrip_cohort_stock Div_Pricestrip_cohort_reit Div_Pricestrip_cohort_infra Div_Pricestrip_cohort_small Div_Pricestrip_cohort_growth Div_Pricestrip_cohort_NR Div_Pricestrip_cohort_value Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value...
            stripexpret_bond stripexpret_stock stripexpret_reit stripexpret_infra stripexpret_small stripexpret_growth stripexpret_NR stripexpret_value...
            stripexpret_capgain_stock stripexpret_capgain_reit stripexpret_capgain_infra stripexpret_capgain_small stripexpret_capgain_growth stripexpret_capgain_NR  stripexpret_capgain_value...
            pdm fwdrates60 discrates60


    save APoutputMCinput N Psi Sigma Sig eps2 Omega L0 L1 y0nom_1 I_y1 mu_m pi0 I_divgrm I_pi mu_reit I_divgr_reit A0m I_pdm Api Bpi Am Bm A_reit B_reit
end


%% Shock-exposure and shock-price elasticities
% For Appendix
disp('Shock exposure and shock elasticity analysis')
maxmat = StripHor;

eps_g_m      = zeros(maxmat,1);
eps_sg_m     = zeros(maxmat,1);
eps_p_m      = zeros(maxmat,1);
eps_g_reit   = zeros(maxmat,1);
eps_sg_reit  = zeros(maxmat,1);
eps_p_reit   = zeros(maxmat,1);
eps_g_infra  = zeros(maxmat,1);
eps_sg_infra  = zeros(maxmat,1);
eps_p_infra   = zeros(maxmat,1);
eps_g_small   = zeros(maxmat,1);
eps_sg_small  = zeros(maxmat,1);
eps_p_small   = zeros(maxmat,1);
eps_g_growth  = zeros(maxmat,1);
eps_sg_growth = zeros(maxmat,1);
eps_p_growth  = zeros(maxmat,1);
eps_g_value   = zeros(maxmat,1);
eps_sg_value  = zeros(maxmat,1);
eps_p_value   = zeros(maxmat,1);
eps_g_NR      = zeros(maxmat,1);
eps_sg_NR     = zeros(maxmat,1);
eps_p_NR      = zeros(maxmat,1);
eps_sdf       = zeros(maxmat,1);

for ii=1:7
    % Cash-flows objects
    if ii == 1
        AA0 = mu_m;
        AA1 = I_divgrm'*Psi;
        AA2 = I_divgrm'*Sig;
    elseif ii == 2
        AA0 = mu_reit;
        AA1 = I_divgr_reit'*Psi;
        AA2 = I_divgr_reit'*Sig;
    elseif ii == 3
        AA0 = mu_infra;
        AA1 = I_divgr_infra'*Psi;
        AA2 = I_divgr_infra'*Sig;
    elseif ii == 4
        AA0 = mu_small;
        AA1 = I_divgr_small'*Psi;
        AA2 = I_divgr_small'*Sig;
    elseif ii == 5
        AA0 = mu_growth;
        AA1 = I_divgr_growth'*Psi;
        AA2 = I_divgr_growth'*Sig;
    elseif ii == 6
        AA0 = mu_NR;
        AA1 = I_divgr_NR'*Psi;
        AA2 = I_divgr_NR'*Sig;
    elseif ii == 7
        AA0 = mu_value;
        AA1 = I_divgr_value'*Psi;
        AA2 = I_divgr_value'*Sig;
    end    
    % SDF objects
    BB0 = -y0nom_1 +pi0 - 0.5*(L0)'*(L0);
    BB1 = -I_y1' + I_pi'*Psi -(L0)'*(L1);
    BB2 = -L0' + I_pi'*Sig;
    BB3 = -0.5*(L1)'*(L1);
    BB4 = -L1';

    funcS.name     = 'S';
    funcS.T        = StripHor;
    funcS.GAMMA0   = BB0;
    funcS.GAMMA1   = BB1;
    funcS.GAMMA3   = BB3;
    funcS.PSI0     = BB2;
    funcS.PSI1     = BB4;
    funcS.THETA11  = Psi;
    funcS.LAMBDA10 = Sig;

    funcG.name     = 'Gstock';
    funcG.T        = funcS.T;
    funcG.GAMMA0   = AA0;
    funcG.GAMMA1   = AA1;
    funcG.GAMMA3   = 0*funcS.GAMMA3;
    funcG.PSI0     = AA2;
    funcG.PSI1     = 0*funcS.PSI1;
    funcG.THETA11  = Psi;
    funcG.LAMBDA10 = Sig;

    funcSG  = add_functionals(funcS,funcG);
    elastG  = shock_elasticity_affine(funcG);
    elastSG = shock_elasticity_affine(funcSG);
    elastS  = shock_elasticity_affine(funcS);

    eps_sdf  = -squeeze(elastS.elast_0);
    
    % evaluate elasticities to all 10 shocks at state z=0 (average)
    if ii ==1
        eps_sg_m = squeeze(elastSG.elast_0);
        eps_g_m  = squeeze(elastG.elast_0);
        eps_p_m  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==2
        eps_sg_reit = squeeze(elastSG.elast_0);
        eps_g_reit  = squeeze(elastG.elast_0);
        eps_p_reit  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==3
        eps_sg_infra = squeeze(elastSG.elast_0);
        eps_g_infra  = squeeze(elastG.elast_0);
        eps_p_infra  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==4
        eps_sg_small = squeeze(elastSG.elast_0);
        eps_g_small  = squeeze(elastG.elast_0);
        eps_p_small  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==5
        eps_sg_growth = squeeze(elastSG.elast_0);
        eps_g_growth  = squeeze(elastG.elast_0);
        eps_p_growth  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==6
        eps_sg_NR = squeeze(elastSG.elast_0);
        eps_g_NR  = squeeze(elastG.elast_0);
        eps_p_NR  = squeeze(elastG.elast_0-elastSG.elast_0);
    elseif ii==7
        eps_sg_value = squeeze(elastSG.elast_0);
        eps_g_value  = squeeze(elastG.elast_0);
        eps_p_value  = squeeze(elastG.elast_0-elastSG.elast_0);
    end
end


figure('units','normalized','outerposition',[0 0 1 1]);
title('Shock Exposure Elasticities');
subplot(2,6,1);alphad    = I_pi'*Sig; alphad = alphad/sqrt(alphad*alphad');             P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Inflation');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,2);alphad    = I_gdp'*Sig; alphad = alphad/sqrt(alphad*alphad');            P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('GDP growth');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,3);alphad    = I_y1'*Sig; alphad = alphad/sqrt(alphad*alphad');             P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Short rate');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,4);alphad    = I_yspr'*Sig; alphad = alphad/sqrt(alphad*alphad');           P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Slope');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,5);alphad    = I_pdm'*Sig; alphad = alphad/sqrt(alphad*alphad');            P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('PD mkt');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,6);alphad    = I_divgrm'*Sig; alphad = alphad/sqrt(alphad*alphad');         P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr mkt');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,7);alphad    = I_divgr_small'*Sig; alphad = alphad/sqrt(alphad*alphad');    P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr small');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,8);alphad    = I_divgr_growth'*Sig; alphad = alphad/sqrt(alphad*alphad');  P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr growth');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,9);alphad    = I_divgr_value'*Sig; alphad = alphad/sqrt(alphad*alphad');  P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)'  (alphad*eps_g_value)'],'LineWidth',2);title('Divgr value');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,10);alphad    = I_divgr_reit'*Sig; alphad = alphad/sqrt(alphad*alphad');     P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr reit');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,11);alphad    = I_divgr_infra'*Sig; alphad = alphad/sqrt(alphad*alphad');    P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr infra');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
subplot(2,6,12);alphad    = I_divgr_NR'*Sig; alphad = alphad/sqrt(alphad*alphad');      P=plot(1:maxmat,[(alphad*eps_g_m)' (alphad*eps_g_reit)' (alphad*eps_g_infra)' (alphad*eps_g_small)' (alphad*eps_g_growth)' (alphad*eps_g_NR)' (alphad*eps_g_value)'],'LineWidth',2);title('Divgr NR');xlim([0,StripHor]);
NameArray = {'Color'};ValueArray = {'b','r','g','c','m','k','y'}';set(P,NameArray,ValueArray)
legend('market','reit','infra','small','growth','NR','value','Location','NorthEast');
set(findall(gcf,'-property','FontSize'),'FontSize',12)
if printfigs==1
    savefig('shockexposureelast_main')
    print('shockexposureelast_main','-depsc')
    movefile('shockexposureelast_main.eps', '..\..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
title('Shock Price Elasticities');
subplot(2,6,1);alphad     = I_pi'*Sig;           alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Infl shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,2);alphad     = I_gdp'*Sig;          alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('GDP gr shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,3);alphad     = I_y1'*Sig;           alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Short rate shock');ylim([mean(alphad*eps_sdf)-0.03,mean(alphad*eps_sdf)+.03]);xlim([0,StripHor]);
subplot(2,6,4);alphad     = I_yspr'*Sig;         alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Slope shock'); ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,5);alphad     = I_pdm'*Sig;          alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('PD mkt shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,6);alphad     = I_divgrm'*Sig;       alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr mkt shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,7);alphad     = I_divgr_small'*Sig;  alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr small shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,8);alphad     = I_divgr_growth'*Sig; alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr growth shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,9);alphad     = I_divgr_value'*Sig;  alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr value shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,10);alphad    = I_divgr_reit'*Sig;   alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr reit shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,11);alphad    = I_divgr_infra'*Sig;  alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr infra shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
subplot(2,6,12);alphad    = I_divgr_NR'*Sig;     alphad = alphad/sqrt(alphad*alphad');   plot(1:maxmat,(alphad*eps_sdf)','LineWidth',2,'Color','black');title('Divgr NR shock');ylim([mean(alphad*eps_sdf)-0.02,mean(alphad*eps_sdf)+.02]);xlim([0,StripHor]);
set(findall(gcf,'-property','FontSize'),'FontSize',12)
if printfigs==1
    savefig('shockpriceelast_main')
    print('shockpriceelast_main','-depsc')
    movefile('shockpriceelast_main.eps', '..\..\..\Figures');
end