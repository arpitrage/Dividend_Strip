function obj = solvenul_main(x,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,I_pd_reit,I_divgr_reit,I_pd_infra,I_divgr_infra,I_pd_small,I_divgr_small,I_pd_growth,I_divgr_growth,I_pd_NR,I_divgr_NR,I_pd_value,I_divgr_value,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,A0_reit,k1_reit,r0_reit,mu_reit,A0_infra,k1_infra,r0_infra,mu_infra,A0_small,k1_small,r0_small,mu_small,A0_growth,k1_growth,r0_growth,mu_growth,PDm_strip_8q_data,sharestrip_8q_data,A0_NR,k1_NR,r0_NR,mu_NR,A0_value,k1_value,r0_value,mu_value,taureal,yielddatareal)
global counts 

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
L1(I_divgr_small==1,I_pi==1)          = x(i);  i = i+1;
L1(I_divgr_small==1,I_gdp==1)         = x(i);  i = i+1;
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
L1(I_divgr_growth==1,I_pi==1)          = x(i);  i = i+1;
L1(I_divgr_growth==1,I_gdp==1)         = x(i);  i = i+1;
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

FF = [];

%% arbitrage-free definition of real risk-free rate
y0_1   =  y0nom_1 - pi0 - .5*I_pi'*(Sig*Sig')*I_pi + I_pi'*Sig*L0; 

%% No arbitrage restrictions on CRSP vw stock return
aa = (I_divgrm+k1m*I_pdm+I_pi)'*Sig;
cc = ((I_divgrm+k1m*I_pdm+I_pi)'*Psi -I_pdm' -I_y1');
FF_stock=[];
FF_new = 400*(L0(find(I_divgrm==1))  - (((r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)')-aa(1:5)*L0(1:5))/aa(6)));                     FF = [FF FF_new]; FF_stock=[FF_stock FF_new];    
FF_new = 10*(L1(find(I_divgrm==1),:)    -((cc-aa(1:5)*L1(1:5,:))/aa(6))) ; FF = [FF FF_new]; FF_stock=[FF_stock FF_new];    
if mod(counts,100) == 0
    disp(['stock exp. return violation:  ',num2str(nansum(abs(FF_stock)))])
end

%% No arbitrage restrictions on REIT return
aa_reit = (I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Sig;
cc_reit = ((I_divgr_reit+k1_reit*I_pd_reit+I_pi)'*Psi -I_pd_reit' -I_y1');
FF_reit=[];
FF_new = 400*(L0(find(I_divgr_reit==1))  - (((r0_reit + pi0 - y0nom_1 +0.5*(aa_reit)*(aa_reit)')-aa_reit(1:7)*L0(1:7))/aa_reit(8)));                        FF = [FF FF_new];  FF_reit=[FF_reit FF_new];    
FF_new = 10*(L1(find(I_divgr_reit==1),:) -((cc_reit-aa_reit(1:7)*L1(1:7,:))/aa_reit(8))) ;                   FF = [FF FF_new]; FF_reit=[FF_reit FF_new];    
if mod(counts,100) == 0
    disp(['reit exp. return violation:  ',num2str(nansum(abs(FF_reit)))])
end

%% No arbitrage restriction on infra returns
aa_infra = (I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Sig;
cc_infra = ((I_divgr_infra+k1_infra*I_pd_infra+I_pi)'*Psi -I_pd_infra' -I_y1');
FF_infra=[];
FF_new = 400*(L0(find(I_divgr_infra==1))  - (((r0_infra + pi0 - y0nom_1 +0.5*(aa_infra)*(aa_infra)')-aa_infra(1:9)*L0(1:9))/aa_infra(10)));                            FF = [FF FF_new];FF_infra=[FF_infra FF_new];    
FF_new = 10*(L1(find(I_divgr_infra==1),:) -((cc_infra  -aa_infra(1:9)*L1(1:9,:))/aa_infra(10))) ;              FF = [FF FF_new];FF_infra=[FF_infra FF_new];    
if mod(counts,100) == 0
    disp(['infra exp. return violation:  ',num2str(nansum(abs(FF_infra)))])
end

%% No arbitrage restriction on small returns
aa_small = (I_divgr_small+k1_small*I_pd_small+I_pi)'*Sig;
cc_small = ((I_divgr_small+k1_small*I_pd_small+I_pi)'*Psi -I_pd_small' -I_y1');
FF_small=[];
FF_new = 400*(L0(find(I_divgr_small==1))  - (((r0_small + pi0 - y0nom_1 +0.5*(aa_small)*(aa_small)')-aa_small(1:11)*L0(1:11))/aa_small(12)));  FF = [FF FF_new];FF_small=[FF_small FF_new];    
FF_new = 10*(L1(find(I_divgr_small==1),:) -((cc_small -aa_small(1:11)*L1(1:11,:))/aa_small(12)));              FF = [FF FF_new];FF_small=[FF_small FF_new];    
if mod(counts,100) == 0
    disp(['small exp. return violation:  ',num2str(nansum(abs(FF_small)))])
end

%% No arbitrage restriction on growth returns
aa_growth = (I_divgr_growth+k1_growth*I_pd_growth+I_pi)'*Sig;
cc_growth = ((I_divgr_growth+k1_growth*I_pd_growth+I_pi)'*Psi -I_pd_growth' -I_y1');
FF_growth=[];
FF_new = 400*(L0(find(I_divgr_growth==1))  - (((r0_growth + pi0 - y0nom_1 +0.5*(aa_growth)*(aa_growth)')-aa_growth(1:13)*L0(1:13))/aa_growth(14)));  FF = [FF FF_new];FF_growth=[FF_growth FF_new];    
FF_new = 10*(L1(find(I_divgr_growth==1),:) -((cc_growth  -aa_growth(1:13)*L1(1:13,:))/aa_growth(14))) ;              FF = [FF FF_new];FF_growth=[FF_growth FF_new];    
if mod(counts,100) == 0
    disp(['growth exp. return violation:  ',num2str(nansum(abs(FF_growth)))])
end

%% No arbitrage restriction on NR returns
aa_NR = (I_divgr_NR+k1_NR*I_pd_NR+I_pi)'*Sig;
cc_NR = ((I_divgr_NR+k1_NR*I_pd_NR+I_pi)'*Psi -I_pd_NR' -I_y1');
FF_NR=[];
FF_new = 400*(L0(find(I_divgr_NR==1))  - (((r0_NR + pi0 - y0nom_1 +0.5*(aa_NR)*(aa_NR)')-aa_NR(1:15)*L0(1:15))/aa_NR(16))); FF = [FF FF_new];FF_NR=[FF_NR FF_new];    
FF_new = 10*(L1(find(I_divgr_NR==1),:) -((cc_NR -aa_NR(1:15)*L1(1:15,:))/aa_NR(16))) ; FF = [FF FF_new];FF_NR=[FF_NR FF_new];    
if mod(counts,100) == 0
    disp(['NR exp. return violation:  ',num2str(nansum(abs(FF_NR)))])
end
 
%% No arbitrage restriction on value returns
aa_value = (I_divgr_value+k1_value*I_pd_value+I_pi)'*Sig;
cc_value = ((I_divgr_value+k1_value*I_pd_value+I_pi)'*Psi -I_pd_value' -I_y1');
FF_value=[];
FF_new = 400*(L0(find(I_divgr_value==1))  - (((r0_value + pi0 - y0nom_1 +0.5*(aa_value)*(aa_value)')-aa_value(1:17)*L0(1:17))/aa_value(18)));  FF = [FF FF_new];FF_value=[FF_value FF_new];    
FF_new = 10*(L1(find(I_divgr_value==1),:) -((cc_value  -aa_value(1:17)*L1(1:17,:))/aa_value(18)));  FF = [FF FF_new];FF_value=[FF_value FF_new];    
if mod(counts,100) == 0
    disp(['value exp. return violation:  ',num2str(nansum(abs(FF_value)))])
end
 

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

    equitydivstripriskprem(j) = 400*(I_divgrm+I_pi+Bm(:,j))'*Sig*L0;    
end

%% Match div strip risk premia
% divfuturepricediv = exp(Am-Api + (Bm-Bpi)'*X2');
divfuturereturn = zeros(striphorizon,T);
divfuturereturn(2:end,2:end) = exp(Am(1:end-1)-Api(1:end-1) + (Bm(:,1:end-1)-Bpi(:,1:end-1))'*X2(2:end,:)' ...
    -Am(2:end)+Api(2:end) - (Bm(:,2:end)-Bpi(:,2:end))'*X2(1:end-1,:)'...
    +mu_m+pi0 + (I_divgrm+I_pi)'*X2(2:end,:)') -1 ;
portfdivfuturereturn_model = 400*mean(mean(divfuturereturn(2:29,117:158),2)); % avg of first 28 quarterly strip returns, avg from 2003.Q1-2014.Q2 
portfdivfuturereturn_data = (0.0041+0.0059+0.0067+0.0072+0.0084+0.0090+0.0095)*1200/7; % from Binsbergen and Koijen, JFE 2017, Table 1, US panel, average of the 1- through 7-year dividend future returns
FF_futstripportfret = 1000*(portfdivfuturereturn_model-portfdivfuturereturn_data)^2;
FF        = [FF FF_futstripportfret];
if mod(counts,100) == 0
    disp(['Futures strip risk premium violation:  ',num2str(FF_futstripportfret)])
end

%% Match level of the PD ratio in model and data
FF_new    = nansum((exp(A0m+I_pdm'*X2'-log(4))' - PDm_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['PDm violation:  ',num2str(FF_new)])
end

%% Match level of the REIT PD ratio in model and data
FF_new    = nansum((exp(A0_reit+I_pd_reit'*X2'-log(4))' - PD_reit_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['PD_reit violation:  ',num2str(FF_new)])
end

%% Match level of the infrastructure PD ratio in model and data
FF_new    = nansum((exp(A0_infra+I_pd_infra'*X2'-log(4))' - PD_infra_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['PD_infra violation:  ',num2str(FF_new)])
end

%% Match level of the small PD ratio in model and data
FF_new    = nansum((exp(A0_small+I_pd_small'*X2'-log(4))' - PD_small_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['pd_small violation:  ',num2str(FF_new)])
end

%% Match level of the growth PD ratio in model and data
FF_new    = nansum((exp(A0_growth+I_pd_growth'*X2'-log(4))' - PD_growth_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['pd_growth violation:  ',num2str(FF_new)])
end
 
%% Match level of the NR PD ratio in model and data
FF_new    = nansum((exp(A0_NR+I_pd_NR'*X2'-log(4))' - PD_NR_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['pd_NR violation:  ',num2str(FF_new)])
end

%% Match level of the value PD ratio in model and data
FF_new    = nansum((exp(A0_value+I_pd_value'*X2'-log(4))' - PD_value_model/4).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['pd_value violation:  ',num2str(FF_new)])
end

%% Insist on matching the 5-year yield more closely since it is in the state space
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
if mod(counts,100) == 0
    disp(['20-qtr bond yield violation:  ',num2str(FF_newa+FF_newb+FF_newc+FF_newd+FF_newe+FF_newf+FF_newg+FF_newh+FF_newi+FF_newj+FF_newk+FF_newl+FF_newm+FF_newn+FF_newo+FF_newp+FF_newq+FF_newr+FF_news)])
end

%% Match Nominal Yield Curve
% Pricing nominal bond yields of maturities stored in tau
FF_new    = nansum((400*(kron(ones(length(X2),1),-Api(tau)'./tau) -((Bpi(:,tau)'./kron(tau',ones(1,N)))*X2')' - yielddata)).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['Nominal yield pricing errors:  ',num2str(sum(FF_new),'%.1f')])
end


%% Match Real Yield Curve
% Pricing TIIS yields of maturities stored in taureal
FF_new    = nansum((400*(kron(ones(length(X2),1),-A(taureal)'./taureal) -((B(:,taureal)'./kron(taureal',ones(1,N)))*X2')' - yielddatareal)).^2);
FF        = [FF FF_new];
if mod(counts,100) == 0
    disp(['Real yield pricing errors:  ',num2str(sum(FF_new),'%.1f')])
end

%% Match short horizon market strips PD ratios from Binsbergen, Brandt, and Koijen (AER)
PDm_strip_2q = exp(Am(1)+Bm(:,1)'*X2')'+exp(Am(2)+Bm(:,2)'*X2')';
PDm_strip_4q = PDm_strip_2q+ exp(Am(3)+Bm(:,3)'*X2')'+exp(Am(4)+Bm(:,4)'*X2')';
PDm_strip_6q = PDm_strip_4q+ exp(Am(5)+Bm(:,5)'*X2')'+exp(Am(6)+Bm(:,6)'*X2')';
PDm_strip_8q = PDm_strip_6q+ exp(Am(7)+Bm(:,7)'*X2')'+exp(Am(8)+Bm(:,8)'*X2')';

sharestrip_2q = PDm_strip_2q./PDm_model;
sharestrip_4q = PDm_strip_4q./PDm_model;
sharestrip_6q = PDm_strip_6q./PDm_model;
sharestrip_8q = PDm_strip_8q./PDm_model;


FF_strip8    = 10*nansum((PDm_strip_8q - PDm_strip_8q_data).^2);
FF_strip8b    = 10*nansum((100*(sharestrip_8q - sharestrip_8q_data)).^2);
FF_strip     = FF_strip8;%FF_strip2+FF_strip4+FF_strip6+FF_strip8;
FF_stripb    = FF_strip8b;%FF_strip2b+FF_strip4b+FF_strip6b+FF_strip8b;

FF           = [FF FF_strip FF_stripb];

if mod(counts,100) == 0
    disp(['short strip PD violation:  ',num2str(FF_strip)])
    disp(['strip share violation:  ',num2str(FF_stripb)])
end

%% Conditions on the behavior of long-term interest rates (50 years +)
%Forcing the nominal bond yield to stay above 5.85% per year on average (nom. growth is 5.85%)
ynomlongmin = (x0+pi0)*400;
yreallongmin = (x0)*400;
tuningp = 10;
FF_new    = (tuningp*abs(min(-400*Api(200)/200-ynomlongmin,0))).^2;
FF_posnom1    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(400)/400-ynomlongmin,0))).^2;
FF_posnom2    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(800)/800-ynomlongmin,0))).^2;
FF_posnom3    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(1200)/1200-ynomlongmin,0))).^2;
FF_posnom4    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(1600)/1600-ynomlongmin,0))).^2;
FF_posnom5    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(2000)/2000-ynomlongmin,0))).^2;
FF_posnom6    = FF_new; FF        = [FF FF_new];
% Forcing the real yield to stay above real GDP growth on average
FF_new    = (tuningp*abs(min(-400*A(200)/200-yreallongmin ,0))).^2;
FF_posreal1    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*A(400)/400-yreallongmin,0))).^2;
FF_posreal2    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*A(800)/800-yreallongmin,0))).^2;
FF_posreal3    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*A(1200)/1200-yreallongmin,0))).^2;
FF_posreal4    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*A(1600)/1600-yreallongmin,0))).^2;
FF_posreal5    = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*A(2000)/2000-yreallongmin,0))).^2;
FF_posreal6    = FF_new; FF        = [FF FF_new];
% Forcing the real term structure to stay below the nominal one by at least 2%
% uncond infl is 3.28% so this forces the inflation risk premium to be
% greater than zero
FF_new    = (tuningp*abs(min(-400*Api(200)/200+400*A(200)/200-2,0))).^2;
FF_real   = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(400)/400+400*A(400)/400-2,0))).^2;
FF_real2   = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(800)/800+400*A(800)/800-2,0))).^2;
FF_real3   = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(1200)/1200+400*A(1200)/1200-2,0))).^2;
FF_real4   = FF_new; FF        = [FF FF_new];
FF_new    = (tuningp*abs(min(-400*Api(2000)/2000+400*A(2000)/2000-2,0))).^2;
FF_real5   = FF_new; FF        = [FF FF_new];
% Forcing the term structure to flatten out
yielddiffnom = abs(400*(-Api(400)/400+Api(200)/200)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffnom)-1,0)).^2;
FF_flatnom = FF_new; FF        = [FF FF_new];
yielddiffnom = abs(400*(-Api(800)/800+Api(600)/600)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffnom)-0.5,0)).^2;
FF_flatnom2 = FF_new; FF        = [FF FF_new];
yielddiffnom = abs(400*(-Api(2000)/2000+Api(1000)/1000)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffnom)-0.25,0)).^2;
FF_flatnom3 = FF_new; FF        = [FF FF_new];
yielddiffreal = abs(400*(-A(400)/400+A(200)/200)); % abs. annual % difference in real yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffreal)-1,0)).^2;
FF_flatreal  = FF_new; FF        = [FF FF_new];
yielddiffreal = abs(400*(-A(800)/800+A(600)/600)); % abs. annual % difference in real yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffreal)-0.5,0)).^2;
FF_flatreal2  = FF_new; FF        = [FF FF_new];
yielddiffreal = abs(400*(-A(2000)/2000+A(1000)/1000)); % abs. annual % difference in real yield betw 100 and 50 year yield
FF_new    = tuningp*(max(abs(yielddiffreal)-0.25,0)).^2;
FF_flatreal3  = FF_new; FF        = [FF FF_new];
if mod(counts,100) == 0
   disp(['Violations of regularity conditions on very long-term yield:   ',num2str(sum([FF_posnom1,FF_posnom2,FF_posnom3,FF_posnom4,FF_posnom5,FF_posnom6,...
        FF_posreal1,FF_posreal2,FF_posreal3,FF_posreal4,...
        FF_real,FF_real2,FF_real3,FF_real4,FF_real5,...
        FF_flatnom,FF_flatnom2,FF_flatnom3,...
        FF_flatreal,FF_flatreal2,FF_flatreal3]),'%.1f')])  
end



%% good deal bounds
nomshortrate = y0nom_1 + I_y1'*X2'; % 1 by T
L = kron(L0,ones(1,T)) + L1*X2';    % N by T 
eps_struct = Sig\eps2';               % eps2 are VAR resids, they have covariance Sigma, eps_orig has covariance matrix Identity 
eps_struct = eps_struct';
eps_struct = [zeros(1,N);eps_struct];   % N by T; shocks in period 1 set to zero
mnom(1)  = -nomshortrate(1) - .5*L(:,1)'*L(:,1)-L(:,1)'*eps_struct(1,:)';
mreal(1) = mnom(1) +  pi0 + I_pi'*X2(1,:)';
for t =2 :T   
   mnom(t) = -nomshortrate(t-1) - .5*L(:,t-1)'*L(:,t-1) -L(:,t-1)'*eps_struct(t,:)';
   mreal(t) = mnom(t) + pi0 + I_pi'*X2(t,:)';   
end
FF_gooddeal = 100*(exp(max(std(mnom)-1.5,0))^2 -1);
FF      = [FF FF_gooddeal];
FF_keepLsmall = 10*sum(exp(max(sqrt(sum(L.^2))-3,0)).^2 -1);
FF      = [FF FF_keepLsmall];
if mod(counts,100) == 0
    disp(['Max SR violation:  ',num2str(FF_gooddeal,'%.1f')])    
    disp(['Small MPR violation:  ',num2str(FF_keepLsmall,'%.1f')])    
end

obj = nansum(abs(FF));

if mod(counts,100) == 0 
    disp(sum(abs(FF)))
end
counts = counts + 1;