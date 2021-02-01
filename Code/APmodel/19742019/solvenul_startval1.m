function obj = solvenul_startval1(x,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,I_pd_reit,I_divgr_reit,I_pd_infra,I_divgr_infra,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,A0_reit,k1_reit,r0_reit,mu_reit,A0_infra,k1_infra,r0_infra,mu_infra,PDm_strip_2q_data,PDm_strip_4q_data,PDm_strip_6q_data,PDm_strip_8q_data,sharestrip_2q_data,sharestrip_4q_data,sharestrip_6q_data,sharestrip_8q_data,taureal,yielddatareal)
global counts

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
L1(I_yspr==1,I_pdm==1)      = x(i);   


FF = [];

%% arbitrage-free definition of real risk-free rate
y0_1   =  y0nom_1 - pi0 - .5*I_pi'*(Sig*Sig')*I_pi + I_pi'*Sig*L0; 


%% No arbitrage restrictions on bond yields
A       = zeros(striphorizon,1); 
B       = zeros(N,striphorizon);
Api     = zeros(striphorizon,1); 
Bpi     = zeros(N,striphorizon);

A(1)    = - y0_1 ;  
B(:,1)  = -(I_y1'-I_pi'*Psi + I_pi'*Sig*L1)';

Api(1)  = -y0nom_1; 
Bpi(:,1)= -I_y1';

for j = 1:striphorizon-1
    Api(j+1)  = - y0nom_1 + Api(j) + .5*Bpi(:,j)'*(Sig*Sig')*Bpi(:,j)- Bpi(:,j)'*Sig*L0;
    Bpi(:,j+1)= (Bpi(:,j)'*Psi- I_y1'- Bpi(:,j)'*Sig*L1)';    

    A(j+1)    = - y0_1    + A(j)   + .5*B(:,j)'*(Sig*Sig')*B(:,j)    - B(:,j)'*Sig*(L0-Sig'*I_pi);
    B(:,j+1)  = ((I_pi + B(:,j))'*Psi - I_y1' - (I_pi+B(:,j))'*Sig*L1)';  
    
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


%% Conditions on the behavior of long-term interest rates (50 years +)
% Forcing the nominal bond yield to stay above 5.85% per year on average (nom. growth is 5.85%)
ynomlongmin = (x0+pi0)*400;
yreallongmin = (x0)*400;
tuningp = 100;
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
    disp('Violations of regularity conditions on very long-term yield')
    [FF_posnom1,FF_posnom2,FF_posnom3,FF_posnom4,FF_posnom5,FF_posnom6,...
        FF_posreal1,FF_posreal2,FF_posreal3,FF_posreal4,...
        FF_real,FF_real2,FF_real3,FF_real4,FF_real5,...
        FF_flatnom,FF_flatnom2,FF_flatnom3,...
        FF_flatreal,FF_flatreal2,FF_flatreal3]
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