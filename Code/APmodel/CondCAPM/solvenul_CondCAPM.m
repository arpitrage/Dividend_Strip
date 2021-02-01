function obj = solvenul_CondCAPM(x,N,Psi,Sig,I_pi,I_gdp,I_y1,I_yspr,I_pdm,I_divgrm,striphorizon,y0nom_1,y0nom_20,x0,pi0,X2,tau,yielddata,T,eps2,A0m,k1m,r0_m,mu_m,PDm_strip_8q_data,sharestrip_8q_data,taureal,yielddatareal)
global counts 

L0 = zeros(N,1);
L1 = zeros(N,N);

i=1;

L0(I_divgrm==1)        = x(i);i=i+1;
L1(I_divgrm==1,I_pi==1)             = x(i); i = i+1;
L1(I_divgrm==1,I_y1==1)             = x(i); i = i+1;
L1(I_divgrm==1,I_pdm==1)            = x(i); i = i+1;
L1(I_divgrm==1,I_divgrm==1)         = x(i); 

FF = [];

%% arbitrage-free definition of real risk-free rate
y0_1   =  y0nom_1 - pi0 - .5*I_pi'*(Sig*Sig')*I_pi + I_pi'*Sig*L0; 

%% No arbitrage restrictions on CRSP vw stock return
aa = (I_divgrm+k1m*I_pdm+I_pi)'*Sig;
cc = ((I_divgrm+k1m*I_pdm+I_pi)'*Psi -I_pdm' -I_y1');
FF_stock=[];
FF_new = 400*(L0(find(I_divgrm==1))  - (((r0_m + pi0 - y0nom_1 + 0.5*(aa)*(aa)')-aa(1:5)*L0(1:5))/aa(6)));                     
FF = [FF FF_new]; FF_stock=[FF_stock FF_new];    
FF_new = 10*(L1(find(I_divgrm==1),:)    -((cc-aa(1:5)*L1(1:5,:))/aa(6))) ; 
FF = [FF FF_new]; FF_stock=[FF_stock FF_new];    
if mod(counts,100) == 0
    disp(['stock exp. return violation:  ',num2str(nansum(abs(FF_stock)))])
end


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

for j = 1:striphorizon-1
    Api(j+1)  = - y0nom_1 + Api(j) + .5*Bpi(:,j)'*(Sig*Sig')*Bpi(:,j)- Bpi(:,j)'*Sig*L0;
    Bpi(:,j+1)= (Bpi(:,j)'*Psi- I_y1'- Bpi(:,j)'*Sig*L1)';    

    A(j+1)    = - y0_1    + A(j)   + .5*B(:,j)'*(Sig*Sig')*B(:,j)    - B(:,j)'*Sig*(L0-Sig'*I_pi);
    B(:,j+1)  = ((I_pi + B(:,j))'*Psi - I_y1' - (I_pi+B(:,j))'*Sig*L1)';  
    
    Am(j+1)        = Am(j) + mu_m - y0_1 + .5*(I_divgrm +Bm(:,j))'*Sig*Sig'*(I_divgrm+Bm(:,j)) -(I_divgrm+Bm(:,j))'*Sig*(L0-Sig'*I_pi); 
    Bm(:,j+1)      = ((I_divgrm+I_pi+Bm(:,j))'*Psi - I_y1'  - (I_divgrm+I_pi+Bm(:,j))'*Sig*L1)';
    PDm_model      = PDm_model+exp(Am(j+1)+Bm(:,j+1)'*X2')';
    
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
% FF        = [FF FF_futstripportfret];
% if mod(counts,100) == 0
%     disp(['Futures strip risk premium violation:  ',num2str(FF_futstripportfret,'%.1f')])
% end

%% Match level of the PD ratio in model and data
FF_new    = nansum((exp(A0m+I_pdm'*X2'-log(4))' - PDm_model/4).^2);
% FF        = [FF FF_new];
% if mod(counts,100) == 0
%     disp(['PDm violation:  ',num2str(FF_new,'%.1f')])
% end


%% Insist on matching the 5-year yield more closely since it is in the state space
% FF_newa    = 40000*abs(-Api(20)/20 - y0nom_20);
% FF        = [FF FF_newa];
% FF_newb    = 4000*abs(-Bpi(1,20)/20 - 0);
% FF        = [FF FF_newb];
% FF_newc    = 4000*abs(-Bpi(2,20)/20 - 0);
% FF        = [FF FF_newc];
% FF_newd    = 4000*abs(-Bpi(3,20)/20 - 1);
% FF        = [FF FF_newd];
% FF_newe    = 4000*abs(-Bpi(4,20)/20 - 1);
% FF        = [FF FF_newe];
% FF_newf    = 4000*abs(-Bpi(5,20)/20 - 0);
% FF        = [FF FF_newf];
% FF_newg    = 4000*abs(-Bpi(6,20)/20 - 0);
% FF        = [FF FF_newg];
% if mod(counts,100) == 0
%     disp(['20-qtr bond yield violation:  ',num2str(FF_newa+FF_newb+FF_newc+FF_newd+FF_newe+FF_newf+FF_newg)])
% end

%% Match Nominal Yield Curve
% Pricing nominal bond yields of maturities stored in tau
FF_new    = nansum((400*(kron(ones(length(X2),1),-Api(tau)'./tau) -((Bpi(:,tau)'./kron(tau',ones(1,N)))*X2')' - yielddata)).^2);
% FF        = [FF FF_new];
% if mod(counts,100) == 0
%     disp(['Nominal yield pricing errors:  ',num2str(sum(FF_new),'%.1f')])
% end


%% Match Real Yield Curve
% Pricing TIIS yields of maturities stored in taureal
FF_new    = nansum((400*(kron(ones(length(X2),1),-A(taureal)'./taureal) -((B(:,taureal)'./kron(taureal',ones(1,N)))*X2')' - yielddatareal)).^2);
% FF        = [FF FF_new];
% if mod(counts,100) == 0
%     disp(['Real yield pricing errors:  ',num2str(sum(FF_new),'%.1f')])
% end



% %% Match short horizon market strips PD ratios from Binsbergen, Brandt, and Koijen (AER)
% PDm_strip_2q = exp(Am(1)+Bm(:,1)'*X2')'+exp(Am(2)+Bm(:,2)'*X2')';
% PDm_strip_4q = PDm_strip_2q+ exp(Am(3)+Bm(:,3)'*X2')'+exp(Am(4)+Bm(:,4)'*X2')';
% PDm_strip_6q = PDm_strip_4q+ exp(Am(5)+Bm(:,5)'*X2')'+exp(Am(6)+Bm(:,6)'*X2')';
% PDm_strip_8q = PDm_strip_6q+ exp(Am(7)+Bm(:,7)'*X2')'+exp(Am(8)+Bm(:,8)'*X2')';
% 
% sharestrip_2q = PDm_strip_2q./PDm_model;
% sharestrip_4q = PDm_strip_4q./PDm_model;
% sharestrip_6q = PDm_strip_6q./PDm_model;
% sharestrip_8q = PDm_strip_8q./PDm_model;
% 
% % FF_strip2    = 10*nansum((PDm_strip_2q - PDm_strip_2q_data).^2);
% % FF_strip4    = 10*nansum((PDm_strip_4q - PDm_strip_4q_data).^2);
% % FF_strip6    = 10*nansum((PDm_strip_6q - PDm_strip_6q_data).^2);
% FF_strip8    = 10*nansum((PDm_strip_8q - PDm_strip_8q_data).^2);
% % FF_strip2b    = 10*nansum((100*(sharestrip_2q - sharestrip_2q_data)).^2);
% % FF_strip4b    = 10*nansum((100*(sharestrip_4q - sharestrip_4q_data)).^2);
% % FF_strip6b    = 10*nansum((100*(sharestrip_6q - sharestrip_6q_data)).^2);
% FF_strip8b    = 10*nansum((100*(sharestrip_8q - sharestrip_8q_data)).^2);
% 
% FF_strip     = FF_strip8;%FF_strip2+FF_strip4+FF_strip6+FF_strip8;
% FF_stripb    = FF_strip8b;%FF_strip2b+FF_strip4b+FF_strip6b+FF_strip8b;
% 
% FF           = [FF FF_strip FF_stripb];
% 
% if mod(counts,100) == 0
%     disp(['strip PD violation:  ',num2str(FF_strip,'%.1f')])
%     disp(['strip share violation:  ',num2str(FF_stripb,'%.1f')])
% end

% %% Conditions on the behavior of long-term interest rates (50 years +)
% %Forcing the nominal bond yield to stay above 5.85% per year on average (nom. growth is 5.85%)
% ynomlongmin = (x0+pi0)*400;
% yreallongmin = (x0)*400;
% tuningp = 10;
% FF_new    = (tuningp*abs(min(-400*Api(200)/200-ynomlongmin,0))).^2;
% FF_posnom1    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(400)/400-ynomlongmin,0))).^2;
% FF_posnom2    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(800)/800-ynomlongmin,0))).^2;
% FF_posnom3    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(1200)/1200-ynomlongmin,0))).^2;
% FF_posnom4    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(1600)/1600-ynomlongmin,0))).^2;
% FF_posnom5    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(2000)/2000-ynomlongmin,0))).^2;
% FF_posnom6    = FF_new; FF        = [FF FF_new];
% % Forcing the real yield to stay above real GDP growth on average
% FF_new    = (tuningp*abs(min(-400*A(200)/200-yreallongmin ,0))).^2;
% FF_posreal1    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*A(400)/400-yreallongmin,0))).^2;
% FF_posreal2    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*A(800)/800-yreallongmin,0))).^2;
% FF_posreal3    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*A(1200)/1200-yreallongmin,0))).^2;
% FF_posreal4    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*A(1600)/1600-yreallongmin,0))).^2;
% FF_posreal5    = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*A(2000)/2000-yreallongmin,0))).^2;
% FF_posreal6    = FF_new; FF        = [FF FF_new];
% % Forcing the real term structure to stay below the nominal one by at least 2%
% % uncond infl is 3.28% so this forces the inflation risk premium to be
% % greater than zero
% FF_new    = (tuningp*abs(min(-400*Api(200)/200+400*A(200)/200-2,0))).^2;
% FF_real   = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(400)/400+400*A(400)/400-2,0))).^2;
% FF_real2   = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(800)/800+400*A(800)/800-2,0))).^2;
% FF_real3   = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(1200)/1200+400*A(1200)/1200-2,0))).^2;
% FF_real4   = FF_new; FF        = [FF FF_new];
% FF_new    = (tuningp*abs(min(-400*Api(2000)/2000+400*A(2000)/2000-2,0))).^2;
% FF_real5   = FF_new; FF        = [FF FF_new];
% % Forcing the term structure to flatten out
% yielddiffnom = abs(400*(-Api(400)/400+Api(200)/200)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffnom)-1,0)).^2;
% FF_flatnom = FF_new; FF        = [FF FF_new];
% yielddiffnom = abs(400*(-Api(800)/800+Api(600)/600)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffnom)-0.5,0)).^2;
% FF_flatnom2 = FF_new; FF        = [FF FF_new];
% yielddiffnom = abs(400*(-Api(2000)/2000+Api(1000)/1000)); % abs. annual % difference in nominal yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffnom)-0.25,0)).^2;
% FF_flatnom3 = FF_new; FF        = [FF FF_new];
% yielddiffreal = abs(400*(-A(400)/400+A(200)/200)); % abs. annual % difference in real yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffreal)-1,0)).^2;
% FF_flatreal  = FF_new; FF        = [FF FF_new];
% yielddiffreal = abs(400*(-A(800)/800+A(600)/600)); % abs. annual % difference in real yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffreal)-0.5,0)).^2;
% FF_flatreal2  = FF_new; FF        = [FF FF_new];
% yielddiffreal = abs(400*(-A(2000)/2000+A(1000)/1000)); % abs. annual % difference in real yield betw 100 and 50 year yield
% FF_new    = tuningp*(max(abs(yielddiffreal)-0.25,0)).^2;
% FF_flatreal3  = FF_new; FF        = [FF FF_new];
% if mod(counts,100) == 0
%     disp(['Violations of regularity conditions on very long-term yield:   ',num2str(sum([FF_posnom1,FF_posnom2,FF_posnom3,FF_posnom4,FF_posnom5,FF_posnom6,...
%         FF_posreal1,FF_posreal2,FF_posreal3,FF_posreal4,FF_posreal5,FF_posreal6,...
%         FF_real,FF_real2,FF_real3,FF_real4,FF_real5,...
%         FF_flatnom,FF_flatnom2,FF_flatnom3,...
%         FF_flatreal,FF_flatreal2,FF_flatreal3]),'%.1f')])  
% end


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