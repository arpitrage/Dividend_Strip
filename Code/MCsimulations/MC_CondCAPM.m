%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MC simulations for the Conditional CAPM model (last paragraph of Appendix A.2)
%
% Theoretical validation program for main model
% Simulate cash flows for a panel of ficticious PE funds 
% Show that our method recovers the right exposures 
% Study properties of RAP, VAsdf, and IRR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;clc;close all;
dispirr = 0; % set to 1 if you want to compute fund IRRs as well (is slow)
printfigs =1; % set to 1 if you want to save the figures

%% Load results from AP model
load ../APmodel/CondCAPM/APoutputMCinput_CondCAPM

%% Set fund panel parameters
H = 64; % Maximum cash-flow distribution horizon
striphorizon=H;
Nf = 60;   % (average) number of funds per vintage
Nv = 100;  % number of complete vintages (with H cash flows)
TT = Nv+H-1; % time series dimension of the data, e.g. SDF or Div
S = Nf*Nv;
% Total cash distributed over life of the fund
%totcash = 1.28; % for Nv=100
totcash = 1.28;

%% Set fund cash flow profiles and preliminary check on cost of portfolio
% Profile 3: some bond, stock div, and stock gain exposure
% Horizon profile
beta3a = ones(1,H);
beta3a = beta3a*(totcash/3)/sum(beta3a);
beta3b = 1024 - 1*([1:H]-32).^2;
beta3b = beta3b*(totcash/3)/sum(beta3b);
beta3c = linspace(0,0.10,H);
beta3c = beta3c*(totcash/3)/sum(beta3c);
beta3 = [beta3a,beta3b,beta3c]; % H by K

% vintage shifters
a3a = [-.005,0,0.005,0.01];
a3b = [.025,0,-0.01,-0.015]; 
a3c = [0,0,0,0.005];
a3  = [a3a;a3b;a3c];

% true abnormal profit = extra cash flow generated for zero risk
rng(56);
c3 = 0.00+ 0.00*randn(S,1);%ones(S,1)*0.00;
% c3 = 0.001+ 0.002*randn(S,1);%ones(S,1)*0.00;

% idiosyncratic fund cash-flows are assumed to be iid, mean-zero
rng(12); 
idiovol = 0.02; % quarterly idiosyncratic cash flow volatility
v =idiovol*randn(S,H); % normally distributed with stdev idiovol
v = v - kron(mean(v,2),ones(1,H)); % make sure that idiosyncratic shocks are mean zero for each fund-spell 

%% Simulate PE cash flow panel and VAR, SDF, and listed CFs
% define simulation objects
PECF3 = ones(S,H);
vasdf3 = zeros(S,1);
irr3   = zeros(S,1);

% strucural VAR shocks,     
rng(2);
eps_struct=randn(N,TT+1);
eps_struct = (eps_struct - kron(mean(eps_struct,2),ones(1,TT+1)))./kron(std(eps_struct,0,2),ones(1,TT+1)); % make sure that idiosyncratic shocks are mean-zero, stdev-one for each fund 

% Form the VAR, initialized at its unconditional mean of zero in period 1
Z = zeros(N,TT+1); % N by H+1
for t=2:TT+1
    Z(:,t)       = Psi*Z(:,t-1) + Sig*eps_struct(:,t);
end
L             = L0 + L1*Z; % MPRs 
nomshortrate  = y0nom_1+I_y1'*Z; % nominal short rate at end of last period
sdf           = -nomshortrate(1:TT) - .5*sum(L(:,1:TT).*L(:,1:TT)) -sum(L(:,1:TT).*eps_struct(:,2:TT+1));
SDF           = exp(sdf);
divgr_stock   = mu_m + pi0 + (I_divgrm+I_pi)'*Z;
Div_stock     = exp(cumsum(divgr_stock)); % aggregate stock market dividend in levels, normalized to 1 in period 0
pdm           = A0m+I_pdm'*Z; 
PDm           = exp(pdm);
pdm_break1    = prctile(pdm,25);
pdm_break2    = prctile(pdm,50);
pdm_break3    = prctile(pdm,75);
index_vint    = 1+(pdm>pdm_break1) + (pdm>pdm_break2) + (pdm>pdm_break3);

StripHor = H; 
zcbprices                 = exp(kron((Api(1:StripHor)')',ones(1,TT+1)) +Bpi(:,1:StripHor)'*Z);
divstripprices_stock      = exp(kron((Am(1:StripHor)')',ones(1,TT+1))+Bm(:,1:StripHor)'*Z);
Div_gain_stock = PDm.*Div_stock; % These are the cash flows on the gain strips
divgainstripprices_stock = (kron(ones(H,1),PDm)-cumsum(divstripprices_stock))./kron(ones(H,1),PDm); % These are the time-t "prices" of the gain strips, size is HxT

% Construct the cost of the replicating portfolio/PE cash flow stream
trueprice3  = sum((kron(ones(H,1),a3a(index_vint))+kron(beta3a',ones(1,TT+1))).*zcbprices)+...
    sum((kron(ones(H,1),a3b(index_vint))+kron(beta3b',ones(1,TT+1))).*divstripprices_stock)+...
    sum((kron(ones(H,1),a3c(index_vint))+kron(beta3c',ones(1,TT+1))).*divgainstripprices_stock);

disp(['True cost of the portfolio averaged over all vintages:  ',num2str(mean(trueprice3(1:Nv)),'%.2f')])

for s = 1:S % loop over funds s 
    vv         = floor((s-1)/Nf)+1; % vintage indicator vv (starts at 1 and goes through Nv)
    indexvv    = 1+(pdm(vv)>pdm_break1) + (pdm(vv)>pdm_break2) + (pdm(vv)>pdm_break3);

    % form the actual true PE fund cash flows
    PECF3(s,:) = c3(s)+...
        (a3a(indexvv)+beta3a) + ...
        (a3b(indexvv)+ beta3b).*(Div_stock(vv+1:vv+H)/Div_stock(vv)) +...
        (a3c(indexvv)+ beta3c).*(Div_gain_stock(vv+1:vv+H)/Div_gain_stock(vv)) +...
        +v(s,:); 

    % Compute GPME
    vasdf3(s)   = sum(cumprod(SDF(vv:vv+H-1)).*PECF3(s,:)) -trueprice3(vv);
    
    % Compute IRR
    if dispirr ==1
        irr3(s)  = irr([-trueprice3(vv),PECF3(s,:)]);
    end   
end

%% Now estimate back the exposure coefficients from the PE fund panel data
ZCBmat2      = ones(S,H);
Divmatstock2 = zeros(S,H);
Divmatgainstock2 = zeros(S,H);
Horizonmat2  = ones(S,H);
V1mat2  = ones(S,H);
V2mat2  = ones(S,H);
V3mat2  = ones(S,H);
V4mat2  = ones(S,H);

% create vintage indicator variables
indexvintmat = reshape(repmat(index_vint(1:Nv),Nf,1),Nf*Nv,1);
vintbin1     = (indexvintmat==1);
vintbin2     = (indexvintmat==2);
vintbin3     = (indexvintmat==3);
vintbin4     = (indexvintmat==4);

for h = 1:H
    ZCBmat       = reshape(repmat(zcbprices(h,h:h+Nv-1),Nf,1),Nf*Nv,1);
    Divmatstock  = reshape(repmat(Div_stock(h+1:h+Nv)./Div_stock(1:Nv),Nf,1),Nf*Nv,1);
    Divmatgainstock  = reshape(repmat(Div_gain_stock(h+1:h+Nv)./Div_gain_stock(1:Nv),Nf,1),Nf*Nv,1);
    Horizonmat   = reshape(repmat(h*ones(1,Nv),Nf,1),Nf*Nv,1);
    
    % form S x H matrix of CF horizons
    Horizonmat2(:,h) = Horizonmat;      
    V1mat2(:,h) = vintbin1;
    V2mat2(:,h) = vintbin2;
    V3mat2(:,h) = vintbin3;
    V4mat2(:,h) = vintbin4;
    
    % form S x H matrix of listed CF realizations
    ZCBmat2(:,h) = ZCBmat;
    Divmatstock2(:,h) = Divmatstock;
    Divmatgainstock2(:,h) = Divmatgainstock;
%     Divmatreit2(:,h)  = Divmatreit;       
end
xx2 = reshape(Divmatstock2,S*H,1);
xx3 = reshape(Divmatgainstock2,S*H,1);
zz  = reshape(Horizonmat2,S*H,1);
vv1  = reshape(V1mat2,S*H,1);
vv2  = reshape(V2mat2,S*H,1);
vv3  = reshape(V3mat2,S*H,1);
vv4  = reshape(V4mat2,S*H,1);

ZZ  = zeros(S*H,H);
for h =1:H
    ZZ(:,h) = (zz==h); % Horizon dummies
end
% Cannot include all 4 vintage dummies. We normalize the second vintage 
% dummy effect to zero. Drop that one. 
XX = [ZZ  ZZ.*xx2 ZZ.*xx3 vv1 vv3 vv4 vv1.*xx2 vv3.*xx2 vv4.*xx2 vv1.*xx3 vv3.*xx3 vv4.*xx3];
yy = reshape(PECF3,S*H,1);
b3est = (XX'*XX)\(XX'*yy);
beta3est = reshape(b3est(1:3*H),H,3);
a3aest = [b3est(3*H+1),0,b3est(3*H+2),b3est(3*H+3)];
a3best = [b3est(3*H+4),0,b3est(3*H+5),b3est(3*H+6)];
a3cest = [b3est(3*H+7),0,b3est(3*H+8),b3est(3*H+9)];
resids3 = yy - XX*b3est;
Rsq3 = 1 - (sum(resids3.^2)/sum((yy-mean(yy)).^2));


%% Build RAP
% part of the PE cash flow not accounted for by strip exposure
resids3 = reshape(resids3,S,H);%PECF3 - kron(ones(S,1),beta3est(:,1)')- Divmat2.*kron(ones(S,1),beta3est(:,2)');
% true cost of the portfolio = initial outlay on buying the PE fund
trueprice3mat = reshape(repmat(trueprice3(1:Nv),Nf,1),Nf*Nv,1);
% estimated cost of the portfolio
stripcostest3  = sum((kron(ones(H,1),a3aest(index_vint(1:Nv)))+kron(beta3est(:,1),ones(1,Nv))).*zcbprices(:,1:Nv))+...
    sum((kron(ones(H,1),a3best(index_vint(1:Nv)))+kron(beta3est(:,2),ones(1,Nv))).*divstripprices_stock(:,1:Nv))+...
    sum((kron(ones(H,1),a3cest(index_vint(1:Nv)))+kron(beta3est(:,3),ones(1,Nv))).*divgainstripprices_stock(:,1:Nv));

% Calculate risk-adjusted profit
RAP3a = sum(resids3.*ZCBmat2,2);
RAP3b = reshape(repmat(stripcostest3,Nf,1),Nf*Nv,1)-trueprice3mat; 
RAP3 = RAP3a+RAP3b;


%% Report simulation results
disp('---------------------------')
disp(['mean VAsdf:   ',num2str(mean(vasdf3),'%.2f')])
disp(['stdev VAsdf:   ',num2str(std(vasdf3),'%.2f')])
disp(['min VAsdf:   ',num2str(min(vasdf3),'%.2f')])
disp(['5th percentile VAsdf:   ',num2str(prctile(vasdf3,5),'%.2f')])
disp(['50th percentile VAsdf:   ',num2str(prctile(vasdf3,50),'%.2f')])
disp(['95th percentile VAsdf:   ',num2str(prctile(vasdf3,95),'%.2f')])
disp(['max VAsdf:   ',num2str(max(vasdf3),'%.2f')])
disp(['fraction of VAsdf realizations below 0:   ',num2str(sum(vasdf3<0)/S,'%.2f')])
if dispirr ==1
    disp('---------------------------')
    disp(['mean IRR:   ',num2str(mean(irr3),'%.3f')])
    disp(['stdev IRR:   ',num2str(std(irr3),'%.3f')])
    disp(['min IRR:   ',num2str(min(irr3),'%.3f')])
    disp(['5th percentile IRR:   ',num2str(prctile(irr3,5),'%.3f')])
    disp(['50th percentile IRR:   ',num2str(prctile(irr3,50),'%.3f')])
    disp(['95th percentile IRR:   ',num2str(prctile(irr3,95),'%.3f')])
    disp(['max IRR:   ',num2str(max(irr3),'%.3f')])
    disp(['fraction of IRR realizations below 0:   ',num2str(sum(irr3<0)/S,'%.2f')])
    disp('---------------------------')
end
disp('---------------------------')
disp('RAP statistics')
disp(['mean RAP:   ',num2str(mean(RAP3),'%.4f')])
disp(['stdev RAP:   ',num2str(std(RAP3),'%.3f')])
disp(['min RAP:   ',num2str(min(RAP3),'%.2f')])
disp(['5th percentile RAP:   ',num2str(prctile(RAP3,5),'%.3f')])
disp(['50th percentile RAP:   ',num2str(prctile(RAP3,50),'%.3f')])
disp(['95th percentile RAP:   ',num2str(prctile(RAP3,95),'%.3f')])
disp(['max RAP:   ',num2str(max(RAP3),'%.2f')])
disp(['fraction of RAP realizations below 0:   ',num2str(sum(RAP3<0)/S,'%.3f')])
disp('---------------------------')

disp(['R-squared from PE replicating portfolio regression in %:   ',num2str(Rsq3*100,'%.1f')])

% Plot true and estimated exposures: horizon effects and vintage effects
figure('units','normalized','outerposition',[0 0 1 1])
subplot(3,2,1);bar([beta3a' beta3est(:,1)]);title('Horizon bond exposure');xlim([0,65]);ylim([-0.014,0.03]);
subplot(3,2,2);bar([a3a' a3aest']);title('Vintage bond exposure');ylim([-0.01,0.02]);
subplot(3,2,3);bar([beta3b' beta3est(:,2)]);title('Horizon stock exposure');xlim([0,65]);ylim([-0.01,0.03]);
subplot(3,2,4);bar([a3b' a3best']);title('Vintage stock exposure');ylim([-0.014,0.03]);
subplot(3,2,5);bar([beta3c' beta3est(:,3)]);title('Horizon reit exposure');xlim([0,65]);ylim([-0.01,0.03]);
legend('Truth','Estimate','Location','NorthWest')
subplot(3,2,6);bar([a3c' a3cest']);title('Vintage reit exposure');ylim([-0.005,0.01]);
legend('Truth','Estimate','Location','NorthWest')
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('MCexposures_CondCAPM')
    print('MC_exposures_CondCAPM','-depsc')
    movefile('MC_exposures_CondCAPM.eps', '..\..\Figures');
end

% Plot VAsdf & RAP histogram
figure('units','normalized','outerposition',[0 0 1 1]);
index3=find(and(vasdf3>-1.75,vasdf3<1.5));
subplot(1,2,1); hist(vasdf3(index3),50);title('VAsdf distribution');xlim([-1.75,1.5])
line([0,0],[0,1200],'color','red')
subplot(1,2,2); hist(RAP3,50);title('RAP');
line([0,0],[0,450],'color','red')
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('VAsdfRAPhist_CondCAPM')
    print('VAsdfRAPhist_CondCAPM','-depsc')
    movefile('VAsdfRAPhist_CondCAPM.eps', '..\..\Figures');
end   


% Plot IRR (annualized) histogram
if dispirr ==1
    index3=find(and(irr3>-1.5,irr3<1));
    figure('units','normalized','outerposition',[0 0 1 1]);
    hist(4*irr3(index3),50);
    title('IRR (ann.) distribution');
    xlim([0,.20]);
    set(findall(gcf,'-property','FontSize'),'FontSize',24)
    if printfigs==1
        savefig('IRRhist_CondCAPM')
        print('IRRhist_CondCAPM','-depsc')
        movefile('IRRhist_CondCAPM.eps', '..\..\..\Figures');
    end   
end