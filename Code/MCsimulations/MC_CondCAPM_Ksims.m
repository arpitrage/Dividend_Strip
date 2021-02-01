%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MC simulations for the Conditional CAPM model 
%
% This file does a loop over Monte Carlo simulations to show 
% the properties of the cumulative SDF as well as to create a histogram of
% mean and median VAsdf 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; clc; close all;
printfigs=1; % if you want to save the figures

%% Load results from AP model
load ../APmodel/CondCAPM/APoutputMCinput_CondCAPM

%% Set number of iterations
KK = 5000;

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

%% Simulate PE cash flow panel and VAR, SDF, and listed CFs
% define simulation objects

for kk = 1:KK

    PECF3 = ones(S,H);
    gpme3 = zeros(S,1);

    % idiosyncratic fund cash-flows are assumed to be iid, mean-zero
    rng(12); 
    idiovol = 0.02; % quarterly idiosyncratic cash flow volatility
    v =idiovol*randn(S,H); % normally distributed with stdev idiovol
    % v =idiovol*(rand(S,H)-0.5); % uniformly distributed with idiovol controlling the largest and smallest values
    v = v - kron(mean(v,2),ones(1,H)); % make sure that idiosyncratic shocks are mean zero for each fund-spell 

    % strucural VAR shocks, 
    % here, we don't want to fix the seed so we are actually drawing different epsilon shocks in each of the KK simulations
    rng(kk);
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
    sdfrn         = -nomshortrate(1:TT) ;
    SDF           = exp(sdf);
    SDFrn        = exp(sdfrn);
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


    % These are the "dividends" on the "price strips" P_t+h/P_t
    Div_gain_stock = PDm.*Div_stock;
    % These are the time-t "prices" of the "price strips" P_t+h/P_t, size is HxT
    divgainstripprices_stock = (kron(ones(H,1),PDm)-cumsum(divstripprices_stock))./kron(ones(H,1),PDm);


    % Construct the cost of the replicating portfolio/PE cash flow stream
    trueprice3  = sum((kron(ones(H,1),a3a(index_vint))+kron(beta3a',ones(1,TT+1))).*zcbprices)+...
        sum((kron(ones(H,1),a3b(index_vint))+kron(beta3b',ones(1,TT+1))).*divstripprices_stock)+...
        sum((kron(ones(H,1),a3c(index_vint))+kron(beta3c',ones(1,TT+1))).*divgainstripprices_stock);

    if kk==1
        disp(['True cost of the portfolio averaged over all vintages:  ',num2str(mean(trueprice3(1:Nv)),'%.2f')])
    end

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
        gpme3(s)   = sum(cumprod(SDF(vv:vv+H-1)).*PECF3(s,:)) -trueprice3(vv);
        if rem(s,Nf) == 0        
            cumulSDF(vv,:) = cumprod(SDF(vv:vv+H-1));
            cumulSDFrn(vv,:) = cumprod(SDFrn(vv:vv+H-1));
            bondpricingerror(vv,1:H) = cumulSDF(vv,1:H)-zcbprices(1:H,vv)';
        end
    end

    meanSDF(kk) = mean(SDF(1:Nv));
    meancumulSDF(kk,:) = mean(cumulSDF);
    meancumulSDFrn(kk,:) = mean(cumulSDFrn);
    meanbondprice(kk,:)= mean(zcbprices,2)';
    meanbondpricingerror(kk,:)= mean(bondpricingerror);
    meanVAsdf(kk) = mean(gpme3);
    medianVAsdf(kk) = prctile(gpme3,50);
end


figure('units','normalized','outerposition',[0 0 1 1]);
mybins = [-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-.4,-.3,-.2,-.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,3];
subplot(1,2,1); hist(meanVAsdf,mybins);title('Mean VAsdf');xlim([-1.25,2]);
line([0,0],[0,1200],'color','red')
subplot(1,2,2); hist(medianVAsdf,mybins);title('Median VAsdf');
line([0,0],[0,3500],'color','red')
if printfigs==1
    savefig('MC_MeanMedianVAsdf_K')
    print('MC_MeanMedianVAsdf_K','-depsc')
    movefile('MC_MeanMedianVAsdf_K.eps', '..\..\Figures');
end

% Properties of the SDF
figure('units','normalized','outerposition',[0 0 1 1]);
mybins = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.25,2.50,2.75,3,3.5,4,4.5,5,7.5,10,15,20];
subplot(3,2,1);hist(meanSDF,mybins);title('Mean 1-qtr SDF');xlim([0 3]);line([mean(meanSDF),mean(meanSDF)],[0,KK],'color','red');
subplot(3,2,2);hist(meancumulSDF(:,4),mybins);title('Mean 4-qtr SDF');xlim([0 3]);line([mean(meancumulSDF(:,4)),mean(meancumulSDF(:,4))],[0,2*KK/5],'color','red');
subplot(3,2,3);hist(meancumulSDF(:,8),mybins);title('Mean 8-qtr SDF');xlim([0 3]);line([mean(meancumulSDF(:,8)),mean(meancumulSDF(:,8))],[0,KK/5],'color','red');
subplot(3,2,4);hist(meancumulSDF(:,20),mybins);title('Mean 20-qtr SDF');xlim([0 3]);line([mean(meancumulSDF(:,20)),mean(meancumulSDF(:,20))],[0,4*KK/5],'color','red');
subplot(3,2,5);hist(meancumulSDF(:,40),mybins);title('Mean 40-qtr SDF');xlim([0 3]);line([mean(meancumulSDF(:,40)),mean(meancumulSDF(:,40))],[0,KK],'color','red');
subplot(3,2,6);hist(meancumulSDF(:,60),mybins);title('Mean 60-qtr SDF');xlim([0 3]);line([mean(meancumulSDF(:,50)),mean(meancumulSDF(:,60))],[0,KK],'color','red');
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('MC_CumSDF_K')
    print('MC_CumSDF_K','-depsc')
    movefile('MC_CumSDF_K.eps', '..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
mybins = [-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-.4,-.3,-.2,-.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,3];
subplot(3,2,1);hist(meanbondpricingerror(:,1),mybins);title('Mean pricing error 1-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,1)),mean(meanbondpricingerror(:,1))],[0,KK/2],'color','red');%line([x x], [y1 y2]); is the easy command;
subplot(3,2,2);hist(meanbondpricingerror(:,4),mybins);title('Mean pricing error 4-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,4)),mean(meanbondpricingerror(:,4))],[0,KK/2],'color','red');
subplot(3,2,3);hist(meanbondpricingerror(:,8),mybins);title('Mean pricing error 8-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,8)),mean(meanbondpricingerror(:,8))],[0,2*KK/5],'color','red');
subplot(3,2,4);hist(meanbondpricingerror(:,20),mybins);title('Mean pricing error 20-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,20)),mean(meanbondpricingerror(:,20))],[0,2*KK/5],'color','red');
subplot(3,2,5);hist(meanbondpricingerror(:,40),mybins);title('Mean pricing error 40-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,40)),mean(meanbondpricingerror(:,40))],[0,KK],'color','red');
subplot(3,2,6);hist(meanbondpricingerror(:,60),mybins);title('Mean pricing error 60-qtr bond');xlim([-1.2 1.2]);line([mean(meanbondpricingerror(:,60)),mean(meanbondpricingerror(:,60))],[0,KK],'color','red');
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('MC_Bondpricingerror_K')
    print('MC_Bondpricingerror_K','-depsc')
    movefile('MC_Bondpricingerror_K.eps', '..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
plot([meancumulSDF(1:5,:)' meanbondprice'])
title('mean SDF'); legend('sim K=1','sim K=2','sim K=3','sim K=4','sim K=5','true K=1','true K=2','true K=3','true K=4','true K=5');xlim([0,64]);
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('MC_SimulcumSDF_K')
    print('MC_SimulcumSDF_K','-depsc')
    movefile('MC_SimulcumSDF_K.eps', '..\..\Figures');
end

figure('units','normalized','outerposition',[0 0 1 1]);
plot([mean(meancumulSDF',2) mean(meancumulSDFrn',2) mean(meanbondprice',2)])
title('mean SDF'); legend('mean of simulations','mean of simulations RN-sdf','true mean');xlim([0,64]);
set(findall(gcf,'-property','FontSize'),'FontSize',24)
if printfigs==1
    savefig('MC_SimulmeancumSDF_K')
    print('MC_SimulmeancumSDF_K','-depsc')
    movefile('MC_SimulmeancumSDF_K.eps', '..\..\Figures');
end