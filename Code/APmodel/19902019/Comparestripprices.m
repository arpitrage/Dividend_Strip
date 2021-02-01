% Compute correlation of strip prices from two AP models:
% 1. AP model estimated on sample 1974-2019
% 2. AP model estimated on the sample 1990-2019

clear; clc; close all;

load APoutputPEinput_19902019 
%             zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
%             Div_cohort_stock Div_cohort_reit Div_cohort_infra Div_cohort_small Div_cohort_large Div_cohort_growth Div_cohort_value Div_cohort_NR... 
%             Div_Pricestrip_cohort_stock Div_Pricestrip_cohort_reit Div_Pricestrip_cohort_infra Div_Pricestrip_cohort_small Div_Pricestrip_cohort_growth Div_Pricestrip_cohort_NR Div_Pricestrip_cohort_value...
%             Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value...
%             stripexpret_bond stripexpret_stock stripexpret_reit stripexpret_infra stripexpret_small stripexpret_growth stripexpret_NR stripexpret_value...
%             stripexpret_capgain_stock stripexpret_capgain_reit stripexpret_capgain_infra stripexpret_capgain_small stripexpret_capgain_growth stripexpret_capgain_NR  stripexpret_capgain_value...
%             pdm fwdrates60 discrates60
        
strippricemat_short =[zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
    Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value];        

clear zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
            Div_cohort_stock Div_cohort_reit Div_cohort_infra Div_cohort_small Div_cohort_large Div_cohort_growth Div_cohort_value Div_cohort_NR... 
            Div_Pricestrip_cohort_stock Div_Pricestrip_cohort_reit Div_Pricestrip_cohort_infra Div_Pricestrip_cohort_small Div_Pricestrip_cohort_growth Div_Pricestrip_cohort_NR Div_Pricestrip_cohort_value...
            Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value...
            stripexpret_bond stripexpret_stock stripexpret_reit stripexpret_infra stripexpret_small stripexpret_growth stripexpret_NR stripexpret_value...
            stripexpret_capgain_stock stripexpret_capgain_reit stripexpret_capgain_infra stripexpret_capgain_small stripexpret_capgain_growth stripexpret_capgain_NR  stripexpret_capgain_value...
            pdm fwdrates60 discrates60;
        
load ../19742019/APoutputPEinput

strippricemat_long =[zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
    Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value];        

clear zcbprices divstripprices_stock divstripprices_reit divstripprices_infra divstripprices_small divstripprices_growth divstripprices_NR divstripprices_value...
            Div_cohort_stock Div_cohort_reit Div_cohort_infra Div_cohort_small Div_cohort_large Div_cohort_growth Div_cohort_value Div_cohort_NR... 
            Div_Pricestrip_cohort_stock Div_Pricestrip_cohort_reit Div_Pricestrip_cohort_infra Div_Pricestrip_cohort_small Div_Pricestrip_cohort_growth Div_Pricestrip_cohort_NR Div_Pricestrip_cohort_value...
            Price_Pricestrip_cohort_stock Price_Pricestrip_cohort_reit Price_Pricestrip_cohort_infra Price_Pricestrip_cohort_small Price_Pricestrip_cohort_growth Price_Pricestrip_cohort_NR Price_Pricestrip_cohort_value...
            stripexpret_bond stripexpret_stock stripexpret_reit stripexpret_infra stripexpret_small stripexpret_growth stripexpret_NR stripexpret_value...
            stripexpret_capgain_stock stripexpret_capgain_reit stripexpret_capgain_infra stripexpret_capgain_small stripexpret_capgain_growth stripexpret_capgain_NR  stripexpret_capgain_value...
            pdm fwdrates60 discrates60;
        
% focus on common sample 1990-2019       
strippricemat_long = strippricemat_long(65:end,:);    

% the two strip price matrices now have dimension 120 by 960
I = size(strippricemat_long,2);
H = 64;
meanCorr=[];medianCorr=[];minCorr=[];maxCorr=[];
for i = 1:I
    aux = nancorr2(strippricemat_long(:,i),strippricemat_short(:,i));
    Corrmat(i) = aux;
    if rem(i,H)==0
        meanCorr = [meanCorr;mean(Corrmat(i-H+1:i))];
        medianCorr = [medianCorr;median(Corrmat(i-H+1:i))];
        minCorr = [minCorr;min(Corrmat(i-H+1:i))];
        maxCorr = [maxCorr;max(Corrmat(i-H+1:i))];
    end
end