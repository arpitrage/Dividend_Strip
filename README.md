# Dividend_Strip
Contains data and documentation for paper: "Valuing Private Equity Investments Strip by Strip" by Arpit Gupta and Stijn Van Nieuwerburgh

There are three major sections to the code. The first section is the asset pricing model. The second section is the Private Equity model. The second section takes the output from the first section. The third section contains the Monte Carlo simulation exercises reported in Appendix A of the paper. Also see a video tutorial at: https://youtu.be/NPp-JZLOZ8c, and associated code in the notebook folder.

---
## Paper and Code

The latest version of our paper and associated replication code is included in their respective directories. 


---
##  Section 1: Asset Pricing Model

<ins>Overview:</ins> The code for the asset pricing model starts from data on macro aggregates and listed cash flow growth rates and price-dividend ratios and estimates the market prices of risk in an exponentially-affine model of the Stochastic Discount Factor. Its outputs are measures of model fit, strip cash flows, strip prices, and strip expected returns.


<ins>Location of Files:</ins>  The directory Code/APmodel contains all the relevant files. The code is in matlab. There are two subdirectories. The folder Code/APmodel /19742019 estimates the AP model on a sample of quarterly data that spans the period from 1974.Q1 until 2019.Q4. this is the main set of results used in the paper. 

The folder Code/APmodel/19902019 contains the same files and structure, but estimates the model on a shorter period from 1990.Q1 until 2019.Q4. 

The folder Code/APmodel/CondCAPM contains the same files and structure, but estimates the Conditional CAPM model, a special case of our general model, on the sample from 1974.Q1 until 2019.Q4. we use this model in the Monte Carlo analysis.

<ins>List of Files:</ins>  
-	APmain.m
-	APloaddata.m
-	APdata.xlsx in the folder Data
-	Psimatrix.mat (produced by APstarval1.m)
-	To obtain good starting values for MPR estimation:
--	Step 1: APstartval1.m, solvenul_startval1.m, x_guess_1.mat (produced by APstartval1.m)
--	Step 2: APstartval2.m, solvenul_startval2.m, x_guess_2.mat (produced by APstartval2.m)
--	Step 3: APstartval3.m, solvenul_startval3.m, x_guess_3.mat (produced by APstartval3.m)
--	Step 4: APstartval4.m, solvenul_startval4.m, x_guess_4.mat (produced by APstartval4.m)
--	Step 5: APstartval5.m, solvenul_startval5.m, x_guess_5.mat (produced by APstartval5.m)
-	ols.m, tdis_inv.m, beta_inv.m, beta_pdf.m: auxiliary files used to run OLS regression to estimate VAR dynamics
-	cols.m: auxiliary file to count the number of columns in a data structure
-	matrix2latex.m: auxiliary file to write data into latex
-	add_functionals.m and shock_elasticity_affine.m: auxiliary file used for the shock price and shock exposure elasticity analysis
-	Conditional CAPM folder: CondCAPMmain.m is the main file to run the conditional CAPM


<ins>Details on file hierarchy and how to run the code:</ins> 

**APmain.m** is the main file that estimates the market prices of risk. It produces all the AP figures that are in the paper and creates the dividend and gain strip cash flow, price, and expected return data that is used as an input in the PE analysis. 

APmain calls the following files: **APloaddata.m**, **Psimatrix.mat**,** x_guess_main.mat** (or **x_guess_1.mat**, **x_guess_2.mat**, **x_guess_3.mat**, **x_guess_4.mat**, and **x_guess_5.mat** if not starting values are available yet), and **solvenul_main.m**. The file **solvenul_main.m** computes the function value that the MPR estimation minimizes. It calculates that function value from a set of bond and stock market moments.  The estimated market price of risk estimates are stored in the data file **x_guess_main.mat**.

The figures are produced by APmain and saved in the directory Figures as eps files. The code also saves several data structures in the Figures folder that will be used when producing the paper in latex: **L0mat_main.tex**, **L1mat_main.tex**, **Psimat_main.tex**, **Sigmat_main.tex**, and **TableLambda_main.tex**.
The main output file that contains all the AP output that is used as an input in the PE analysis is saved in the file **APoutputPEinput.mat**.
The last part of APmain.m performs the shock exposure and shock price elasticity analysis in Appendix D of the paper. This analysis uses the auxiliary files **add_functionals.m** and **shock_elasticity_affine.m**.

**APloaddata.m** loads in the aggregate data used in the asset pricing model estimation from APdata.xlsx in the folder Data. It sets up the VAR that describes the dynamics of the state variables, collected in the matrix X2.

**APstartval1.m** is the first file in a series of files that is used to produce good starting values for the main estimation in APmain. If one were to extend the time series or apply the model to a different country, then it would be wise to start over to re-estimate the market prices of risk. This would be the first file to run in that case. APstartval1 only uses some of bond market moments to pin down a subset of market price of risk parameters, associated with the innovations to the first four variables in the VAR. In the process it also produces the estimates for the VAR companion matrix Psi and the VAR residuals eps2 which are collected in the data structure **Psimatrix.mat**. APstartval1.m calls the file solvenul_startval1.m, which computes the function value that the MPR estimation minimizes. It calculates that function value from a set of bond market moments.  The resulting market price of risk estimates are stored in the data file x_guess_1.mat.

APstartval2.m is the second file in a series of files that is used to produce good starting values for the main estimation in APmain. It takes the MPR estimates from the first step as given by loading x_guess_1.mat and estimates a second set of MPR parameters associated with the fifth and sixth rows of the VAR, namely the price-dividend ratio and dividend growth rate for the aggregate stock market. APstartval2.m calls the file solvenul_startval2.m, which computes the function value that the estimation minimizes. It calculates that function value from a set of aggregate stock market moments.  The resulting market price of risk estimates are stored in the data file x_guess_2.mat.

APstartval3.m is the third file in a series of files that is used to produce good starting values for the main estimation in APmain. It takes the MPR estimates from the first and second steps as given by loading x_guess_1.mat and x_guess_2.mat, and estimates a set of MPR parameters associated with the eight and tenth rows of the VAR, namely the dividend growth rate for real estate and infrastructures stocks. APstartval3.m calls the file solvenul_startval3.m, which computes the function value that the estimation minimizes. It calculates that function value from a set of real estate and infrastructure stock market moments.  The resulting market price of risk estimates are stored in the data file x_guess_3.mat.

APstartval4.m is the fourth file in a series of files that is used to produce good starting values for the main estimation in APmain. It takes the MPR estimates from the first, second, and third steps as given by loading x_guess_1.mat, x_guess_2.mat, and x_guess_3.mat, and estimates a set of MPR parameters associated with the 12th and 14th rows of the VAR, namely the dividend growth rate for small and growth stocks. APstartval4.m calls the file solvenul_startval4.m, which computes the function value that the estimation minimizes. It calculates that function value from a set of small and value stock market moments.  The resulting market price of risk estimates are stored in the data file x_guess_4.mat.

APstartval5.m is the fifth and last file in a series of files that is used to produce good starting values for the main estimation in APmain. It takes the MPR estimates from the first through fourth steps as given by loading x_guess_1.mat, x_guess_2.mat, x_guess_3.mat, and x_guess_4.mat, and estimates a set of MPR parameters associated with the 16th and 18th rows of the VAR, namely the dividend growth rate for natural resource and value stocks. APstartval5.m calls the file solvenul_startval5.m, which computes the function value that the estimation minimizes. It calculates that function value from a set of natural resource and value stock market moments.  The resulting market price of risk estimates are stored in the data file x_guess_5.mat.
CondCAPMmain.m is the main file to run the conditional CAPM. It calls CondCAPM_loaddata.m, Psimatrix_ConCAPM.mat, x_guess_CondCAPM.mat, and solvenul_CAPM.m. The file produces the output in APoutputMCinput_CondCAPM.mat.



---
##  Section 2: PE Cash Flow Model

Overview: The code for the PE cash flow model takes as input the strip cash flows, strip prices and strip expected returns from the asset pricing model. It estimates the exposures of PE cash flows to the strip cash flows (the 2-factor model uses OLS and the 15-factor model uses Elastic Net) and calculates expected returns for each PE category-vintage pair as well as Risk-Adjusted Performance (RAP) for each individual PE fund, as well as for each category-vintage pair.

Inputs: Panel of private equity cash flow distribution data and outputs from the asset pricing model. 

This data is purchased from Preqin and cannot be shared. However, a fake data set of the same structure is created to demonstrate that the code runs and produces the figures and tables in the paper. We describe this process below. 

For robustness, we also perform the analysis on Burgiss data. Again, we are not at liberty to share this data. Researchers can apply to access this data through the Kenan Flagler Private Equity data Consortium.

Location of Files: The directory PEmodel contains all the relevant files. The main files are coded in R.


List of Files: 
-	1_divLoad.R
-	2_preqinLoad.R
-	3_linkData.R
-	4_sumstats.R
-	5_elastic_cv.R

Details on file hierarchy and how to run the code:
The code should be run sequentially, starting with 1_divLoad.R and and finishing with 5_elastic_cv.R. Any package dependencies are listed at the top of each file (files were created under R version 4.0.0).
1_divLoad.R — This file loads the APoutputPEinput.mat matrix of data from the Asset Pricing Model. It transforms this data into .Rda format to be consistent with our record for PE funds, separated into four sub-files: 1) DividendStripOct20.Rda, which contains cash flow and price information on dividend and capital gains strips, 2) ExpectedReturnOct20.Rda, which contains expected returns for these strips, 3) DiscountRatesOct20.Rda, which includes forward rates beyond 60 months, and 3) States.Rda which includes price/dividend ratios (divided into quartiles).
 2_preqinLoad.R — Loads our Preqin data, including both raw cash flow data, as well as information on Preqin-estimated returns. Fund-leveld data come in the form of irregularly spaced distributions and call events, so this file converts the data into a panel suitable for regression (including 0s where appropriate). Fund cash flows after month 60 are also truncated, and discounted using the appropriate forward rate to the last full year of fund operation. Produces a file YearlyCashFlowOct20.Rda which is our core Preqin dataset. 
3_linkData.R — This file links our Preqin PE data with our dividend/capital gains strip data, taking care to match the timing across both sets of data. This file connects the four .Rda files produced in 1_divLoad.R with YearlyCashFlowOct20.Rda to produce a file used for analysis: MergedCashFlowOct20.Rda.
4_sumstats.R — Produces fund summary statistics for the number and size of funds over time, as well as fund distributions over time across category and horizon.
5_elastic_cv.R — This is the core file producing our analysis. This file is divided into a few sections. First, the file loads the PE and strip data mentioned above. Next, the file performs some minor cleaning to focus on funds after 1981 and address late in life cash flows in Infrastructure funds (cash flows beyond year 13 are collapsed to before). Next, we outline a number of helper functions used in the subsequent steps of the analysis. After this step, we run our analysis separately for each fund category. Within each category, we:


1.	Subset funds which meet the category definition.
2.	Transform data into matrix format for subsequent model fitting.
3.	Run our standard 2-factor OLS model.
4.	Run our full-factor Elastic Net model. We use the cva.glmnet function from the glmnet package using 10 folds, and subsequently extract the best-fit model.
5.	We extract coefficients from our model estimate (using a helper function).
6.	We produce a complete time-series of our sample period, drawing on our AP model, and combine with our coefficient estimates to produce estimates of: portfolio weights and expected returns. 
7.	We estimate profits using fitted cash flows compared with realized cash flows. 
8.	We plot estimates within each fund category.

Finally, we also create additional figures and tables which combine these estimates across all fund categories.
6_elastic_cv.R — This file generates the cross-validatation tests found in Appendix G. These include Table G1, which shows our hyperparameters across fund category, Figure G1, which shows the variation in coefficients as we adjust our lambda parameter, Figure G2, which shows variation in coefficients as we adjust alpha, Figure G3, which shows how our MSE varies across combinations of alpha and lambda, and Figure G4 which shows how our RAP estimates also vary across alpha and lambda.
Synthetic Data Creation:
Code: The directory “Synthetic” contains the following code, which should be run in order:
1_synthetic_gen.R — This generates synthetic data using the “synthpop” package in R. 
2_linkData.R — This links synthetic data to our dividend strip data.
3_sumstats.R, 4_elastic_cv.R — As with the code for the real data, these analysis do summary statistics and core model analysis for our synthetic data. 
Data: Our synthetic data can be found under /Synthetic Data, saved as MergedCashFlowOct20.Rda.


---
## Section 3: Monte Carlo Simulation Exercise

Overview: A third set of files produce the Monte Carlo simulations reported in Appendix A of the paper.
Location of Files: The directory Code/MCsimulations contains all the relevant files. The main files are coded in matlab.
List of Files: 
•	MCmain.m
•	MC_CondCAPM.m
•	MC_CondCAPM_Ksims.m

Details on file hierarchy and how to run the code:
The file MCmain.m is the main file. It reads in the output from the asset pricing model APoutputMCinput.mat. It simulates a panel of artificial PE cash flows driven by three listed cash flow factors (bonds, market dividend strips, and market gain strips). It also simulates a time series of the SDF, strip cash flows, and strip prices consistent with that SDF. It computes the VAsdf statistic, IRR statistic, and RAP statistic for each simulated fund. It then estimates back the exposures of the funds to the risk factors to assess how well the estimation recovers the true exposures. The ouput is saved in the form of three figures in the folder Figures.

MC_CondCAPM.m repeats the same analysis but for the Conditional CAPM rather than for the main model. It pulls results from the Conditional CAPM model stored in APoutputMCinput_CondCAPM.mat.

MC_CondCAPM_Ksims.m repeats the MC analysis for the Conditional CAPM many times (K times) and keeps track of the mean and median VAsdf and mean of the cumulative SDF across these K iterations.



