rm(list = ls())

# install.packages("lme4")

library(lubridate)
library(lfe)
library(lme4)
library(R.matlab)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#library(qpcR)
library(statar)
library(tidyverse)
library(stargazer)
library(broom)
library(Hmisc)
library(hrbrthemes)
library(data.table)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}



# Dividend strip: cohort.series.quarterly
# Prices of those dividends: price.strip.quarterly.feb
# Capital gains strip dividends: dividend.strip.capital.gains
# Price of the capital gains strips: price.strip.capital.gains
# Expected Returns All: strip.return.quarterly
 
######################################### Standard Factors - Dividends  #########################################
cohort <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/cohortdivforAG_withXSd_Oct2019.mat")



Div.cohort.nr.quarterly = cohort$Div.cohort.NR   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.nr, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.nr)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.nr, -Age) %>% 
  mutate(cohort.nr = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Div.cohort.gdp.quarterly = cohort$GDP.cohort   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.gdp, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.gdp)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.gdp, -Age) %>% 
  mutate(cohort.gdp = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 
Div.cohort.reit.quarterly = cohort$Div.cohort.reit   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.reit, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.reit)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.reit, -Age) %>% 
  mutate(cohort.reit = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

 

Div.cohort.infra.quarterly = cohort$Div.cohort.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.infra, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.infra)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.infra, -Age) %>% 
  mutate(cohort.infra = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

 
Div.cohort.stock.quarterly = cohort$Div.cohort.stock   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.stock, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.stock)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.stock, -Age) %>% 
  mutate(cohort.stock = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

 


Div.cohort.small.quarterly = cohort$Div.cohort.small   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.small, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.small)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.small, -Age) %>% 
  mutate(cohort.small = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Div.cohort.large.quarterly = cohort$Div.cohort.large   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.large, V, 1:64) %>% 
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.large))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.large, -Age) %>% 
  mutate(cohort.large = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

 

Div.cohort.growth.quarterly = cohort$Div.cohort.growth   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.growth, V, 1:64) %>% 
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.growth))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.growth, -Age) %>% 
  mutate(cohort.growth = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 
Div.cohort.value.quarterly = cohort$Div.cohort.value   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.value, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.value))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.value, -Age) %>% 
  mutate(cohort.value = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 


Div.cohort.lowINV.quarterly = cohort$Div.cohort.lowINV   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.lowINV, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.lowINV))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.lowINV, -Age) %>% 
  mutate(cohort.lowINV = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 

Div.cohort.highINV.quarterly = cohort$Div.cohort.highINV   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.highINV, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.highINV))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.highINV, -Age) %>% 
  mutate(cohort.highINV = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 


Div.cohort.lowOP.quarterly = cohort$Div.cohort.lowOP   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.lowOP, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.lowOP))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.lowOP, -Age) %>% 
  mutate(cohort.lowOP = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 

Div.cohort.highOP.quarterly = cohort$Div.cohort.highOP   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.highOP, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.highOP))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.highOP, -Age) %>% 
  mutate(cohort.highOP = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)
 


# save everything in this at end
cohort.series.quarterly = left_join(Div.cohort.reit.quarterly, Div.cohort.infra.quarterly) %>% 
  left_join(Div.cohort.stock.quarterly) %>%
  left_join(Div.cohort.small.quarterly) %>%
  left_join(Div.cohort.large.quarterly) %>%
  left_join(Div.cohort.growth.quarterly) %>% 
  left_join(Div.cohort.value.quarterly) %>%
  left_join(Div.cohort.lowINV.quarterly) %>% 
  left_join(Div.cohort.highINV.quarterly) %>%
  left_join(Div.cohort.lowOP.quarterly) %>% 
  left_join(Div.cohort.highOP.quarterly) %>% 
  left_join(Div.cohort.gdp.quarterly) %>% 
  left_join(Div.cohort.nr.quarterly) %>% select(-age.quarter, -vintage.quarter) %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)

          


  









######################################### Standard Factors - Price  #########################################
dividend.strip.prices  <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/pricesforAG_withXSd_Oct2019.mat")

# Clean + Organize ZCB Matrix
price.nr.quarterly = dividend.strip.prices$divstripprices.NR %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.nr.feb, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.nr.feb))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.nr.feb, -Age) %>% 
  mutate(price.nr.feb = V) %>% select(-V)

price.small.quarterly = dividend.strip.prices$divstripprices.small %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.meq1.feb, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.meq1.feb))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.meq1.feb, -Age) %>% 
  mutate(price.meq1.feb = V) %>% select(-V)

price.infra.quarterly = dividend.strip.prices$divstripprices.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.infra, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.infra))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.infra, -Age) %>% 
  mutate(price.infra.feb = V) %>% select(-V)


price.reit.quarterly = dividend.strip.prices$divstripprices.reit %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.reit, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.reit))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.reit, -Age) %>% 
  mutate(price.reit.feb = V) %>% select(-V)


price.stock.quarterly = dividend.strip.prices$divstripprices.stock %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.stock, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.stock))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.stock, -Age) %>% 
  mutate(price.stock.feb = V) %>% select(-V)


price.bond.quarterly = dividend.strip.prices$zcbprices %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.bond, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.bond))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.bond, -Age) %>% 
  mutate(price.bond.feb = V) %>% select(-V)

price.growth.quarterly =dividend.strip.prices$divstripprices.growth %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.growth.feb, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.growth.feb))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.growth.feb, -Age) %>% 
  mutate(price.growth.feb = V) %>% select(-V)


price.value.quarterly =dividend.strip.prices$divstripprices.value %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.value.feb, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.value.feb))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -price.value.feb, -Age) %>% 
  mutate(price.value.feb = V) %>% select(-V)






price.small.quarterly = price.small.quarterly %>% select(-quarter) 
price.bond.quarterly = price.bond.quarterly %>% select(-quarter) 
price.stock.quarterly  = price.stock.quarterly %>% select(-quarter)
price.reit.quarterly = price.reit.quarterly %>% select(-quarter)
price.infra.quarterly = price.infra.quarterly %>% select(-quarter)
price.value.quarterly = price.value.quarterly %>% select(-quarter)

price.growth.quarterly = price.growth.quarterly %>% dplyr::select(-quarter)
price.nr.quarterly = price.nr.quarterly %>% dplyr::select(-quarter)



price.strip.quarterly.feb = left_join(price.bond.quarterly, price.small.quarterly) %>%
  left_join(price.reit.quarterly) %>% 
  left_join(price.infra.quarterly) %>%
  left_join(price.stock.quarterly) %>%
  left_join(price.nr.quarterly) %>% 
  left_join(price.growth.quarterly) %>%
  left_join(price.value.quarterly) %>%
  mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)




######################################### Gains Strips - Div  #########################################


capital.gains.strips <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/cohortPricestripsforAG_withXSd_Oct2019.mat")

Div.Pricestrip.cohort.growth = capital.gains.strips$Div.Pricestrip.cohort.growth  %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.growth, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.growth))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.growth, -Age) %>% 
  mutate(gain.cohort.growth = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Div.Pricestrip.cohort.infra = capital.gains.strips$Div.Pricestrip.cohort.infra  %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.infra, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.infra))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.infra, -Age) %>% 
  mutate(gain.cohort.infra = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Div.Pricestrip.cohort.reit = capital.gains.strips$Div.Pricestrip.cohort.reit %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.reit, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.reit))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.reit, -Age) %>% 
  mutate(gain.cohort.reit = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Div.Pricestrip.cohort.small = capital.gains.strips$Div.Pricestrip.cohort.small %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.small, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.small))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.small, -Age) %>% 
  mutate(gain.cohort.small = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Div.Pricestrip.cohort.nr = capital.gains.strips$Div.Pricestrip.cohort.NR %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(gain.cohort.nr, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", gain.cohort.nr))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -gain.cohort.nr, -Age) %>% 
  mutate(gain.cohort.nr = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)





Div.Pricestrip.cohort.stock = capital.gains.strips$Div.Pricestrip.cohort.stock %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.stock, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.stock))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.stock, -Age) %>% 
  mutate(gain.cohort.stock = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Div.Pricestrip.cohort.value = capital.gains.strips$Div.Pricestrip.cohort.value %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(gain.cohort.value, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", gain.cohort.value))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -gain.cohort.value, -Age) %>% 
  mutate(gain.cohort.value = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



# save everything in this at end
dividend.strip.capital.gains = left_join(Div.Pricestrip.cohort.growth, Div.Pricestrip.cohort.infra) %>% 
  left_join(Div.Pricestrip.cohort.reit) %>%
  left_join(Div.Pricestrip.cohort.small) %>%
  left_join(Div.Pricestrip.cohort.stock) %>%
  left_join(Div.Pricestrip.cohort.nr) %>%
  left_join(Div.Pricestrip.cohort.value) %>%
  select(-age.quarter, -vintage.quarter) %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)


#save(divided.strip.capital.gains, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/PriceGainApr19.Rda")



######################################### Gains Strips - Price  #########################################


Price.Pricestrip.cohort.growth = capital.gains.strips$Price.Pricestrip.cohort.growth %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.growth, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.growth))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.growth, -Age) %>% 
  mutate(price.cohort.growth = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Price.Pricestrip.cohort.infra = capital.gains.strips$Price.Pricestrip.cohort.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.infra, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.infra))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.infra, -Age) %>% 
  mutate(price.cohort.infra = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)




Price.Pricestrip.cohort.reit = capital.gains.strips$Price.Pricestrip.cohort.reit %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.reit, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.reit))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.reit, -Age) %>% 
  mutate(price.cohort.reit = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Price.Pricestrip.cohort.small = capital.gains.strips$Price.Pricestrip.cohort.small %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.small, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.small))  ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.small, -Age) %>% 
  mutate(price.cohort.small = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)



Price.Pricestrip.cohort.nr = capital.gains.strips$Price.Pricestrip.cohort.NR %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.nr, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.nr))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.nr, -Age) %>% 
  mutate(price.cohort.nr = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Price.Pricestrip.cohort.stock = capital.gains.strips$Price.Pricestrip.cohort.stock %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.stock, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.stock))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.stock, -Age) %>% 
  mutate(price.cohort.stock = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


Price.Pricestrip.cohort.value = capital.gains.strips$Price.Pricestrip.cohort.value %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(price.cohort.value, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", price.cohort.value))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -price.cohort.value, -Age) %>% 
  mutate(price.cohort.value = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)


  
  



price.strip.capital.gains = left_join(Price.Pricestrip.cohort.growth, Price.Pricestrip.cohort.infra) %>% 
  left_join(Price.Pricestrip.cohort.reit) %>% 
  left_join(Price.Pricestrip.cohort.small) %>%
  left_join(Price.Pricestrip.cohort.stock) %>%
  left_join(Price.Pricestrip.cohort.nr) %>%
  left_join(Price.Pricestrip.cohort.value) %>%
  select(-age.quarter, -vintage.quarter) %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)


######################################### Save all Factors #########################################



# Dividend strip: cohort.series.quarterly
# Prices of those dividends: 
# Capital gains strip dividends: 
# Price of the capital gains strips: 

save(cohort.series.quarterly, price.strip.quarterly.feb, dividend.strip.capital.gains, price.strip.capital.gains, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/DividendStripOct19.Rda")


######################################### GZ Credit Spread #########################################

gz.measure <- fread("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GZ_M_August_2016.csv")

gz.measure = gz.measure %>% as.data.frame() %>% mutate(gz_spread = `gz spr`)

gz.measure = gz.measure %>% 
  mutate(destring = stringr::str_split(date, "m", n = 2, simplify = FALSE),
         year = as.numeric( sapply(destring, "[", 1)),
         month = as.numeric(sapply(destring, "[", 2))) 
        

gz.quarterly = gz.measure %>% filter(month %in% c(1, 4, 7, 10))


gz.quarterly = gz.quarterly %>% mutate(Transaction.Quarter = ifelse(month == 1, year,
                                                                ifelse(month ==4 , year + .25,
                                                                       ifelse(month == 7, year + .5, year + .75))))


temp = gz.quarterly %>% filter(year >= 1990)

# Generate category based on full period
q1 = quantile(temp$gz_spread, probs = 0.33)
q2 = quantile(temp$gz_spread, probs = 0.67)

# Generate category based on full period -- alternative quintile method
aq1 = quantile(temp$gz_spread, probs = 0.20)
aq2 = quantile(temp$gz_spread, probs = 0.40)
aq3 = quantile(temp$gz_spread, probs = 0.60)
aq4 = quantile(temp$gz_spread, probs = 0.80)


# Generate category based on full period
gz.quarterly = gz.quarterly %>% mutate(gz_category = ifelse(gz_spread < q1, 1,
                                                            ifelse(gz_spread >= q1 & 
                                                                     gz_spread < q2, 2, 3 )))

gz.quarterly = gz.quarterly %>% 
  mutate(gz_category_quintile = ifelse(gz_spread < aq1, 1,
      ifelse(gz_spread >= aq1 &  gz_spread < aq2, 2, 
            ifelse(gz_spread >= aq2 & gz_spread < aq3, 3, 
                   ifelse(gz_spread >= aq3 & gz_spread < aq4, 4, 5)))))



gz.quarterly = gz.quarterly %>% select(Transaction.Quarter, gz_spread, gz_category, month, gz_category_quintile)

gz.yearly = gz.quarterly %>% filter(month == 1) %>% mutate(Vintage = Transaction.Quarter) %>% select(-Transaction.Quarter)

gz.quarterly = gz.quarterly %>% select(-month)












######################################### PD Ratio #########################################





pd_ratio <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/pricesforAG_withstripsD_Nov2018.mat")

PD.stock.quarterly = pd_ratio$PDm.model %>% as.data.frame() %>% mutate(Transaction.Quarter = (row_number() -1 )/ 4 + 1974) %>% 
  mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>% mutate(initial.PD.stock = V1/100) %>% select(-V1)

PD.reit.quarterly = pd_ratio$PD.reit.model %>% as.data.frame() %>% mutate(Transaction.Quarter = (row_number() -1 )/ 4 + 1974) %>% 
  mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>% mutate(initial.PD.reit = V1/100) %>% select(-V1)

PD.infra.quarterly = pd_ratio$PD.infra.model %>% as.data.frame() %>% mutate(Transaction.Quarter = (row_number() -1 )/ 4 + 1974) %>% 
  mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>% mutate(initial.PD.infra = V1/100) %>% select(-V1)


PD.stock.yearly = PD.stock.quarterly %>% filter(quarter == 4) %>% select(-quarter)
PD.reit.yearly = PD.reit.quarterly %>% filter(quarter == 4) %>% select(-quarter)
PD.infra.yearly = PD.infra.quarterly %>% filter(quarter == 4) %>% select(-quarter)

PD.stock.quarterly = PD.stock.quarterly %>% select(-quarter) 
PD.reit.quarterly  = PD.reit.quarterly %>% select(-quarter)
PD.infra.quarterly = PD.infra.quarterly %>% select(-quarter)

PD.initial.quarterly = left_join(PD.stock.quarterly, PD.reit.quarterly) %>% left_join(PD.infra.quarterly) 
PD.initial.yearly = left_join(PD.stock.yearly, PD.reit.yearly) %>% left_join(PD.infra.yearly) 
PD.initial.yearly = PD.initial.yearly %>% mutate(Vintage = floor(Transaction.Quarter)) %>% select(-Transaction.Quarter)

#temp = PD.initial.yearly %>% filter(Vintage >= 1990)

# Generate category based on initial period
#q1 = quantile(temp$initial.PD.stock, probs = 0.33)
#q2 = quantile(temp$initial.PD.stock, probs = 0.67)

# Generate category based on full period
q1.full = quantile(PD.initial.yearly$initial.PD.stock, probs = 0.25)
q2.full = quantile(PD.initial.yearly$initial.PD.stock, probs = 0.75)


q1.q.full = quantile(PD.initial.quarterly$initial.PD.stock, probs = 0.33)
q2.q.full = quantile(PD.initial.quarterly$initial.PD.stock, probs = 0.67)

q1.b.full = quantile(PD.initial.quarterly$initial.PD.stock, probs = 0.25)
q2.b.full = quantile(PD.initial.quarterly$initial.PD.stock, probs = 0.50)
q3.b.full = quantile(PD.initial.quarterly$initial.PD.stock, probs = 0.75)

# Generate category based on initial period
#PD.initial.yearly = PD.initial.yearly %>% mutate(pd_category = ifelse(initial.PD.stock < q1, 1,
#                                                            ifelse(initial.PD.stock >= q1 & 
#                                                                     initial.PD.stock < q2, 2, 3 )))

# Generate category based on full period
PD.initial.yearly = PD.initial.yearly %>% mutate(pd_category  = ifelse(initial.PD.stock <= q1.full, 1,
                                                                      ifelse(initial.PD.stock > q1.full & 
                                                                               initial.PD.stock <= q2.full, 2, 3 )))


PD.initial.yearly = PD.initial.yearly %>% select(Vintage, pd_category)





PD.initial.quarterly = PD.initial.quarterly %>% mutate(pd_category.quarterly = ifelse(initial.PD.stock <= q1.q.full, 1,
                                                                           ifelse(initial.PD.stock > q1.q.full & 
                                                                                    initial.PD.stock <= q2.q.full, 2, 3 )))

PD.initial.quarterly = PD.initial.quarterly %>% 
  mutate(pd_category.4.quarterly = ifelse(initial.PD.stock <= q1.b.full, 1,
          ifelse(initial.PD.stock > q1.b.full & 
                   initial.PD.stock <= q2.b.full, 2,
                 ifelse(initial.PD.stock > q2.b.full &
                          initial.PD.stock <= q3.b.full, 3, 4 ))))



PD.initial.quarterly = PD.initial.quarterly %>%
  mutate(Vintage.Quarter = Transaction.Quarter) %>% select( Vintage.Quarter, pd_category.quarterly, pd_category.4.quarterly) 


save(gz.yearly, gz.quarterly, PD.initial.yearly, PD.initial.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/GZSpreadApr19.Rda")


######################################### Expected Return  ############################################################################################

cohort.strip.return <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/stripexpretforAG_withXSd_Oct2019.mat")


# Two new factors
strip.return.nr = cohort.strip.return$stripexpret.NR %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(nr.return, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", nr.return))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -nr.return, -Age) %>% 
  mutate(nr.return = V) %>% select(-V)


strip.return.small = cohort.strip.return$stripexpret.small %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(small, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", small))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -small, -Age) %>% 
  mutate(small.return = V) %>% select(-V)


strip.return.growth = cohort.strip.return$stripexpret.growth %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(growth, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", growth))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -growth, -Age) %>% 
  mutate(growth.return = V) %>% select(-V)



# Bond Expected Return
strip.return.bond = cohort.strip.return$stripexpret.bond %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(bond, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", bond))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -bond, -Age) %>% 
  mutate(bond.return = V) %>% select(-V)

strip.return.stock = cohort.strip.return$stripexpret.stock %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(stock, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", stock))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -stock, -Age) %>% 
  mutate(stock.return = V) %>% select(-V)

strip.return.reit = cohort.strip.return$stripexpret.reit %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(reit, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", reit))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -reit, -Age) %>% 
  mutate(reit.return = V) %>% select(-V)

strip.return.infra = cohort.strip.return$stripexpret.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(infra, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", infra))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -infra, -Age) %>% 
  mutate(infra.return = V) %>% select(-V)

strip.return.value = cohort.strip.return$stripexpret.value %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(value, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", value))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -value, -Age) %>% 
  mutate(value.return = V) %>% select(-V)




# Capital gains expected returns
strip.return.nr.gain = cohort.strip.return$stripexpret.capgain.NR %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(nr.gain.return, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", nr.gain.return))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -nr.gain.return, -Age) %>% 
  mutate(nr.gain.return = V) %>% select(-V)

strip.return.growth.gain = cohort.strip.return$stripexpret.capgain.growth %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(stock, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", stock))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -stock, -Age) %>% 
  mutate(growth.gain.return = V) %>% select(-V)

strip.return.stock.gain = cohort.strip.return$stripexpret.capgain.stock %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(reit, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", reit))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -reit, -Age) %>% 
  mutate(stock.gain.return = V) %>% select(-V)

strip.return.small.gain = cohort.strip.return$stripexpret.capgain.small %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(infra, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", infra))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -infra, -Age) %>% 
  mutate(small.gain.return = V) %>% select(-V)

strip.return.reit.gain = cohort.strip.return$stripexpret.capgain.reit %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(infra, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", infra))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -infra, -Age) %>% 
  mutate(reit.gain.return = V) %>% select(-V)

strip.return.infra.gain = cohort.strip.return$stripexpret.capgain.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(infra, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", infra))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -infra, -Age) %>% 
  mutate(infra.gain.return = V) %>% select(-V)

strip.return.value.gain = cohort.strip.return$stripexpret.capgain.infra %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(value.gain.return, V, 1:64) %>% mutate(quarter = row_number() %% 4, quarter = ifelse(quarter == 0, 4, quarter)) %>%
  mutate(Vintage = (row_number -1 ) / 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", value.gain.return))) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% as.data.frame() %>% select(-row_number, -value.gain.return, -Age) %>% 
  mutate(value.gain.return = V) %>% select(-V)






strip.return.bond = strip.return.bond %>% select(-quarter) 
strip.return.stock  = strip.return.stock %>% select(-quarter)
strip.return.reit = strip.return.reit %>% select(-quarter)
strip.return.infra = strip.return.infra %>% select(-quarter)
strip.return.small = strip.return.small %>% select(-quarter)
strip.return.growth = strip.return.growth %>% select(-quarter)
strip.return.nr = strip.return.nr %>% select(-quarter)
strip.return.value = strip.return.value %>% select(-quarter)

strip.return.stock.gain  = strip.return.stock.gain %>% select(-quarter)
strip.return.reit.gain = strip.return.reit.gain %>% select(-quarter)
strip.return.infra.gain = strip.return.infra.gain %>% select(-quarter)
strip.return.small.gain = strip.return.small.gain %>% select(-quarter)
strip.return.growth.gain = strip.return.growth.gain %>% select(-quarter)
strip.return.nr.gain = strip.return.nr.gain %>% select(-quarter)
strip.return.value.gain = strip.return.value.gain %>% select(-quarter)


strip.return.quarterly = left_join(strip.return.bond, strip.return.stock) %>% 
  left_join(strip.return.reit) %>%
  left_join(strip.return.infra) %>%
  left_join(strip.return.small) %>%
  left_join(strip.return.growth)   %>%
  left_join(strip.return.nr)    %>%
  left_join(strip.return.value)  

strip.return.quarterly = left_join(strip.return.quarterly, strip.return.stock.gain) %>% 
  left_join(strip.return.reit.gain) %>%
  left_join(strip.return.infra.gain) %>%
  left_join(strip.return.small.gain) %>%
  left_join(strip.return.growth.gain)  %>%
  left_join(strip.return.nr.gain)  %>% 
  left_join(strip.return.value.gain)  %>% 
  mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)



 

 
save(strip.return.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/ExpectedReturnOct19.Rda")









######################################### Forward Rates #########################################



forward.rates.mat <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/proposal/MatlabcodeAPmodel/fwdrates60forAG_withXSc_April2019.mat")


forward.rates.quarterly = forward.rates.mat$fwdrates60 %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(forward.rate, V, 1:20) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", forward.rate))) %>%
  mutate(Transaction.Quarter = Vintage + 15 + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -forward.rate, -Age, -vintage.quarter ) %>% 
  mutate(forward.rate = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

forward.rates.yearly = forward.rates.quarterly %>% mutate(year = floor(Transaction.Quarter)) %>% filter(age.quarter == 4)  %>% 
  select(-age.quarter, -Transaction.Quarter) %>% unique 

discount.rates.quarterly = forward.rates.mat$discrates60   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(discount.rate, V, 1:20) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", discount.rate))) %>%
  mutate(Transaction.Quarter = Vintage + 15 + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -discount.rate, -Age, -vintage.quarter) %>% 
  mutate(discount.rate = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

discount.rates.yearly = discount.rates.quarterly %>% mutate(year = floor(Transaction.Quarter)) %>% filter(age.quarter == 4)  %>% 
  select(-age.quarter, -Transaction.Quarter) %>% unique 





# save everything in this at end
late.discount.rates.yearly = left_join(forward.rates.yearly, discount.rates.yearly)
late.discount.rates.quarterly = left_join(forward.rates.quarterly, discount.rates.quarterly) %>% select(-age.quarter)

late.discount.rates.quarterly = late.discount.rates.quarterly %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)

save(late.discount.rates.yearly, late.discount.rates.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/DiscountRatesJune19.Rda")







states <- read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Analysis/2019_06_05/states.csv")
states = states %>% as.data.frame() %>% mutate(slope2 = as.numeric(as.character(slope)))

states = states %>% mutate(abv_median_pdm = pdm > median(pdm),
                           bel_median_pdm = pdm <= median(pdm),
                           abv_median_slope = slope > median(slope),
                           bel_median_slope = slope <= median(slope))

states = states %>%
  mutate(pdm_slope_state = ifelse(bel_median_pdm == 1 & abv_median_slope == 1, 1,
                        ifelse(bel_median_pdm == 1 & bel_median_slope == 1,2,
                          ifelse(abv_median_pdm == 1 & abv_median_slope == 1,3,
                            ifelse(abv_median_pdm == 1 & bel_median_slope == 1,4,NA)))))


save(states, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/States.Rda")










