rm(list = ls())

# install.packages("lme4")

library(lubridate)
library(R.matlab)
library(tidyverse)
library(broom)
library(Hmisc)
library(data.table)




######################################### Standard Factors - Dividends  #########################################
cohort <- readMat("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Code/APmodel/19742019/APoutputPEinput.mat")


Div.cohort.nr.quarterly = cohort$Div.cohort.NR   %>% as.data.frame() %>% mutate(row_number = row_number()) %>%
  gather(cohort.nr, V, 1:64) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", cohort.nr)) ) %>%
  mutate(Transaction.Quarter = Vintage + 0.25 * Age) %>% 
  mutate(age.quarter = (Transaction.Quarter - floor(Transaction.Quarter)) * 4 + 1) %>%
  as.data.frame() %>% select(-row_number, -cohort.nr, -Age) %>% 
  mutate(cohort.nr = V) %>% select(-V) %>% arrange(Vintage, Transaction.Quarter)

 
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



# save everything in this at end
cohort.series.quarterly = left_join(Div.cohort.reit.quarterly, Div.cohort.infra.quarterly) %>% 
  left_join(Div.cohort.stock.quarterly) %>%
  left_join(Div.cohort.small.quarterly) %>%
  left_join(Div.cohort.large.quarterly) %>%
  left_join(Div.cohort.growth.quarterly) %>% 
  left_join(Div.cohort.value.quarterly) %>%
  left_join(Div.cohort.nr.quarterly) %>% select(-age.quarter, -vintage.quarter) %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)














######################################### Standard Factors - Price  #########################################
dividend.strip.prices  <- cohort

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


capital.gains.strips <- cohort

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

save(cohort.series.quarterly, price.strip.quarterly.feb, dividend.strip.capital.gains, price.strip.capital.gains, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DividendStripOct20.Rda")


######################################### Expected Return  ############################################################################################

cohort.strip.return <- cohort


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






save(strip.return.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/ExpectedReturnOct20.Rda")









######################################### Forward Rates #########################################



forward.rates.mat <- cohort


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

save(late.discount.rates.yearly, late.discount.rates.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DiscountRatesOct20.Rda")







states <- cohort
states = states %>% as.data.frame()


pdm = cohort$pdm %>% as.data.frame() %>%
  gather(V, pdm, 1:184) %>%
  mutate(vintage.quarter = row_number() %% 4, vintage.quarter = ifelse(vintage.quarter == 0, 4, vintage.quarter)) %>%
  mutate(Vintage = (row_number() -1 )/ 4 + 1974) %>% mutate(Age = as.numeric(gsub("V", "", V))) %>% 
  as.data.frame() %>% select( -V, -Age, -vintage.quarter ) %>% 
  arrange(Vintage)
  



states = pdm %>% mutate(abv_median_pdm = pdm > median(pdm),
                           bel_median_pdm = pdm <= median(pdm))



q1.q.full = quantile(states$pdm, probs = 0.33)
q2.q.full = quantile(states$pdm, probs = 0.67)

q1.b.full = quantile(states$pdm, probs = 0.25)
q2.b.full = quantile(states$pdm, probs = 0.50)
q3.b.full = quantile(states$pdm, probs = 0.75)


# Generate category based on full period

PD.initial.quarterly = states %>% mutate(pd_category.quarterly = ifelse(pdm <= q1.q.full, 1,
                                                                                      ifelse(pdm > q1.q.full & 
                                                                                               pdm <= q2.q.full, 2, 3 )),)


PD.initial.quarterly = PD.initial.quarterly %>% 
  mutate(pd_category.4.quarterly = ifelse(pdm <= q1.b.full, 1,
                                          ifelse(pdm > q1.b.full & 
                                                   pdm <= q2.b.full, 2,
                                                 ifelse(pdm > q2.b.full &
                                                          pdm <= q3.b.full, 3, 4 ))))


PD.initial.quarterly = PD.initial.quarterly %>% mutate(Vintage.Quarter = Vintage) %>% select(-Vintage)
save(PD.initial.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/States.Rda")










