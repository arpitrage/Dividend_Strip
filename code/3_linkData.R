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

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


#########################################  Load Dividend Series #########################################
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/ExpectedReturnOct19.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/DividendStripOct19.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/YearlyCashFlowOct19.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/GZSpreadApr19.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/States.Rda")

#########################################  Spread Variables #########################################




# Connect with GZ Spread + PD Ratio
fund.quarterly.div = left_join(fund.quarterly, PD.initial.quarterly)
fund.quarterly.div = left_join(fund.quarterly.div, states)


#########################################  Dividend Strips #########################################


# Connect with Dividend Series
fund.quarterly.div = left_join(fund.quarterly.div, cohort.series.quarterly)
fund.quarterly.div = left_join(fund.quarterly.div, price.strip.quarterly.feb)

fund.quarterly.div = fund.quarterly.div %>% mutate(price.meq1.feb = ifelse(is.na(price.meq1.feb), 1, price.meq1.feb),
                                                   price.infra.feb = ifelse(is.na(price.infra.feb), 1, price.infra.feb),
                                                   price.reit.feb = ifelse(is.na(price.reit.feb), 1, price.reit.feb),
                                                   price.stock.feb = ifelse(is.na(price.stock.feb), 1, price.stock.feb),
                                                   price.growth.feb = ifelse(is.na(price.growth.feb), 1, price.growth.feb),
                                                   price.bond.feb = ifelse(is.na(price.bond.feb), 1, price.bond.feb) )


#########################################  Capital Gains Strip #########################################
fund.quarterly.div = left_join(fund.quarterly.div, dividend.strip.capital.gains)

fund.quarterly.div = left_join(fund.quarterly.div, price.strip.capital.gains)




fund.quarterly.div = fund.quarterly.div %>% mutate(price.cohort.growth = ifelse(is.na(price.cohort.growth), 1, price.cohort.growth),
                                                   price.cohort.infra = ifelse(is.na(price.cohort.infra), 1, price.cohort.infra),
                                                   price.cohort.reit = ifelse(is.na(price.cohort.reit), 1, price.cohort.reit),
                                                   price.cohort.small = ifelse(is.na(price.cohort.small), 1, price.cohort.small),
                                                   price.cohort.stock = ifelse(is.na(price.cohort.stock), 1, price.cohort.stock))


 

#########################################  Expected Return #########################################




# Add Expected Return for div strip approach
fund.quarterly.div = left_join(fund.quarterly.div, strip.return.quarterly)

fund.quarterly.div = fund.quarterly.div %>% mutate(bond.return = ifelse(is.na(bond.return), 0, bond.return),
                                             stock.return =  ifelse(is.na(stock.return), 0, stock.return),
                                             reit.return =  ifelse(is.na(reit.return), 0, reit.return),
                                             infra.return = ifelse(is.na(infra.return), 0, infra.return),
                                             small.return = ifelse(is.na(small.return), 0, small.return),
                                             growth.return = ifelse(is.na(growth.return), 0, growth.return),
                                             stock.gain.return =  ifelse(is.na(stock.gain.return), 0, stock.gain.return),
                                             reit.gain.return =  ifelse(is.na(reit.gain.return), 0, reit.gain.return),
                                             infra.gain.return = ifelse(is.na(infra.gain.return), 0, infra.gain.return),
                                             small.gain.return = ifelse(is.na(small.gain.return), 0, small.gain.return),
                                             growth.gain.return = ifelse(is.na(growth.gain.return), 0, growth.gain.return))




 


#########################################  Categories #########################################

# Only vintages beyond 1990; div and price information available beyond then 
#fund.quarterly.div = fund.quarterly.div %>% filter(Vintage >= 1990)

fund.categories <- fund.quarterly.div %>% select(fund.category) %>% unique()

fund.abbreviations = c("pe", "vc", "ff", "rs", "in", "re", "nr", "df")


fund.quarterly.div$VintageFactor = as.factor(fund.quarterly.div$Vintage)
fund.quarterly.div$fund.category.f = as.factor(fund.quarterly.div$fund.category)


#fund.quarterly.div = fund.quarterly.div %>% mutate(AgeQuarterFactor = as.factor(Age.Quarter))
#fund.quarterly.div = fund.quarterly.div %>% mutate(gz_categoryFactor = as.factor(gz_category))
fund.quarterly.div = fund.quarterly.div %>% mutate(pd_categoryFactor = as.factor(pd_category.quarterly))
fund.quarterly.div = fund.quarterly.div %>% mutate(pd_categoryFactorQuartile = as.factor(pd_category.4.quarterly))
fund.quarterly.div = fund.quarterly.div %>% mutate(pdm_factor = as.factor(pdm_slope_state))


fund.quarterly.div = fund.quarterly.div %>% mutate(Age.Quarter = Age.Quarter + 1, 
                                           Age.Year = floor(Age.Quarter),
                                           AgeFactor = as.factor(Age.Year))

save(fund.quarterly.div, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/MergedCashFlowOct19.Rda")



# load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/MergedCashFlowJune19.Rda")
vintage.distribution = fund.quarterly.div %>% filter(Vintage.Quarter <= 2010) %>% 
  select(Fund.ID, Vintage.Quarter) %>% unique %>% select(Vintage.Quarter) 
vector.distribution = as.numeric(vintage.distribution[,1])  

save(vector.distribution, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/VintageDistribution.Rda")




