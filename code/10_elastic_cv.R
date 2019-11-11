
# New method -- replicate the portfolio on cash; price is not 1
# Remaining TODO:
# - cross validation for lasso
# - color the factor exposures differently for gain v. dividends



rm(list = ls())
# Libraries
{
  library(gt)
  library(glue)
  
  library(lubridate)
  
  
  library(R.matlab)
  
  
  library(tidyverse)  
  
  library(stargazer)
  library(broom)
  library(Hmisc)
  library(hrbrthemes)
  library(FinCal)
  library(penalized)
  
  library(PACLasso)
  library(mgcv)
  
  library(glmnet)
  library(glmnetUtils)
  library(data.table)
  
}
 
######################################### ANALYSIS ############################################################################################


# Load Files
{
  #load(file = "/scratch/ag5808/Infrastructure/Data/Clean/MergedCashFlowAug18.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/ExpectedReturnOct19.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/DividendStripOct19.Rda")
  
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/MergedCashFlowOct19.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/GZSpreadApr19.Rda")
  
  
  fund.quarterly.div = fund.quarterly.div %>% filter(Vintage >= 1981)
  fund.quarterly.div = fund.quarterly.div %>% mutate(Vintage.New = floor(Vintage.Quarter))
  
  fund.categories <- fund.quarterly.div %>% select(fund.category) %>% unique()
  fund.abbreviations = c("pe", "vc", "ff", "rs", "in", "re", "nr", "df")
  
  fund.quarterly.div <- fund.quarterly.div %>% 
    filter( !is.na(gain.cohort.stock) & !is.na(cohort.value)  ) 
  
  
  # Fix late in life infra exposure 
  fund.quarterly.div.noinfra = fund.quarterly.div %>% filter(fund.category != fund.categories[5,1])
  fund.quarterly.div.infra = fund.quarterly.div %>% filter(fund.category == fund.categories[5,1])
  
  
  fund.quarterly.div.infra = fund.quarterly.div.infra %>%
    mutate(late.life.cash = ifelse(Age.Quarter >= 13 , net.cf.distribution.rescale, 0 )) %>%
    group_by(Fund.ID) %>% mutate(total.late.cash = sum(late.life.cash, na.rm = TRUE)) %>% as.data.frame() %>%
    mutate(net.cf.distribution.rescale = ifelse(Age.Quarter == 12.75, net.cf.distribution.rescale + total.late.cash, net.cf.distribution.rescale) ) %>%
    filter(Age.Quarter <= 12.75) %>% select(-late.life.cash, -total.late.cash)
  
  
  fund.quarterly.div = rbind(fund.quarterly.div.infra, fund.quarterly.div.noinfra)
  
  # Bond exposure
  fund.quarterly.div = fund.quarterly.div %>% mutate(bond = 1)
}


# Portfolios generate
{
  portfolio.fund.yearly = fund.quarterly.div %>% 
    group_by(Vintage.New, Transaction.Quarter,fund.category) %>% 
    mutate(net.cf.distribution.rescale = mean(net.cf.distribution.rescale, na.rm = TRUE)) %>%
    select(net.cf.distribution.rescale, Transaction.Quarter, year,   
           fund.category, Vintage.New) %>% 
    as.data.frame() %>% unique %>% mutate(Vintage.Quarter = Vintage.New) 
  
  
  portfolio.fund.yearly.stub = fund.quarterly.div %>%
    select(Transaction.Quarter,  
           year,   
           cohort.stock, 
           cohort.growth, 
           cohort.small, 
           cohort.reit, 
           cohort.infra, 
           cohort.value,
           cohort.nr,
           pd_categoryFactorQuartile,
           pd_category.4.quarterly,
           gain.cohort.stock,
           gain.cohort.small,
           gain.cohort.growth,
           gain.cohort.reit,
           gain.cohort.infra,
           gain.cohort.nr,
           gain.cohort.value,
           Vintage.Quarter,
           Vintage.New,
           price.bond.feb) %>% unique
  
  portfolio.fund.yearly = left_join(portfolio.fund.yearly,  portfolio.fund.yearly.stub)
  
  
  portfolio.fund.yearly = portfolio.fund.yearly %>%
    group_by(Vintage.New, fund.category) %>%
    mutate(Fund.ID = group_indices(),
           Firm.ID = Fund.ID) %>% as.data.frame()
  
  portfolio.fund.yearly = portfolio.fund.yearly %>%
    mutate(Age.Quarter = Transaction.Quarter - Vintage.Quarter,
           Age.Quarter = Age.Quarter + 1, 
           Age.Year = floor(Age.Quarter),
           AgeFactor = as.factor(Age.Year) )
  
  
  #portfolio.fund.yearly = portfolio.fund.yearly %>% group_by(Vintage, fund.category) %>% mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame() 
  
  
  portfolio.fund.quarterly = fund.quarterly.div %>% 
    group_by(Vintage.Quarter, Transaction.Quarter, fund.category) %>% 
    mutate(net.cf.distribution.rescale = mean(net.cf.distribution.rescale, na.rm = TRUE)) %>%
    select(net.cf.distribution.rescale, 
           Transaction.Quarter,  
           year,   
           bond,
           cohort.stock, 
           cohort.growth, 
           cohort.small, 
           cohort.reit, 
           cohort.infra, 
           cohort.value,
           cohort.nr,
           Age.Quarter, 
           AgeFactor,
           pd_categoryFactorQuartile,
           pd_category.4.quarterly,
           fund.category, 
           Vintage.New,
           Vintage.Quarter,
           gain.cohort.stock,
           gain.cohort.small,
           gain.cohort.growth,
           gain.cohort.reit,
           gain.cohort.infra,
           gain.cohort.nr,
           gain.cohort.value,
           price.bond.feb) %>% as.data.frame() %>% unique 
  
  
  portfolio.fund.quarterly = portfolio.fund.quarterly %>%
    group_by(Vintage.Quarter, fund.category) %>%
    mutate(Fund.ID = group_indices(),
           Firm.ID = Fund.ID) %>% as.data.frame()
  
  
  # portfolio.fund.quarterly = portfolio.fund.quarterly %>% group_by(Vintage.Quarter, fund.category) %>% mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame() 
  
  
  set.seed(123)
  
  
  # random portfolios
  portfolio.random = fund.quarterly.div %>% select(Firm.ID, Fund.ID, Fund.Size, fund.category, Vintage.Quarter) %>% unique
  portfolio.random$portfolio.random <- sample(5, size = nrow(portfolio.random), replace = TRUE)
  
  
  portfolio.fund.random = left_join(fund.quarterly.div, portfolio.random) 
  portfolio.fund.random = portfolio.fund.random %>% 
    group_by(portfolio.random, Vintage.Quarter, fund.category, Transaction.Quarter) %>% 
    mutate(net.cf.distribution.rescale = mean(net.cf.distribution.rescale, na.rm = TRUE)) %>%
    select(net.cf.distribution.rescale, 
           portfolio.random, 
           Transaction.Quarter, 
           year,   
           cohort.stock, 
           cohort.growth, 
           cohort.small, 
           cohort.reit, 
           cohort.infra, 
           cohort.value,
           cohort.nr,
           pdm_slope_state,
           Age.Quarter,
           Age.Year,
           AgeFactor,
           pd_categoryFactorQuartile,
           pd_category.4.quarterly,
           fund.category, 
           Vintage.New,
           Vintage.Quarter, 
           gain.cohort.stock,
           gain.cohort.small,
           gain.cohort.growth,
           gain.cohort.reit,
           gain.cohort.infra,
           gain.cohort.nr,
           gain.cohort.value,
           price.bond.feb) %>% as.data.frame() %>% unique 
  
  
  portfolio.fund.random = portfolio.fund.random %>%
    group_by(Vintage.Quarter, fund.category, portfolio.random) %>%
    mutate(Fund.ID = group_indices(),
           Firm.ID = Fund.ID) %>% as.data.frame()
  
  
  # portfolio.fund.random = portfolio.fund.random %>% group_by(Fund.ID) %>% mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame() 
  
  
  # Portfolio generate
  aq1 = quantile(fund.quarterly.div$Fund.Size, probs = 0.20)
  aq2 = quantile(fund.quarterly.div$Fund.Size, probs = 0.40)
  aq3 = quantile(fund.quarterly.div$Fund.Size, probs = 0.60)
  aq4 = quantile(fund.quarterly.div$Fund.Size, probs = 0.80)
  
  
  # size portfolios
  portfolio.fund.size = fund.quarterly.div %>%
    mutate(portfolio.size = ifelse(Fund.Size < aq1, 1,
                                   ifelse(Fund.Size >= aq1 & Fund.Size < aq2, 2,
                                          ifelse(Fund.Size >= aq2 & Fund.Size < aq3, 3,
                                                 ifelse(Fund.Size >= aq3 & Fund.Size < aq4, 4,
                                                        ifelse(Fund.Size >= aq4 , 5, NA))))))
  
  portfolio.fund.size = portfolio.fund.size %>% 
    group_by(portfolio.size, Vintage.Quarter, Transaction.Quarter, fund.category) %>%
    mutate(net.cf.distribution.rescale = mean(net.cf.distribution.rescale, na.rm = TRUE)) %>%
    select(net.cf.distribution.rescale, portfolio.size, Transaction.Quarter, year,   
           cohort.stock, 
           cohort.growth, 
           cohort.small, 
           cohort.reit, 
           cohort.infra, 
           cohort.value,
           cohort.nr,
           Age.Quarter, AgeFactor, pdm_slope_state,
           pd_categoryFactor, pd_category.quarterly, pd_categoryFactorQuartile, pd_category.4.quarterly,
           fund.category, Vintage.New, Vintage.Quarter,
           gain.cohort.stock,
           gain.cohort.small,
           gain.cohort.growth,
           gain.cohort.reit,
           gain.cohort.infra,
           gain.cohort.nr,
           gain.cohort.value,
           price.bond.feb) %>% as.data.frame() %>% unique 
  
  
  portfolio.fund.size = portfolio.fund.size %>%
    group_by(Vintage.Quarter, fund.category, portfolio.size) %>%
    mutate(Fund.ID = group_indices(),
           Firm.ID = Fund.ID) %>% as.data.frame()
  # portfolio.fund.size = portfolio.fund.size %>% group_by(Fund.ID) %>% mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame() 
  
  
}

wkdir <- "/Users/agupta011/Dropbox/Research/Infrastructure/Analysis/2019_10_31/"

# Programs
{ 
  '%!in%' <- function(x,y)!('%in%'(x,y))
  

  R2 <- function(dataset, estimates ) 
  {
    
    predictions <- predict(estimates,  data = dataset)
    predictions.unlist <- as.numeric(unlist(predictions)[,1])
    dataset$predictions <- predictions.unlist
    
    rss <- sum((dataset$predictions - dataset$net.cf.distribution.rescale)^2)
    tss <- sum((dataset$net.cf.distribution.rescale)^2 )
    
    r2 = round(1 - rss/tss, 3)
    
    final.pen = list(r2, rss)
    
    return(final.pen)  
    
    
  }
  
  
  # strip.data.vintage.pen.5 =  strip.data.vintage.pen.7.quarterly 
  MissingVars <- function(strip.data.vintage.pen.5)
  {
    
    if("bond.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(bond.pd = 0)
    }
    
    
    if("cohort.stock.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.stock.pd = 0)
    }
    
    if("cohort.small.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.small.pd = 0)
    }
    
    
    if("cohort.value.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.value.pd = 0)
    }
    
    if("cohort.growth.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.growth.pd = 0)
    }
    
    if("cohort.reit.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.reit.pd = 0)
    }
    
    if("cohort.infra.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.infra.pd = 0)
    }
    
    if("cohort.nr.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.nr.pd = 0)
    }
    
    
    if("gain.cohort.small.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.small.pd = 0)
    }
    
    if("gain.cohort.stock.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.stock.pd = 0)
    }
    
    if("gain.cohort.growth.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.growth.pd = 0)
    }
    
    if("gain.cohort.reit.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.reit.pd = 0)
    }
    
    if("gain.cohort.infra.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.infra.pd = 0)
    }
    
    if("gain.cohort.nr.pd" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.nr.pd = 0)
    }
    
    
    
    
    if("bond" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(bond = 0)
    }
    
    
    if("cohort.stock" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.stock = 0)
    }
    
    if("cohort.small" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.small = 0)
    }
    
    if("cohort.growth" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.growth = 0)
    }
    
    if("cohort.reit" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.reit = 0)
    }
    
    if("cohort.infra" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.infra = 0)
    }
    
    if("cohort.nr" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.nr = 0)
    }
    
    
    if("cohort.value" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(cohort.value = 0)
    }
    
    
    
    if("gain.cohort.small" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.small = 0)
    }
    
    if("gain.cohort.stock" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.stock = 0)
    }
    
    if("gain.cohort.growth" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.growth = 0)
    }
    
    if("gain.cohort.reit" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.reit = 0)
    }
    
    if("gain.cohort.infra" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.infra = 0)
    }
    
    if("gain.cohort.nr" %!in% names(strip.data.vintage.pen.5)){
      strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>% mutate(gain.cohort.nr = 0)
    }
    
    strip.data.vintage.pen.5 = strip.data.vintage.pen.5 %>%
      mutate(bond = ifelse(is.na(bond), 0, bond),
             cohort.stock = ifelse(is.na(cohort.stock), 0, cohort.stock),
             cohort.small = ifelse(is.na(cohort.small), 0, cohort.small),
             cohort.growth = ifelse(cohort.growth, 0, cohort.small),
             cohort.reit = ifelse(is.na(cohort.reit), 0, cohort.reit),
             cohort.infra = ifelse(is.na(cohort.infra), 0, cohort.infra),
             cohort.nr = ifelse(is.na(cohort.nr), 0, cohort.nr),
             cohort.value = ifelse(is.na(cohort.value), 0, cohort.value),
             gain.cohort.small = ifelse(is.na(gain.cohort.small), 0, gain.cohort.small),
             gain.cohort.stock = ifelse(is.na(gain.cohort.stock), 0, gain.cohort.stock),
             gain.cohort.growth = ifelse(is.na(gain.cohort.growth), 0, gain.cohort.growth),
             gain.cohort.reit = ifelse(is.na(gain.cohort.reit), 0, gain.cohort.reit),
             gain.cohort.infra = ifelse(is.na(gain.cohort.infra), 0, gain.cohort.infra),
             gain.cohort.nr = ifelse(is.na(gain.cohort.nr), 0, gain.cohort.nr) )
    
    
    
    return(strip.data.vintage.pen.5)
    
  }
  
  
  OLSCoefs <- function(ols, model.list)
  {
    
    # Model 3 
    #r2_ols = round(glance(ols.3)[1,1], 2)
    #r2_ols.alt = round(glance(ols.3.yearly)[1,1], 2)
    
    ols.broom = tidy(ols)
    ols.broom = ols.broom %>% mutate(AgeFactor = as.numeric(stringr::str_extract(term,"\\d+\\.*\\d*")))
    
    ols.broom = ols.broom %>% 
      mutate(Type = ifelse(grepl("gain.cohort.small", term) & grepl("AgeFactor", term), "gain.cohort.small",
                           ifelse(grepl("gain.cohort.stock", term) & grepl("AgeFactor", term), "gain.cohort.stock",
                                  ifelse(grepl("gain.cohort.infra", term) & grepl("AgeFactor", term), "gain.cohort.infra",
                                         ifelse(grepl("gain.cohort.reit", term) & grepl("AgeFactor", term), "gain.cohort.reit",
                                                ifelse(grepl("gain.cohort.growth", term) & grepl("AgeFactor", term), "gain.cohort.growth",
                                                       ifelse(grepl("gain.cohort.nr", term) & grepl("AgeFactor", term), "gain.cohort.nr",
                                                              ifelse(grepl("cohort.stock", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.stock",
                                                                     ifelse(grepl("cohort.infra", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.infra",
                                                                            ifelse(grepl("cohort.reit", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.reit",
                                                                                   ifelse(grepl("cohort.small", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.small",
                                                                                          ifelse(grepl("cohort.growth", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.growth",
                                                                                                 ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                        ifelse(grepl("gain.cohort.reit", term) & grepl("pd_category", term), "gain.cohort.reit.pd",
                                                                                                               ifelse(grepl("gain.cohort.nr", term) & grepl("pd_category", term), "gain.cohort.nr.pd",
                                                                                                                      ifelse(grepl("gain.cohort.infra", term) & grepl("pd_category", term), "gain.cohort.infra.pd",
                                                                                                                             ifelse(grepl("gain.cohort.small", term)& grepl("pd_category", term), "gain.cohort.small.pd", 
                                                                                                                                    ifelse(grepl("gain.cohort.growth", term)& grepl("pd_category", term), "gain.cohort.growth.pd", 
                                                                                                                                           ifelse(grepl("gain.cohort.value", term)& grepl("pd_category", term), "gain.cohort.value.pd", 
                                                                                                                                                  ifelse(grepl(":", term) == 0 & grepl("AgeFactor", term), "bond", 
                                                                                                                                                         ifelse(grepl("cohort.reit", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.reit.pd",
                                                                                                                                                                ifelse(grepl("cohort.nr", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.nr.pd",
                                                                                                                                                                       ifelse(grepl("cohort.stock", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.stock.pd",
                                                                                                                                                                              ifelse(grepl("cohort.nr", term) & grepl("AgeFactor", term), "cohort.nr",
                                                                                                                                                                                     ifelse(grepl("cohort.infra", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.infra.pd",
                                                                                                                                                                                            ifelse(grepl("cohort.small", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.small.pd", 
                                                                                                                                                                                                   ifelse(grepl("cohort.growth", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.growth.pd", 
                                                                                                                                                                                                          ifelse(grepl("cohort.value", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.value.pd", 
                                                                                                                                                                                                                 ifelse(grepl(":", term) == 0 & grepl("pd_category", term), "bond.pd",""))))))))))))))))))))))))))))) %>% filter(Type != "")
    
    ols.broom.age = ols.broom %>% mutate(three.factor.ols  = estimate) %>%
      filter(grepl(".pd", Type) == 0) %>% dplyr::select(AgeFactor, three.factor.ols, Type)
    
    ols.broom.age.wide = spread(ols.broom.age, Type, three.factor.ols) %>% mutate_all(funs(replace(., is.na(.), 0)))
    
    
    
    
    ols.broom.age.pd = ols.broom %>% mutate(three.factor.ols.pd  = estimate) %>%
      filter(grepl(".pd", Type) == 1) %>% dplyr::select(AgeFactor, three.factor.ols.pd, Type) %>% 
      mutate(pd_categoryFactorQuartile = AgeFactor) %>% select(-AgeFactor)
    
    ols.broom.age.pd.wide = spread(ols.broom.age.pd, Type, three.factor.ols.pd) %>% mutate_all(funs(replace(., is.na(.), 0)))
    zero = c(1, rep(0, length(model.list)))
    ols.broom.age.pd.wide = rbind(ols.broom.age.pd.wide, zero)
    
    #ols.broom.age.pd.wide = ols.broom.age.pd.wide %>% 
    #  mutate(fit3.pd.deviation.bond = fit3.b.bond.estimate.pd - mean(fit3.b.bond.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small = fit3.b.small.estimate.pd - mean(fit3.b.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock = fit3.b.stock.estimate.pd - mean(fit3.b.stock.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small.gain = fit3.b.small.estimate.pd - mean(fit3.b.gain.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock.gain = fit3.b.stock.estimate.pd - mean(fit3.b.gain.stock.estimate.pd, na.rm = TRUE)) %>% arrange(pd_categoryFactorQuartile)
    
    
    
    
    final.ols = list(ols.broom.age.wide, ols.broom.age.pd.wide)
    
    return(final.ols)  
    
  }
  
  
  OLSCoefsNoPD <- function(ols, model.list)
  {
    
    # Model 3 
    #r2_ols = round(glance(ols.3)[1,1], 2)
    #r2_ols.alt = round(glance(ols.3.yearly)[1,1], 2)
    
    ols.broom = tidy(ols)
    ols.broom = ols.broom %>% mutate(AgeFactor = as.numeric(stringr::str_extract(term,"\\d+\\.*\\d*")))
    
    ols.broom = ols.broom %>% 
      mutate(Type = ifelse(grepl("gain.cohort.small", term) & grepl("AgeFactor", term), "gain.cohort.small",
                           ifelse(grepl("gain.cohort.stock", term) & grepl("AgeFactor", term), "gain.cohort.stock",
                                  ifelse(grepl("gain.cohort.infra", term) & grepl("AgeFactor", term), "gain.cohort.infra",
                                         ifelse(grepl("gain.cohort.reit", term) & grepl("AgeFactor", term), "gain.cohort.reit",
                                                ifelse(grepl("gain.cohort.growth", term) & grepl("AgeFactor", term), "gain.cohort.growth",
                                                       ifelse(grepl("gain.cohort.nr", term) & grepl("AgeFactor", term), "gain.cohort.nr",
                                                              ifelse(grepl("cohort.stock", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.stock",
                                                                     ifelse(grepl("cohort.infra", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.infra",
                                                                            ifelse(grepl("cohort.reit", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.reit",
                                                                                   ifelse(grepl("cohort.small", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.small",
                                                                                          ifelse(grepl("cohort.growth", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.growth",
                                                                                                 ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                        ifelse(grepl("gain.cohort.reit", term) & grepl("pd_category", term), "gain.cohort.reit.pd",
                                                                                                               ifelse(grepl("gain.cohort.nr", term) & grepl("pd_category", term), "gain.cohort.nr.pd",
                                                                                                                      ifelse(grepl("gain.cohort.infra", term) & grepl("pd_category", term), "gain.cohort.infra.pd",
                                                                                                                             ifelse(grepl("gain.cohort.small", term)& grepl("pd_category", term), "gain.cohort.small.pd", 
                                                                                                                                    ifelse(grepl("gain.cohort.growth", term)& grepl("pd_category", term), "gain.cohort.growth.pd", 
                                                                                                                                           ifelse(grepl("gain.cohort.value", term)& grepl("pd_category", term), "gain.cohort.value.pd", 
                                                                                                                                                  ifelse(grepl(":", term) == 0 & grepl("AgeFactor", term), "bond", 
                                                                                                                                                         ifelse(grepl("cohort.reit", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.reit.pd",
                                                                                                                                                                ifelse(grepl("cohort.nr", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.nr.pd",
                                                                                                                                                                       ifelse(grepl("cohort.stock", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.stock.pd",
                                                                                                                                                                              ifelse(grepl("cohort.nr", term) & grepl("AgeFactor", term), "cohort.nr",
                                                                                                                                                                                     ifelse(grepl("cohort.infra", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.infra.pd",
                                                                                                                                                                                            ifelse(grepl("cohort.small", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.small.pd", 
                                                                                                                                                                                                   ifelse(grepl("cohort.growth", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.growth.pd", 
                                                                                                                                                                                                          ifelse(grepl("cohort.value", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.value.pd", 
                                                                                                                                                                                                                 ifelse(grepl(":", term) == 0 & grepl("pd_category", term), "bond.pd",""))))))))))))))))))))))))))))) %>% filter(Type != "")
    
    ols.broom.age = ols.broom %>% mutate(three.factor.ols  = estimate) %>%
      filter(grepl(".pd", Type) == 0) %>% dplyr::select(AgeFactor, three.factor.ols, Type)
    
    ols.broom.age.wide = spread(ols.broom.age, Type, three.factor.ols) %>% mutate_all(funs(replace(., is.na(.), 0)))
    
    
    
    
    
    
    final.ols = list(ols.broom.age.wide)
    
    return(final.ols)  
    
  }
  
  
  OLSCoefsPDM <- function(ols, model.list)
  {
    
    # Model 3 
    #r2_ols = round(glance(ols.3)[1,1], 2)
    #r2_ols.alt = round(glance(ols.3.yearly)[1,1], 2)
    
    ols.broom = tidy(ols)
    ols.broom = ols.broom %>% mutate(AgeFactor = as.numeric(stringr::str_extract(term,"\\d+\\.*\\d*")))
    
    ols.broom = ols.broom %>% 
      mutate(Type = ifelse(grepl("gain.cohort.small", term) & grepl("AgeFactor", term), "gain.cohort.small",
                           ifelse(grepl("gain.cohort.stock", term) & grepl("AgeFactor", term), "gain.cohort.stock",
                                  ifelse(grepl("gain.cohort.infra", term) & grepl("AgeFactor", term), "gain.cohort.infra",
                                         ifelse(grepl("gain.cohort.reit", term) & grepl("AgeFactor", term), "gain.cohort.reit",
                                                ifelse(grepl("gain.cohort.growth", term) & grepl("AgeFactor", term), "gain.cohort.growth",
                                                       ifelse(grepl("gain.cohort.nr", term) & grepl("AgeFactor", term), "gain.cohort.nr",
                                                              ifelse(grepl("cohort.stock", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.stock",
                                                                     ifelse(grepl("cohort.infra", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.infra",
                                                                            ifelse(grepl("cohort.reit", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.reit",
                                                                                   ifelse(grepl("cohort.small", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.small",
                                                                                          ifelse(grepl("cohort.growth", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.growth",
                                                                                                 ifelse(grepl("gain.cohort.stock", term) & grepl("pdm_slope_state", term), "gain.cohort.stock.pd",
                                                                                                        ifelse(grepl("gain.cohort.reit", term) & grepl("pdm_slope_state", term), "gain.cohort.reit.pd",
                                                                                                               ifelse(grepl("gain.cohort.nr", term) & grepl("pdm_slope_state", term), "gain.cohort.nr.pd",
                                                                                                                      ifelse(grepl("gain.cohort.infra", term) & grepl("pdm_slope_state", term), "gain.cohort.infra.pd",
                                                                                                                             ifelse(grepl("gain.cohort.small", term)& grepl("pdm_slope_state", term), "gain.cohort.small.pd", 
                                                                                                                                    ifelse(grepl("gain.cohort.growth", term)& grepl("pdm_slope_state", term), "gain.cohort.growth.pd", 
                                                                                                                                           ifelse(grepl("gain.cohort.value", term)& grepl("pdm_slope_state", term), "gain.cohort.value.pd", 
                                                                                                                                                  ifelse(grepl(":", term) == 0 & grepl("AgeFactor", term), "bond", 
                                                                                                                                                         ifelse(grepl("cohort.reit", term) & grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.reit.pd",
                                                                                                                                                                ifelse(grepl("cohort.nr", term) & grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.nr.pd",
                                                                                                                                                                       ifelse(grepl("cohort.stock", term) & grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.stock.pd",
                                                                                                                                                                              ifelse(grepl("cohort.nr", term) & grepl("AgeFactor", term), "cohort.nr",
                                                                                                                                                                                     ifelse(grepl("cohort.infra", term) & grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.infra.pd",
                                                                                                                                                                                            ifelse(grepl("cohort.small", term)& grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.small.pd", 
                                                                                                                                                                                                   ifelse(grepl("cohort.growth", term)& grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.growth.pd", 
                                                                                                                                                                                                          ifelse(grepl("cohort.value", term)& grepl("pdm_slope_state", term) & grepl("gain", term) == 0, "cohort.value.pd", 
                                                                                                                                                                                                                 ifelse(grepl(":", term) == 0 & grepl("pdm_slope_state", term), "bond.pd",""))))))))))))))))))))))))))))) %>% filter(Type != "")
    
    ols.broom.age = ols.broom %>% mutate(three.factor.ols  = estimate) %>%
      filter(grepl(".pd", Type) == 0) %>% dplyr::select(AgeFactor, three.factor.ols, Type)
    
    ols.broom.age.wide = spread(ols.broom.age, Type, three.factor.ols) %>% mutate_all(funs(replace(., is.na(.), 0)))
    
    
    
    
    ols.broom.age.pd = ols.broom %>% mutate(three.factor.ols.pd  = estimate) %>%
      filter(grepl(".pd", Type) == 1) %>% dplyr::select(AgeFactor, three.factor.ols.pd, Type) %>% 
      mutate(pd_categoryFactorQuartile = AgeFactor) %>% select(-AgeFactor)
    
    ols.broom.age.pd.wide = spread(ols.broom.age.pd, Type, three.factor.ols.pd) %>% mutate_all(funs(replace(., is.na(.), 0)))
    zero = c(1, rep(0, length(model.list)))
    ols.broom.age.pd.wide = rbind(ols.broom.age.pd.wide, zero)
    
    #ols.broom.age.pd.wide = ols.broom.age.pd.wide %>% 
    #  mutate(fit3.pd.deviation.bond = fit3.b.bond.estimate.pd - mean(fit3.b.bond.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small = fit3.b.small.estimate.pd - mean(fit3.b.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock = fit3.b.stock.estimate.pd - mean(fit3.b.stock.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small.gain = fit3.b.small.estimate.pd - mean(fit3.b.gain.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock.gain = fit3.b.stock.estimate.pd - mean(fit3.b.gain.stock.estimate.pd, na.rm = TRUE)) %>% arrange(pd_categoryFactorQuartile)
    
    
    
    
    final.ols = list(ols.broom.age.wide, ols.broom.age.pd.wide)
    
    return(final.ols)  
    
  }
  
  LassoCoefs <- function(lasso, model.list)
  {
    
    # Penalized
    penalized.broom = coef(lasso) %>% as.matrix() 
    factor = rownames(penalized.broom)
    penalized.broom = penalized.broom %>% as.data.frame()
    penalized.broom$factor = factor
    penalized.broom = penalized.broom %>% 
      mutate(value = s0) %>% select(-s0)
    
    penalized.broom.pd = penalized.broom %>%
      separate(factor, c("model", "pd_categoryFactorQuartile"), "_pd_") %>% 
      filter(!is.na(pd_categoryFactorQuartile))
    
    
    penalized.broom.age = penalized.broom %>%
      separate(factor, c("model", "pd_categoryFactorQuartile"), "_pd_") %>%
      filter(is.na(pd_categoryFactorQuartile)) %>% 
      select(-pd_categoryFactorQuartile)  %>%
      separate(model, c("model", "AgeFactor"), "_") 
      
    penalized.broom.age = penalized.broom.age %>%
      mutate(Type = ifelse(model == "bond", "Bond",
                           ifelse(model == "cohort.small", "Small",
                                  ifelse(model == "cohort.stock", "Stock",
                                         ifelse(model == "cohort.value", "Value",
                                                ifelse(model == "cohort.growth", "Growth",
                                                       ifelse(model == "gain.cohort.small", "Small Gain",
                                                              ifelse(model == "gain.cohort.stock", "Stock Gain",
                                                                     ifelse(model == "cohort.reit", "REIT",
                                                                            ifelse(model == "cohort.infra", "Infra",
                                                                                   ifelse(model == "cohort.growth", "Growth",
                                                                                          ifelse(model == "cohort.value", "Value",
                                                                                              ifelse(model == "gain.cohort.reit", "REIT Gain",
                                                                                                 ifelse(model == "gain.cohort.infra", "Infra Gain",
                                                                                                        ifelse(model == "gain.cohort.growth", "Growth Gain",
                                                                                                               ifelse(model == "cohort.nr", "Natural Resources (NR)",
                                                                                                                      ifelse(model == "gain.cohort.nr", "NR Gain", 
                                                                                                                             ifelse(model == "gain.cohort.value", "Value Gain", ""))))))))))))))))))
    
    
    penalized.broom.pd = penalized.broom.pd %>%
      mutate(Type = ifelse(model == "bond", "Bond",
                           ifelse(model == "cohort.small", "Small",
                                  ifelse(model == "cohort.stock", "Stock",
                                         ifelse(model == "cohort.value", "Value",
                                                ifelse(model == "cohort.growth", "Growth",
                                                       ifelse(model == "gain.cohort.small", "Small Gain",
                                                              ifelse(model == "gain.cohort.stock", "Stock Gain",
                                                                     ifelse(model == "cohort.reit", "REIT",
                                                                            ifelse(model == "cohort.infra", "Infra",
                                                                                   ifelse(model == "cohort.growth", "Growth",
                                                                                          ifelse(model == "cohort.value", "Value",
                                                                                              ifelse(model == "gain.cohort.reit", "REIT Gain",
                                                                                                 ifelse(model == "gain.cohort.infra", "Infra Gain",
                                                                                                        ifelse(model == "gain.cohort.growth", "Growth Gain",
                                                                                                               ifelse(model == "cohort.nr", "Natural Resources (NR)",
                                                                                                                      ifelse(model == "gain.cohort.nr", "NR Gain",
                                                                                                                             ifelse(model == "gain.cohort.value", "Value Gain", ""))))))))))))))))))
    
    
    
    
    
    
    penalized.broom.age = penalized.broom.age %>% 
      filter(!is.na(AgeFactor)) %>% 
      mutate(AgeFactor = as.numeric(AgeFactor))
    
       
    penalized.broom.pd = penalized.broom.pd %>%
      mutate(pd_categoryFactorQuartile = as.numeric(pd_categoryFactorQuartile),
             model = paste0(model, ".pd"))
    
    
    penalized.broom.age.wide = penalized.broom.age %>% select(-Type) %>%
      spread(model, value)
    
    penalized.broom.pd.wide = penalized.broom.pd %>% select(-Type) %>%
      spread(model, value)
    
    penalized.broom.age = penalized.broom.age %>%
      mutate(Div_or_Gain =  ifelse(grepl("Gain", Type), "Gain", "Dividend"))
    
    
    penalized.broom.age = penalized.broom.age %>%
      mutate(factor_type =  ifelse(grepl("bond", model), "Bond",
                                   ifelse(grepl("small", model), "Small",
                                          ifelse(grepl("stock", model), "Stock",
                                                 ifelse(grepl("reit", model), "REIT",
                                                        ifelse(grepl("growth", model), "Growth",
                                                               ifelse(grepl("value", model), "Value",
                                                                      ifelse(grepl("infra", model), "Infra",
                                                                             ifelse(grepl("nr", model), "Natural Resources","")))))))))

    
    
    penalized.broom.age = penalized.broom.age %>%
      group_by(Type) %>%
      mutate(sum.estimates = sum(value)) %>% as.data.frame() %>%
      filter(sum.estimates != 0) %>% select(-sum.estimates)
    
    
    penalized.broom.pd = penalized.broom.pd %>%
      group_by(Type) %>%
      mutate(sum.estimates = sum(value)) %>% as.data.frame() %>%
      filter(sum.estimates != 0) %>% select(-sum.estimates)
    
    final.pen = list(penalized.broom.age.wide, penalized.broom.pd.wide, penalized.broom.age, penalized.broom.pd)
    
    return(final.pen)  
    
    
    
  }
  
  LassoCoefsNoPD <- function(lasso, model.list)
  {
    
    # Penalized
    penalized.broom = coef(lasso) %>% as.data.frame()
    penalized.broom = penalized.broom %>% mutate(temp = row.names(penalized.broom))
    names(penalized.broom) <- c("estimate", "term")
    
    penalized.broom = penalized.broom %>% mutate(AgeFactor = as.numeric(stringr::str_extract(term,"\\d+\\.*\\d*")))
    penalized.broom = penalized.broom %>% 
      mutate(Type = ifelse(grepl("gain.cohort.small", term) & grepl("AgeFactor", term), "gain.cohort.small",
                           ifelse(grepl("gain.cohort.stock", term) & grepl("AgeFactor", term), "gain.cohort.stock",
                                  ifelse(grepl("gain.cohort.infra", term) & grepl("AgeFactor", term), "gain.cohort.infra",
                                         ifelse(grepl("gain.cohort.reit", term) & grepl("AgeFactor", term), "gain.cohort.reit",
                                                ifelse(grepl("gain.cohort.growth", term) & grepl("AgeFactor", term), "gain.cohort.growth",
                                                       ifelse(grepl("gain.cohort.nr", term) & grepl("AgeFactor", term), "gain.cohort.nr",
                                                              ifelse(grepl("cohort.stock", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.stock",
                                                                     ifelse(grepl("cohort.value", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.value",
                                                                            ifelse(grepl("cohort.infra", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.infra",
                                                                                   ifelse(grepl("cohort.reit", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.reit",
                                                                                          ifelse(grepl("cohort.small", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.small",
                                                                                                 ifelse(grepl("cohort.growth", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.growth",
                                                                                                        ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                               ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                                      ifelse(grepl("gain.cohort.nr", term) & grepl("pd_category", term), "gain.cohort.nr.pd",
                                                                                                                             ifelse(grepl("gain.cohort.infra", term) & grepl("pd_category", term), "gain.cohort.infra.pd",
                                                                                                                                    ifelse(grepl("gain.cohort.small", term)& grepl("pd_category", term), "gain.cohort.small.pd", 
                                                                                                                                           ifelse(grepl("gain.cohort.growth", term)& grepl("pd_category", term), "gain.cohort.growth.pd", 
                                                                                                                                                  ifelse(grepl("gain.cohort.value", term)& grepl("pd_category", term), "gain.cohort.value.pd", 
                                                                                                                                                         ifelse(grepl(":", term) == 0 & grepl("AgeFactor", term), "bond", 
                                                                                                                                                                ifelse(grepl("cohort.reit", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.reit.pd",
                                                                                                                                                                       ifelse(grepl("cohort.nr", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.nr.pd",
                                                                                                                                                                              ifelse(grepl("cohort.stock", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.stock.pd",
                                                                                                                                                                                     ifelse(grepl("cohort.nr", term) & grepl("AgeFactor", term), "cohort.nr",
                                                                                                                                                                                            ifelse(grepl("cohort.infra", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.infra.pd",
                                                                                                                                                                                                   ifelse(grepl("cohort.small", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.small.pd", 
                                                                                                                                                                                                          ifelse(grepl("cohort.growth", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.growth.pd", 
                                                                                                                                                                                                                 ifelse(grepl("cohort.value", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.value.pd", 
                                                                                                                                                                                                                        ifelse(grepl(":", term) == 0 & grepl("pd_category", term), "bond.pd","")))))))))))))))))))))))))))))) %>% filter(Type != "")
    
    penalized.broom.age = penalized.broom %>% mutate(three.factor.pen  = estimate) %>%
      filter(grepl(".pd", Type) == 0) %>% dplyr::select(AgeFactor, three.factor.pen, Type)
    
    
    
    
    penalized.wide.age = spread(penalized.broom.age, Type, three.factor.pen) %>% mutate_all(funs(replace(., is.na(.), 0)))
    
    
    
    final.pen = list(penalized.wide.age)
    
    return(final.pen)  
    
    
    
  }
  
  LassoCoefsPDM <- function(lasso, model.list)
  {
    
    # Penalized
    penalized.broom = coef(lasso) %>% as.data.frame()
    penalized.broom = penalized.broom %>% mutate(temp = row.names(penalized.broom))
    names(penalized.broom) <- c("estimate", "term")
    
    penalized.broom = penalized.broom %>% mutate(AgeFactor = as.numeric(stringr::str_extract(term,"\\d+\\.*\\d*")))
    penalized.broom = penalized.broom %>% 
      mutate(Type = ifelse(grepl("gain.cohort.small", term) & grepl("AgeFactor", term), "gain.cohort.small",
                           ifelse(grepl("gain.cohort.stock", term) & grepl("AgeFactor", term), "gain.cohort.stock",
                                  ifelse(grepl("gain.cohort.infra", term) & grepl("AgeFactor", term), "gain.cohort.infra",
                                         ifelse(grepl("gain.cohort.reit", term) & grepl("AgeFactor", term), "gain.cohort.reit",
                                                ifelse(grepl("gain.cohort.growth", term) & grepl("AgeFactor", term), "gain.cohort.growth",
                                                       ifelse(grepl("gain.cohort.nr", term) & grepl("AgeFactor", term), "gain.cohort.nr",
                                                              ifelse(grepl("cohort.stock", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.stock",
                                                                     ifelse(grepl("cohort.infra", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.infra",
                                                                            ifelse(grepl("cohort.reit", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.reit",
                                                                                   ifelse(grepl("cohort.small", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.small",
                                                                                          ifelse(grepl("cohort.growth", term) & grepl("AgeFactor", term) & grepl("gain", term) == 0, "cohort.growth",
                                                                                                 ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                        ifelse(grepl("gain.cohort.stock", term) & grepl("pd_category", term), "gain.cohort.stock.pd",
                                                                                                               ifelse(grepl("gain.cohort.nr", term) & grepl("pd_category", term), "gain.cohort.nr.pd",
                                                                                                                      ifelse(grepl("gain.cohort.infra", term) & grepl("pd_category", term), "gain.cohort.infra.pd",
                                                                                                                             ifelse(grepl("gain.cohort.small", term)& grepl("pd_category", term), "gain.cohort.small.pd", 
                                                                                                                                    ifelse(grepl("gain.cohort.growth", term)& grepl("pd_category", term), "gain.cohort.growth.pd", 
                                                                                                                                           ifelse(grepl("gain.cohort.value", term)& grepl("pd_category", term), "gain.cohort.value.pd", 
                                                                                                                                                  ifelse(grepl(":", term) == 0 & grepl("AgeFactor", term), "bond", 
                                                                                                                                                         ifelse(grepl("cohort.reit", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.reit.pd",
                                                                                                                                                                ifelse(grepl("cohort.nr", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.nr.pd",
                                                                                                                                                                       ifelse(grepl("cohort.stock", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.stock.pd",
                                                                                                                                                                              ifelse(grepl("cohort.nr", term) & grepl("AgeFactor", term), "cohort.nr",
                                                                                                                                                                                     ifelse(grepl("cohort.infra", term) & grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.infra.pd",
                                                                                                                                                                                            ifelse(grepl("cohort.small", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.small.pd", 
                                                                                                                                                                                                   ifelse(grepl("cohort.growth", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.growth.pd", 
                                                                                                                                                                                                          ifelse(grepl("cohort.value", term)& grepl("pd_category", term) & grepl("gain", term) == 0, "cohort.value.pd", 
                                                                                                                                                                                                                 ifelse(grepl(":", term) == 0 & grepl("pd_category", term), "bond.pd",""))))))))))))))))))))))))))))) %>% filter(Type != "")
    
    penalized.broom.age = penalized.broom %>% mutate(three.factor.pen  = estimate) %>%
      filter(grepl(".pd", Type) == 0) %>% dplyr::select(AgeFactor, three.factor.pen, Type)
    
    
    
    penalized.broom.pd = penalized.broom %>% mutate(three.factor.pen.pd  = estimate) %>%
      filter(grepl(".pd", Type) == 1) %>% dplyr::select(AgeFactor, three.factor.pen.pd, Type) %>% 
      mutate(pd_categoryFactorQuartile = AgeFactor) %>% select(-AgeFactor)
    
    penalized.broom.pd = penalized.broom.pd %>% 
      group_by(Type) %>% mutate(avg.factor = mean(three.factor.pen.pd)) %>% as.data.frame() %>%
      mutate(deviation.factor = three.factor.pen.pd - avg.factor)
    
    
    penalized.wide.pd = spread(penalized.broom.pd, Type, three.factor.pen.pd) %>% mutate_all(funs(replace(., is.na(.), 0)))
    penalized.wide.age = spread(penalized.broom.age, Type, three.factor.pen) %>% mutate_all(funs(replace(., is.na(.), 0)))
    
    
    
    final.pen = list(penalized.wide.age, penalized.wide.pd)
    
    return(final.pen)  
    
    
    
  }

  # skeleton = strip.data.vintage.pen.7 model.list = model.list7 price.list = price.list7
  QGenerate <- function(skeleton, model.list, price.list) {
    
    # replace coefficients to be zero for missing vars
    skeleton[is.na(skeleton)] <- 0
    
    
    
    # first need to adjust bond positions for the pd corrections
    for (number in 1:length(model.list)) {
      
      div = model.list[number]
      gz.correction = paste0(div, ".pd")
      
      # adjusted coefficient for pd term
      skeleton$temp = as.numeric(unlist(skeleton[div])) + as.numeric(unlist(skeleton[gz.correction]))
      
      myname = paste0("adj.", div)
      skeleton = mutate(skeleton, !!myname := temp)
      
      
    }
    
    
    
    
    
    # next generate the distribution amounts; which are div * price
    for (number in 1:length(model.list)) {
      
      div.unadj = model.list[number]
      
      
      div = paste0("adj.", model.list[number])
      price = price.list[number]
      
      myname = paste0("distribution.weight.", div)
      myname2 = paste0("distribution.unadj.weight.", div.unadj)
      
      skeleton$temp = as.numeric(unlist(skeleton[div])) * as.numeric(unlist(skeleton[price]))
      skeleton = mutate(skeleton, !!myname := temp)
      
      skeleton$temp2 = as.numeric(unlist(skeleton[div.unadj])) * as.numeric(unlist(skeleton[price]))
      skeleton = mutate(skeleton, !!myname2 := temp2)
      
    }
    
    
    
 
    return(skeleton)
    
    
  }
  
  HorizonRepresentationAge <- function(skeleton) 
  {
    age.coef = skeleton %>% 
      gather(model, value, -AgeFactor)
    
    age.coef = age.coef %>%
      mutate(Type = ifelse(model == "bond", "Bond",
                           ifelse(model == "cohort.small", "Small",
                                  ifelse(model == "cohort.stock", "Stock",
                                         ifelse(model == "cohort.value", "Value",
                                                ifelse(model == "cohort.growth", "Growth",
                                                       ifelse(model == "gain.cohort.small", "Small Gain",
                                                              ifelse(model == "gain.cohort.stock", "Stock Gain",
                                                                     ifelse(model == "cohort.reit", "REIT",
                                                                            ifelse(model == "cohort.infra", "Infra",
                                                                                   ifelse(model == "cohort.growth", "Growth",
                                                                                          ifelse(model == "gain.cohort.reit", "REIT Gain",
                                                                                                 ifelse(model == "gain.cohort.infra", "Infra Gain",
                                                                                                        ifelse(model == "gain.cohort.growth", "Growth Gain",
                                                                                                               ifelse(model == "cohort.nr", "Natural Resources (NR)",
                                                                                                                      ifelse(model == "gain.cohort.nr", "NR Gain", ""))))))))))))))))
    
    
    
    
    
    
    age.coef = age.coef %>%
      group_by(Type) %>%
      mutate(sum.estimates = sum(value)) %>% as.data.frame() %>%
      filter(sum.estimates != 0) %>% select(-sum.estimates)
    
    

    return(age.coef)
  }
  
  HorizonRepresentationPD <- function(skeleton) 
  {
   
    
    age.coef = age.coef %>% 
      select(pd_categoryFactorQuartile, contains(("q."))) %>%
      gather(model, value, contains(("q."))) %>%
      filter(!is.infinite(value)) %>% 
      group_by(model, pd_categoryFactorQuartile) %>% 
      mutate(value = mean(value, na.rm = TRUE)) %>%
      select(pd_categoryFactorQuartile, model, value) %>% unique %>% as.data.frame()
    
    age.coef = age.coef %>%
      mutate(Type = ifelse(model == "q.bond", "Bond",
                           ifelse(model == "q.cohort.small", "Small",
                                  ifelse(model == "q.cohort.stock", "Stock",
                                         ifelse(model == "q.gain.cohort.small", "Small Gain",
                                                ifelse(model == "q.gain.cohort.stock", "Stock Gain",
                                                       ifelse(model == "q.cohort.reit", "REIT",
                                                              ifelse(model == "q.cohort.infra", "Infra",
                                                                     ifelse(model == "q.cohort.growth", "Growth",
                                                                            ifelse(model == "q.gain.cohort.reit", "REIT Gain",
                                                                                   ifelse(model == "q.gain.cohort.infra", "Infra Gain",
                                                                                          ifelse(model == "q.gain.cohort.growth", "Growth Gain", 
                                                                                                 ifelse(model == "q.cohort.nr", "Natural Resources",
                                                                                                        ifelse(model == "q.gain.cohort.nr", "Natural Resources Gain", ""))))))))))))))
    
    
    
    
    
    age.coef = age.coef %>%
      group_by(Type) %>%
      mutate(sum.estimates = sum(value)) %>% as.data.frame() %>%
      filter(sum.estimates != 0) %>% select(-sum.estimates)
    
    
    
    return(age.coef)
  }
  
  
  HorizonRepresentationPDAlt <- function(skeleton) 
  {
   
    
    age.coef = skeleton %>% 
      select(pd_categoryFactorQuartile, contains((".pd"))) %>%
      gather(model, value, contains((".pd"))) %>%
      filter(!is.infinite(value)) %>% 
      group_by(model, pd_categoryFactorQuartile) %>% 
      mutate(value = mean(value, na.rm = TRUE)) %>%
      select(pd_categoryFactorQuartile, model, value) %>% unique %>% as.data.frame()
    
    age.coef = age.coef %>%
      mutate(Type = ifelse(model == "bond.pd", "Bond",
                           ifelse(model == "cohort.small.pd", "Small",
                                  ifelse(model == "cohort.stock.pd", "Stock",
                                         ifelse(model == "gain.cohort.small.pd", "Small Gain",
                                                ifelse(model == "gain.cohort.stock.pd", "Stock Gain",
                                                       ifelse(model == "cohort.reit.pd", "REIT",
                                                              ifelse(model == "cohort.infra.pd", "Infra",
                                                                     ifelse(model == "cohort.growth.pd", "Growth",
                                                                            ifelse(model == "gain.cohort.reit.pd", "REIT Gain",
                                                                                   ifelse(model == "gain.cohort.infra.pd", "Infra Gain",
                                                                                          ifelse(model == "gain.cohort.growth.pd", "Growth Gain", 
                                                                                                 ifelse(model == "cohort.nr.pd", "Natural Resources",
                                                                                                        ifelse(model == "gain.cohort.nr.pd", "Natural Resources Gain", ""))))))))))))))
    
    
    
    
    return(age.coef)
  }
  # skeleton = strip.data.vintage.pen.q.2
  ExpectedReturns <- function(skeleton, model.list, price.list, return.list)
  {
    # first need to adjust bond positions for the pd corrections
    for (number in 1:length(model.list)) {
      
      div = model.list[number]  # ie, cohort.stock
      q = paste0("distribution.weight.adj.", div)     # distribution.weight.adj.cohort.stock
      price = price.list[number]  # price.stock.feb
      return = return.list[number]
      
      # construct price * q (the distribution.weight)
      skeleton$temp = as.numeric(unlist(skeleton[q])) 
      
      myname = paste0("return.weight.", q)
      skeleton = mutate(skeleton, !!myname := temp)
      
      # returns
      skeleton$temp2 = as.numeric(unlist(skeleton[myname])) * as.numeric(unlist(skeleton[return]))
      myname2 = paste0("return.factor.", div)
      skeleton = mutate(skeleton, !!myname2 := temp2)
      
    }
    
    
    # total weights across factors
    skeleton$quarterly.weights <- as.numeric(rowSums(skeleton[,grepl("return.weight.", names(skeleton))]))
    
    # Rescaled weights
    skeleton = skeleton %>%
      group_by(Vintage.Quarter) %>%
      mutate(total.weights = sum(quarterly.weights, na.rm = TRUE)) %>%
      as.data.frame()
    
    skeleton = skeleton %>%
      mutate(quarterly.weights.rescale = quarterly.weights / total.weights)
    
    # Verify sum to one
    skeleton = skeleton %>% group_by(Vintage.Quarter) %>%
      mutate(total.sum = sum(quarterly.weights.rescale, na.rm = TRUE))  %>%
      as.data.frame()
    #summary(skeleton$total.sum)
    
    
    # total return across factors
    skeleton$expected.return <- as.numeric(rowSums(skeleton[,grepl("return.factor.", names(skeleton))]))
    
    
    skeleton = skeleton %>% 
      group_by(Vintage.Quarter) %>%  mutate(Age = row_number())  %>% as.data.frame() 
    
    skeleton = skeleton %>% mutate(quarterly.weights.age = quarterly.weights.rescale * Age)
    skeleton = skeleton %>% group_by(Vintage.Quarter) %>% 
      mutate(total.sum = sum(quarterly.weights.age, na.rm = TRUE)/4) %>% as.data.frame()
    
    skeleton = skeleton %>% mutate(total.sum = ifelse(total.sum < 3, 3,
                                                      ifelse(total.sum > 15, 15, total.sum)))
    
    
    skeleton = skeleton %>% group_by(Vintage.Quarter) %>% mutate(total.expected.return = sum(expected.return, na.rm = TRUE)) %>% as.data.frame()
    
    
    # annualize return
    skeleton.collapse = skeleton %>% select(Vintage.Quarter, total.expected.return, total.sum) %>% unique
    skeleton.collapse = skeleton.collapse %>%  mutate(annualized.return = ((1  + total.expected.return)^(1/(total.sum))  - 1)) %>% as.data.frame()  
    
    
    
    
    # Horizon effects
    age.expected.return = skeleton %>%
      select(Age.Quarter, contains("return.factor.")) %>%
      gather(model, value, contains(("return.factor."))) %>%
      group_by(model, Age.Quarter) %>% 
      select(Age.Quarter, model, value) %>% unique %>% as.data.frame()
    
    
    
    age.expected.return = age.expected.return %>%
      mutate(Type = ifelse(model == "return.factor.bond", "Bond",
                           ifelse(model == "return.factor.cohort.small", "Small",
                                  ifelse(model == "return.factor.cohort.value", "Value",
                                         ifelse(model == "return.factor.cohort.stock", "Stock",
                                                ifelse(model == "return.factor.gain.cohort.small", "Small Gain",
                                                       ifelse(model == "return.factor.gain.cohort.stock", "Stock Gain",
                                                              ifelse(model == "return.factor.cohort.reit", "REIT",
                                                                     ifelse(model == "return.factor.cohort.infra", "Infra",
                                                                            ifelse(model == "return.factor.cohort.growth", "Growth",
                                                                                   ifelse(model == "return.factor.gain.cohort.reit", "REIT Gain",
                                                                                          ifelse(model == "return.factor.gain.cohort.infra", "Infra Gain",
                                                                                                 ifelse(model == "return.factor.gain.cohort.growth", "Growth Gain", 
                                                                                                        ifelse(model == "return.factor.cohort.nr", "Natural Resources",
                                                                                                               ifelse(model == "return.factor.gain.cohort.nr", "Natural Resources Gain", "")))))))))))))))
    
    
    
    
    
    age.expected.return.collapsed = age.expected.return %>%
      group_by(Age.Quarter, Type) %>%
      mutate(expected.return.new = mean(value, na.rm = TRUE)) %>% 
      filter(Age.Quarter <= 14.75) %>% as.data.frame() %>% 
      select(Age.Quarter, expected.return.new, Type) %>% unique 
    
    
    
    
    total.expected.return = list(skeleton.collapse, age.expected.return.collapsed)
    return(total.expected.return)
    
    
    
    
  }
  
  
  #strip.data.vintage.pen.q.2$newvariable <- as.numeric(rowSums(strip.data.vintage.pen.q.2[,grepl("distribution.weight.adj", names(strip.data.vintage.pen.q.2))]))
  
  #strip.data.vintage.pen.q.7$newvariable <- as.numeric(rowSums(strip.data.vintage.pen.q.7[,grepl("distribution.weight.adj", names(strip.data.vintage.pen.q.7))]))
  
  # skeleton = strip.data.vintage.pen.q.5
    # skeleton = strip.data.vintage.pen.q.2 predictions =  fund.subset.predictions2 fund.subset.base = fund.subset.new
 
  # %>%filter(Fund.ID == 7105); turns out to be similar in profits
  # %>%filter(Fund.ID == 406); 50 cent discrepancy but reasonable in that both positive
  # %>%filter(Fund.ID == 447); try this one
  
  # weirdo = fund.subset.new %>% filter(Fund.ID == 406)
  # time5 = strip.data.vintage.pen.q.5 %>% filter(Vintage.Quarter == 1999.75)
  # time5.factor = time5 %>% select(contains("distribution.weight.adj"))
  # time7 = strip.data.vintage.pen.q.7 %>% filter(Vintage.Quarter == 1999.75)
  # 
  
  
  # 5 factor
  
  # temp5 = profit.subset.pen.5 %>% mutate(total.profit5 = total.profit) %>% select(total.profit5, Fund.ID, Vintage.New)
  # temp7 = profit.subset.pen.7 %>% mutate(total.profit7 = total.profit) %>% select(total.profit7, Fund.ID, Vintage.New)
  # temp.both = left_join(temp5, temp7)
  
  
  # fund.7 = fund.subset.predictions7 %>%  filter(Fund.ID == 447) 
  
  # skeleton = strip.data.vintage.pen.q.7 %>% filter(Vintage.Quarter == 1999.75)
  # skeleton = strip.data.vintage.pen.q.5 %>% filter(Vintage.Quarter == 1999.75)
  # predictions = fund.subset.predictions7 %>%   filter(Fund.ID == 447) 
  # predictions = fund.subset.predictions5 %>%   filter(Fund.ID == 447) 
  # fund.subset.base = fund.subset.new %>%   filter(Fund.ID == 447) 
  
  # temp.profit.pen.7 = Profit(strip.data.vintage.pen.q.7,fund.subset.predictions7, fund.subset.new )
  
  
  Profit <- function(skeleton, predictions, fund.subset.base) # skeleton = strip.data.vintage.pen.q.7  predictions = fund.subset.predictions7 fund.subset.base = fund.subset.new
  {
     
    # skeleton.temp = skeleton %>% select(contains("distribution.weight.adj"), Vintage.Quarter) 
    # 
    #
    # time5.factor = time5 %>% select(contains("distribution.weight.adj"), Vintage.Quarter) 
    #skeleton = time5.factor
    # time7.factor = time7 %>% select(contains("distribution.weight.adj"), Vintage.Quarter) 
    #skeleton = time7.factor
     
    
    # Sum the variables to form denominator
    skeleton$newvariable <- as.numeric(rowSums(skeleton[,grepl("distribution.weight.adj", names(skeleton))]))
    
    skeleton = skeleton %>% group_by(Vintage.Quarter) %>% 
      mutate(total.dist = sum(newvariable, na.rm = TRUE)) %>% as.data.frame() %>%
      mutate(profits.adj =  -1 + total.dist)
    
    profits.adj = skeleton %>% select(Vintage.Quarter, total.dist, profits.adj) %>% unique
    
    predictions = predictions %>% select(-net.cf.distribution.rescale)
    
    # Calculate profits here using the alternate approach
    fund.subset.new.profit.adj = left_join(fund.subset.base, predictions) 
    
    
    fund.subset.new.profit.adj = fund.subset.new.profit.adj %>%
      mutate(period.excess.profit.adj = net.cf.distribution.rescale - predictions, 
             discounted.period.excess.profit.adj = period.excess.profit.adj * price.bond.feb)
    
    fund.subset.new.profit.adj = fund.subset.new.profit.adj %>% group_by(Fund.ID) %>%
      mutate(total.profit.adj = sum(discounted.period.excess.profit.adj, na.rm = TRUE)) %>% as.data.frame()
    
    
    fund.subset.new.profit.fund.id = fund.subset.new.profit.adj %>% 
      select(Vintage.Quarter, Fund.ID, Firm.ID, total.profit.adj, pme, tvpi, irr, Vintage.New) %>% unique()
    
    fund.subset.new.profit.fund.id = left_join(fund.subset.new.profit.fund.id, profits.adj)
    
    
    fund.subset.new.profit.fund.id = fund.subset.new.profit.fund.id %>%
      mutate(total.profit = total.profit.adj + profits.adj)
    
    

    
    # Aggregate profit
    profit.subset = fund.subset.new.profit.fund.id %>% select(total.profit, Firm.ID, Fund.ID, Vintage.New, pme, tvpi, irr) %>% 
      unique %>% filter(Vintage.New <= 2010) %>% mutate(positive.profit = ifelse(total.profit > 0, "Excess Profits", " Losses"))
    
    #avg_profit.re = round(mean(profit.subset$total.profit), 3)
    
    profit.subset = profit.subset %>% mutate(is.above.10 = ifelse(total.profit > 0.10, 1, 0))
    
    profit.subset.vintage = profit.subset %>% group_by(Vintage.New) %>% mutate(avg.profit = mean(total.profit, na.rm = TRUE)) %>% select(Vintage.New, avg.profit)
    
    
    
    
    
    
    
    
    profit.return = list(profit.subset, profit.subset, profit.subset.vintage)
    
    return(profit.return)
    
    
    
  }
  
 
  
  NameClean <- function(skeleton) 
  {
    
    
    
    skeleton = skeleton %>%
      mutate(Type = ifelse(category == "bond", "Bond",
                           ifelse(category == "cohort.small", "Small",
                                  ifelse(category == "cohort.stock", "Stock",
                                         ifelse(category == "cohort.value", "Value",
                                                ifelse(category == "cohort.growth", "Growth",
                                                       ifelse(category == "gain.cohort.small", "Small Gain",
                                                              ifelse(category == "gain.cohort.stock", "Stock Gain",
                                                                     ifelse(category == "cohort.reit", "REIT",
                                                                            ifelse(category == "cohort.infra", "Infra",
                                                                                   ifelse(category == "cohort.growth", "Growth",
                                                                                          ifelse(category == "gain.cohort.reit", "REIT Gain",
                                                                                                 ifelse(category == "gain.cohort.infra", "Infra Gain",
                                                                                                        ifelse(category == "gain.cohort.growth", "Growth Gain",
                                                                                                               ifelse(category == "cohort.nr", "Natural Resources",
                                                                                                                      ifelse(category == "gain.cohort.nr", "Natural Resources Gain", ""))))))))))))))))
    
    
    
    
    
    
    
    
    skeleton = skeleton %>%
      group_by(category) %>%
      mutate(sum.estimates = sum(value)) %>% as.data.frame() %>%
      filter(sum.estimates != 0) %>% select(-sum.estimates)
    
    
    
    
    return(skeleton)
  }
  
} 


# Run Analysis
for (i in 1:nrow(fund.categories) ) {
  
  #####################################  Subsetting ###################################
  
  { 
    fund.subset <- fund.quarterly.div %>% filter(fund.category == fund.categories[i,1])
     
    fund.subset.quarterly <- portfolio.fund.quarterly %>% 
      filter(fund.category == fund.categories[i,1]) %>%
      filter( !is.na(gain.cohort.stock) & !is.na(cohort.value) )  
    
      }
  
  #####################################  Matrix Generation ###################################
  
  
  # Standard full factor matrix
  {
    
    
    # Matrix for y Variables
    fund.subset.y = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
    
    # Matrix for x variables
    
    # Age Dummies
    AgeFactor = fund.subset
    dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
    fund.subset.age <- cbind(fund.subset, dummies)
    
    # PD Dummies
    PD = fund.subset
    dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
    fund.subset.pd <- cbind(fund.subset, dummiesPD)
    
    
    if(i == 5){
      
      fund.subset.age = fund.subset.age %>%
        mutate(AgeFactor_13 = 0,
               AgeFactor_14 = 0,
               AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
    }
    
    if(i == 8){
      
      fund.subset.age = fund.subset.age %>%
        mutate(AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
    }
    
     
    
    # Cross dummies with all div vars to generate exposures
    model.list7 = c("bond", 
                    "cohort.stock", 
                    "cohort.small", 
                    "cohort.growth", 
                    "cohort.reit", 
                    "cohort.infra", 
                    "cohort.nr",
                    "cohort.value",
                    "gain.cohort.stock",
                    "gain.cohort.small",
                    "gain.cohort.growth",
                    "gain.cohort.reit",
                    "gain.cohort.infra",
                    "gain.cohort.nr",
                    "gain.cohort.value")
    
    
    for (number in 1:length(model.list7)) {
      
      for(year in 1:16) {
        div = model.list7[number]
        new.name = paste0(div, "_", year)
        age.name = paste0("AgeFactor_", year)
        
        # Standard
        fund.subset.age$temp = as.numeric(unlist(fund.subset.age[div])) * as.numeric(unlist(fund.subset.age[age.name]))
        fund.subset.age = mutate(fund.subset.age, !!new.name := temp)
        
        
      }
    
      for(pd in 1:4) {
        div = model.list7[number]
        new.name = paste0(div, "_pd_", pd)
        pd.name = paste0("pd_categoryFactorQuartile_", pd)
        
        # PD Coefs
        fund.subset.pd$temp = as.numeric(unlist(fund.subset.pd[div])) * as.numeric(unlist(fund.subset.pd[pd.name]))
        fund.subset.pd = mutate(fund.subset.pd, !!new.name := temp)
        
        
      }
      
      
      }
    
    # Age Subset
    fund.subset.x.age = fund.subset.age %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.small_"),
             starts_with("cohort.stock_"),
             starts_with("cohort.growth_"),
             starts_with("cohort.reit_"),
             starts_with("cohort.infra_"),
             starts_with("cohort.nr_"),
             starts_with("cohort.value_"),
             contains("gain.cohort.stock_"),
             contains("gain.cohort.small_"),
             contains("gain.cohort.growth_"),
             contains("gain.cohort.reit_"),
             contains("gain.cohort.infra_"),
             contains("gain.cohort.nr_"),
             contains("gain.cohort.value_")) %>% as.matrix()
  
    
    # PD Subset
    fund.subset.x.pd = fund.subset.pd %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.small_"),
             starts_with("cohort.stock_"),
             starts_with("cohort.growth_"),
             starts_with("cohort.reit_"),
             starts_with("cohort.infra_"),
             starts_with("cohort.nr_"),
             starts_with("cohort.value_"),
             contains("gain.cohort.stock_"),
             contains("gain.cohort.small_"),
             contains("gain.cohort.growth_"),
             contains("gain.cohort.reit_"),
             contains("gain.cohort.infra_"),
             contains("gain.cohort.nr_"),
             contains("gain.cohort.value_")) %>% as.matrix()
    
    
    # Full X Matrix
    fund.subset.x.age.pd = cbind(fund.subset.x.age,fund.subset.x.pd )
    
  }

   
  # 2 factor model 
  {
    # 2 factor model
    # Age Subset
    fund.subset.x.age.2 = fund.subset.age %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.stock_")) %>% as.matrix()
    
    
    # PD Subset
    fund.subset.x.pd.2 = fund.subset.pd %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.stock_")) %>% as.matrix()
    
    
    # Full x Matrix
    fund.subset.x.age.pd.2 = cbind(fund.subset.x.age.2,fund.subset.x.pd.2)
    
    
    
    
    
    
  }
  
  
  # Quarterly model
  {
    
    
    # Matrix for y Variables
    fund.subset.y.quarterly = fund.subset.quarterly %>% select(net.cf.distribution.rescale) %>% as.matrix()
    
    # Matrix for x variables
    
    # Age Dummies
    AgeFactor = fund.subset.quarterly
    dummies.quarterly = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
    fund.subset.age.quarterly <- cbind(fund.subset.quarterly, dummies.quarterly)
    
    # PD Dummies
    PD.quarterly = fund.subset.quarterly
    dummiesPD.quarterly = dummies::dummy(PD.quarterly$pd_categoryFactorQuartile, sep = "_")
    fund.subset.pd.quarterly <- cbind(fund.subset.quarterly, dummiesPD.quarterly)
     
    
    if(i == 5){
      
      fund.subset.age.quarterly = fund.subset.age.quarterly %>%
        mutate(AgeFactor_13 = 0,
               AgeFactor_14 = 0,
               AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
    }
    
    if(i == 8){
      
      fund.subset.age.quarterly = fund.subset.age.quarterly %>%
        mutate(AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
    }
    
    for (number in 1:length(model.list7)) {
      
      for(year in 1:16) {
        div = model.list7[number]
        new.name = paste0(div, "_", year)
        age.name = paste0("AgeFactor_", year)
        
        # Standard
        fund.subset.age.quarterly$temp = as.numeric(unlist(fund.subset.age.quarterly[div])) * as.numeric(unlist(fund.subset.age.quarterly[age.name]))
        fund.subset.age.quarterly = mutate(fund.subset.age.quarterly, !!new.name := temp)
        
        
      }
      
      for(pd in 1:4) {
        div = model.list7[number]
        new.name = paste0(div, "_pd_", pd)
        pd.name = paste0("pd_categoryFactorQuartile_", pd)
        
        # PD Coefs
        fund.subset.pd.quarterly$temp = as.numeric(unlist(fund.subset.pd.quarterly[div])) * as.numeric(unlist(fund.subset.pd.quarterly[pd.name]))
        fund.subset.pd.quarterly = mutate(fund.subset.pd.quarterly, !!new.name := temp)
        
        
      }
      
      
    }
    
    # Age Subset
    fund.subset.x.age.quarterly = fund.subset.age.quarterly %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.small_"),
             starts_with("cohort.stock_"),
             starts_with("cohort.growth_"),
             starts_with("cohort.reit_"),
             starts_with("cohort.infra_"),
             starts_with("cohort.nr_"),
             starts_with("cohort.value_"),
             contains("gain.cohort.stock_"),
             contains("gain.cohort.small_"),
             contains("gain.cohort.growth_"),
             contains("gain.cohort.reit_"),
             contains("gain.cohort.infra_"),
             contains("gain.cohort.nr_"),
             contains("gain.cohort.value_")) %>% as.matrix()
    
    
    # PD Subset
    fund.subset.x.pd.quarterly = fund.subset.pd.quarterly %>% 
      select(starts_with("bond_"), 
             starts_with("cohort.small_"),
             starts_with("cohort.stock_"),
             starts_with("cohort.growth_"),
             starts_with("cohort.reit_"),
             starts_with("cohort.infra_"),
             starts_with("cohort.nr_"),
             contains("gain.cohort.stock_"),
             contains("gain.cohort.small_"),
             contains("gain.cohort.growth_"),
             contains("gain.cohort.reit_"),
             contains("gain.cohort.infra_"),
             contains("gain.cohort.nr_"),
             contains("gain.cohort.value_")) %>% as.matrix()
    
    
    # Full X Matrix
    fund.subset.x.age.pd.quarterly = cbind(fund.subset.x.age.quarterly,fund.subset.x.pd.quarterly )
    
  }
  
  
  
  
  
  
  #####################################  Analysis: Model Fit ###################################
  
  
  # PE 
  # i = 1
  if(i == 1 | i == 3 | i == 4 ) {
    

    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.small", "gain.cohort.small", "gain.cohort.stock")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.stock")
    return.list1 =  c("bond.return", "stock.return", "small.return", "small.gain.return", "stock.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "stock.return", "small.return")
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.stock", 
                      "cohort.small", 
                      "gain.cohort.stock",
                      "gain.cohort.small")
      
      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.stock", 
                      "cohort.small")
      
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                         nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
     
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
     
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
      
  }
  
  
  # VC
  # i = 2
  if(i == 2) {
    
    lambda1 = 1
    
    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.growth", "cohort.small", "gain.cohort.small", "gain.cohort.growth")
    price.list1 =  c("price.bond.feb", "price.growth.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.growth")
    return.list1 =  c("bond.return", "growth.return", "small.return", "small.gain.return", "growth.gain.return")
    
    
    model.list2 = c("bond", "cohort.growth", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.growth.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "growth.return", "small.return")
    
    
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.growth", 
                      "cohort.small", 
                      "gain.cohort.growth",
                      "gain.cohort.small")
      
      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.growth_"),
               contains("gain.cohort.growth_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.growth_"),
               contains("gain.cohort.growth_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.growth", 
                      "cohort.small")
      
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.growth_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.growth_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
    
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
  }
  
  
  # RE 
  # i = 6
  if(i == 6) {
    
    lambda1 = 0.5
    
    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.reit", "gain.cohort.stock", "gain.cohort.reit")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.reit.feb", "price.cohort.stock", "price.cohort.reit")
    return.list1 =  c("bond.return", "stock.return", "reit.return", "stock.gain.return", "growth.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.reit")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.reit.feb")
    return.list2 =  c("bond.return", "stock.return", "reit.return")
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.stock", 
                      "cohort.reit", 
                      "gain.cohort.stock",
                      "gain.cohort.reit")
      
      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.reit_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.reit_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.reit_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.reit_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.stock", 
                      "cohort.reit")
      
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.stock_"),
               starts_with("cohort.reit_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.reit_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
    
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    
    
  }
  
  
  # IN 
  # i = 5
  if(i == 5) {
    
    lambda1 = 0.25
    
    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.infra", "gain.cohort.stock", "gain.cohort.infra")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.infra.feb", "price.cohort.stock", "price.cohort.infra")
    return.list1 =  c("bond.return", "stock.return", "infra.return", "stock.gain.return", "infra.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.infra")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.infra.feb")
    return.list2 =  c("bond.return", "stock.return", "infra.return")
    
    
     
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      fund.subset.age.5 = fund.subset.age.5 %>%
        mutate(AgeFactor_13 = 0,
               AgeFactor_14 = 0,
               AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.stock", 
                      "cohort.infra", 
                      "gain.cohort.stock",
                      "gain.cohort.infra")
      
      
      
      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.infra_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.infra_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.infra_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.infra_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      fund.subset.age.3 = fund.subset.age.3 %>%
        mutate(AgeFactor_13 = 0,
               AgeFactor_14 = 0,
               AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.stock", 
                      "cohort.infra")
      
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.stock_"),
               starts_with("cohort.infra_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.infra_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
    
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
  }
  
  # NR 
  if(i == 7) {
    
    lambda1 = 0.25
    
    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.nr", "gain.cohort.stock", "gain.cohort.nr")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.nr.feb", "price.cohort.stock", "price.cohort.nr")
    return.list1 =  c("bond.return", "stock.return", "nr.return", "stock.gain.return", "nr.gain.return")
    
    
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.stock", 
                      "cohort.nr", 
                      "gain.cohort.stock",
                      "gain.cohort.nr")
      
      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.nr_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.nr_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.nr_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.nr_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.stock", 
                      "cohort.nr")
      
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.stock_"),
               starts_with("cohort.nr_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.nr_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
    
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    
  }
  
  # DF 
  # i = 8
  if(i ==  8 ) {
    
    
    model.list0 = c("bond", "cohort.stock")
    price.list0 =  c("price.bond.feb", "price.stock.feb")
    return.list0 =  c("bond.return", "stock.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.small", "gain.cohort.small", "gain.cohort.stock")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.stock")
    return.list1 =  c("bond.return", "stock.return", "small.return", "small.gain.return", "stock.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "stock.return", "small.return")
    
    
    # 5 factor
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.5 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.5 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.5 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list5 = c("bond", 
                      "cohort.stock", 
                      "cohort.small", 
                      "gain.cohort.stock",
                      "gain.cohort.small")

      fund.subset.age.5 = fund.subset.age.5 %>%
          mutate(AgeFactor_15 = 0,
                 AgeFactor_16 = 0)
        

      
      for (number in 1:length(model.list5)) {
        
        for(year in 1:16) {
          div = model.list5[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.5$temp = as.numeric(unlist(fund.subset.age.5[div])) * as.numeric(unlist(fund.subset.age.5[age.name]))
          fund.subset.age.5 = mutate(fund.subset.age.5, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list5[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.5$temp = as.numeric(unlist(fund.subset.pd.5[div])) * as.numeric(unlist(fund.subset.pd.5[pd.name]))
          fund.subset.pd.5 = mutate(fund.subset.pd.5, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.5 = fund.subset.age.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.5 = fund.subset.pd.5 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_"),
               contains("gain.cohort.stock_"),
               contains("gain.cohort.small_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.5 = cbind(fund.subset.x.age.5,fund.subset.x.pd.5 )
      
    }
    
    # 3 factor 
    {
      
      
      # Matrix for y Variables
      #fund.subset.y.3 = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
      
      # Matrix for x variables
      
      # Age Dummies
      AgeFactor = fund.subset
      dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
      fund.subset.age.3 <- cbind(fund.subset, dummies)
      
      # PD Dummies
      PD = fund.subset
      dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
      fund.subset.pd.3 <- cbind(fund.subset, dummiesPD)
      
      # Cross dummies with all div vars to generate exposures
      model.list3 = c("bond", 
                      "cohort.stock", 
                      "cohort.small")
      
      fund.subset.age.3 = fund.subset.age.3 %>%
        mutate(AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
      for (number in 1:length(model.list3)) {
        
        for(year in 1:16) {
          div = model.list3[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.3$temp = as.numeric(unlist(fund.subset.age.3[div])) * as.numeric(unlist(fund.subset.age.3[age.name]))
          fund.subset.age.3 = mutate(fund.subset.age.3, !!new.name := temp)
          
          
        }
        
        for(pd in 1:4) {
          div = model.list3[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.3$temp = as.numeric(unlist(fund.subset.pd.3[div])) * as.numeric(unlist(fund.subset.pd.3[pd.name]))
          fund.subset.pd.3 = mutate(fund.subset.pd.3, !!new.name := temp)
          
          
        }
        
        
      }
      
      # Age Subset
      fund.subset.x.age.3 = fund.subset.age.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # PD Subset
      fund.subset.x.pd.3 = fund.subset.pd.3 %>% 
        select(starts_with("bond_"), 
               starts_with("cohort.small_"),
               starts_with("cohort.stock_")) %>% as.matrix()
      
      
      # Full X Matrix
      fund.subset.x.age.pd.3 = cbind(fund.subset.x.age.3,fund.subset.x.pd.3 )
      
    }
    
    
    # Fit 5 factor
    penalized5 = cva.glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.3 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 AgeFactor:cohort.small + 
                 pd_categoryFactorQuartile + 
                 pd_categoryFactorQuartile:cohort.stock + 
                 pd_categoryFactorQuartile:cohort.small, data = fund.subset)
    
    
    
    # Fit 5 factor
    penalized3 = cva.glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
  }
  
  
  # Full Model List
  {
    model.list7 = c("bond", 
                    "cohort.stock", 
                    "cohort.small", 
                    "cohort.growth", 
                    "cohort.reit", 
                    "cohort.infra", 
                    "cohort.nr",
                    "cohort.value",
                    "gain.cohort.stock",
                    "gain.cohort.small",
                    "gain.cohort.growth",
                    "gain.cohort.reit",
                    "gain.cohort.infra",
                    "gain.cohort.nr",
                    "gain.cohort.value")
    
    price.list7 =  c("price.bond.feb", 
                     "price.stock.feb", 
                     "price.meq1.feb",  
                     "price.growth.feb", 
                     "price.reit.feb", 
                     "price.infra.feb", 
                     "price.nr.feb",
                     "price.value.feb",
                     "price.cohort.stock",
                     "price.cohort.small",
                     "price.cohort.growth",
                     "price.cohort.reit",
                     "price.cohort.infra",
                     "price.cohort.nr",
                     "price.cohort.value")
    
    
    return.list7 =  c("bond.return",
                      "stock.return",
                      "small.return",
                      "growth.return",
                      "reit.return",
                      "infra.return",
                      "nr.return",
                      "value.return",
                      "stock.gain.return",
                      "small.gain.return", 
                      "growth.gain.return",
                      "reit.gain.return",
                      "infra.gain.return",
                      "nr.gain.return",
                      "value.gain.return")
    
    
    model.list6 = c("bond", 
                    "cohort.stock", 
                    "cohort.small", 
                    "cohort.growth", 
                    "cohort.reit", 
                    "cohort.infra", 
                    "cohort.nr")
    
    price.list6 =  c("price.bond.feb", 
                     "price.stock.feb", 
                     "price.meq1.feb",  
                     "price.growth.feb", 
                     "price.reit.feb", 
                     "price.infra.feb", 
                     "price.nr.feb",
                     "price.value.feb")
    
    
    return.list6 =  c("bond.return",
                      "stock.return",
                      "small.return",
                      "growth.return",
                      "reit.return",
                      "infra.return",
                      "nr.return")
    
    
    
  }
  
  # Runs across specifications
  {
    
    
    # Full Model
    penalized7 = cva.glmnet(x = fund.subset.x.age.pd, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                           nfolds = 10, lower.limits = 0, intercept = FALSE)
     
    # Quarterly
    penalized7.quarterly = cva.glmnet(x = fund.subset.x.age.pd.quarterly, y = fund.subset.y.quarterly, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
      
    
    ols.2 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:cohort.stock + 
                 pd_categoryFactorQuartile +
                 pd_categoryFactorQuartile:cohort.stock , data = fund.subset)
    
    
    
    ols.2.q = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                   AgeFactor:cohort.stock + 
                   pd_categoryFactorQuartile +
                   pd_categoryFactorQuartile:cohort.stock  , data = fund.subset.quarterly)
    
      
    penalized2 = cva.glmnet(x = fund.subset.x.age.pd.2, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
     
    
    
  }
  
  
  # Extract Best fit Model
  {
    # 2 factor
    {
    number.of.alphas.tested <- length(penalized2$alpha)
    
    cv.glmnet.dt <- data.table()
    
    for (j in 1:number.of.alphas.tested){
      glmnet.model <- penalized2$modlist[[j]]
      min.mse <-  min(glmnet.model$cvm)
      min.lambda <- glmnet.model$lambda.min
      alpha.value <- penalized2$alpha[j]
      new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
      cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
    }
    
    best.params.2 <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
    
    
    penalized2.model <- glmnet(x = fund.subset.x.age.pd.2, y = fund.subset.y,
                      lower.limits = 0, 
                      lambda = best.params.2$min_lambda, 
                      alpha = best.params.2$alpha, intercept = FALSE)
    }
    
    # 3 factor
    {
      number.of.alphas.tested <- length(penalized3$alpha)
      
      cv.glmnet.dt <- data.table()
      
      for (j in 1:number.of.alphas.tested){
        glmnet.model <- penalized3$modlist[[j]]
        min.mse <-  min(glmnet.model$cvm)
        min.lambda <- glmnet.model$lambda.min
        alpha.value <- penalized3$alpha[j]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      
      best.params.3 <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
      
      
      penalized3.model <- glmnet(x = fund.subset.x.age.pd.3, y = fund.subset.y,
                                 lower.limits = 0, 
                                 lambda = best.params.3$min_lambda, 
                                 alpha = best.params.3$alpha, intercept = FALSE)
    }
    
    
    # 5 factor
    {
      number.of.alphas.tested <- length(penalized5$alpha)
      
      cv.glmnet.dt <- data.table()
      
      for (j in 1:number.of.alphas.tested){
        glmnet.model <- penalized5$modlist[[j]]
        min.mse <-  min(glmnet.model$cvm)
        min.lambda <- glmnet.model$lambda.min
        alpha.value <- penalized5$alpha[j]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      
      best.params.5 <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
      
      
      penalized5.model <- glmnet(x = fund.subset.x.age.pd.5, y = fund.subset.y,
                                 lower.limits = 0, 
                                 lambda = best.params.5$min_lambda, 
                                 alpha = best.params.5$alpha, intercept = FALSE)
    }
    
    # 7 factor
    {
      number.of.alphas.tested <- length(penalized7$alpha)
      
      cv.glmnet.dt <- data.table()
      
      for (j in 1:number.of.alphas.tested){
        glmnet.model <- penalized7$modlist[[j]]
        min.mse <-  min(glmnet.model$cvm)
        min.lambda <- glmnet.model$lambda.min
        alpha.value <- penalized7$alpha[j]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      
      best.params.7 <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
      
      
      penalized7.model <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                 lower.limits = 0, 
                                 lambda = best.params.7$min_lambda, 
                                 alpha = best.params.7$alpha, intercept = FALSE)
  
      results <-predict(penalized7.model, fund.subset.x.age.pd, type="response")
    
      
      penalized7.model.all <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                 lower.limits = 0, 
                                 alpha = best.params.7$alpha, intercept = FALSE)
      
      plot(penalized7.model.all)
      legend("bottomright", legend = colnames(fund.subset.x.age.pd))
      }
    
    
    # 7 factor - Quarterly
    {
      number.of.alphas.tested <- length(penalized7.quarterly$alpha)
      
      cv.glmnet.dt <- data.table()
      
      for (j in 1:number.of.alphas.tested){
        glmnet.model <- penalized7.quarterly$modlist[[j]]
        min.mse <-  min(glmnet.model$cvm)
        min.lambda <- glmnet.model$lambda.min
        alpha.value <- penalized7.quarterly$alpha[j]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      
      best.params.7.quarterly <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
      
      
      penalized7.quarterly.model <- glmnet(x = fund.subset.x.age.pd.quarterly , y = fund.subset.y.quarterly,
                                 lower.limits = 0, 
                                 lambda = best.params.7.quarterly$min_lambda, 
                                 alpha = best.params.7.quarterly$alpha, intercept = FALSE)
    }
    
    
    
  }
  
  
  # Store predictions from models
  {
    
    # 2 factor OLS
    predictions2.ols <- predict(ols.2,  newdata = fund.subset)
    predictions.unlist2.ols <- predictions2.ols
    fund.subset$predictions <- predictions.unlist2.ols
    
    fund.subset.predictions2.ols = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb)
    
    
    
    # 2 factor
    predictions2 <-predict(penalized2.model, fund.subset.x.age.pd.2, type="response")
    predictions.unlist2 <- as.numeric(unlist(predictions2)[,1])
    fund.subset$predictions <- predictions.unlist2
    
    fund.subset.predictions2 = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
    
    # 3 factor
    predictions3 <-predict(penalized3.model, fund.subset.x.age.pd.3, type="response")
    predictions.unlist3 <- as.numeric(unlist(predictions3)[,1])
    fund.subset$predictions <- predictions.unlist3
    
    fund.subset.predictions3 = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
      
    
    # 5 factor
    predictions5 <-predict(penalized5.model, fund.subset.x.age.pd.5, type="response")
    predictions.unlist5 <- as.numeric(unlist(predictions5)[,1])
    fund.subset$predictions <- predictions.unlist5
    
    fund.subset.predictions5 = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
 
     
    # 7 (full) factor
    predictions7 <- predict(penalized7.model, fund.subset.x.age.pd, type = "response")
    predictions.unlist7 <- as.numeric(unlist(predictions7)[,1])
    fund.subset$predictions <- predictions.unlist7
    
    fund.subset.predictions7 = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
    
    # Quarterly
    predictions7.quarterly <- predict(penalized7.quarterly.model, fund.subset.x.age.pd.quarterly, type = "response")
    predictions.unlist7.quarterly <- as.numeric(unlist(predictions7.quarterly)[,1])
    fund.subset.quarterly$predictions <- predictions.unlist7.quarterly
    
    fund.subset.predictions7.quarterly = fund.subset.quarterly %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
    
  }
  
  
  ####################################  Analysis/Plot: Cross Validation  #####################################
  
  
  #####################################  Analysis: R2  ###################################
  
  
  
  # OLS
  r2_ols.2 = round(glance(ols.2)[1,1], 3)
  

  
  
  # 2 factor
  rss <- sum((fund.subset.predictions2$predictions - fund.subset.predictions2$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions2$net.cf.distribution.rescale)^2 )
  r2_pen.2 = round(1 - rss/tss, 3)
  
  
  # 5 factor
  rss <- sum((fund.subset.predictions5$predictions - fund.subset.predictions5$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions5$net.cf.distribution.rescale)^2 )
  r2_pen.5 = round(1 - rss/tss, 3)
  
 
  
  
  # 3 factor
  rss <- sum((fund.subset.predictions3$predictions - fund.subset.predictions3$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions3$net.cf.distribution.rescale)^2 )
  r2_pen.3 = round(1 - rss/tss, 3)
    
  
  # 7 factor 
  rss <- sum((fund.subset.predictions7$predictions - fund.subset.predictions7$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions7$net.cf.distribution.rescale)^2 )
  r2_pen.7 = round(1 - rss/tss, 2)
  
  
  # 7 factor - Quarterly
  rss <- sum((fund.subset.predictions7.quarterly$predictions - fund.subset.predictions7.quarterly$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions7.quarterly$net.cf.distribution.rescale)^2 )
  r2_pen.7.quarterly = round(1 - rss/tss, 2)
  
  
   
  r2_pen = rbind( r2_pen.2, r2_pen.3, r2_pen.5, r2_pen.7, r2_pen.7.quarterly)

    model = rbind("2 factor", "3 factor", "5 factor", 
                 "full factors","full factors, yearly collapse")
  
  
  
  
  r2.table = cbind(model, r2_pen) %>% as.data.frame()
  r2.table = r2.table %>% rename(Model = V1, 
                                 R2 = V2)
  
   
  
  #####################################  Analysis: Extract Coefficients  ###################################
  
  
  # 2 factor - OLS
  temp = OLSCoefs(ols.2, model.list0)
  ols.2.broom.age.wide = temp[[1]]
  ols.2.broom.age.pd.wide = temp[[2]]
   

  # 2 factor - PEN
  temp2 = LassoCoefs(penalized2.model, model.list0)
  penalized.2.wide.age = temp2[[1]]
  penalized.2.wide.pd = temp2[[2]]
  age.coef.pen.2 = temp2[[3]]
  pd.coef.pen.2 = temp2[[4]]
  
  
  
  # 3 factor
  temp2 = LassoCoefs(penalized3.model, model.list2)
  penalized.3.wide.age = temp2[[1]]
  penalized.3.wide.pd = temp2[[2]]
  age.coef.pen.3 = temp2[[3]]
  pd.coef.pen.3 = temp2[[4]]
  
  
  # 5 factor
  temp2 = LassoCoefs(penalized5.model, model.list1)
  penalized.5.wide.age = temp2[[1]]
  penalized.5.wide.pd = temp2[[2]]
  age.coef.pen.5 = temp2[[3]]
  pd.coef.pen.5 = temp2[[4]]
  
  
  
  
  # All Factor
  temp7 = LassoCoefs(penalized7.model, model.list7)
  penalized.7.wide.age = temp7[[1]]
  penalized.7.wide.pd = temp7[[2]]
  age.coef.pen.7 = temp7[[3]]
  pd.coef.pen.7 = temp7[[4]]
  
    
  # Quarterly Portfolio
  temp7.quarterly = LassoCoefs(penalized7.quarterly.model, model.list7)
  penalized.7.quarterly.wide.age = temp7.quarterly[[1]]
  penalized.7.quarterly.wide.pd = temp7.quarterly[[2]]
  age.coef.pen.7.quarterly = temp7.quarterly[[3]]
  pd.coef.pen.7.quarterly = temp7.quarterly[[4]]
  
   
  
  
  
  #####################################  Analysis: Vintage Skeleton   #####################################  
  k = 2
  
  
  # Main Quarterly Strip Dataset -- prices + expected returns; no div strips (names are in coefficinets; just prices)
  strip.data.vintage = left_join(price.strip.quarterly.feb, PD.initial.quarterly) %>%
    left_join(price.strip.capital.gains) %>%
    left_join(strip.return.quarterly) 
  
  
  strip.data.vintage = strip.data.vintage %>% mutate(Age.Quarter = (Transaction.Quarter - Vintage.Quarter) + 1,
                                                     AgeFactor = floor(Age.Quarter))
  
  
  strip.data.vintage = strip.data.vintage %>%
    mutate(pd_categoryFactorQuartile = pd_category.4.quarterly)
  
  
  #####################################  Analysis: Data frames of models   #####################################  
  k = 3
  
  # 2 factor
  strip.data.vintage.ols.2 = left_join(strip.data.vintage, ols.2.broom.age.wide)
  strip.data.vintage.ols.2 = left_join(strip.data.vintage.ols.2, ols.2.broom.age.pd.wide)
  
  # PEN
  strip.data.vintage.pen.2 = left_join(strip.data.vintage, penalized.2.wide.age)
  strip.data.vintage.pen.2 = left_join(strip.data.vintage.pen.2, penalized.2.wide.pd)
  
  
    
  # PEN
  strip.data.vintage.pen.3 = left_join(strip.data.vintage, penalized.3.wide.age)
  strip.data.vintage.pen.3 = left_join(strip.data.vintage.pen.3, penalized.3.wide.pd) 
  
  strip.data.vintage.pen.3 <- MissingVars(strip.data.vintage.pen.3)
  
  
  # 5 Factor
    
  # PEN    
  strip.data.vintage.pen.5 = left_join(strip.data.vintage, penalized.5.wide.age)
  strip.data.vintage.pen.5 = left_join(strip.data.vintage.pen.5, penalized.5.wide.pd) 
  
  strip.data.vintage.pen.5 <- MissingVars(strip.data.vintage.pen.5)
  
  
   
  
  # 7 Factor -- All    
  strip.data.vintage.pen.7 = left_join(strip.data.vintage, penalized.7.wide.age)
  strip.data.vintage.pen.7 = left_join(strip.data.vintage.pen.7, penalized.7.wide.pd)
  strip.data.vintage.pen.7 <- MissingVars(strip.data.vintage.pen.7)
  
    
  
  # 7 Factor -- Quarterly    
  strip.data.vintage.pen.7.quarterly = left_join(strip.data.vintage, penalized.7.quarterly.wide.age)
  strip.data.vintage.pen.7.quarterly = left_join(strip.data.vintage.pen.7.quarterly, penalized.7.quarterly.wide.pd)
  strip.data.vintage.pen.7.quarterly <- MissingVars(strip.data.vintage.pen.7.quarterly)
  
    
  
  
  #####################################  Analysis: Generate portfolio weights  ###################################
  k = 5
  
  # 2 factor
  strip.data.vintage.ols.2 = strip.data.vintage.ols.2   
  
  
  strip.data.vintage.ols.q.2 <- QGenerate(strip.data.vintage.ols.2, model.list0, price.list0)
  
  strip.data.vintage.pen.2 = strip.data.vintage.pen.2 %>%
    mutate(cohort.stock = ifelse(is.na(cohort.stock), 0, cohort.stock))
  

  
  strip.data.vintage.pen.q.2 <- QGenerate(strip.data.vintage.pen.2, model.list0, price.list0)
  
  # 3 factor
  #strip.data.vintage.ols.q.3 <- QGenerate(strip.data.vintage.ols.3, model.list2, price.list2)
  strip.data.vintage.pen.q.3 <- QGenerate(strip.data.vintage.pen.3, model.list2, price.list2)
  
  
  # 5 factor
  #strip.data.vintage.ols.q.5 <- QGenerate(strip.data.vintage.ols.5, model.list1, price.list1)
  strip.data.vintage.pen.q.5 <- QGenerate(strip.data.vintage.pen.5, model.list1, price.list1)
  
   
  # 7 factor -- All
  strip.data.vintage.pen.q.7 <- QGenerate(strip.data.vintage.pen.7, model.list7, price.list7)
  
    
  # 7 factor -- Quarterly
  strip.data.vintage.pen.q.7.quarterly <- QGenerate(strip.data.vintage.pen.7.quarterly, model.list7, price.list7)
  
   
  #####################################  Analysis: Horizon Representation - Factor  ###################################
  k = 6
  
  
  
  
  # 2 factor
  age.coef.ols.2 <- HorizonRepresentationAge(ols.2.broom.age.wide)
   
  pd.coef.ols.2 <- HorizonRepresentationPDAlt(ols.2.broom.age.pd.wide)
   
  #####################################  Analysis: Expected Returns - Vintage ###################################
  k = 7
  
  # 2 factor model
  temp.expected.return.ols = ExpectedReturns(strip.data.vintage.ols.q.2, model.list0, price.list0, return.list0)
  
  vintage.expected.return.collapsed.ols.2= temp.expected.return.ols[[1]]
  age.expected.return.collapsed.ols.2= temp.expected.return.ols[[2]]
  
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.2, model.list0, price.list0, return.list0)
  
  vintage.expected.return.collapsed.pen.2= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.2= temp.expected.return.pen[[2]]
  
  
  
  # 3 factor model
  #temp.expected.return.ols = ExpectedReturns(strip.data.vintage.ols.q.3, model.list2, price.list2, return.list2)
  #
  #vintage.expected.return.collapsed.ols.3= temp.expected.return.ols[[1]]
  #age.expected.return.collapsed.ols.3= temp.expected.return.ols[[2]]
  
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.3, model.list2, price.list2, return.list2)
  
  vintage.expected.return.collapsed.pen.3= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.3= temp.expected.return.pen[[2]]
  
  
  # 5 factor model
  #temp.expected.return.ols = ExpectedReturns(strip.data.vintage.ols.q.5, model.list1, price.list1, return.list1)
  #
  #vintage.expected.return.collapsed.ols.5= temp.expected.return.ols[[1]]
  #age.expected.return.collapsed.ols.5= temp.expected.return.ols[[2]]
  
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.5, model.list1, price.list1, return.list1)
  
  vintage.expected.return.collapsed.pen.5= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.5= temp.expected.return.pen[[2]]
  
  
 
  
  # 7 factor
  # 7 factor -- All
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.7, model.list7, price.list7, return.list7)
  
  vintage.expected.return.collapsed.pen.7= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.7= temp.expected.return.pen[[2]]
  
   
  # 7 factor -- Quarterly
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.7.quarterly, model.list7, price.list7, return.list7)
  
  vintage.expected.return.collapsed.pen.7.quarterly= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.7.quarterly= temp.expected.return.pen[[2]]
  
   
  
  
  
  
  
  
  #####################################  Analysis: Profits  ###################################
  k = 8
  
  
  # TVPI, cash on cash
  fund.subset = fund.subset %>% group_by(Firm.ID, Fund.ID) %>% 
    mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame()
  
  # First, link back dataset to main cash flows
  fund.subset.new = fund.subset %>%
    select(Transaction.Quarter, Age.Quarter, Fund.ID, Firm.ID, pme, tvpi, irr, irr.preqin, Vintage.New, year, Vintage.Quarter, net.cf.distribution.rescale)
  
    
  
  fund.subset.quarterly.new = fund.subset.quarterly %>%
    select(Fund.ID, Firm.ID,  Age.Quarter, Vintage.New, Vintage.Quarter,year, net.cf.distribution.rescale) %>%
    mutate(pme = 1, irr = 10) %>% 
    mutate(tvpi = sum(net.cf.distribution.rescale))
  
   
  
  
  
  #avg.tvpi.pen.2 = round(mean(fund.subset.new$irr, na.rm = TRUE), 3)
  
  # Conventional Statistics
  fund.subset.new.profit = fund.subset %>%  
    filter(Vintage.New <= 2010)  %>%
    select(Fund.ID, Firm.ID, irr.preqin, pme, tvpi, Vintage.New) %>% unique
  
  
  
  lagpad <- function(x, k) {
    if (k>0) {
      return (c(rep(NA, k), x)[1 : length(x)] );
    }
    else {
      return (c(x[(-k+1) : length(x)], rep(NA, -k)));
    }
  }
  
  # Persistence in conventional profit measures
  fund.subset.new.profit = fund.subset.new.profit %>% 
    group_by(Firm.ID) %>% 
    arrange(Vintage.New) %>% 
    mutate(last.tvpi = lagpad(tvpi, 1),
           last.pme = lagpad(pme, 1),
           last.irr = lagpad(irr.preqin, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
  
  # TVPI Statistics
  avg.tvpi = round(mean(fund.subset.new.profit$tvpi) - 1, 3)
  sd.tvpi = round(sd(fund.subset.new.profit$tvpi), 3)
  corr.tvpi = round(cor(fund.subset.new.profit$last.tvpi, fund.subset.new.profit$tvpi, use = "complete.obs"), 2)
  
  # IRR 
  avg.irr = round(mean(fund.subset.new.profit  %>%  filter(!is.na(irr.preqin)) %>% .$irr.preqin), 1)
  sd.irr = round(sd(fund.subset.new.profit  %>%  filter(!is.na(irr.preqin)) %>% .$irr.preqin), 1)
  corr.irr = round(cor(fund.subset.new.profit$last.irr, fund.subset.new.profit$irr.preqin, use = "complete.obs"), 2)
  
  # PME Statistics
  avg.pme = round(mean(fund.subset.new.profit$pme, na.rm= TRUE), 3)
  sd.pme = round(sd(fund.subset.new.profit$pme, na.rm= TRUE), 3)
  corr.pme = round(cor(fund.subset.new.profit$last.pme, fund.subset.new.profit$pme, use = "complete.obs"), 2)
  
  
  
  # 2 factor
  {
    # 2 factor
    fund.subset.ols.2 = left_join(fund.subset.new, strip.data.vintage.ols.q.2) %>% filter(Age.Quarter != 0)
    fund.subset.pen.2 = left_join(fund.subset.new, strip.data.vintage.pen.q.2) %>% filter(Age.Quarter != 0)
    
    
    fund.subset.predictions2.ols = fund.subset.predictions2.ols %>%
      mutate(net.cf.distribution.rescale = NA)
    
    temp.profit.ols.2 = Profit(strip.data.vintage.ols.q.2, fund.subset.predictions2.ols, fund.subset.new )
    
    profit.subset.ols.2 = temp.profit.ols.2[[2]]
    fund.subset.vintage.ols.2 = temp.profit.ols.2[[3]]
    
    
    temp.profit.pen.2 = Profit(strip.data.vintage.pen.q.2, fund.subset.predictions2, fund.subset.new )
    
    profit.subset.pen.2 = temp.profit.pen.2[[2]]
    fund.subset.vintage.pen.2 = temp.profit.pen.2[[3]]
    
    avg_profit.ols.2 = round(mean(profit.subset.ols.2$total.profit), 3)
    sd_profit.ols.2 = round(sd(profit.subset.ols.2$total.profit), 3)
    
    avg_profit.pen.2 = round(mean(profit.subset.pen.2$total.profit), 3)
    sd_profit.pen.2 = round(sd(profit.subset.pen.2$total.profit), 3)
    
    # Persistence in new profit measures
    profit.subset.ols.2 = profit.subset.ols.2 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lagpad(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    avg_high_profit.ols.2 = round(mean(profit.subset.ols.2$is.above.10), 3)
    avg_high_profit.pen.2 = round(mean(profit.subset.pen.2$is.above.10), 3)
    
    
    corr.ols.2 = profit.subset.ols.2 %>% filter(!is.na(total.profit), !is.na(irr)) %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5)  
    
    corr.ols.2.irr =  round(cor(corr.ols.2$total.profit, corr.ols.2$irr), 2)
    corr.ols.2.pme =  round(cor(corr.ols.2$total.profit, corr.ols.2$pme), 2)
    corr.ols.2.tvpi = round(cor(corr.ols.2$total.profit, corr.ols.2$tvpi), 2)
    corr.ols.2.profit = round(cor(corr.ols.2$last.profit, corr.ols.2$total.profit, use = "complete.obs"), 2)
    
    
    # Persistence in new profit measures
    profit.subset.pen.2 = profit.subset.pen.2 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lagpad(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    corr.pen.2 = profit.subset.pen.2 %>% filter(!is.na(total.profit), !is.na(irr))  %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5) 
    
    corr.pen.2.irr  = round(cor(corr.pen.2$total.profit, corr.pen.2$irr), 2)
    corr.pen.2.pme  = round(cor(corr.pen.2$total.profit, corr.pen.2$pme), 2)
    corr.pen.2.tvpi = round(cor(corr.pen.2$total.profit, corr.pen.2$tvpi), 2)
    corr.pen.2.profit = round(cor(corr.pen.2$last.profit, corr.pen.2$total.profit, use = "complete.obs"), 2)
    
    
    # TVPI Statistics
    avg.tvpi.pen.2 = round(mean(corr.pen.2$tvpi) - 1, 3)
    sd.tvpi.pen.2 = round(sd(corr.pen.2$tvpi), 3)
    
    avg.tvpi.ols.2 = round(mean(corr.ols.2$tvpi) - 1, 3)
    sd.tvpi.ols.2 = round(sd(corr.ols.2$tvpi), 3)
    
    
    
    
  }
  
  
  # 3 factor
  {
    # First, link back dataset to main cash flows
    
    # fund.subset.ols.3 = left_join(fund.subset.new, strip.data.vintage.ols.q.3) %>% filter(Age.Quarter != 0)
    fund.subset.pen.3 = left_join(fund.subset.new, strip.data.vintage.pen.q.3) %>% filter(Age.Quarter != 0)
    

    
    
    temp.profit.pen.3 = Profit(strip.data.vintage.pen.q.3, fund.subset.predictions3, fund.subset.new)
    
    profit.subset.pen.3 = temp.profit.pen.3[[2]]
    fund.subset.vintage.pen.3 = temp.profit.pen.3[[3]]
    
    #avg_profit.ols.3 = round(mean(profit.subset.ols.3$total.profit), 3)
    avg_profit.pen.3 = round(mean(profit.subset.pen.3$total.profit), 3)
    
    #sd_profit.ols.3 = round(sd(profit.subset.ols.3$total.profit), 3)
    sd_profit.pen.3 = round(sd(profit.subset.pen.3$total.profit), 3)
    
    
    # avg_high_profit.ols.3 = round(mean(profit.subset.ols.3$is.above.10), 3)
    avg_high_profit.pen.3 = round(mean(profit.subset.pen.3$is.above.10), 3)
    
    
    # Persistence in new profit measures
    profit.subset.pen.3 = profit.subset.pen.3 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lagpad(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    #corr.ols.3 = profit.subset.ols.3 %>% filter(!is.na(total.profit), !is.na(irr)) %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5)  
    #
    #corr.ols.3.irr =  round(cor(corr.ols.3$total.profit, corr.ols.3$irr), 2)
    #corr.ols.3.pme =  round(cor(corr.ols.3$total.profit, corr.ols.3$pme), 2)
    #corr.ols.3.tvpi = round(cor(corr.ols.3$total.profit, corr.ols.3$tvpi), 2)
    
    
    corr.pen.3 = profit.subset.pen.3 %>% filter(!is.na(total.profit), !is.na(irr))  %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5) 
    
    corr.pen.3.irr  = round(cor(corr.pen.3$total.profit, corr.pen.3$irr), 2)
    corr.pen.3.pme  = round(cor(corr.pen.3$total.profit, corr.pen.3$pme), 2)
    corr.pen.3.tvpi = round(cor(corr.pen.3$total.profit, corr.pen.3$tvpi), 2)
    corr.pen.3.profit = round(cor(corr.pen.3$last.profit, corr.pen.3$total.profit, use = "complete.obs"), 2)
    
    
    
    avg.tvpi.pen.3 = round(mean(corr.pen.3$tvpi) - 1, 3)
    #avg.tvpi.ols.3 = round(mean(corr.ols.3$tvpi) - 1, 3)
    
    
  }
  
  
  
  # 5 factor
  {
    # fund.subset.ols.5 = left_join(fund.subset.new, strip.data.vintage.ols.q.5) %>% filter(Age.Quarter != 0)
    fund.subset.pen.5 = left_join(fund.subset.new, strip.data.vintage.pen.q.5) %>% filter(Age.Quarter != 0)
    
    
    
    # PEN 5 factor 
    temp.profit.pen.5 = Profit(strip.data.vintage.pen.q.5, fund.subset.predictions5, fund.subset.new)
    
    
    profit.subset.pen.5 = temp.profit.pen.5[[2]]
    fund.subset.vintage.pen.5 = temp.profit.pen.5[[3]]
    
    
    # OLS 5 factor
    #temp.profit.ols.5 = Profit(fund.subset.ols.5)
    
    
    #profit.subset.vintage.ols.5 = temp.profit.ols.5[[1]]
    #profit.subset.ols.5 = temp.profit.ols.5[[2]]
    #fund.subset.vintage.ols.5 = temp.profit.ols.5[[3]]
    
    
    
    
    # avg_profit.ols.5 = round(mean(profit.subset.ols.5$total.profit), 3)
    avg_profit.pen.5 = round(mean(profit.subset.pen.5$total.profit), 3)
    
    # sd_profit.ols.5 = round(sd(profit.subset.ols.5$total.profit), 3)
    sd_profit.pen.5 = round(sd(profit.subset.pen.5$total.profit), 3)
    
    
    #avg_high_profit.ols.5 = round(mean(profit.subset.ols.5$is.above.10), 3)
    avg_high_profit.pen.5 = round(mean(profit.subset.pen.5$is.above.10), 3)
    
    
    #corr.ols.5 = profit.subset.ols.5 %>% filter(!is.na(total.profit), !is.na(irr)) %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5)  
    #
    #corr.ols.5.irr = round(cor(corr.ols.5$total.profit, corr.ols.5$irr), 2)
    #corr.ols.5.pme = round(cor(corr.ols.5$total.profit, corr.ols.5$pme), 2)
    #corr.ols.5.tvpi = round(cor(corr.ols.5$total.profit, corr.ols.5$tvpi), 2)
    
    # Persistence in new profit measures
    profit.subset.pen.5 = profit.subset.pen.5 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lagpad(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    corr.pen.5 = profit.subset.pen.5 %>% filter(!is.na(total.profit), !is.na(irr))  %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5) 
    
    corr.pen.5.irr = round(cor(corr.pen.5$total.profit, corr.pen.5$irr), 2)
    corr.pen.5.pme = round(cor(corr.pen.5$total.profit, corr.pen.5$pme), 2)
    corr.pen.5.tvpi = round(cor(corr.pen.5$total.profit, corr.pen.5$tvpi), 2)
    corr.pen.5.profit = round(cor(corr.pen.5$last.profit, corr.pen.5$total.profit, use = "complete.obs"), 2)
    
    avg.tvpi.pen.5 = round(mean(corr.pen.5$tvpi) - 1, 3)
    # avg.tvpi.ols.5 = round(mean(corr.ols.5$tvpi) - 1, 3)
    
  }
  
   
  
  # 7 factor - All 
  {
    fund.subset.pen.7 = left_join(fund.subset.new, strip.data.vintage.pen.q.7) %>% filter(Age.Quarter != 0)
     
    
    
    # PEN 5 factor 
    temp.profit.pen.7 = Profit(strip.data.vintage.pen.q.7,fund.subset.predictions7, fund.subset.new )
    
    
    profit.subset.pen.7 = temp.profit.pen.7[[2]]
    fund.subset.vintage.pen.7 = temp.profit.pen.7[[3]]
    
    
    
    # Persistence in new profit measures
    profit.subset.pen.7 = profit.subset.pen.7 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lagpad(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    
    avg_profit.pen.7 = round(mean(profit.subset.pen.7$total.profit), 3)
    sd_profit.pen.7 = round(sd(profit.subset.pen.7$total.profit), 3)
    
    avg_high_profit.pen.7 = round(mean(profit.subset.pen.7$is.above.10), 3)
    
    
    
    
    corr.pen.7 = profit.subset.pen.7 %>% filter(!is.na(total.profit), !is.na(irr))  %>% filter(Vintage.New <= 2010 & irr > -.5 & total.profit <= 5) 
    
    corr.pen.7.irr = round(cor(corr.pen.7$total.profit, corr.pen.7$irr), 2)
    corr.pen.7.pme = round(cor(corr.pen.7$total.profit, corr.pen.7$pme), 2)
    corr.pen.7.tvpi = round(cor(corr.pen.7$total.profit, corr.pen.7$tvpi), 2)
    corr.pen.7.profit = round(cor(corr.pen.7$last.profit, corr.pen.7$total.profit, use = "complete.obs"), 2)
    
    avg.tvpi.pen.7 = round(mean(corr.pen.7$tvpi) - 1, 3)
    
    
    
    
    
  }
  
  
    
  # 7 factor -- Quarterly
  {
    fund.subset.pen.7.quarterly = left_join(fund.subset.quarterly.new, strip.data.vintage.pen.q.7.quarterly) %>% filter(Age.Quarter != 0)
    
     
    
    
    # PEN 7 factor 
    temp.profit.pen.7.quarterly = Profit(strip.data.vintage.pen.q.7.quarterly, fund.subset.predictions7.quarterly, fund.subset.quarterly.new )
    
    
    profit.subset.pen.7.quarterly = temp.profit.pen.7.quarterly[[2]]
    fund.subset.vintage.pen.7.quarterly = temp.profit.pen.7.quarterly[[3]]
    
    

    avg_profit.pen.7.quarterly = round(mean(profit.subset.pen.7.quarterly$total.profit), 3)
    sd_profit.pen.7.quarterly = round(sd(profit.subset.pen.7.quarterly$total.profit), 3)
    
    avg_high_profit.pen.7.quarterly = round(mean(profit.subset.pen.7.quarterly$is.above.10), 3)
    
    
    

   
    
    
    
    
  }
  
   
  
  
  #####################################  Table: Model Comparisons  #####################################  
  
  
  blank1 = 0
  blank2 = 0
  blank3 = 0
  

  r2_pen = rbind(blank1, blank2, blank3, r2_ols.2, r2_pen.2, r2_pen.3, r2_pen.5, r2_pen.7, r2_pen.7.quarterly)
   
  persistence = rbind( corr.tvpi, corr.irr, corr.pme, corr.ols.2.profit, corr.pen.2.profit, corr.pen.3.profit, corr.pen.5.profit, corr.pen.7.profit, blank1)
  
  model = rbind("TVPI", "IRR", "PME", "2 factor OLS", "2 factor Elastic Net", "3 factor", "5 factor", 
                "Full factors", "Full factors, Quarterly")
  
  profits.mean = rbind(avg.tvpi, avg.irr, avg.pme-1, avg_profit.ols.2, avg_profit.pen.2, avg_profit.pen.3, avg_profit.pen.5, avg_profit.pen.7, avg_profit.pen.7.quarterly )
  profits.sd = rbind(sd.tvpi, sd.irr, sd.pme , sd_profit.ols.2, sd_profit.pen.2, sd_profit.pen.3, sd_profit.pen.5, sd_profit.pen.7, sd_profit.pen.7.quarterly)
  
  myname2 = paste0(fund.abbreviations[i], "_", "R2")
  myname4 = paste0(fund.abbreviations[i], "_", "r2.table")
  myname5 = paste0(fund.abbreviations[i], "_", "profits.mean")
  myname6 = paste0(fund.abbreviations[i], "_", "profits.sd")
  myname7 = paste0(fund.abbreviations[i], "_", "persistence")
  
  
  
  r2.table = cbind(model, r2_pen, profits.mean, profits.sd, persistence) %>% as.data.frame()
  r2.table = r2.table %>% rename(Model = model, 
                                 R2 = r.squared)
                                 
  r2.table = rename(r2.table, !!myname2 := R2)
  r2.table = rename(r2.table, !!myname5 := profits.mean)
  r2.table = rename(r2.table, !!myname6 := profits.sd)
  r2.table = rename(r2.table, !!myname7 := persistence)
  
  assign(myname4, r2.table)
  
  #r2.table %>%
  #   gt() %>%
  #   tab_header(
  #     title = "Model Comparison"
  #   ) %>% 
  #   cols_merge(
  #     col_1 = vars(pe_profits.mean),
  #     col_2 = vars(pe_profits.sd),
  #     pattern = "{1}<br>({2})"
  #   ) %>%
  #   tab_spanner(
  #     label = "Buyout",
  #     columns = vars(pe_R2, pe_Q_R2, pe_profits.mean)
  #   ) %>%
  #    cols_align(
  #     align = "left",
  #     columns = vars(Model)
  #   ) %>%
  #   cols_label(
  #     pe_R2 = "R^2",
  #     pe_Q_R2 = "Q R^2",
  #     pe_profits.mean = "Profit")
  
  
  #####################################  Plot: Model Comparisons -- 7 factor #####################################
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("TVPI") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against TVPI ", "Correlation: ", corr.pen.7.tvpi,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_tvpi_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=irr)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("Fund IRR") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against IRR. ", "Correlation: ", corr.pen.7.irr,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_irr_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=pme)) + 
    geom_point() +
    #geom_abline(intercept=0, slope=1) +
    xlab("RAP from Elastic Net Model") +
    ylab("K-S PME") +
    #  scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_hline(yintercept=1, linetype="dashed", color = "red", size = 2) + 
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against PME. ", "Correlation: ", corr.pen.7.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  #####################################  Plot: Model Comparisons -- 5 factor #####################################
  
  
  
  q <- ggplot(corr.pen.5, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("TVPI") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against TVPI. ", "Correlation: ", corr.pen.5.tvpi,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_tvpi_pen_5.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.pen.5, aes(x=total.profit, y=irr)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("Fund IRR") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against IRR. ", "Correlation: ", corr.pen.5.irr,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_irr_pen_5.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  q <- ggplot(corr.pen.5, aes(x=total.profit, y=pme)) + 
    geom_point() +
    #geom_abline(intercept=0, slope=1) +
    xlab("RAP from Elastic Net Model") +
    ylab("K-S PME") +
    #  scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_hline(yintercept=1, linetype="dashed", color = "red", size = 2) + 
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against PME. ", "Correlation: ", corr.pen.5.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_pen_5.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  #####################################  Plot: Model Comparisons -- 3 factor #####################################
  
     
  
  q <- ggplot(corr.pen.3, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("TVPI") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against TVPI. ", "Correlation: ", corr.pen.3.tvpi,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_tvpi_pen_3.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.pen.3, aes(x=total.profit, y=irr)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("Fund IRR") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against IRR. ", "Correlation: ", corr.pen.3.irr,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_irr_pen_3.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  q <- ggplot(corr.pen.3, aes(x=total.profit, y=pme)) + 
    geom_point() +
    #geom_abline(intercept=0, slope=1) +
    xlab("RAP from Elastic Net Model") +
    ylab("K-S PME") +
    #  scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_hline(yintercept=1, linetype="dashed", color = "red", size = 2) + 
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against PME. ", "Correlation: ", corr.pen.3.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_pen_3.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  
  
  #####################################  Plot: Model Comparisons -- 2 factor #####################################
  
  q <- ggplot(corr.ols.2, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from OLS Model") +
    ylab("TVPI") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.title.y= element_text(size = 18),
          axis.title.x= element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from OLS Model against TVPI. ", "Correlation: ", corr.ols.2.tvpi,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_tvpi_ols_2.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.ols.2, aes(x=total.profit, y=pme)) + 
    geom_point() +
    xlab("RAP from OLS Model") +
    ylab("K-S PME") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    geom_hline(yintercept=1, linetype="dashed", color = "red", size = 2) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title =paste( "RAP from OLS Model against K-S PME. ", "Correlation: ", corr.ols.2.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_ols_2.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.ols.2, aes(x=total.profit, y=irr)) + 
    geom_point() +
    xlab("RAP from OLS Model") +
    ylab("Fund IRR") +
    #scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.title.y= element_text(size = 18),
          axis.title.x= element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from OLS Model against IRR. ", "Correlation: ", corr.ols.2.irr,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_irr_ols_2.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  
  #####################################  Plot: Factor Plot - Horizon 7 factor ###################################
  
  
  #cbp2 <- c("#000000", "#7570B3", "#56B4E9", "#009E73", "#999999", "#E69F00",
  #          "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#C3D7A4", "#E7298A", "#C4961A")

    # All factors
    g <- ggplot(data = age.coef.pen.7, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(factor_type))) + 
      geom_line(aes(linetype = Div_or_Gain)) + 
      geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
      #geom_point(aes(color = factor(Type))) + 
      # To use for fills, add
      #scale_fill_manual(values = cbp2) +
      # To use for line and point colors, add
      #scale_colour_manual(values=cbp2) +
      #scale_color_viridis(discrete = TRUE, option = "D")+
      scale_x_continuous(breaks=seq(1,16,1)) +
      theme_bw() + 
      xlab("Years from Fund Inception") +
      ylab("b Coefficient") +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
      theme(plot.title = element_text(size = 22),
            axis.text.x= element_text(size = 22),
            axis.text.y= element_text(size = 22),
            axis.title =  element_text(size = 22),
            legend.text =  element_text(size = 14),
            legend.justification=c(1,0),
            #axis.title.y = "b coefficient",
            #axis.title.x = "Years from Fund Inception",
            legend.position="bottom",
            legend.title = element_blank()) + 
      labs(title = "Factor Exposure by Horizon")
    
    g
    
    
    ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_7f_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
 
  
  
   
  
  # All factors -- Quarterly
    g <- ggplot(data = age.coef.pen.7.quarterly, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(factor_type))) + 
      geom_line(aes(linetype = Div_or_Gain)) + 
      geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
      #geom_point(aes(color = factor(Type))) + 
      # To use for fills, add
      #scale_fill_manual(values = cbp2) +
      # To use for line and point colors, add
      #scale_colour_manual(values=cbp2) +
      #scale_color_viridis(discrete = TRUE, option = "D")+
      scale_x_continuous(breaks=seq(1,16,1)) +
      theme_bw() + 
      xlab("Years from Fund Inception") +
      ylab("b Coefficient") +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
      theme(plot.title = element_text(size = 22),
            axis.text.x= element_text(size = 22),
            axis.text.y= element_text(size = 22),
            axis.title =  element_text(size = 22),
            legend.text =  element_text(size = 14),
            legend.justification=c(1,0),
            #axis.title.y = "b coefficient",
            #axis.title.x = "Years from Fund Inception",
            legend.position="bottom",
            legend.title = element_blank()) + 
      labs(title = "Factor Exposure by Horizon, Quarterly Portfolio")
    
    g
    
    
  
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_7f_quarterly_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
    
  
  
  #####################################  Plot: Factor Plot - Horizon 2 factor ###################################
  
  
  
  
  if(i != 5){
    # Horizon - 4 factor --- q; RE
    g <- ggplot(data = age.coef.ols.2, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(Type))) + 
      geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
      scale_x_continuous(breaks=seq(1,16,1)) +
      theme_bw() + 
      xlab("Years from Fund Inception") +
      ylab("b Coefficient") +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
      theme(plot.title = element_text(size = 22),
            axis.text.x= element_text(size = 22),
            axis.text.y= element_text(size = 22),
            axis.title =  element_text(size = 22),
            legend.text =  element_text(size = 14),
            legend.justification=c(1,0),
            #axis.title.y = "b coefficient",
            #axis.title.x = "Years from Fund Inception",
            legend.position=c(1, 0.3),
            legend.title = element_blank()) + 
      labs(title = "Factor Exposure by Horizon")
    
    
    g
    ggsave(paste0(wkdir, fund.abbreviations[i], "_ols_2f_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  if(i == 5){
    # Horizon - 4 factor --- q; RE
    g <- ggplot(data = age.coef.ols.2, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(Type))) + 
      geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
      scale_x_continuous(breaks=seq(1,13,1),  limits = c(1, 13)) + 
      theme_bw() + 
      xlab("Years from Fund Inception") +
      ylab("b Coefficient") +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
      theme(plot.title = element_text(size = 22),
            axis.text.x= element_text(size = 22),
            axis.text.y= element_text(size = 22),
            axis.title =  element_text(size = 22),
            legend.text =  element_text(size = 14),
            legend.justification=c(1,0),
            #axis.title.y = "b coefficient",
            #axis.title.x = "Years from Fund Inception",
            legend.position=c(1, 0.3),
            legend.title = element_blank()) + 
      labs(title = "Factor Exposureby Horizon")
    
    
    g
    ggsave(paste0(wkdir, fund.abbreviations[i], "_ols_2f_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  
  g <- ggplot(data = age.coef.pen.2, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(Type))) + 
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
    scale_x_continuous(breaks=seq(1,16,1)) +
    theme_bw() + 
    xlab("Years from Fund Inception") +
    ylab("b Coefficient") +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
    theme(plot.title = element_text(size = 22),
          axis.text.x= element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 22),
          legend.text =  element_text(size = 14),
          legend.justification=c(1,0),
          #axis.title.y = "b coefficient",
          #axis.title.x = "Years from Fund Inception",
          legend.position=c(1, 0.3),
          legend.title = element_blank()) + 
    labs(title = "Factor Exposure by Horizon")
  
  
  g
  ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_2f_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  #####################################  Plot: PD Plot -- 7 factor   ###################################

  p <- ggplot(data = pd.coef.pen.7 , aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
    theme_bw() + 
    #scale_x_continuous(breaks=seq(1994,2010,1)) +
    xlab("PD Quartile") +
    ylab("a Coefficient") +
    scale_x_discrete(breaks=c("1", "2", "3", "4"), labels = c("1", "2", "3", "4"), limits = c("1", "2", "3", "4")) +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
    theme(plot.title = element_text(size = 22),
          axis.text.x = element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 22),
          #legend.justification=c(1,0),
          legend.text =  element_text(size = 16),
          legend.position = "bottom",
          legend.title = element_blank()) + 
    labs(title = "Factor Exposure by P/D Quartile")
  
  p
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_7f_pd.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
 
  #####################################  Plot: PD Plot -- 5 factor   ###################################
  
    
  # 4-factor
  p <- ggplot(data = pd.coef.pen.5 , aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
    theme_bw() + 
    #scale_x_continuous(breaks=seq(1994,2010,1)) +
    xlab("PD Quartile") +
    ylab("a Coefficient") +
    scale_x_discrete(breaks=c("1", "2", "3", "4"), labels = c("1", "2", "3", "4"), limits = c("1", "2", "3", "4")) +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          axis.text.y= element_text(size = 18),
          legend.justification=c(1,0),
          legend.position=c(0.15, 0.75),
          legend.title = element_blank()) + 
    labs(title = "Factor Exposureby P/D Quartile")
  
  p
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_5f_pd.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
   
  
  
  
  #####################################  Plot: PD Plot -- 3 factor   ###################################
  
 
  # 4-factor
  p <- ggplot(data = pd.coef.pen.3 , aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
    theme_bw() + 
    #scale_x_continuous(breaks=seq(1994,2010,1)) +
    xlab("PD Quartile") +
    ylab("a Coefficient") +
    scale_x_discrete(breaks=c("1", "2", "3", "4"), labels = c("1", "2", "3", "4"), limits = c("1", "2", "3", "4")) +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          axis.text.y= element_text(size = 18),
          legend.justification=c(1,0),
          legend.position=c(0.15, 0.75),
          legend.title = element_blank()) + 
    labs(title = "Factor Exposureby P/D Quartile")
  
  p
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_3f_pd.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
   
  
  
  #####################################  Plot: PD Plot -- 2 factor   ###################################
  
  
  # 4-factor
  p <- ggplot(data = pd.coef.ols.2 , aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
    theme_bw() + 
    #scale_x_continuous(breaks=seq(1994,2010,1)) +
    xlab("PD Quartile") +
    scale_x_discrete(breaks=c("1", "2", "3", "4"), labels = c("1", "2", "3", "4"), limits = c("1", "2", "3", "4")) +
    ylab("a Coefficient") +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
    theme(plot.title = element_text(size = 22),
          axis.text.x = element_text(size = 22, hjust = 1),
          axis.text.y= element_text(size = 22),
          legend.justification=c(1,0),
          legend.text =  element_text(size = 16),
          legend.position = "botton",
          legend.title = element_blank()) + 
    labs(title = "Factor Exposure by P/D Quartile")
  
  p
  
  
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_ols_2f_pd.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  #####################################  Plot: Profit -- 7 factor   ###################################
  
  
    
  g <- ggplot(fund.subset.vintage.pen.7, aes(x = Vintage.New, y = avg.profit))  +
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + theme_bw() +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Average Fund-level profit by Vintage") + 
    ylab("RAP Profit") + xlab("Vintage") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  # 1 -factor
  g <- ggplot(profit.subset.pen.7, aes(total.profit, fill = positive.profit, color = positive.profit))  +
    geom_histogram(binwidth = 0.01) + theme_bw() +
    geom_vline(xintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio", 
         subtitle = paste0("TVPI is:", avg.tvpi.pen.5, "   Risk-Adj Profit is:", avg_profit.pen.7, "  Risk Adj Fraction above 10% is:", avg_high_profit.pen.7)) + 
    ylab("Fund Count") + xlab("Profit Relative to Replicating Portfolio") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_hist_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  #####################################  Plot: Profit -- 5 factor   ###################################
  
  
   
  
  
  
  
  g <- ggplot(fund.subset.vintage.pen.5, aes(x = Vintage.New, y = avg.profit))  +
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + theme_bw() +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Average Fund-level profit by Vintage") + 
    ylab("RAP Profit") + xlab("Vintage") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_5f_pen_profit_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
     
  
  # 1 -factor
  g <- ggplot(profit.subset.pen.5, aes(total.profit, fill = positive.profit, color = positive.profit))  +
    geom_histogram(binwidth = 0.01) + theme_bw() +
    geom_vline(xintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio", 
         subtitle = paste0("TVPI is:", avg.tvpi.pen.5, "   Risk-Adj Profit is:", avg_profit.pen.5, "  Risk Adj Fraction above 10% is:", avg_high_profit.pen.5)) + 
    ylab("Fund Count") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_5f_pen_profit_hist_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  #####################################  Plot: Profit -- 3 factor   ###################################
  
   
  
  
  
  g <- ggplot(fund.subset.vintage.pen.3, aes(x = Vintage.New, y = avg.profit))  +
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + theme_bw() +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Average Fund-level profit by Vintage") + 
    ylab("RAP Profit") + xlab("Vintage") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_3f_pen_profit_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
   
  
  # 1 -factor
  g <- ggplot(profit.subset.pen.3, aes(total.profit, fill = positive.profit, color = positive.profit))  +
    geom_histogram(binwidth = 0.01) + theme_bw() +
    geom_vline(xintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio", subtitle = paste0("Avg Profit is:", avg_profit.pen.3, "   Fraction above 10% is:", avg_high_profit.pen.3)) + 
    ylab("Fund Count") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_3f_pen_profit_hist_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  #####################################  Plot: Profit -- 2 factor   ###################################
  
  
  
  
  
  
  # 4-factor
  g <- ggplot(profit.subset.ols.2, aes(total.profit, fill = positive.profit, color = positive.profit))  +
    geom_histogram(binwidth = 0.01) + theme_bw() +
    geom_vline(xintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio", 
         subtitle = paste0("TVPI is:", avg.tvpi.ols.2, "   Risk-Adj Profit is:", avg_profit.ols.2, "  Risk Adj Fraction above 10% is:", avg_high_profit.ols.2)) + 
    ylab("Fund Count") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_2f_ols_profit_hist_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  g <- ggplot(fund.subset.vintage.ols.2, aes(x = Vintage.New, y = avg.profit))  +
    geom_line() + geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + theme_bw() +
    geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
    theme(plot.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          panel.border = element_blank(),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank()) +
    labs(title = "Average Fund-level profit by Vintage") + 
    ylab("RAP Profit") + xlab("Vintage") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_2f_ols_profit_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  #####################################  Plot: Expected Return - 7 factor  ###################################
  
  
  
  vintage.expected.return.collapsed.pen.7$positive <- ifelse(vintage.expected.return.collapsed.pen.7$annualized.return >= 0, vintage.expected.return.collapsed.pen.7$annualized.return, 0)
  vintage.expected.return.collapsed.pen.7$negative <- ifelse(vintage.expected.return.collapsed.pen.7$annualized.return < 0,  vintage.expected.return.collapsed.pen.7$annualized.return, -1e-36)
  
  vintage.expected.return.collapsed.pen.7 = vintage.expected.return.collapsed.pen.7 %>%
    filter(Vintage.Quarter >= 1981)
  
  if(i != 6 & i != 4){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  if(i == 6  ){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) + 
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  if(i == 4){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  
  v <- ggplot(age.expected.return.collapsed.pen.7  , aes(x = Age.Quarter, y=expected.return.new, fill = Type)) + 
    geom_area() + theme_bw() + 
    ylab("Expected Return") + 
    xlab("Fund Age (Years)") + 
    labs(title = "Expected Return by Horizon and Risk Exposure")+
    scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
    #, subtitle = paste0("Avg Expected Return is:", round(avg.exp.return, 3))) + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          panel.border = element_blank(),
          axis.text.y= element_text(size = 18),
          legend.justification=c(0,1), 
          legend.position=c(0.05, 0.95),
          legend.title = element_blank())  
  v  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_expected_return_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  #####################################  Plot: Expected Return - 5 factor  ###################################
  
  
  
     
  
  
  
  
  
  
  # 4-factor
  v <- ggplot(age.expected.return.collapsed.pen.5  , aes(x = Age.Quarter, y=expected.return.new, fill = Type)) + 
    geom_area() + theme_bw() + 
    ylab("Expected Return") + 
    xlab("Fund Age (Years)") + 
    scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
    labs(title = "Expected Return by Horizon and Risk Exposure")+
    #, subtitle = paste0("Avg Expected Return is:", round(avg.exp.return, 3))) + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          panel.border = element_blank(),
          axis.text.y= element_text(size = 18),
          legend.justification=c(0,1), 
          legend.position=c(0.05, 0.95),
          legend.title = element_blank())  
  v  
  # ggsave(paste0(wkdir, fund.abbreviations[i], "equal_weighted.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_5f_pen_expected_return_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  # 4-factor
  n <- ggplot(vintage.expected.return.collapsed.pen.5 %>% filter(!is.na(annualized.return), annualized.return < 50) , aes(x = Vintage.Quarter, y=annualized.return)) + 
    geom_area() + theme_bw() + 
    ylab("Expected Return") + 
    xlab("Vintage") +
    scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
    labs(title = "Expected Return by Vintage ") + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          panel.border = element_blank(),
          axis.text.y= element_text(size = 18),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank())  
  n  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_5f_pen_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  
  
  
  #####################################  Plot: Expected Return - 3 factor  ###################################
  
  
    
  
  
   
  
  
  
  
  
  # 4-factor
  v <- ggplot(age.expected.return.collapsed.pen.3  , aes(x = Age.Quarter, y=expected.return.new, fill = Type)) + 
    geom_area() + theme_bw() + 
    ylab("Expected Return") + 
    xlab("Fund Age (Years)") + 
    scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
    labs(title = "Expected Return by Horizon and Risk Exposure")+
    #, subtitle = paste0("Avg Expected Return is:", round(avg.exp.return, 3))) + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          panel.border = element_blank(),
          axis.text.y= element_text(size = 18),
          legend.justification=c(0,1), 
          legend.position=c(0.05, 0.95),
          legend.title = element_blank())  
  v  
  # ggsave(paste0(wkdir, fund.abbreviations[i], "equal_weighted.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_3f_pen_expected_return_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  # 4-factor
  n <- ggplot(vintage.expected.return.collapsed.pen.3 %>% filter(!is.na(annualized.return), annualized.return < 50) , aes(x = Vintage.Quarter, y=annualized.return)) + 
    geom_area() + theme_bw() + 
    ylab("Expected Return") + 
    xlab("Vintage") +
    scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
    labs(title = "Expected Return by Vintage ") + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x= element_text(size = 18),
          panel.border = element_blank(),
          axis.text.y= element_text(size = 18),
          legend.justification=c(1,1), 
          legend.position=c(0.95, 0.95),
          legend.title = element_blank())  
  n  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_3f_pen_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  
  
  
  
  
  
  #####################################  Plot: Expected Return - 2 factor  ###################################
  
  
  
  vintage.expected.return.collapsed.ols.2$positive <- ifelse(vintage.expected.return.collapsed.ols.2$annualized.return >= 0, vintage.expected.return.collapsed.ols.2$annualized.return, 0)
  vintage.expected.return.collapsed.ols.2$negative <- ifelse(vintage.expected.return.collapsed.ols.2$annualized.return < 0,  vintage.expected.return.collapsed.ols.2$annualized.return, -1e-36)
  
  vintage.expected.return.collapsed.ols.2 = vintage.expected.return.collapsed.ols.2 %>%
    filter(Vintage.Quarter >= 1981)
  
  if(i != 6  & i != 4){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_2f_ols_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  if(i == 6){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_2f_ols_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  if(i == 4){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage ") + 
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.x= element_text(size = 18),
            panel.border = element_blank(),
            axis.text.y= element_text(size = 18),
            legend.justification=c(1,1), 
            legend.position=c(0.95, 0.95),
            legend.title = element_blank())  
    v  
    ggsave(paste0(wkdir, fund.abbreviations[i], "_2f_ols_expected_return_vintage_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  }
  
  
  
  
  ####################################  Plot: Histogram Profit Comparisons  #####################################
  profit.subset.pen.2 = profit.subset.pen.2 %>% 
    mutate(Model = "Elastic Net Two Factors") %>%
    mutate(avg.profit = mean(total.profit))
  
  profit.subset.pen.7 = profit.subset.pen.7 %>%
    mutate(Model = "Elastic Net Full Factor") %>%
    mutate(avg.profit = mean(total.profit))
  
  multiple.historam.pen.2.7 = rbind(profit.subset.pen.2, profit.subset.pen.7) %>% filter(total.profit <= 2 & total.profit >= -1)
  #multiple.historam.pen.7 = rbind(profit.subset.pen.2, profit.subset.pen.7) %>% filter(total.profit <= 2 )
  
  # Change line colors by groups
  ggplot(multiple.historam.pen.2.7, aes(x=total.profit, color=Model, fill=Model)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.2, binwidth = 0.05)+
    geom_density(alpha=0.7)+
    # geom_vline(data=tempy, aes(xintercept=avg.profit, color=Model),
    #            linetype="dashed")+
    #  scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9"))+
    #  scale_fill_manual(values= c("#E69F00", "#999999", "#56B4E9"))+
    scale_color_manual(values=c( "#E69F00", "#999999","#56B4E9"))+
    scale_fill_manual(values= c( "#E69F00", "#999999","#56B4E9"))+
    ylab("") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") + 
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio",
     subtitle = paste0("TVPI:", avg.tvpi.pen.7, "   Risk-Adj Profit:", avg_profit.pen.7, "  Risk Adj Fraction above 10%:", avg_high_profit.pen.7)) +
    theme_bw() + 
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          axis.text.x= element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 18),
          legend.text =  element_text(size = 14))
          
  
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_hist_comp_q_2_q.png"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  profit.subset.ols.2 = profit.subset.ols.2 %>% 
    mutate(Model = "OLS Two Factors") %>%
    mutate(avg.profit = mean(total.profit))
  
  profit.subset.pen.7 = profit.subset.pen.7 %>%
    mutate(Model = "Elastic Net Full Factor") %>%
    mutate(avg.profit = mean(total.profit))
  
  multiple.historam.pen.7 = rbind(profit.subset.ols.2, profit.subset.pen.7) %>% filter(total.profit <= 2 & total.profit >= -1)
  #multiple.historam.pen.7 = rbind(profit.subset.pen.2, profit.subset.pen.7) %>% filter(total.profit <= 2 )
  
  # Change line colors by groups
  ggplot(multiple.historam.pen.7, aes(x=total.profit, color=Model, fill=Model)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.2, binwidth = 0.05)+
    geom_density(alpha=0.7)+
    # geom_vline(data=tempy, aes(xintercept=avg.profit, color=Model),
    #            linetype="dashed")+
    #  scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9"))+
    #  scale_fill_manual(values= c("#E69F00", "#999999", "#56B4E9"))+
    scale_color_manual(values=c( "#E69F00", "#999999","#56B4E9"))+
    scale_fill_manual(values= c( "#E69F00", "#999999","#56B4E9"))+
    ylab("") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") + 
    labs(title = "Histogram of Fund-Level Profit Relative to Replicating Portfolio",
         subtitle = paste0("TVPI is:", avg.tvpi.pen.7, "   Risk-Adj Profit is:", avg_profit.pen.7, "  Risk Adj Fraction above 10% is:", avg_high_profit.pen.7)) +
    theme_bw() + 
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          axis.text.x= element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 18),
          legend.text =  element_text(size = 14))
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_hist_comp_q.png"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
 
  
  
  
  
  
}








# Put at end now
r2.table = left_join(pe_r2.table, vc_r2.table) %>%
  left_join(re_r2.table) %>% left_join(in_r2.table) %>%
  mutate(pe_profits.sd = paste0("(", pe_profits.sd, ")"),
         vc_profits.sd = paste0("(", vc_profits.sd, ")"),
         re_profits.sd = paste0("(", re_profits.sd, ")"),
         in_profits.sd = paste0("(", in_profits.sd, ")"))

tab.latex = r2.table %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
  #   cols_merge(
  #     col_1 = vars(pe_profits.mean),
  #     col_2 = vars(pe_profits.sd),
  #     pattern = "{1}<br>({2})"
  #   ) %>%
  #cols_merge(
  #  col_1 = vars(vc_profits.mean),
  #  col_2 = vars(vc_profits.sd),
  #  pattern = "{1}<br>({2})"
  #) %>%
  #cols_merge(
#  col_1 = vars(re_profits.mean),
#  col_2 = vars(re_profits.sd),
#  pattern = "{1}<br>({2})"
#) %>%
#cols_merge(
#  col_1 = vars(in_profits.mean),
#  col_2 = vars(in_profits.sd),
#  pattern = "{1}<br>({2})"
#) %>%
tab_spanner(
  label = "Buyout",
  columns = vars(pe_R2,  pe_profits.mean, pe_profits.sd, pe_persistence)
) %>%
  tab_spanner(
    label = "VC",
    columns = vars(vc_R2,  vc_profits.mean, vc_profits.sd, vc_persistence)
  ) %>%
  tab_spanner(
    label = "Real Estate",
    columns = vars(re_R2,  re_profits.mean, re_profits.sd, re_persistence)
  ) %>%
  tab_spanner(
    label = "Infrastructure",
    columns = vars(in_R2,  in_profits.mean, in_profits.sd, in_persistence)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    pe_R2 = "R^2",
    re_R2 = "R^2",
    in_R2 = "R^2",
    vc_R2 = "R^2",
    #pe_Q_R2 = "Q R^2",
    #re_Q_R2 = "Q R^2",
    #in_Q_R2 = "Q R^2",
    #vc_Q_R2 = "Q R^2",
    pe_profits.mean = "Profit",
    vc_profits.mean = "Profit",
    re_profits.mean = "Profit",
    in_profits.mean = "Profit",
    pe_profits.sd = "SD Profit",
    vc_profits.sd = "SD Profit",
    re_profits.sd = "SD Profit",
    in_profits.sd = "SD Profit",
    pe_persistence = "Persistence",
    vc_persistence = "Persistence",
    re_persistence = "Persistence",
    in_persistence = "Persistence"
  ) 

tab.latex

tab.latex %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(wkdir,"latex_out.tex"))


