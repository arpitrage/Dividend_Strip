



rm(list = ls())
# Libraries
{
  library(gt)
  library(glue)
  library(xtable)
  
  library(lubridate)
  
  
  library(R.matlab)
  
  library(stringr)
  
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
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/ExpectedReturnOct20.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DividendStripOct20.Rda")

  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/States.Rda")
  
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/MergedCashFlowOct20.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/IRRJuly20.Rda")
  

  
  
  fund.quarterly.div = fund.quarterly.div %>% filter(Vintage >= 1981)
  fund.quarterly.div = fund.quarterly.div %>% mutate(Vintage.New = floor(Vintage.Quarter))
  
  fund.categories <- fund.quarterly.div %>% filter(!is.na(fund.category)) %>% select(fund.category)  %>% unique()
  fund.abbreviations = c("pe", "vc", "ff", "rs", "in", "re", "nr", "df")
                         

  fund.quarterly.div <- fund.quarterly.div %>% 
    filter( !is.na(gain.cohort.stock) & !is.na(cohort.value)  ) 
  
  # Add in IRRs 
  fund.quarterly.div <- left_join(fund.quarterly.div, irr.all)
  
  #Impose Vintage 2017Q4, last cash flow 2019Q2
  fund.quarterly.div = fund.quarterly.div %>%
    filter(Vintage.Quarter <= 2017.75 & Transaction.Quarter <= 2019.50)
  
  
  # Fix late in life infra exposure 
  fund.quarterly.div.noinfra = fund.quarterly.div %>% filter(fund.category != "Infrastructure")
  fund.quarterly.div.infra = fund.quarterly.div %>% filter(fund.category == "Infrastructure")
  
  
  fund.quarterly.div.infra = fund.quarterly.div.infra %>%
    mutate(late.life.cash = ifelse(Age.Quarter >= 13 , net.cf.distribution.rescale, 0 )) %>%
    group_by(Fund.ID) %>% mutate(total.late.cash = sum(late.life.cash, na.rm = TRUE)) %>% as.data.frame() %>%
    mutate(net.cf.distribution.rescale = ifelse(Age.Quarter == 13, total.late.cash/4, net.cf.distribution.rescale),
           net.cf.distribution.rescale = ifelse(Age.Quarter == 13.25, total.late.cash/4, net.cf.distribution.rescale),
           net.cf.distribution.rescale = ifelse(Age.Quarter == 13.50, total.late.cash/4, net.cf.distribution.rescale),
           net.cf.distribution.rescale = ifelse(Age.Quarter == 13.75, total.late.cash/4, net.cf.distribution.rescale)) %>%
    filter(Age.Quarter <= 13.75) %>% select(-late.life.cash, -total.late.cash)
  
  
  fund.quarterly.div = rbind(fund.quarterly.div.infra, fund.quarterly.div.noinfra)
  
  # Bond exposure
  fund.quarterly.div = fund.quarterly.div %>% mutate(bond = 1)
  
  # Call Components
  fund.quarterly.div = fund.quarterly.div %>% group_by(Fund.ID) %>%
    mutate(discounted.call = net.cf.call.rescale * price.bond.feb,
           all.discounted.call = sum(discounted.call),
           all.call = sum(net.cf.call.rescale)) %>% as.data.frame()
  
  fund.quarterly.div = fund.quarterly.div %>%
    group_by(Fund.ID) %>%
    arrange(Transaction.Quarter) %>%
    mutate(cumulative.calls = cumsum(net.cf.call.rescale)) %>%
    as.data.frame() %>%
    mutate(incremental.call = cumulative.calls - net.cf.call.rescale,
           excess.calls = ifelse(cumulative.calls < -1, 1, 0),
           diff.from.limit = ifelse(net.cf.call.rescale < 0, -1 - incremental.call, 0),
           net.cf.call.rescale.truncated = ifelse(excess.calls == 0, net.cf.call.rescale, pmin(diff.from.limit, 0) ),
           discounted.truncated.call = net.cf.call.rescale.truncated * price.bond.feb)
  
  
  fund.quarterly.div = fund.quarterly.div %>%
    group_by(Fund.ID) %>%
    mutate(all.truncated.call = sum(net.cf.call.rescale.truncated),
           all.truncated.discounted.call = sum(discounted.truncated.call)) %>%
    as.data.frame()
  
  
  vintage.call = fund.quarterly.div %>%
    group_by(Vintage.New, fund.category) %>%
    mutate(avg.truncated.discounted.call = mean(all.truncated.discounted.call)) %>%
    as.data.frame() %>% select(Vintage.New, fund.category, avg.truncated.discounted.call) %>% unique
  
  
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
  
  
  
  
}

wkdir <- "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Figures/"
tables <- "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Tables/"



# Programs
{ 
  lagpad <- function(x, k=1) {
    if (k < 1) {
      stop("k needs to be 1 or greater")
    }
    
    i<-is.vector(x)
    if(is.vector(x)) x <- matrix(x) else x <- matrix(x, nrow(x))
    x <- rbind(matrix(rep(NA, k*ncol(x)), ncol=ncol(x)), matrix(x[1:(nrow(x) - k),], ncol = ncol(x)))
    if(i) x[1:length(x)] else x
  }
  
  
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
    
    # IF missing the PD = 2, then add
    ols.broom.age.pd.wide = ols.broom.age.pd.wide %>%
      mutate(is.two = pd_categoryFactorQuartile == 2)
    
    check.two = sum(ols.broom.age.pd.wide$is.two)
    
    if(check.two == 0 ) {
      two = c(2, rep(0, length(model.list)))
      ols.broom.age.pd.wide = rbind(ols.broom.age.pd.wide, two)
      
    }
    
    ols.broom.age.pd.wide = ols.broom.age.pd.wide %>%
      select(-is.two)
    
    #ols.broom.age.pd.wide = ols.broom.age.pd.wide %>% 
    #  mutate(fit3.pd.deviation.bond = fit3.b.bond.estimate.pd - mean(fit3.b.bond.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small = fit3.b.small.estimate.pd - mean(fit3.b.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock = fit3.b.stock.estimate.pd - mean(fit3.b.stock.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.small.gain = fit3.b.small.estimate.pd - mean(fit3.b.gain.small.estimate.pd, na.rm = TRUE),
    #         fit3.pd.deviation.stock.gain = fit3.b.stock.estimate.pd - mean(fit3.b.gain.stock.estimate.pd, na.rm = TRUE)) %>% arrange(pd_categoryFactorQuartile)
    
    
    
    ols.broom.age.pd.wide = ols.broom.age.pd.wide %>% arrange(pd_categoryFactorQuartile)
    
    
    
    ols.broom.age.pd.wide[1,-1] =   ols.broom.age.pd.wide[1,-1] - ols.broom.age.pd.wide[2,-1]
    ols.broom.age.pd.wide[2,-1] =   ols.broom.age.pd.wide[2,-1] - ols.broom.age.pd.wide[2,-1]
    ols.broom.age.pd.wide[3,-1] =   ols.broom.age.pd.wide[3,-1] - ols.broom.age.pd.wide[2,-1]
    ols.broom.age.pd.wide[4,-1] =   ols.broom.age.pd.wide[4,-1] - ols.broom.age.pd.wide[2,-1]
    
    
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
    
    
    
    #penalized.broom.pd.wide = penalized.broom.pd.wide %>% arrange(pd_categoryFactorQuartile)
    #
    #
    #
    #penalized.broom.pd.wide[1,-1] =   penalized.broom.pd.wide[1,-1] - penalized.broom.pd.wide[2,-1]
    #penalized.broom.pd.wide[2,-1] =   penalized.broom.pd.wide[2,-1] - penalized.broom.pd.wide[2,-1]
    #penalized.broom.pd.wide[3,-1] =   penalized.broom.pd.wide[3,-1] - penalized.broom.pd.wide[2,-1]
    #penalized.broom.pd.wide[4,-1] =   penalized.broom.pd.wide[4,-1] - penalized.broom.pd.wide[2,-1]
    #
    #
    #
    #penalized.broom.pd = penalized.broom.pd %>%
    #  mutate(correction.factor = ifelse(pd_categoryFactorQuartile == 2, value, 0)) %>%
    #  group_by(model) %>% 
    #    mutate(ever.correction = max(correction.factor)) %>%
    #  as.data.frame() %>%
    #  mutate(value = value - ever.correction)
    
    
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
  
  
  
  # skeleton = strip.data.vintage.ols.2 model.list = model.list0 price.list = price.list0
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
  
  
  
  
  Profit <- function(skeleton, predictions, fund.subset.base) # skeleton = strip.data.vintage.pen.q.7  predictions = fund.subset.predictions7 fund.subset.base = fund.subset.new
  {
    
        
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
      select(Vintage.Quarter, Fund.ID, Firm.ID, total.profit.adj, pme, tvpi, irr.npv, Vintage.New) %>% unique()
    
    fund.subset.new.profit.fund.id = left_join(fund.subset.new.profit.fund.id, profits.adj)
    
    
    fund.subset.new.profit.fund.id = fund.subset.new.profit.fund.id %>%
      mutate(total.profit = total.profit.adj + profits.adj)
    
    
    
    
    # Aggregate profit
    profit.subset = fund.subset.new.profit.fund.id %>% select(total.profit, Firm.ID, Fund.ID, Vintage.New, pme, tvpi, irr.npv) %>% 
      unique %>% filter(Vintage.New <= 2010) %>% mutate(positive.profit = ifelse(total.profit > 0, "Excess Profits", " Losses"))
    
    #avg_profit.re = round(mean(profit.subset$total.profit), 3)
    
    profit.subset = profit.subset %>% mutate(is.above.10 = ifelse(total.profit > 0.10, 1, 0))
    
    profit.subset.vintage = profit.subset %>% group_by(Vintage.New) %>% mutate(avg.profit = mean(total.profit, na.rm = TRUE)) %>% select(Vintage.New, avg.profit)
    
    
    
    
    
    
    
    
    profit.return = list(profit.subset, profit.subset, profit.subset.vintage)
    
    return(profit.return)
    
    
    
  }
  
  ProfitAlt <- function(skeleton, predictions, fund.subset.base) # skeleton = strip.data.vintage.pen.q.7  predictions = fund.subset.predictions7 fund.subset.base = fund.subset.new
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
      select(Vintage.Quarter, Fund.ID, Firm.ID, total.profit.adj, pme, tvpi, irr.npv, Vintage.New) %>% unique()
    
    fund.subset.new.profit.fund.id = left_join(fund.subset.new.profit.fund.id, profits.adj)
    
    
    fund.subset.new.profit.fund.id = fund.subset.new.profit.fund.id %>%
      mutate(total.profit = total.profit.adj + profits.adj)
    
    
    
    
    # Aggregate profit
    profit.subset = fund.subset.new.profit.fund.id %>% select(total.profit, Firm.ID, Fund.ID, Vintage.New, pme, tvpi, irr.npv) %>% 
      unique %>% mutate(positive.profit = ifelse(total.profit > 0, "Excess Profits", " Losses"))
    
    #avg_profit.re = round(mean(profit.subset$total.profit), 3)
    
    profit.subset = profit.subset %>% mutate(is.above.10 = ifelse(total.profit > 0.10, 1, 0))
    
    profit.subset.vintage = profit.subset %>% group_by(Vintage.New) %>% mutate(avg.profit = mean(total.profit, na.rm = TRUE)) %>% select(Vintage.New, avg.profit)
    
    
    
    
    
    
    
    
    profit.return = list(profit.subset, profit.subset, profit.subset.vintage)
    
    return(profit.return)
    
    
    
  }
  
  
  # Uses the truncated call distribution (ie the NPV)
  ProfitAltCall <- function(skeleton, predictions, fund.subset.base) # skeleton = strip.data.vintage.pen.q.7  predictions = fund.subset.predictions7 fund.subset.base = fund.subset.new
  {
    
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
      select(Vintage.Quarter, Fund.ID, Firm.ID, total.profit.adj, all.truncated.call.profit, all.truncated.discounted.call.profit, actual.plus.residual.call.profit, GPME, pme, tvpi, irr.npv, Vintage.New) %>% unique()
    
    fund.subset.new.profit.fund.id = left_join(fund.subset.new.profit.fund.id, profits.adj)
    
    
    fund.subset.new.profit.fund.id = fund.subset.new.profit.fund.id %>%
      mutate(total.profit.old = total.profit.adj + profits.adj)
    
    # Incorporate NPV of calls
    fund.subset.new.profit.fund.id = fund.subset.new.profit.fund.id %>%
      mutate(total.profit = total.profit.old + 1 + all.truncated.discounted.call.profit,
             total.profit.actual.call = total.profit.old + 1 + all.truncated.call.profit,
             total.profit.residual =total.profit.old + 1 + actual.plus.residual.call.profit)
    
    
    
    # Aggregate profit
    profit.subset = fund.subset.new.profit.fund.id %>% select(total.profit.old, total.profit, total.profit.actual.call, total.profit.residual, all.truncated.discounted.call.profit, all.truncated.call.profit, actual.plus.residual.call.profit, Firm.ID, Fund.ID, Vintage.New, GPME, pme, tvpi, irr.npv) %>% 
      unique %>% mutate(positive.profit = ifelse(total.profit > 0, "Excess Profits", " Losses"))
    
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
    
    fund.subset.late = fund.subset %>%
      mutate(late_cash = ifelse(Age.Quarter >= 10, net.cf.distribution.rescale, 0)) %>%
      group_by(Fund.ID) %>%
      mutate(total.late.cash = sum(late_cash),
             total.cash = sum(net.cf.distribution.rescale)) %>%
      as.data.frame() %>%
      mutate(fraction.late.cash = total.late.cash/total.cash) %>%
      filter(fraction.late.cash <= 0.10)
    
    
    fund.subset.fund = fund.subset %>%
      select(Fund.ID, Firm.ID, Vintage.New) %>% unique
    
    fund.subset.fund = fund.subset.fund %>%
      group_by(Firm.ID) %>%
      arrange(Vintage.New) %>%
      mutate(sequence_number_fund = row_number() ) %>% as.data.frame()
    
    
    fund.subset.first = fund.subset.fund %>%
      filter(sequence_number_fund == 1)
    
    fund.subset.last = fund.subset.fund %>%
      filter(sequence_number_fund > 1)
    
    fund.subset.first = left_join(fund.subset, fund.subset.first) %>% filter(!is.na(sequence_number_fund))
    
    fund.subset.last = left_join(fund.subset, fund.subset.last) %>% filter(!is.na(sequence_number_fund))
  }
  
  #####################################  Matrix Generation ###################################
  
  
  # Standard full factor matrix
  {
    
    
    # Matrix for y Variables
    fund.subset.y = fund.subset %>% select(net.cf.distribution.rescale) %>% as.matrix()
    fund.subset.y.call = fund.subset %>% select(net.cf.call.rescale.truncated) %>% as.matrix()
    
    # Matrix for x variables
    
    # Age Dummies
    AgeFactor = fund.subset
    dummies = dummies::dummy(AgeFactor$AgeFactor, sep = "_")
    fund.subset.age <- cbind(fund.subset, dummies)
    
    # PD Dummies
    PD = fund.subset
    dummiesPD = dummies::dummy(PD$pd_categoryFactorQuartile, sep = "_")
    fund.subset.pd <- cbind(fund.subset, dummiesPD)
    
    # Infrastructure 
    #if(i == 5){
    #  
    #  fund.subset.age = fund.subset.age %>%
    #    mutate(AgeFactor_13 = 0,
    #           AgeFactor_14 = 0,
    #           AgeFactor_15 = 0,
    #           AgeFactor_16 = 0)
    #  
    #}
    
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
    
    
    if(fund.categories[i,1] != "Real Estate" & fund.categories[i,1] != "Debt Fund" & fund.categories[i,1] != "Infrastructure") {
      
      for (number in 1:length(model.list7)) {
        
        for(year in 1:16) {
          div = model.list7[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age$temp = as.numeric(unlist(fund.subset.age[div])) * as.numeric(unlist(fund.subset.age[age.name]))
          fund.subset.age = mutate(fund.subset.age, !!new.name := temp)
          
          
        }
        
        if(i != 4 & i != 5 & i != 7){
          for(pd in 1:4) {
            div = model.list7[number]
            new.name = paste0(div, "_pd_", pd)
            pd.name = paste0("pd_categoryFactorQuartile_", pd)
            
            # PD Coefs
            fund.subset.pd$temp = as.numeric(unlist(fund.subset.pd[div])) * as.numeric(unlist(fund.subset.pd[pd.name]))
            fund.subset.pd = mutate(fund.subset.pd, !!new.name := temp)
            
            
          }
        }
        
        if(i == 4 | i == 5 | i == 7){
          
          for(pd in 2:4) {
            div = model.list7[number]
            new.name = paste0(div, "_pd_", pd)
            pd.name = paste0("pd_categoryFactorQuartile_", pd)
            
            # PD Coefs
            fund.subset.pd$temp = as.numeric(unlist(fund.subset.pd[div])) * as.numeric(unlist(fund.subset.pd[pd.name]))
            fund.subset.pd = mutate(fund.subset.pd, !!new.name := temp)
            
            
          }
        }
        
      }
    }
    
    
    
    if(fund.categories[i,1] == "Real Estate" | fund.categories[i,1] == "Debt Fund") {
      for (number in 1:length(model.list7)) {
        
        for(year in 1:16) {
          div = model.list7[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age$temp = as.numeric(unlist(fund.subset.age[div])) * as.numeric(unlist(fund.subset.age[age.name]))
          fund.subset.age = mutate(fund.subset.age, !!new.name := temp)
          
          
        }
        
        for(pd in 2:4) {
          div = model.list7[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd$temp = as.numeric(unlist(fund.subset.pd[div])) * as.numeric(unlist(fund.subset.pd[pd.name]))
          fund.subset.pd = mutate(fund.subset.pd, !!new.name := temp)
          
          
        }
        
        
      }
    }
    
    
    if(fund.categories[i,1] == "Infrastructure") {
      for (number in 1:length(model.list7)) {
        
        for(year in 1:13) {
          div = model.list7[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age$temp = as.numeric(unlist(fund.subset.age[div])) * as.numeric(unlist(fund.subset.age[age.name]))
          fund.subset.age = mutate(fund.subset.age, !!new.name := temp)
          
          
        }
        
        for(pd in 2:4) {
          div = model.list7[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd$temp = as.numeric(unlist(fund.subset.pd[div])) * as.numeric(unlist(fund.subset.pd[pd.name]))
          fund.subset.pd = mutate(fund.subset.pd, !!new.name := temp)
          
          
        }
        
        
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
             starts_with("gain.cohort.stock_")) %>% as.matrix()
    
    
    # PD Subset
    fund.subset.x.pd.2 = fund.subset.pd %>% 
      select(starts_with("bond_"), 
             starts_with("gain.cohort.stock_")) %>% as.matrix()
    
    
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
    
    
    #if(i == 5){
    #  
    #  fund.subset.age.quarterly = fund.subset.age.quarterly %>%
    #    mutate(AgeFactor_13 = 0,
    #           AgeFactor_14 = 0,
    #           AgeFactor_15 = 0,
    #           AgeFactor_16 = 0)
    #  
    #}
    
    if(i == 8){
      
      fund.subset.age.quarterly = fund.subset.age.quarterly %>%
        mutate(AgeFactor_15 = 0,
               AgeFactor_16 = 0)
      
    }
    
    
    
    if(fund.categories[i,1] != "Infrastructure") {
      
      for (number in 1:length(model.list7)) {
        
        for(year in 1:16) {
          div = model.list7[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.quarterly$temp = as.numeric(unlist(fund.subset.age.quarterly[div])) * as.numeric(unlist(fund.subset.age.quarterly[age.name]))
          fund.subset.age.quarterly = mutate(fund.subset.age.quarterly, !!new.name := temp)
          
          
        }
        
        
        if(i != 4 & i != 5 & i != 6 & i != 7 & i != 8){
          for(pd in 1:4) {
            div = model.list7[number]
            new.name = paste0(div, "_pd_", pd)
            pd.name = paste0("pd_categoryFactorQuartile_", pd)
            
            # PD Coefs
            fund.subset.pd.quarterly$temp = as.numeric(unlist(fund.subset.pd.quarterly[div])) * as.numeric(unlist(fund.subset.pd.quarterly[pd.name]))
            fund.subset.pd.quarterly = mutate(fund.subset.pd.quarterly, !!new.name := temp)
            
            
          }
        }
        
        if(i == 4 | i == 5 | i == 6 | i == 7 | i == 8){
          
          for(pd in 2:4) {
            div = model.list7[number]
            new.name = paste0(div, "_pd_", pd)
            pd.name = paste0("pd_categoryFactorQuartile_", pd)
            
            # PD Coefs
            fund.subset.pd.quarterly$temp = as.numeric(unlist(fund.subset.pd.quarterly[div])) * as.numeric(unlist(fund.subset.pd.quarterly[pd.name]))
            fund.subset.pd.quarterly = mutate(fund.subset.pd.quarterly, !!new.name := temp)
            
            
          }
        }
      }
    }
    
    
    if(fund.categories[i,1] == "Infrastructure") {
      for (number in 1:length(model.list7)) {
        
        for(year in 1:13) {
          div = model.list7[number]
          new.name = paste0(div, "_", year)
          age.name = paste0("AgeFactor_", year)
          
          # Standard
          fund.subset.age.quarterly$temp = as.numeric(unlist(fund.subset.age.quarterly[div])) * as.numeric(unlist(fund.subset.age.quarterly[age.name]))
          fund.subset.age.quarterly = mutate(fund.subset.age.quarterly, !!new.name := temp)
          
          
        }
        
        for(pd in 2:4) {
          div = model.list7[number]
          new.name = paste0(div, "_pd_", pd)
          pd.name = paste0("pd_categoryFactorQuartile_", pd)
          
          # PD Coefs
          fund.subset.pd.quarterly$temp = as.numeric(unlist(fund.subset.pd.quarterly[div])) * as.numeric(unlist(fund.subset.pd.quarterly[pd.name]))
          fund.subset.pd.quarterly = mutate(fund.subset.pd.quarterly, !!new.name := temp)
          
          
        }
        
        
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
    
    
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.small", "gain.cohort.small", "gain.cohort.stock")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.stock")
    return.list1 =  c("bond.return", "stock.return", "small.return", "small.gain.return", "stock.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "stock.return", "small.return")
    
    
    
    
  }
  
  
  # VC
  # i = 2
  if(i == 2) {
    
    lambda1 = 1
    
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.growth", "cohort.small", "gain.cohort.small", "gain.cohort.growth")
    price.list1 =  c("price.bond.feb", "price.growth.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.growth")
    return.list1 =  c("bond.return", "growth.return", "small.return", "small.gain.return", "growth.gain.return")
    
    
    model.list2 = c("bond", "cohort.growth", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.growth.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "growth.return", "small.return")
    
    
    
    
    
    
  }
  
  
  # RE 
  # i = 6
  if(i == 6) {
    
    lambda1 = 0.5
    
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.reit", "gain.cohort.stock", "gain.cohort.reit")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.reit.feb", "price.cohort.stock", "price.cohort.reit")
    return.list1 =  c("bond.return", "stock.return", "reit.return", "stock.gain.return", "growth.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.reit")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.reit.feb")
    return.list2 =  c("bond.return", "stock.return", "reit.return")
    
    
    
    
  }
  
  
  # IN 
  # i = 5
  if(i == 5) {
    
    lambda1 = 0.25
    
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.infra", "gain.cohort.stock", "gain.cohort.infra")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.infra.feb", "price.cohort.stock", "price.cohort.infra")
    return.list1 =  c("bond.return", "stock.return", "infra.return", "stock.gain.return", "infra.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.infra")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.infra.feb")
    return.list2 =  c("bond.return", "stock.return", "infra.return")
    
    
    
    
    
  }
  
  # NR 
  if(i == 7) {
    
    lambda1 = 0.25
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.nr", "gain.cohort.stock", "gain.cohort.nr")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.nr.feb", "price.cohort.stock", "price.cohort.nr")
    return.list1 =  c("bond.return", "stock.return", "nr.return", "stock.gain.return", "nr.gain.return")
    
    
    
    
    
    
    
  }
  
  # DF 
  # i = 8
  if(i ==  8 ) {
    
    
    model.list0 = c("bond", "gain.cohort.stock")
    price.list0 =  c("price.bond.feb", "price.cohort.stock")
    return.list0 =  c("bond.return", "stock.gain.return")
    
    model.list1 = c("bond", "cohort.stock", "cohort.small", "gain.cohort.small", "gain.cohort.stock")
    price.list1 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb", "price.cohort.small", "price.cohort.stock")
    return.list1 =  c("bond.return", "stock.return", "small.return", "small.gain.return", "stock.gain.return")
    
    model.list2 = c("bond", "cohort.stock", "cohort.small")
    price.list2 =  c("price.bond.feb", "price.stock.feb", "price.meq1.feb")
    return.list2 =  c("bond.return", "stock.return", "small.return")
    
    
    
    
    
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
    
    
    
    
    
  }
  
  # Runs across specifications
  {
    
    # Calls 
    # ols.0.call = lm(net.cf.call.rescale.truncated ~ 0 + AgeFactor, data = fund.subset)
    
    # Full Model
    penalized7 = cva.glmnet(x = fund.subset.x.age.pd, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    # Quarterly
    penalized7.quarterly = cva.glmnet(x = fund.subset.x.age.pd.quarterly, y = fund.subset.y.quarterly, alpha = seq(0, 1, len = 11)^3,
                                      nfolds = 10, lower.limits = 0, intercept = FALSE)
    
    
    
    ols.2 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:gain.cohort.stock + 
                 pd_categoryFactorQuartile +
                 pd_categoryFactorQuartile:gain.cohort.stock , data = fund.subset)
    
    
    
    ols.2.q = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                   AgeFactor:gain.cohort.stock + 
                   pd_categoryFactorQuartile +
                   pd_categoryFactorQuartile:gain.cohort.stock  , data = fund.subset.quarterly)
    
    
    
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
  
  
  #####################################  Analysis: R2  ###################################
  
  # OLS 
  rss <- sum((fund.subset$predictions - fund.subset$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset$net.cf.distribution.rescale - mean(fund.subset$net.cf.distribution.rescale) )^2 )
  r2_ols.2 = round(1 - rss/tss, 3)
  
  
  
  # 2 factor
  rss <- sum((fund.subset.predictions2$predictions - fund.subset.predictions2$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions2$net.cf.distribution.rescale - mean(fund.subset.predictions2$net.cf.distribution.rescale) )^2 )
  r2_pen.2 = round(1 - rss/tss, 3)
  
  
  
  
  # 7 factor 
  rss <- sum((fund.subset.predictions7$predictions - fund.subset.predictions7$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions7$net.cf.distribution.rescale - mean(fund.subset.predictions7$net.cf.distribution.rescale))^2 )
  r2_pen.7 = round(1 - rss/tss, 2)
  
  
  # 7 factor - Quarterly
  rss <- sum((fund.subset.predictions7.quarterly$predictions - fund.subset.predictions7.quarterly$net.cf.distribution.rescale)^2)
  tss <- sum((fund.subset.predictions7.quarterly$net.cf.distribution.rescale - mean(fund.subset.predictions7.quarterly$net.cf.distribution.rescale))^2 )
  r2_pen.7.quarterly = round(1 - rss/tss, 2)
  
  
  
  r2_pen = rbind( r2_pen.2, r2_pen.7, r2_pen.7.quarterly)
  
  model = rbind("2 factor", "full factors","full factors, yearly collapse")
  
  
  
  
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
    mutate(gain.cohort.stock = ifelse(is.na(gain.cohort.stock), 0, gain.cohort.stock))
  
  
  
  strip.data.vintage.pen.q.2 <- QGenerate(strip.data.vintage.pen.2, model.list0, price.list0)
  
  
  
  # 7 factor -- All
  strip.data.vintage.pen.q.7 <- QGenerate(strip.data.vintage.pen.7, model.list7, price.list7)
  
  
  # 7 factor -- Quarterly
  strip.data.vintage.pen.q.7.quarterly <- QGenerate(strip.data.vintage.pen.7.quarterly, model.list7, price.list7)
  
   
  #####################################  Analysis: Horizon Representation - Factor  ###################################
  k = 6
  
  
  
  
  # 2 factor
  age.coef.ols.2 <- HorizonRepresentationAge(ols.2.broom.age.wide)
  age.coef.ols.2 <- age.coef.ols.2 %>%
    mutate(Div_or_Gain = ifelse(Type == "Bond", "Dividend", "Stock Gain"),
           factor_type = ifelse(Type == "Bond", "Bond", "Stock")) %>%
    mutate(specification = "OLS")
  
  
  pd.coef.ols.2 <- HorizonRepresentationPDAlt(ols.2.broom.age.pd.wide)
  
  pd.coef.ols.2 <- pd.coef.ols.2 %>%
    mutate(Div_or_Gain = ifelse(Type == "Bond", "Dividend", "Stock Gain"),
           factor_type = ifelse(Type == "Bond", "Bond", "Stock")) %>%
    mutate(specification = "OLS")
  
  
  age.coef.pen.7 = age.coef.pen.7 %>%
    mutate(specification = "Elastic Net")
  
  pd.coef.pen.7 = pd.coef.pen.7 %>%
    mutate(specification = "Elastic Net")
  
  
  
  # Save as separate names so as to stitch together
  myname_factors.2.age = paste0(fund.abbreviations[i], "_", "2.age")
  myname_factors.2.pd = paste0(fund.abbreviations[i], "_", "2.pd")
  
  myname_factors.7.age = paste0(fund.abbreviations[i], "_", "7.age")
  myname_factors.7.pd = paste0(fund.abbreviations[i], "_", "7.pd")
  
  assign(myname_factors.2.age, age.coef.ols.2)
  assign(myname_factors.2.pd, pd.coef.ols.2)
  assign(myname_factors.7.age, age.coef.pen.7)
  assign(myname_factors.7.pd, pd.coef.pen.7)
  
  
  #####################################  Analysis: Expected Returns - Vintage ###################################
  k = 7
  
  # 2 factor model
  temp.expected.return.ols = ExpectedReturns(strip.data.vintage.ols.q.2, model.list0, price.list0, return.list0)
  
  vintage.expected.return.collapsed.ols.2= temp.expected.return.ols[[1]]
  age.expected.return.collapsed.ols.2= temp.expected.return.ols[[2]]
  
  temp.expected.return.pen = ExpectedReturns(strip.data.vintage.pen.q.2, model.list0, price.list0, return.list0)
  
  vintage.expected.return.collapsed.pen.2= temp.expected.return.pen[[1]]
  age.expected.return.collapsed.pen.2= temp.expected.return.pen[[2]]
  
  
  
  
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
    select(Transaction.Quarter, all.truncated.discounted.call.profit, all.truncated.call.profit, actual.plus.residual.call.profit, Age.Quarter, Fund.ID, Firm.ID, GPME, pme, tvpi, irr, irr.npv, irr.preqin, Vintage.New, year, Vintage.Quarter, net.cf.distribution.rescale)
  
  
  
  fund.subset.quarterly.new = fund.subset.quarterly %>%
    select(Fund.ID, Firm.ID,  Age.Quarter, Vintage.New, Vintage.Quarter,year, net.cf.distribution.rescale) %>%
    mutate(pme = 1, irr.npv = 10) %>% 
    mutate(tvpi = sum(net.cf.distribution.rescale),
           all.truncated.discounted.call = -1)
  
  
  
  # Lots of profit statistics
  {
    
    # Conventional Statistics
    fund.subset.new.profit = fund.subset %>%  
      filter(Vintage.New <= 2010)  %>%
      select(Fund.ID, Firm.ID, irr.npv, GPME, pme, tvpi, Vintage.New) %>% unique
    
    
    
    # Persistence in conventional profit measures
    fund.subset.new.profit = fund.subset.new.profit %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.tvpi = lag(tvpi, 1),
             last.pme = lag(pme, 1),
             last.irr = lag(irr.npv, 1),
             last.gpme = lag(GPME, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    # Quartile Measures - TVPI 
    fund.subset.new.profit = fund.subset.new.profit %>%
      group_by(Vintage.New) %>%
      mutate(q1.tvpi = quantile(tvpi, probs = 0.25),
             q2.tvpi = quantile(tvpi, probs = 0.50),
             q3.tvpi = quantile(tvpi, probs = 0.75)) %>%
      as.data.frame() %>%
      mutate(bottom.quartile.tvpi = tvpi < q1.tvpi,
             top.quartile.tvpi = tvpi >= q3.tvpi)
    
    # Quartile Measures - IRR 
    fund.subset.new.profit = fund.subset.new.profit %>%
      group_by(Vintage.New) %>%
      mutate(q1.irr = quantile(irr.npv, probs = 0.25, na.rm = TRUE),
             q2.irr = quantile(irr.npv, probs = 0.50, na.rm = TRUE),
             q3.irr = quantile(irr.npv, probs = 0.75, na.rm = TRUE)) %>%
      as.data.frame() %>%
      mutate(bottom.quartile.irr = irr.npv < q1.irr,
             top.quartile.irr = irr.npv >= q3.irr)
    
    
    # Quartile Measures - PME 
    fund.subset.new.profit = fund.subset.new.profit %>%
      group_by(Vintage.New) %>%
      mutate(q1.pme = quantile(pme, probs = 0.25, na.rm = TRUE),
             q2.pme = quantile(pme, probs = 0.50, na.rm = TRUE),
             q3.pme = quantile(pme, probs = 0.75, na.rm = TRUE)) %>%
      as.data.frame() %>%
      mutate(bottom.quartile.pme = pme < q1.pme,
             top.quartile.pme = pme >= q3.pme)
    
    
    # Persistence in quantile measures
    fund.subset.new.profit = fund.subset.new.profit %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.bottom.quartile.tvpi = lag(bottom.quartile.tvpi, 1),
             last.top.quartile.tvpi = lag(top.quartile.tvpi, 1),
             last.bottom.quartile.irr = lag(bottom.quartile.irr, 1),
             last.top.quartile.irr = lag(top.quartile.irr, 1),
             last.bottom.quartile.pme = lag(bottom.quartile.pme, 1),
             last.top.quartile.pme = lag(top.quartile.pme, 1)) %>% 
      as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    # TVPI Statistics
    avg.tvpi = round(mean(fund.subset.new.profit$tvpi) - 1, 3)
    sd.tvpi = round(sd(fund.subset.new.profit$tvpi), 3)
    corr.tvpi = round(cor(fund.subset.new.profit$last.tvpi, fund.subset.new.profit$tvpi, use = "complete.obs"), 2)
    
    # IRR 
    avg.irr = round(mean(fund.subset.new.profit  %>%  filter(!is.na(irr.npv)) %>% .$irr.npv), 3)
    sd.irr = round(sd(fund.subset.new.profit  %>%  filter(!is.na(irr.npv)) %>% .$irr.npv), 3)
    corr.irr = round(cor(fund.subset.new.profit$last.irr, fund.subset.new.profit$irr.npv, use = "complete.obs"), 2)
    
    # PME Statistics
    avg.pme = round(mean(fund.subset.new.profit$pme, na.rm= TRUE), 3)
    sd.pme = round(sd(fund.subset.new.profit$pme, na.rm= TRUE), 3)
    corr.pme = round(cor(fund.subset.new.profit$last.pme, fund.subset.new.profit$pme, use = "complete.obs"), 2)
    
    
    #GPME Statistics
    avg.gpme = round(mean(fund.subset.new.profit$GPME, na.rm= TRUE), 3)
    sd.gpme = round(sd(fund.subset.new.profit$GPME, na.rm= TRUE), 3)
    corr.gpme = round(cor(fund.subset.new.profit$last.gpme, fund.subset.new.profit$GPME, use = "complete.obs"), 2)
    
    
    # Quartile statistics
    corr.top.quartile.tvpi = round(cor(fund.subset.new.profit$last.top.quartile.tvpi, fund.subset.new.profit$top.quartile.tvpi, use = "complete.obs"), 2)
    corr.bottom.quartile.tvpi = round(cor(fund.subset.new.profit$last.bottom.quartile.tvpi, fund.subset.new.profit$bottom.quartile.tvpi, use = "complete.obs"), 2)
    
    
    corr.top.quartile.irr = round(cor(fund.subset.new.profit$last.top.quartile.irr, fund.subset.new.profit$top.quartile.irr, use = "complete.obs"), 2)
    corr.bottom.quartile.irr = round(cor(fund.subset.new.profit$last.bottom.quartile.irr, fund.subset.new.profit$bottom.quartile.irr, use = "complete.obs"), 2)
    
    
    corr.top.quartile.pme = round(cor(fund.subset.new.profit$last.top.quartile.pme, fund.subset.new.profit$top.quartile.pme, use = "complete.obs"), 2)
    corr.bottom.quartile.pme = round(cor(fund.subset.new.profit$last.bottom.quartile.pme, fund.subset.new.profit$bottom.quartile.pme, use = "complete.obs"), 2)
    
    
    
    bottom.trans.pme = round(mean(fund.subset.new.profit %>% filter(last.bottom.quartile.pme == 1) %>% .$bottom.quartile.pme, na.rm = TRUE), 2)
    top.trans.pme = round(mean(fund.subset.new.profit %>% filter(last.top.quartile.pme == 1) %>% .$top.quartile.pme, na.rm = TRUE), 2)
    
    bottom.trans.tvpi = round(mean(fund.subset.new.profit %>% filter(last.bottom.quartile.tvpi == 1) %>% .$bottom.quartile.tvpi, na.rm = TRUE), 2)
    top.trans.tvpi = round(mean(fund.subset.new.profit %>% filter(last.top.quartile.tvpi == 1) %>% .$top.quartile.tvpi, na.rm = TRUE), 2)
    
    bottom.trans.irr = round(mean(fund.subset.new.profit %>% filter(last.bottom.quartile.irr == 1) %>% .$bottom.quartile.irr, na.rm = TRUE), 2)
    top.trans.irr = round(mean(fund.subset.new.profit %>% filter(last.top.quartile.irr == 1) %>% .$top.quartile.irr, na.rm = TRUE), 2)
    }
  
  #is.character(sprintf("%.3f",round(7.0503,3)))
  
  
  
  
  # 2 factor
  {
    # 2 factor
    fund.subset.ols.2 = left_join(fund.subset.new, strip.data.vintage.ols.q.2) %>% filter(Age.Quarter != 0)
    fund.subset.pen.2 = left_join(fund.subset.new, strip.data.vintage.pen.q.2) %>% filter(Age.Quarter != 0)
    
    
    fund.subset.predictions2.ols = fund.subset.predictions2.ols %>%
      mutate(net.cf.distribution.rescale = NA)
    
    temp.profit.ols.2 = ProfitAltCall(strip.data.vintage.ols.q.2, fund.subset.predictions2.ols, fund.subset.new )
    
    profit.subset.ols.2 = temp.profit.ols.2[[2]] %>% filter(Vintage.New <= 2010)
    fund.subset.vintage.ols.2 = temp.profit.ols.2[[3]]
    
    
    temp.profit.pen.2 = ProfitAltCall(strip.data.vintage.pen.q.2, fund.subset.predictions2, fund.subset.new )
    
    profit.subset.pen.2 = temp.profit.pen.2[[2]]
    fund.subset.vintage.pen.2 = temp.profit.pen.2[[3]]
    
    
    # Profits
    avg_profit.ols.2 = round(mean(profit.subset.ols.2$total.profit, na.rm = TRUE), 3)
    sd_profit.ols.2 = round(sd(profit.subset.ols.2$total.profit, na.rm = TRUE), 3)
    
    avg_profit.pen.2 = round(mean(profit.subset.pen.2$total.profit, na.rm = TRUE), 3)
    sd_profit.pen.2 = round(sd(profit.subset.pen.2$total.profit, na.rm = TRUE), 3)
    
    
    # Uses actual sum call
    avg_profit.ols.sumcall.2 = round(mean(profit.subset.ols.2$total.profit.actual.call, na.rm = TRUE), 3)
    sd_profit.ols.sumcall.2 = round(sd(profit.subset.ols.2$total.profit.actual.call, na.rm = TRUE), 3)
    
    
    # uses call plus residual
    avg_profit.ols.residual.2 = round(mean(profit.subset.ols.2$total.profit.residual, na.rm = TRUE), 3)
    sd_profit.ols.residual.2 = round(sd(profit.subset.ols.2$total.profit.residual, na.rm = TRUE), 3)
    
    
    
    avg_profit.ols.call.2 = round(mean(profit.subset.ols.2$total.profit.old, na.rm = TRUE), 3)
    sd_profit.ols.call.2 = round(sd(profit.subset.ols.2$total.profit.old, na.rm = TRUE), 3)
    
    
    # Persistence in new profit measures
    profit.subset.ols.2 = profit.subset.ols.2 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lag(total.profit, 1),
             last.profit.call = lag(total.profit.old, 1)) %>% 
      as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    # Quartile Measures
    profit.subset.ols.2 = profit.subset.ols.2 %>%
      group_by(Vintage.New) %>%
      mutate(q1.profit = quantile(total.profit, probs = 0.25, na.rm = TRUE),
             q2.profit = quantile(total.profit, probs = 0.50, na.rm = TRUE),
             q3.profit = quantile(total.profit, probs = 0.75, na.rm = TRUE)) %>%
      as.data.frame() %>%
      mutate(bottom.quartile = total.profit < q1.profit,
             top.quartile = total.profit >= q3.profit)
    
    
    # Persistence in new profit measures
    profit.subset.ols.2 = profit.subset.ols.2 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lag(total.profit, 1),
             last.bottom.quartile = lag(bottom.quartile, 1),
             last.top.quartile = lag(top.quartile, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    avg_high_profit.ols.2 = round(mean(profit.subset.ols.2$is.above.10, na.rm = TRUE), 3)
    avg_high_profit.pen.2 = round(mean(profit.subset.pen.2$is.above.10, na.rm = TRUE), 3)
    
    
    avg_high_profit.ols.2 = round(mean(profit.subset.ols.2$is.above.10, na.rm = TRUE), 3)
    avg_high_profit.pen.2 = round(mean(profit.subset.pen.2$is.above.10, na.rm = TRUE), 3)
    
    
    corr.ols.2 = profit.subset.ols.2 %>% filter(!is.na(total.profit), !is.na(irr.npv)) %>% filter(Vintage.New <= 2010)  
    
    corr.ols.2.irr =  round(cor(corr.ols.2$total.profit, corr.ols.2$irr.npv), 2)
    corr.ols.2.pme =  round(cor(corr.ols.2$total.profit, corr.ols.2$pme), 2)
    corr.ols.2.gpme =  round(cor(corr.ols.2$total.profit, corr.ols.2$GPME, use = "complete.obs"), 2)
    corr.ols.2.tvpi = round(cor(corr.ols.2$total.profit, corr.ols.2$tvpi), 2)
    corr.ols.2.profit = round(cor(corr.ols.2$last.profit, corr.ols.2$total.profit, use = "complete.obs"), 2)
    corr.ols.2.profit.call = round(cor(corr.ols.2$last.profit.call, corr.ols.2$total.profit.old, use = "complete.obs"), 2)
    
    corr.ols.2.top.quartile = round(cor(corr.ols.2$last.top.quartile, corr.ols.2$top.quartile, use = "complete.obs"), 2)
    corr.ols.2.bottom.quartile = round(cor(corr.ols.2$last.bottom.quartile, corr.ols.2$bottom.quartile, use = "complete.obs"), 2)
    
    bottom.trans.2 = round(mean(corr.ols.2 %>% filter(last.bottom.quartile == 1) %>% .$bottom.quartile, na.rm = TRUE), 2)
    top.trans.2 = round(mean(corr.ols.2 %>% filter(last.top.quartile == 1) %>% .$top.quartile, na.rm = TRUE), 2)
    
    
    # Persistence in new profit measures
    profit.subset.pen.2 = profit.subset.pen.2 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lag(total.profit, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    corr.pen.2 = profit.subset.pen.2 %>% filter(!is.na(total.profit), !is.na(irr.npv))  %>% filter(Vintage.New <= 2010 & irr.npv > -.5 & total.profit <= 5) 
    
    corr.pen.2.irr  = round(cor(corr.pen.2$total.profit, corr.pen.2$irr.npv), 2)
    corr.pen.2.pme  = round(cor(corr.pen.2$total.profit, corr.pen.2$pme), 2)
    corr.pen.2.tvpi = round(cor(corr.pen.2$total.profit, corr.pen.2$tvpi), 2)
    corr.pen.2.profit = round(cor(corr.pen.2$last.profit, corr.pen.2$total.profit, use = "complete.obs"), 2)
    
    
    # TVPI Statistics
    avg.tvpi.pen.2 = round(mean(corr.pen.2$tvpi, na.rm = TRUE) - 1, 3)
    sd.tvpi.pen.2 = round(sd(corr.pen.2$tvpi, na.rm = TRUE), 3)
    
    avg.tvpi.ols.2 = round(mean(corr.ols.2$tvpi, na.rm = TRUE) - 1, 3)
    sd.tvpi.ols.2 = round(sd(corr.ols.2$tvpi, na.rm = TRUE), 3)
    
    
    
    
  }
  
  
  
  # 7 factor - All 
  {
    fund.subset.pen.7 = left_join(fund.subset.new, strip.data.vintage.pen.q.7) %>% filter(Age.Quarter != 0)
    
    
    
    # PEN 5 factor 
    temp.profit.pen.7 = ProfitAltCall(strip.data.vintage.pen.q.7,fund.subset.predictions7, fund.subset.new )
    
    
    profit.subset.pen.7 = temp.profit.pen.7[[2]] %>% filter(Vintage.New <= 2010)
    fund.subset.vintage.pen.7 = temp.profit.pen.7[[3]]
    
    
    
    # Persistence in new profit measures
    profit.subset.pen.7 = profit.subset.pen.7 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lag(total.profit, 1),
             last.profit.call = lag(total.profit.old, 1)) %>% as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    # Quartile Measures
    profit.subset.pen.7 = profit.subset.pen.7 %>%
      group_by(Vintage.New) %>%
      mutate(q1.profit = quantile(total.profit, probs = 0.25, na.rm = TRUE),
             q2.profit = quantile(total.profit, probs = 0.50, na.rm = TRUE),
             q3.profit = quantile(total.profit, probs = 0.75, na.rm = TRUE)) %>%
      as.data.frame() %>%
      mutate(bottom.quartile = total.profit < q1.profit,
             top.quartile = total.profit >= q3.profit)
    
    
    # Persistence in new profit measures
    profit.subset.pen.7 = profit.subset.pen.7 %>% 
      group_by(Firm.ID) %>% 
      arrange(Vintage.New) %>% 
      mutate(last.profit = lag(total.profit, 1),
             last.bottom.quartile = lag(bottom.quartile, 1),
             last.top.quartile = lag(top.quartile, 1)) %>% 
      as.data.frame() %>% arrange(Firm.ID, Vintage.New)
    
    
    # Profit measures
    # benchmark, uses NPV of calls
    avg_profit.pen.7 = round(mean(profit.subset.pen.7$total.profit, na.rm = TRUE), 3)
    sd_profit.pen.7 = round(sd(profit.subset.pen.7$total.profit, na.rm = TRUE), 3)
    
    avg_high_profit.pen.7 = round(mean(profit.subset.pen.7$is.above.10, na.rm = TRUE), 3)
    
    # "Old" measure, uses $1 call
    avg_profit.pen.call.7 = round(mean(profit.subset.pen.7$total.profit.old, na.rm = TRUE), 3)
    sd_profit.pen.call.7 = round(sd(profit.subset.pen.7$total.profit.old, na.rm = TRUE), 3)
    
    # Uses actual sum call
    avg_profit.pen.sumcall.7 = round(mean(profit.subset.pen.7$total.profit.actual.call, na.rm = TRUE), 3)
    sd_profit.pen.sumcall.7 = round(sd(profit.subset.pen.7$total.profit.actual.call, na.rm = TRUE), 3)
    
    
    # uses call plus residual
    avg_profit.pen.residual.7 = round(mean(profit.subset.pen.7$total.profit.residual, na.rm = TRUE), 3)
    sd_profit.pen.residual.7 = round(sd(profit.subset.pen.7$total.profit.residual, na.rm = TRUE), 3)
    
    
    
    
    
    corr.pen.7 = profit.subset.pen.7 %>% filter(!is.na(total.profit), !is.na(irr.npv))  %>% filter(Vintage.New <= 2010 & irr.npv > -.5 & total.profit <= 5) 
    
    corr.pen.7.irr = round(cor(corr.pen.7$total.profit, corr.pen.7$irr.npv, use = "complete.obs"), 2)
    corr.pen.7.pme = round(cor(corr.pen.7$total.profit, corr.pen.7$pme, use = "complete.obs"), 2)
    corr.pen.7.gpme = round(cor(corr.pen.7$total.profit, corr.pen.7$GPME, use = "complete.obs"), 2)
    corr.pen.7.tvpi = round(cor(corr.pen.7$total.profit, corr.pen.7$tvpi, use = "complete.obs"), 2)
    corr.pen.7.profit = round(cor(corr.pen.7$last.profit, corr.pen.7$total.profit, use = "complete.obs"), 2)
    corr.pen.7.profit.call = round(cor(corr.pen.7$last.profit.call, corr.pen.7$total.profit.old, use = "complete.obs"), 2)
    
    avg.tvpi.pen.7 = round(mean(corr.pen.7$tvpi) - 1, 3)
    
    
    
    corr.pen.7.top.quartile = round(cor(corr.pen.7$last.top.quartile, corr.pen.7$top.quartile, use = "complete.obs"), 2)
    corr.pen.7.bottom.quartile = round(cor(corr.pen.7$last.bottom.quartile, corr.pen.7$bottom.quartile, use = "complete.obs"), 2)
    
    
    bottom.trans.7 = round(mean(corr.pen.7 %>% filter(last.bottom.quartile == 1) %>% .$bottom.quartile, na.rm = TRUE), 2)
    top.trans.7 = round(mean(corr.pen.7 %>% filter(last.top.quartile == 1) %>% .$top.quartile, na.rm = TRUE), 2)
    
    
    
    
    
    
    temp.profit.pen.7.alt = ProfitAlt(strip.data.vintage.pen.q.7,fund.subset.predictions7, fund.subset.new )
    
    
    profit.subset.pen.7.alt = temp.profit.pen.7.alt[[2]]
    fund.subset.vintage.pen.7.alt = temp.profit.pen.7.alt[[3]]
    
    
    
    
    
  }
  
  
  
  
  #####################################  Table: Model Comparisons  #####################################  
  
  
  blank1 = 0
  blank2 = 0
  blank3 = 0
  
  
  # r2_pen = rbind(blank1, blank2, blank3, r2_ols.2, r2_pen.2, r2_pen.7, r2_pen.7.quarterly)
  
  persistence = rbind( corr.tvpi, corr.irr, corr.pme, corr.ols.2.profit, corr.pen.7.profit  )
  
  top.quartile = rbind( corr.top.quartile.tvpi, corr.top.quartile.irr, corr.top.quartile.pme, corr.ols.2.top.quartile, corr.pen.7.top.quartile)
  bottom.quartile = rbind( corr.bottom.quartile.tvpi, corr.bottom.quartile.irr, corr.bottom.quartile.pme, corr.ols.2.bottom.quartile, corr.pen.7.bottom.quartile)
  
  top.quartile.trans = rbind(top.trans.tvpi, top.trans.irr, top.trans.pme, top.trans.2, top.trans.7)
  bottom.quartile.trans = rbind(bottom.trans.tvpi, bottom.trans.irr, bottom.trans.pme, bottom.trans.2, bottom.trans.7)
  
  
  model = rbind("TVPI", "IRR", "PME-1",  "2 factor OLS: NPV Call", "15 factor Elastic Net: NPV Call", "2 factor OLS: Sum Call", "15 factor Elastic Net: Sum Call", "2 factor OLS: Residual Call", "15 factor Elastic Net: Residual Call")
  model2 = rbind("TVPI", "IRR", "PME-1", "2 factor OLS", "15 factor Elastic Net")
  
  profits.mean = rbind(avg.tvpi, avg.irr, avg.pme-1, avg_profit.ols.2, avg_profit.pen.7, avg_profit.ols.sumcall.2, avg_profit.pen.sumcall.7, avg_profit.ols.residual.2, avg_profit.pen.residual.7)
  profits.sd = rbind(sd.tvpi, sd.irr, sd.pme , sd_profit.ols.2, sd_profit.pen.7, sd_profit.ols.sumcall.2,  sd_profit.pen.sumcall.7, sd_profit.ols.residual.2, sd_profit.pen.residual.7)
  
  #myname2 = paste0(fund.abbreviations[i], "_", "R2")
  myname4 = paste0(fund.abbreviations[i], "_", "r2.table")
  myname5 = paste0(fund.abbreviations[i], "_", "profits.mean")
  myname6 = paste0(fund.abbreviations[i], "_", "profits.sd")
  myname7 = paste0(fund.abbreviations[i], "_", "persistence")
  myname8 = paste0(fund.abbreviations[i], "_", "top.quartile")
  myname9 = paste0(fund.abbreviations[i], "_", "bottom.quartile")
  myname10 = paste0(fund.abbreviations[i], "_", "avg_high_profit.pen.7")
  myname11 = paste0(fund.abbreviations[i], "_", "r2.table.persistence")
  myname12 = paste0(fund.abbreviations[i], "_", "r2.table.persistence.trans")
  
  
  
  r2.table = cbind(model, profits.mean, profits.sd) %>% as.data.frame()
  r2.table = r2.table %>% rename(Model = V1)
  
  #r2.table = rename(r2.table, !!myname2 := V2)
  r2.table = rename(r2.table, !!myname5 := V2)
  r2.table = rename(r2.table, !!myname6 := V3)
  #r2.table = rename(r2.table, !!myname7 := V4)
  
  assign(myname4, r2.table)
  
  r2.table.persistence = cbind(model2, persistence, top.quartile, bottom.quartile) %>% as.data.frame()
  r2.table.persistence = r2.table.persistence %>% rename(Model = V1)
  r2.table.persistence = rename(r2.table.persistence, !!myname7 := V2)
  r2.table.persistence = rename(r2.table.persistence, !!myname8 := V3)
  r2.table.persistence = rename(r2.table.persistence, !!myname9 := V4)
  
  assign(myname11, r2.table.persistence)
  
  
  
  r2.table.persistence.trans = cbind(model2, top.quartile.trans, bottom.quartile.trans) %>% as.data.frame()
  r2.table.persistence.trans = r2.table.persistence.trans %>% rename(Model = V1)
  r2.table.persistence.trans = rename(r2.table.persistence.trans, !!myname8 := V2)
  r2.table.persistence.trans = rename(r2.table.persistence.trans, !!myname9 := V3)
  
  assign(myname12, r2.table.persistence.trans)
  
  
  
  
  assign(myname10, avg_high_profit.pen.7)
  
  
  
  
  #####################################  Plot: Model Comparisons -- 7 factor #####################################
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("TVPI") +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against TVPI ", "Correlation: ", corr.pen.7.tvpi,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_tvpi_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=irr.npv)) + 
    geom_point() +
    xlab("RAP from Elastic Net Model") +
    ylab("Fund IRR") +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_smooth(method=lm) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against IRR. ", "Correlation: ", corr.pen.7.irr,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_irr_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=pme-1)) + 
    geom_point() +
    #geom_abline(intercept=0, slope=1) +
    xlab("RAP from Elastic Net Model") +
    ylab("PME-1") +
    #  scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_smooth(method=lm) +
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against PME-1. ", "Correlation: ", corr.pen.7.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.pen.7, aes(x=total.profit, y=GPME-1)) + 
    geom_point() +
    #geom_abline(intercept=0, slope=1) +
    xlab("RAP from Elastic Net Model") +
    ylab("GPME-1") +
    #  scale_x_continuous(breaks=seq(-1, 1, 0.5), limits = c(-1, 1)) +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_smooth(method=lm) +
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title = paste("RAP from Elastic Net Model against GPME-1. ", "Correlation: ", corr.pen.7.gpme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_gpme_pen_7.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  #####################################  Plot: Model Comparisons -- 2 factor #####################################
  
  q <- ggplot(corr.ols.2, aes(x=total.profit, y=tvpi)) + 
    geom_point() +
    xlab("RAP from OLS Model") +
    ylab("TVPI") +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
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
    ylab("PME-1") +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_smooth(method=lm) + 
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y= element_text(size = 18),
          legend.title = element_blank()) + 
    labs(title =paste( "RAP from OLS Model against PME-1. ", "Correlation: ", corr.ols.2.pme,sep="") )
  q
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_comparison_pme_ols_2.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  q <- ggplot(corr.ols.2, aes(x=total.profit, y=irr.npv)) + 
    geom_point() +
    xlab("RAP from OLS Model") +
    ylab("Fund IRR") +
    geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
    geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 1.5) + 
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
  
  g <- ggplot(fund.subset.vintage.pen.7 %>% filter(Vintage.New <= 2010), aes(x = Vintage.New, y = avg.profit))  +
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
  
  
  g <- ggplot(fund.subset.vintage.pen.7.alt, aes(x = Vintage.New, y = avg.profit))  +
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
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_vintage_long_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
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
         subtitle = paste0("TVPI is:", avg.tvpi.pen.7, "   Risk-Adj Profit is:", avg_profit.pen.7, "  Risk Adj Fraction above 10% is:", avg_high_profit.pen.7)) + 
    ylab("Fund Count") + xlab("Profit Relative to Replicating Portfolio") 
  g
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_hist_q.eps"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
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
  
  
  
  
  g <- ggplot(fund.subset.vintage.ols.2 %>% filter(Vintage.New <= 2010), aes(x = Vintage.New, y = avg.profit))  +
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
    filter(Vintage.Quarter >= 1980)
  
  avg.return = mean(vintage.expected.return.collapsed.pen.7$annualized.return)
  avg.return = round(avg.return, 3)
  
  if(i != 1 & i != 3 & i != 6 & i != 4 & i != 5 & i != 7 & i != 8){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
      scale_y_continuous(limits = c(0, 0.20), breaks = seq(0, 0.20, 0.05)) + 
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
  
  
  
  if(i == 1 | i == 3  ){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) + 
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
  
  
  
  
  
  if(i == 4 | i == 5 | i == 7 | i == 8){
    v <- ggplot(vintage.expected.return.collapsed.pen.7) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.05, 0.15), breaks = seq(-0.05, 0.15, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
    labs(title = "Expected Return by Horizon andc Risk Exposure")+
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
  
  
  
  
  #####################################  Plot: Expected Return - 2 factor  ###################################
  
  
  
  vintage.expected.return.collapsed.ols.2$positive <- ifelse(vintage.expected.return.collapsed.ols.2$annualized.return >= 0, vintage.expected.return.collapsed.ols.2$annualized.return, 0)
  vintage.expected.return.collapsed.ols.2$negative <- ifelse(vintage.expected.return.collapsed.ols.2$annualized.return < 0,  vintage.expected.return.collapsed.ols.2$annualized.return, -1e-36)
  
  vintage.expected.return.collapsed.ols.2 = vintage.expected.return.collapsed.ols.2 %>%
    filter(Vintage.Quarter >= 1980)
  
  avg.return = mean(vintage.expected.return.collapsed.ols.2$annualized.return)
  avg.return = round(avg.return, 3)
  
  
  if(i != 1 & i != 3 & i != 6 & i != 4 & i != 5 & i != 7 & i != 8){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.05, 0.25), breaks = seq(-0.05, 0.25, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
      scale_y_continuous(limits = c(0, 0.20), breaks = seq(0, 0.20, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
  
  
  
  if(i == 1 | i == 3){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
  
  
  
  if(i == 4 | i == 5 | i == 7 | i == 8){
    v <- ggplot(vintage.expected.return.collapsed.ols.2) +
      geom_area(aes(x=Vintage.Quarter, y=positive)) +
      geom_area(aes(x=Vintage.Quarter, y=negative))+ theme_bw() + 
      scale_y_continuous(limits = c(-0.05, 0.15), breaks = seq(-0.05, 0.15, 0.05)) + 
      ylab("Expected Return") + 
      xlab("Vintage") +
      labs(title = "Expected Return by Vintage",
           subtitle = paste0("Average: ", avg.return)) + 
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
  
  
  
  
  ####################################   Plot: Histogram Profit Comparisons  #####################################
  
  
  
  profit.subset.ols.2 = profit.subset.ols.2 %>% 
    mutate(Model = "OLS Two Factors") %>%
    mutate(avg.profit = mean(total.profit))
  
  profit.subset.pen.7 = profit.subset.pen.7 %>%
    mutate(Model = "Elastic Net Full Factor") %>%
    mutate(avg.profit = mean(total.profit))
  
  multiple.historam.pen.7 = rbind(profit.subset.ols.2, profit.subset.pen.7) %>% filter(total.profit <= 2 & total.profit >= -1)
  #multiple.historam.pen.7 = rbind(profit.subset.pen.2, profit.subset.pen.7) %>% filter(total.profit <= 2 )
  
  
  # Save as separate names so as to stitch together
  myname_histogram = paste0(fund.abbreviations[i], "_", "histogram")
  assign(myname_histogram, multiple.historam.pen.7)
  
  
  
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
         subtitle = paste0("TVPI is:", avg.tvpi, "   Risk-Adj Profit is:", avg_profit.pen.7, "  Risk Adj Fraction above 10% is:", avg_high_profit.pen.7)) +
    theme_bw() + 
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          axis.text.x= element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 18),
          legend.text =  element_text(size = 14))
  
  ggsave(paste0(wkdir, fund.abbreviations[i], "_7f_pen_profit_hist_comp_q.png"), width = 14.4/1.5, height = 7.24/1.5 )
  
  
  
  
  
  
  
  
}




#####################################  Plot: Make Composite Main Age Effects ##################################### 
# 

pe_2.age = pe_2.age %>%
  mutate(sector = "Buyout")
vc_2.age = vc_2.age %>%
  mutate(sector = "Venture Capital")
re_2.age = re_2.age %>%
  mutate(sector = "Real Estate")
ff_2.age = ff_2.age %>%
  mutate(sector = "Fund of Funds")

pe_7.age = pe_7.age %>%
  mutate(sector = "Buyout")
vc_7.age = vc_7.age %>%
  mutate(sector = "Venture Capital")
re_7.age = re_7.age %>%
  mutate(sector = "Real Estate")
ff_7.age = ff_7.age %>%
  mutate(sector = "Fund of Funds")


all.age = rbind(pe_2.age, pe_7.age,
                vc_2.age, vc_7.age,
                re_2.age, re_7.age,
                ff_2.age, ff_7.age) %>%
  mutate(specification.f = as.factor(specification),
         sector.f = as.factor(sector))

all.age$specification.f <- relevel(all.age$specification.f,  "OLS", "Elastic Net")
all.age$sector.f <- reorder(all.age$sector.f,  "Buyout", "Venture Capital", "Real Estate", "Fund of Funds")


all.age$sector.f <- factor(all.age$sector.f,  levels = c("Buyout", "Venture Capital", "Real Estate", "Fund of Funds"))

all.age = all.age %>%
  mutate(Div_or_Gain = ifelse(Div_or_Gain == "Stock Gain", "Gain", Div_or_Gain))

# facet_grid(row_variable ~ column_variable)


g <- ggplot(data = all.age, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(factor_type))) + 
  #facet_grid(sector ~ specification, scales = "free") +
  geom_line(aes(linetype = Div_or_Gain)) + 
  facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 
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
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title =  element_text(size = 16),
        legend.text =  element_text(size = 16),
        legend.justification=c(1,0),
        #axis.title.y = "b coefficient",
        #axis.title.x = "Years from Fund Inception",
        legend.position="bottom",
        legend.title = element_blank()) + 
  labs(title = element_blank())

g
ggsave(paste0(wkdir, "main_age.eps"), width = 11.9, height = 13 )






rs_2.age = rs_2.age %>%
  mutate(sector = "Restructuring")
df_2.age = df_2.age %>%
  mutate(sector = "Debt Fund")
nr_2.age = nr_2.age %>%
  mutate(sector = "Natural Resources")
in_2.age = in_2.age %>%
  mutate(sector = "Infrastructure")

rs_7.age = rs_7.age %>%
  mutate(sector = "Restructuring")
df_7.age = df_7.age %>%
  mutate(sector = "Debt Fund")
in_7.age = in_7.age %>%
  mutate(sector = "Infrastructure")
nr_7.age = nr_7.age %>%
  mutate(sector = "Natural Resources")


secondary.age = rbind(rs_2.age, rs_7.age,
                      df_2.age, df_7.age,
                      in_2.age, in_7.age,
                      nr_2.age, nr_7.age) %>%
  mutate(specification.f = as.factor(specification),
         sector.f = as.factor(sector))

secondary.age$specification.f <- relevel(secondary.age$specification.f,  "OLS", "Elastic Net")
secondary.age$sector.f <- reorder(secondary.age$sector.f,  "Restructuring", "Debt Fund", "Infrastructure", "Natural Resources")


secondary.age$sector.f <- factor(secondary.age$sector.f,  levels = c("Restructuring", "Debt Fund", "Infrastructure", "Natural Resources"))

secondary.age = secondary.age %>%
  mutate(Div_or_Gain = ifelse(Div_or_Gain == "Stock Gain", "Gain", Div_or_Gain))





g <- ggplot(data = secondary.age, aes(x = AgeFactor , y = value, group = factor(Type), color = factor(factor_type))) + 
  #facet_grid(sector ~ specification, scales = "free") +
  geom_line(aes(linetype = Div_or_Gain)) + 
  facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 
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
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title =  element_text(size = 16),
        legend.text =  element_text(size = 16),
        legend.justification=c(1,0),
        #axis.title.y = "b coefficient",
        #axis.title.x = "Years from Fund Inception",
        legend.position="bottom",
        legend.title = element_blank()) + 
  labs(title = element_blank())

g
ggsave(paste0(wkdir, "secondary_age.eps"), width = 11.9, height = 13 )

#####################################  Plot: Make Composite PD Effects ##################################### 







# Kludge -- fill in those missing 0s for some caetgories 

re_7.pd.1.quartile = re_7.pd %>% filter(pd_categoryFactorQuartile == 2) %>%
  mutate(pd_categoryFactorQuartile = 1,
         value = 0)
re_7.pd.alt = rbind(re_7.pd, re_7.pd.1.quartile)

in_7.pd.1.quartile = in_7.pd %>% filter(pd_categoryFactorQuartile == 2) %>%
  mutate(pd_categoryFactorQuartile = 1,
         value = 0)
in_7.pd.alt = rbind(in_7.pd, in_7.pd.1.quartile)



pe_2.pd = pe_2.pd %>%
  mutate(sector = "Buyout")  %>% select(-factor_type)
vc_2.pd = vc_2.pd %>%
  mutate(sector = "Venture Capital")    %>% select(-factor_type)
re_2.pd = re_2.pd %>%
  mutate(sector = "Real Estate")    %>% select(-factor_type)
ff_2.pd = ff_2.pd %>%
  mutate(sector = "Fund of Funds") %>% select(-factor_type)

pe_7.pd = pe_7.pd %>%
  mutate(sector = "Buyout")   
vc_7.pd = vc_7.pd %>%
  mutate(sector = "Venture Capital")   
re_7.pd.alt = re_7.pd.alt %>%
  mutate(sector = "Real Estate")  
ff_7.pd = ff_7.pd %>%
  mutate(sector = "Fund of Funds")



pe_7.pd = pe_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
vc_7.pd = vc_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
re_7.pd.alt = re_7.pd.alt %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
ff_7.pd = ff_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))





all.pd = rbind(pe_2.pd, pe_7.pd,
               vc_2.pd, vc_7.pd,
               re_2.pd, re_7.pd.alt,
               ff_2.pd, ff_7.pd) %>%
  mutate(specification.f = as.factor(specification),
         sector.f  = as.factor(sector))



all.pd$specification.f <- relevel(all.pd$specification.f,  "OLS", "Elastic Net")
#all.pd$sector.f <- reorder(all.pd$sector.f,  "Buyout", "Venture Capital", "Real Estate", "Infrastructure")

all.pd$sector.f <- factor(all.pd$sector.f,  levels = c("Buyout", "Venture Capital", "Real Estate", "Fund of Funds"))


all.pd = all.pd %>%
  mutate(Div_or_Gain = ifelse(Div_or_Gain == "Stock Gain", "Gain", Div_or_Gain))


all.pd = all.pd %>%
  mutate(factor_type = gsub(" Gain", "", Type))


g <- ggplot(data = all.pd, aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
  #facet_grid(sector ~ specification, scales = "free") +
  geom_line(aes(linetype = Div_or_Gain)) + 
  facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 
  geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
  #geom_point(aes(color = factor(Type))) + 
  # To use for fills, add
  #scale_fill_manual(values = cbp2) +
  # To use for line and point colors, add
  #scale_colour_manual(values=cbp2) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_x_continuous(breaks=seq(1,4,1)) +
  theme_bw() + 
  xlab("P/D Quartile") +
  ylab("b Coefficient") +
  geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
  theme(plot.title = element_text(size = 22),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title =  element_text(size = 16),
        legend.text =  element_text(size = 16),
        legend.justification=c(1,0),
        #axis.title.y = "b coefficient",
        #axis.title.x = "Years from Fund Inception",
        legend.position="bottom",
        legend.title = element_blank()) + 
  labs(title = element_blank())

g

ggsave(paste0(wkdir, "main_pd.eps"), width = 11.9, height = 13 )








rs_7.pd.1.quartile = rs_7.pd %>% filter(pd_categoryFactorQuartile == 2) %>%
  mutate(pd_categoryFactorQuartile = 1,
         value = 0)
rs_7.pd.alt = rbind(rs_7.pd, rs_7.pd.1.quartile)

df_7.pd.1.quartile = df_7.pd %>% filter(pd_categoryFactorQuartile == 2) %>%
  mutate(pd_categoryFactorQuartile = 1,
         value = 0)
df_7.pd.alt = rbind(df_7.pd, df_7.pd.1.quartile)

nr_7.pd.1.quartile = nr_7.pd %>% filter(pd_categoryFactorQuartile == 2) %>%
  mutate(pd_categoryFactorQuartile = 1,
         value = 0)
nr_7.pd.alt = rbind(nr_7.pd, nr_7.pd.1.quartile)




rs_2.pd = rs_2.pd %>%
  mutate(sector = "Restructuring") %>% select(-factor_type)
df_2.pd = df_2.pd %>%
  mutate(sector = "Debt Fund") %>% select(-factor_type)
nr_2.pd = nr_2.pd %>%
  mutate(sector = "Natural Resources") %>% select(-factor_type)
in_2.pd = in_2.pd %>%
  mutate(sector = "Infrastructure")   %>% select(-factor_type)

rs_7.pd = rs_7.pd %>%
  mutate(sector = "Restructuring")
df_7.pd = df_7.pd %>%
  mutate(sector = "Debt Fund")
nr_7.pd = nr_7.pd %>%
  mutate(sector = "Natural Resources")
in_7.pd.alt = in_7.pd.alt %>%
  mutate(sector = "Infrastructure")   


rs_7.pd = rs_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
df_7.pd = df_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
in_7.pd.alt = in_7.pd.alt %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))
nr_7.pd = nr_7.pd %>%
  mutate(Div_or_Gain = ifelse(str_detect(Type, "Gain"), "Gain", "Dividend"))



secondary.pd = rbind(rs_2.pd, rs_7.pd,
                     df_2.pd, df_7.pd,
                     in_2.pd, in_7.pd.alt,
                     nr_2.pd, nr_7.pd) %>%
  mutate(specification.f = as.factor(specification),
         sector.f = as.factor(sector))

secondary.pd$specification.f <- relevel(secondary.pd$specification.f,  "OLS", "Elastic Net")
#secondary.pd$sector.f <- reorder(secondary.pd$sector.f,  "Restructuring", "Debt Fund", "Fund of Funds", "Natural Resources")

secondary.pd$sector.f <- factor(secondary.pd$sector.f,  levels = c("Restructuring", "Debt Fund", "Infrastructure", "Natural Resources"))

secondary.pd = secondary.pd %>%
  mutate(Div_or_Gain = ifelse(Div_or_Gain == "Stock Gain", "Gain", Div_or_Gain))




g <- ggplot(data = secondary.pd, aes(x = pd_categoryFactorQuartile , y = value, group = factor(Type), color = factor(Type))) + 
  #facet_grid(sector ~ specification, scales = "free") +
  geom_line(aes(linetype = Div_or_Gain)) + 
  facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 
  geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
  #geom_point(aes(color = factor(Type))) + 
  # To use for fills, add
  #scale_fill_manual(values = cbp2) +
  # To use for line and point colors, add
  #scale_colour_manual(values=cbp2) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_x_continuous(breaks=seq(1,16,1)) +
  theme_bw() + 
  xlab("P/D Quartile") +
  ylab("b Coefficient") +
  geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = "grey50") + 
  theme(plot.title = element_text(size = 22),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title =  element_text(size = 16),
        legend.text =  element_text(size = 16),
        legend.justification=c(1,0),
        #axis.title.y = "b coefficient",
        #axis.title.x = "Years from Fund Inception",
        legend.position="bottom",
        legend.title = element_blank()) + 
  labs(title = element_blank())

g





ggsave(paste0(wkdir, "secondary_pd.eps"), width = 11.9, height = 13 )



#####################################  Plot: Histogram Profit Plots ##################################### 



summary(pe_histogram$total.profit)
summary(pe_histogram$total.profit.old)


pe_histogram.plot = pe_histogram %>% mutate(sector =  paste0("Buyout TVPI ", round(as.numeric(pe_r2.table[1,2]), 2),", RAP ", round(as.numeric(pe_r2.table[5,2]), 2), ", RAP > 10% ", round(pe_avg_high_profit.pen.7, 2))) 
vc_histogram.plot = vc_histogram %>% mutate(sector =  paste0("Venture Capital TVPI ", round(as.numeric(vc_r2.table[1,2]), 2),", RAP ", round(as.numeric(vc_r2.table[5,2]), 2), ", RAP > 10% ",  round(vc_avg_high_profit.pen.7, 2)) )
re_histogram.plot = re_histogram %>% mutate(sector =  paste0("Real Estate TVPI ", round(as.numeric(re_r2.table[1,2]), 2),", RAP ", round(as.numeric(re_r2.table[5,2]), 2), ", RAP > 10% ",  round(re_avg_high_profit.pen.7, 2)) )
in_histogram.plot = in_histogram %>% mutate(sector =  paste0("Infrastructure TVPI ", round(as.numeric(in_r2.table[1,2]), 2),", RAP ", round(as.numeric(in_r2.table[5,2]), 2), ", RAP > 10% ",  round(in_avg_high_profit.pen.7, 2))) 
df_histogram.plot = df_histogram %>% mutate(sector =  paste0("Debt Fund TVPI ", round(as.numeric(df_r2.table[1,2]), 2),", RAP ", round(as.numeric(df_r2.table[5,2]), 2), ", RAP > 10% ",  round(df_avg_high_profit.pen.7, 2)) )
rs_histogram.plot = rs_histogram %>% mutate(sector =  paste0("Restructuring TVPI ", round(as.numeric(rs_r2.table[1,2]), 2),", RAP ", round(as.numeric(rs_r2.table[5,2]), 2), ", RAP > 10% ",  round(rs_avg_high_profit.pen.7, 2))) 
ff_histogram.plot = ff_histogram %>% mutate(sector =  paste0("Fund of Funds TVPI ", round(as.numeric(ff_r2.table[1,2]), 2),", RAP ", round(as.numeric(ff_r2.table[5,2]), 2), ", RAP > 10% ",  round(ff_avg_high_profit.pen.7, 2)) )
nr_histogram.plot = nr_histogram %>% mutate(sector =  paste0("Natural Resources TVPI ", round(as.numeric(nr_r2.table[1,2]), 2),", RAP ", round(as.numeric(nr_r2.table[5,2]), 2), ", RAP > 10% ",  round(nr_avg_high_profit.pen.7, 2)) )

ols.vc = vc_histogram.plot %>% filter(Model == "OLS Two Factors")
ols.vc.pen = vc_histogram.plot %>% filter(Model == "Elastic Net Full Factor")

tempy = ols.vc %>% filter(Vintage.New >= 1990 & Vintage.New <= 1997)
summary(tempy$total.profit)
# -0.09501 
tempy2 = ols.vc.pen %>% filter(Vintage.New >= 1990 & Vintage.New <= 1997)
summary(tempy2$total.profit)
# -0.0115 



total.histogram = rbind(pe_histogram.plot, 
                        vc_histogram.plot,
                        re_histogram.plot,
                        in_histogram.plot,
                        df_histogram.plot,
                        rs_histogram.plot,
                        ff_histogram.plot,
                        nr_histogram.plot) %>%
  mutate(sector.f = as.factor(sector))


total.histogram$sector.f <- factor(total.histogram$sector.f,  
                                   levels = c(paste0("Buyout TVPI ", round(as.numeric(pe_r2.table[1,2]), 2),", RAP ", round(as.numeric(pe_r2.table[5,2]), 2), ", RAP > 10% ", round(pe_avg_high_profit.pen.7, 2)) ,
                                              paste0("Venture Capital TVPI ", round(as.numeric(vc_r2.table[1,2]), 2),", RAP ", round(as.numeric(vc_r2.table[5,2]), 2), ", RAP > 10% ",  round(vc_avg_high_profit.pen.7, 2)) , 
                                              paste0("Real Estate TVPI ", round(as.numeric(re_r2.table[1,2]), 2),", RAP ", round(as.numeric(re_r2.table[5,2]), 2), ", RAP > 10% ",  round(re_avg_high_profit.pen.7, 2)) , 
                                              paste0("Fund of Funds TVPI ", round(as.numeric(ff_r2.table[1,2]), 2),", RAP ", round(as.numeric(ff_r2.table[5,2]), 2), ", RAP > 10% ",  round(ff_avg_high_profit.pen.7, 2)) , 
                                              paste0("Restructuring TVPI ", round(as.numeric(rs_r2.table[1,2]), 2),", RAP ", round(as.numeric(rs_r2.table[5,2]), 2), ", RAP > 10% ",  round(rs_avg_high_profit.pen.7, 2)), 
                                              paste0("Debt Fund TVPI ", round(as.numeric(df_r2.table[1,2]), 2),", RAP ", round(as.numeric(df_r2.table[5,2]), 2), ", RAP > 10% ",  round(df_avg_high_profit.pen.7, 2)) , 
                                              paste0("Infrastructure TVPI ", round(as.numeric(in_r2.table[1,2]), 2),", RAP ", round(as.numeric(in_r2.table[5,2]), 2), ", RAP > 10% ",  round(in_avg_high_profit.pen.7, 2)) , 
                                              paste0("Natural Resources TVPI ", round(as.numeric(nr_r2.table[1,2]), 2),", RAP ", round(as.numeric(nr_r2.table[5,2]), 2), ", RAP > 10% ",  round(nr_avg_high_profit.pen.7, 2))))



total.histogram.temp = total.histogram %>% filter(Vintage.New <= 2010)


tempy = vc_histogram.plot %>% filter(Model == "OLS Two Factors")



# Change line colors by groups
g <- ggplot(total.histogram, aes(x=total.profit, color=Model, fill=Model)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.2, binwidth = 0.05)+
  geom_density(alpha=0.7)+
  facet_wrap( ~ sector.f , scales = "free", nrow = 4) + 
  scale_color_manual(values=c( "#E69F00", "#999999","#56B4E9"))+
  scale_fill_manual(values= c( "#E69F00", "#999999","#56B4E9"))+
  ylab("") + xlab("Profit Relative to $1 Committed to Replicating Portfolio") +
  theme_bw() + 
  theme(plot.title = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22),
        axis.title =  element_text(size = 18),
        legend.text =  element_text(size = 14),
        legend.position="bottom",
        legend.title = element_blank())

g


ggsave(paste0(wkdir, "all_histograms.png"), width = 11.9, height = 13)



pe_histogram.profit.time = pe_histogram %>% mutate(sector = "Buyout")
vc_histogram.profit.time = vc_histogram %>% mutate(sector = "Venture Capital")
re_histogram.profit.time = re_histogram %>% mutate(sector = "Real Estate")
in_histogram.profit.time = in_histogram %>% mutate(sector = "Infrastructure")
df_histogram.profit.time = df_histogram %>% mutate(sector = "Debt Fund")
rs_histogram.profit.time = rs_histogram %>% mutate(sector = "Restructuring")
ff_histogram.profit.time = ff_histogram %>% mutate(sector = "Fund of Funds")
nr_histogram.profit.time = nr_histogram %>% mutate(sector = "Natural Resources")

profit.time = rbind(pe_histogram.profit.time, 
                    vc_histogram.profit.time,
                    re_histogram.profit.time,
                    ff_histogram.profit.time) %>%
  mutate(sector.f = as.factor(sector),
         Model = ifelse(Model == "OLS Two Factors", "OLS", "Elastic Net"),
         specification.f = as.factor(Model))



profit.time.plot = profit.time %>%
  group_by(Vintage.New, sector, Model) %>%
  mutate(avg.profit.calc = mean(total.profit)) %>%
  as.data.frame() %>%
  select(avg.profit.calc, Vintage.New, sector.f, specification.f)

#facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 

profit.time.plot$specification.f <- factor(profit.time.plot$specification.f,  levels = c("OLS", "Elastic Net"))
profit.time.plot$sector.f <- factor(profit.time.plot$sector.f,  levels = c("Buyout", "Venture Capital", "Real Estate", "Fund of Funds"))


# 4-factor
g <- ggplot(profit.time.plot %>% filter(Vintage.New <= 2010), aes(x = Vintage.New, y = avg.profit.calc))  +
  geom_line() + 
  geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
  theme_bw() +
  #coord_cartesian(ylim = c(-0.5, 1.50)) + 
  facet_wrap( ~ sector.f + specification.f, nrow = 4, scales = "free") + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
  theme(plot.title = element_text(size = 20),
        strip.background = element_blank(),
        axis.text.x= element_text(size = 16),
        strip.text.x = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.y= element_text(size = 18),
        panel.border = element_blank(),
        legend.justification=c(1,1), 
        panel.spacing = unit(1.5, "lines"),
        legend.position=c(0.95, 0.95),
        legend.title = element_blank()) +
  ylab("RAP Profit") + xlab("Vintage") 
g


ggsave(paste0(wkdir, "all_profit_time.png"), width = 11.9, height = 13)


# 4-factor
g <- ggplot(profit.time.plot %>% filter(Vintage.New <= 2010), aes(x = Vintage.New, y = avg.profit.calc))  +
  geom_line() + 
  geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
  theme_bw() +
  coord_cartesian(ylim = c(-0.5, 1.50)) + 
  facet_wrap( ~ sector.f + specification.f, nrow = 4, scales = "free") + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
  theme(plot.title = element_text(size = 20),
        strip.background = element_blank(),
        axis.text.x= element_text(size = 16),
        strip.text.x = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.y= element_text(size = 18),
        panel.border = element_blank(),
        legend.justification=c(1,1), 
        panel.spacing = unit(1.5, "lines"),
        legend.position=c(0.95, 0.95),
        legend.title = element_blank()) +
  ylab("RAP Profit") + xlab("Vintage") 
g


ggsave(paste0(wkdir, "all_profit_time_v2.png"), width = 11.9, height = 13)






profit.time.alt = rbind(rs_histogram.profit.time, 
                        df_histogram.profit.time,
                        in_histogram.profit.time,
                        nr_histogram.profit.time) %>%
  mutate(sector.f = as.factor(sector),
         Model = ifelse(Model == "OLS Two Factors", "OLS", "Elastic Net"),
         specification.f = as.factor(Model))



profit.time.plot.alt = profit.time.alt %>%
  group_by(Vintage.New, sector, Model) %>%
  mutate(avg.profit.calc = mean(total.profit)) %>%
  as.data.frame() %>%
  select(avg.profit.calc, Vintage.New, sector.f, specification.f)

#facet_wrap( ~ sector.f + specification.f , scales = "free", nrow = 4) + 

profit.time.plot.alt$specification.f <- factor(profit.time.plot.alt$specification.f,  levels = c("OLS", "Elastic Net"))
profit.time.plot.alt$sector.f <- factor(profit.time.plot.alt$sector.f,  levels = c("Restructuring", "Debt Fund", "Infrastructure", "Natural Resources"))


# 4-factor
g <- ggplot(profit.time.plot.alt %>% filter(Vintage.New <= 2010), aes(x = Vintage.New, y = avg.profit.calc))  +
  geom_line() + 
  geom_point(shape = 16, fill = "white", size = .5, stroke = 5)  + 
  theme_bw() +
  facet_wrap( ~ sector.f + specification.f, nrow = 4, scales = "free_x") + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1, color = "grey50") + 
  theme(plot.title = element_text(size = 20),
        strip.background = element_blank(),
        axis.text.x= element_text(size = 16),
        strip.text.x = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.y= element_text(size = 18),
        panel.border = element_blank(),
        legend.justification=c(1,1), 
        panel.spacing = unit(1.5, "lines"),
        legend.position=c(0.95, 0.95),
        legend.title = element_blank()) +
  ylab("RAP Profit") + xlab("Vintage") 
g


ggsave(paste0(wkdir, "all_profit_time_alt.png"), width = 11.9, height = 13)







#####################################  Table: R2 ##################################### 


# Put at end now
r2.table = left_join(pe_r2.table, vc_r2.table) %>%
  left_join(re_r2.table) %>% left_join(ff_r2.table) %>%
  mutate(pe_profits.sd = paste0("(", pe_profits.sd, ")"),
         vc_profits.sd = paste0("(", vc_profits.sd, ")"),
         re_profits.sd = paste0("(", re_profits.sd, ")"),
         ff_profits.sd = paste0("(", ff_profits.sd, ")"))

r2.table = r2.table %>%
  mutate(pe_profits.mean = as.numeric(pe_profits.mean),
         vc_profits.mean = as.numeric(vc_profits.mean),
         re_profits.mean = as.numeric(re_profits.mean),
         ff_profits.mean = as.numeric(ff_profits.mean))



r2.table[2,1] <- "IRR (%)"

tab.latex = r2.table %>%
  gt() %>%
  fmt_number(
    columns = vars(pe_profits.mean, vc_profits.mean, re_profits.mean, ff_profits.mean),
    drop_trailing_zeros = FALSE,
    rows = 2,
    decimals = 3) %>%
  fmt_number(
    columns = vars(pe_profits.mean, vc_profits.mean, re_profits.mean, ff_profits.mean),
    drop_trailing_zeros = FALSE,
    rows = 4:7,
    decimals = 3) %>%
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
  columns = vars(pe_profits.mean, pe_profits.sd)
) %>%
  tab_spanner(
    label = "VC",
    columns = vars(vc_profits.mean, vc_profits.sd)
  ) %>%
  tab_spanner(
    label = "Real Estate",
    columns = vars(re_profits.mean, re_profits.sd)
  ) %>%
  tab_spanner(
    label = "Fund of Funds",
    columns = vars(ff_profits.mean, ff_profits.sd)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    # pe_R2 = "R^2",
    # re_R2 = "R^2",
    # in_R2 = "R^2",
    # vc_R2 = "R^2",
    #pe_Q_R2 = "Q R^2",k
    #re_Q_R2 = "Q R^2",
    #in_Q_R2 = "Q R^2",
    #vc_Q_R2 = "Q R^2",
    pe_profits.mean = "RAP",
    vc_profits.mean = "RAP",
    re_profits.mean = "RAP",
    ff_profits.mean = "RAP",
    pe_profits.sd = "SD RAP",
    vc_profits.sd = "SD RAP",
    re_profits.sd = "SD RAP",
    ff_profits.sd = "SD RAP") 

tab.latex

tab.latex %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out.tex"))




r2.table.xtable <- xtable(r2.table)
# print.xtable(r2.table.xtable, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_raw.tex"))






# Put at end now
r2.table.alt = left_join(rs_r2.table, df_r2.table)  %>%
  left_join(in_r2.table) %>% left_join(nr_r2.table) %>%
  mutate(rs_profits.sd = paste0("(", rs_profits.sd, ")"),
         df_profits.sd = paste0("(", df_profits.sd, ")"),
         in_profits.sd = paste0("(", in_profits.sd, ")"),
         nr_profits.sd = paste0("(", nr_profits.sd, ")"))

r2.table.alt[2,1] <- "IRR (%)"

tab.latex.alt = r2.table.alt %>%
  #select(-rs_persistence, -df_persistence, -in_persistence, -nr_persistence) %>%
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
  label = "Restructuring",
  columns = vars(rs_profits.mean, rs_profits.sd)
) %>%
  tab_spanner(
    label = "Debt Fund",
    columns = vars(df_profits.mean, df_profits.sd)
  ) %>%
  tab_spanner(
    label = "Infrastructure",
    columns = vars(in_profits.mean, in_profits.sd)
  ) %>%
  tab_spanner(
    label = "Natural Resources",
    columns = vars(nr_profits.mean, nr_profits.sd)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    rs_profits.mean = "RAP",
    df_profits.mean = "RAP",
    in_profits.mean = "RAP",
    nr_profits.mean = "RAP",
    rs_profits.sd = "SD RAP",
    df_profits.sd = "SD RAP",
    in_profits.sd = "SD RAP",
    nr_profits.sd = "SD RAP") 

tab.latex.alt

tab.latex.alt %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out_alt.tex"))



r2.table.alt.xtable <- xtable(r2.table.alt)
# print.xtable(r2.table.alt.xtable, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_alt_raw.tex"))



#####################################  Table: Persistence Quartiles ##################################### 



#pe_r2.table.alt = pe_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
#pe_r2.table.persistence$pe_persistence = pe_r2.table.alt$pe_persistence
#
#vc_r2.table.alt = vc_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
#vc_r2.table.persistence$vc_persistence = vc_r2.table.alt$vc_persistence
#
#re_r2.table.alt = re_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
#re_r2.table.persistence$re_persistence = re_r2.table.alt$re_persistence
#
#ff_r2.table.alt = ff_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
#ff_r2.table.persistence$ff_persistence = ff_r2.table.alt$ff_persistence


# Put at end now
r2.table.persistence1 = left_join(pe_r2.table.persistence, vc_r2.table.persistence)

r2.table.persistence2 = left_join(re_r2.table.persistence, ff_r2.table.persistence)

tab.latex.persistence1 = r2.table.persistence1 %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
  tab_spanner(
    label = "Buyout",
    columns = vars(pe_persistence, pe_top.quartile, pe_bottom.quartile)
  ) %>%
  tab_spanner(
    label = "VC",
    columns = vars(vc_persistence, vc_top.quartile, vc_bottom.quartile)
  ) %>%
  #tab_spanner(
  #  label = "Real Estate",
  #  columns = vars(re_persistence, re_top.quartile, re_bottom.quartile)
  #) %>%
  #tab_spanner(
  #  label = "Fund of Funds",
  #  columns = vars(ff_persistence, ff_top.quartile, ff_bottom.quartile)
  #) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    pe_persistence = "Persistence",
    vc_persistence = "Persistence",
    #re_persistence = "Persistence",
    #ff_persistence = "Persistence",
    pe_top.quartile = "Q4 Persistence",
    vc_top.quartile = "Q4 Persistence",
    #re_top.quartile = "Q4 Persistence",
    #ff_top.quartile = "Q4 Persistence",
    pe_bottom.quartile = "Q1 Persistence",
    vc_bottom.quartile = "Q1 Persistence",
    #re_bottom.quartile = "Q1 Persistence",
    #ff_bottom.quartile = "Q1 Persistence",
  ) 

tab.latex.persistence1

tab.latex.persistence1 %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out_persistence1.tex"))



r2.table.persistence1.export <- xtable(r2.table.persistence1)
print.xtable(r2.table.persistence1.export, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_persistence1_raw.tex"))
 

tab.latex.persistence2 = r2.table.persistence2 %>% 
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
  #tab_spanner(
  #  label = "Buyout",
  #  columns = vars(pe_persistence, pe_top.quartile, pe_bottom.quartile)
  #) %>%
  #tab_spanner(
  #  label = "VC",
  #  columns = vars(vc_persistence, vc_top.quartile, vc_bottom.quartile)
  #) %>%
  tab_spanner(
    label = "Real Estate",
    columns = vars(re_persistence, re_top.quartile, re_bottom.quartile)
  ) %>%
  tab_spanner(
    label = "Fund of Funds",
    columns = vars(ff_persistence, ff_top.quartile, ff_bottom.quartile)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    #pe_persistence = "Persistence",
    #vc_persistence = "Persistence",
    re_persistence = "Persistence",
    ff_persistence = "Persistence",
    #pe_top.quartile = "Q4 Persistence",
    #vc_top.quartile = "Q4 Persistence",
    re_top.quartile = "Q4 Persistence",
    ff_top.quartile = "Q4 Persistence",
    #pe_bottom.quartile = "Q1 Persistence",
    #vc_bottom.quartile = "Q1 Persistence",
    re_bottom.quartile = "Q1 Persistence",
    ff_bottom.quartile = "Q1 Persistence",
  ) 

tab.latex.persistence2

tab.latex.persistence2 %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out_persistence2.tex"))

r2.table.persistence2.export <- xtable(r2.table.persistence2)
print.xtable(r2.table.persistence2.export, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_persistence2_raw.tex"))


#####################################  Table: Persistence Quartiles Transition ##################################### 



#pe_r2.table.alt = pe_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
pe_r2.table.persistence.trans$pe_persistence = pe_r2.table.persistence$pe_persistence
#
##vc_r2.table.alt = vc_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
vc_r2.table.persistence.trans$vc_persistence = vc_r2.table.persistence$vc_persistence
#
##re_r2.table.alt = re_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
re_r2.table.persistence.trans$re_persistence = re_r2.table.persistence$re_persistence
#
##ff_r2.table.alt = ff_r2.table %>% filter(Model != "2 factor OLS: $1 Call" & Model != "15 factor Elastic Net: $1 Call")
ff_r2.table.persistence.trans$ff_persistence = ff_r2.table.persistence$ff_persistence


# Put at end now
r2.table.persistence1.trans = left_join(pe_r2.table.persistence.trans, vc_r2.table.persistence.trans)

r2.table.persistence2.trans = left_join(re_r2.table.persistence.trans, ff_r2.table.persistence.trans)


r2.table.persistence1.trans = r2.table.persistence1.trans %>%
  mutate(pe_top.quartile = as.numeric(pe_top.quartile),
         pe_bottom.quartile = as.numeric(pe_bottom.quartile),
         pe_persistence = as.numeric(pe_persistence),
         vc_top.quartile = as.numeric(vc_top.quartile),
         vc_bottom.quartile = as.numeric(vc_bottom.quartile),
         vc_persistence = as.numeric(vc_persistence))




tab.latex.persistence1.trans = r2.table.persistence1.trans %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
  fmt_number(
    columns = vars(pe_top.quartile, pe_bottom.quartile, pe_persistence, vc_top.quartile, vc_bottom.quartile, vc_persistence),
    drop_trailing_zeros = FALSE,
    decimals = 2) %>%
  tab_spanner(
    label = "Buyout",
    columns = vars(pe_persistence, pe_top.quartile, pe_bottom.quartile)
  ) %>%
  tab_spanner(
    label = "VC",
    columns = vars(vc_persistence, vc_top.quartile, vc_bottom.quartile)
  ) %>%
  #tab_spanner(
  #  label = "Real Estate",
  #  columns = vars(re_persistence, re_top.quartile, re_bottom.quartile)
  #) %>%
  #tab_spanner(
  #  label = "Fund of Funds",
  #  columns = vars(ff_persistence, ff_top.quartile, ff_bottom.quartile)
  #) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    pe_persistence = "Persistence",
    vc_persistence = "Persistence",
    #re_persistence = "Persistence",
    #ff_persistence = "Persistence",
    pe_top.quartile = "Top Quart",
    vc_top.quartile = "Bottom Quart",
    #re_top.quartile = "Q4 Persistence",
    #ff_top.quartile = "Q4 Persistence",
    pe_bottom.quartile = "Top Quart",
    vc_bottom.quartile = "Bottom Quart",
    #re_bottom.quartile = "Q1 Persistence",
    #ff_bottom.quartile = "Q1 Persistence",
  ) 

tab.latex.persistence1.trans

tab.latex.persistence1.trans %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out_persistence1_trans.tex"))

r2.table.persistence1.trans.export <- xtable(r2.table.persistence1.trans)
#print.xtable(r2.table.persistence1.trans.export, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_persistence1_trans_raw.tex"))



r2.table.persistence2.trans = r2.table.persistence2.trans %>%
  mutate(ff_top.quartile = as.numeric(ff_top.quartile),
         ff_bottom.quartile = as.numeric(ff_bottom.quartile),
         re_top.quartile = as.numeric(re_top.quartile),
         re_bottom.quartile = as.numeric(re_bottom.quartile))



tab.latex.persistence2.trans = r2.table.persistence2.trans %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  )  %>% 
  fmt_number(
    columns = vars(ff_top.quartile, ff_bottom.quartile, re_top.quartile, re_bottom.quartile),
    drop_trailing_zeros = FALSE,
    decimals = 2) %>%
  #tab_spanner(
  #  label = "Buyout",
  #  columns = vars(pe_persistence, pe_top.quartile, pe_bottom.quartile)
  #) %>%
  #tab_spanner(
  #  label = "VC",
  #  columns = vars(vc_persistence, vc_top.quartile, vc_bottom.quartile)
  #) %>%
  tab_spanner(
    label = "Real Estate",
    columns = vars(re_persistence, re_top.quartile, re_bottom.quartile)
  ) %>%
  tab_spanner(
    label = "Fund of Funds",
    columns = vars(ff_persistence, ff_top.quartile, ff_bottom.quartile)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    #pe_persistence = "Persistence",
    #vc_persistence = "Persistence",
    re_persistence = "Persistence",
    ff_persistence = "Persistence",
    #pe_top.quartile = "Q4 Persistence",
    #vc_top.quartile = "Q4 Persistence",
    re_top.quartile = "Top Quart",
    ff_top.quartile = "Bottom Quart",
    #pe_bottom.quartile = "Q1 Persistence",
    #vc_bottom.quartile = "Q1 Persistence",
    re_bottom.quartile = "Top Quart",
    ff_bottom.quartile = "Bottom Quart",
  ) 

tab.latex.persistence2.trans

tab.latex.persistence2.trans %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(tables,"latex_out_persistence2_trans.tex"))

tab.latex.persistence2.trans.export <- xtable(r2.table.persistence2.trans)
#print.xtable(tab.latex.persistence2.trans.export, omitlatexcom=T, include.rownames=FALSE,booktabs=TRUE, only.contents = TRUE, include.colnames = FALSE, floating = FALSE, file =  paste0(tables, "latex_out_persistence2_trans_raw.tex"))

