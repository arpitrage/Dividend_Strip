# Play around with model fit and specifications here 



# New method -- replicate the portfolio on cash; price is not 1
# Remaining TODO:



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
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/ExpectedReturnOct20.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DividendStripOct20.Rda")
  
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/States.Rda")
  
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/MergedCashFlowOct20.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/PD_June20.Rda")
  load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/IRRJuly20.Rda")
  
  
  
  
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
  
  # Buyout: average fund in vintage
  # buyout = fund.quarterly.div %>% filter(fund.category  == "Buyout")
  # buyout.temp = buyout %>% filter(Vintage >= 1989) %>% select(Fund.ID, Firm.ID, Vintage, irr.preqin) %>%
  #  unique %>% mutate(counter = 1) %>% group_by(Vintage) %>% mutate(total.funds = sum(counter)) %>%
  #  as.data.frame()
  # summary(buyout.temp$total.funds)
  # sd(buyout.temp$irr.preqin, na.rm = TRUE)
  # Numbr of Vintages
  # CX IRR 
  
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

wkdir <- "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Appendix/"

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
  
  
 
  
  
  
  
  
  #####################################  Analysis: Models ###################################
  
  
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
  
  
  #####################################  Analysis: Run Specifications ###################################
  
  # Runs across specifications
  {
    
    
    # Full Model
    penalized7 = cva.glmnet(x = fund.subset.x.age.pd, y = fund.subset.y, alpha = seq(0, 1, len = 11)^3,
                            nfolds = 10, lower.limits = 0, intercept = FALSE)
    
     
    
    ols.2 = lm(net.cf.distribution.rescale ~ 0 + AgeFactor + 
                 AgeFactor:gain.cohort.stock + 
                 pd_categoryFactorQuartile +
                 pd_categoryFactorQuartile:gain.cohort.stock , data = fund.subset)
    
     
    
    
    
    
  }
  
  
  # Extract Best fit Model
  {
    
    
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
      

      
      penalized7.model.all <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                     lower.limits = 0, 
                                     alpha = best.params.7$alpha, intercept = FALSE)
      
    

      
     
    }
    
    
    
  }
  
  
  
  # Store predictions from models
  {
    
    # 2 factor OLS
    predictions2.ols <- predict(ols.2,  newdata = fund.subset)
    predictions.unlist2.ols <- predictions2.ols
    fund.subset$predictions <- predictions.unlist2.ols
    
    fund.subset.predictions2.ols = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
    
    
    
    
    # 7 (full) factor
    predictions7 <- predict(penalized7.model, fund.subset.x.age.pd, type = "response")
    predictions.unlist7 <- as.numeric(unlist(predictions7)[,1])
    fund.subset$predictions <- predictions.unlist7
    
    fund.subset.predictions7 = fund.subset %>%
      select(Fund.ID, Firm.ID, Age.Quarter, Vintage.Quarter, predictions, price.bond.feb, net.cf.distribution.rescale)
    
    
     
    
  }
  
  
  
  
  
  #####################################  Plot Model Outcomes ##################################
    
    
    
    # Plot coefficients varying across lambdas 
    {
      
    penalized7.model.all.df = penalized7.model.all$beta %>% as.matrix() %>% as.data.frame()
    
    lambda <- penalized7.model.all$lambda %>% as.data.frame()
    s0 <- penalized7.model.all$a0 %>% as.data.frame()
    s0$s0 <- rownames(s0)
    lambda = cbind(lambda, s0)
    names(lambda) <- c("lambda", "dot", "model_s0")
    
    
    penalized7.model.all.df$factor <- rownames(penalized7.model.all.df)  
    
    penalized7.model.all.df.temp <- penalized7.model.all.df %>%
      pivot_longer(-factor, names_to = "model_s0", values_to = "value") %>%
      as.data.frame()
    
    penalized7.model.all.df.temp <- left_join(penalized7.model.all.df.temp, lambda)    

    penalized.broom.pd = penalized7.model.all.df.temp %>%
      separate(factor, c("model", "pd_categoryFactorQuartile"), "_pd_") %>% 
      filter(!is.na(pd_categoryFactorQuartile))
    
    
    penalized.broom.age = penalized7.model.all.df.temp %>%
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
    
    
    
    
    
    
    
      
    
    
    
    
   
    penalized.broom.age.compressed = penalized.broom.age %>%
      group_by(model_s0, Type) %>%
      mutate(category.sum = sum(value)) %>%
      select(model_s0, Type, category.sum, lambda) %>% unique %>% as.data.frame()
    
    # clean s0 
    penalized.broom.age.compressed = penalized.broom.age.compressed %>%
      separate(model_s0, c("s", "model_number"), "s") 
      
    penalized.broom.age.compressed = penalized.broom.age.compressed %>%
      filter(category.sum > 0) %>%
      mutate(model_number = as.numeric(model_number))
    
    
    
    
    g <- ggplot(data = penalized.broom.age.compressed, aes(x = log(lambda) , y = category.sum, group = factor(Type), color = factor(Type))) + 
      geom_line() + 
      geom_point(shape = 16, fill = "white", size = .2, stroke = 2) +
      theme_bw() + 
      xlab("Log(Lambda)") +
      geom_text(x=log(best.params.7$min_lambda) + 2, y=0.03, label="Cross Validated Lambda", color = "gray") + 
      ylab("Sum of Coefficients in Category") +
      geom_vline(xintercept = log(best.params.7$min_lambda), linetype = "dashed", color = "gray") + 
      theme(plot.title = element_text(size = 22),
          axis.text.x= element_text(size = 22),
          axis.text.y= element_text(size = 22),
          axis.title =  element_text(size = 22),
          legend.text =  element_text(size = 14),
          legend.justification=c(1,0),
          legend.position="bottom",
          legend.title = element_blank())
      
    g
    ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_7f_cv.png"), width = 14.4/1.5, height = 7.24/1.5 )
    
    
    myname1 = paste0(fund.abbreviations[i], "_", "coefs")
    assign(myname1, penalized.broom.age.compressed)
    

    
    
    
     
    }
    
  
    # plot across alphas
    {
    
      
      best.lambda = best.params.7$min_lambda
      best.alpha = round(best.params.7$alpha, 3)
      
      
      all.beta.matrix = NULL
      for (j in 1:number.of.alphas.tested){
        
        k = j - 1
        sx = paste0("s", k)
        
        alpha.value <- penalized7$alpha[j]
        
        
        penalized7.model.alphas <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                          lower.limits = 0, 
                                          alpha = alpha.value,
                                          lambda = best.lambda, intercept = FALSE)
        
        
       beta.matrix = penalized7.model.alphas$beta %>% as.matrix()
       colnames(beta.matrix) <- sx
        
        all.beta.matrix = cbind(all.beta.matrix, beta.matrix)
       
      
        
      }
      
      
      alpha <- penalized7$alpha
      s0 <- colnames(all.beta.matrix) %>% as.data.frame()
      alpha = cbind(alpha, s0)
      names(alpha) <- c("alpha", "model_s0")
      
      
      all.beta.matrix.df <- all.beta.matrix %>%  as.data.frame()
      all.beta.matrix.df$factor <- rownames(all.beta.matrix.df)  
      
      penalized7.model.all.df.temp.alpha <- all.beta.matrix.df %>%
        pivot_longer(-factor, names_to = "model_s0", values_to = "value") %>%
        as.data.frame()
      
      penalized7.model.all.df.temp.alpha <- left_join(penalized7.model.all.df.temp.alpha, alpha)    
      
      penalized.broom.pd.alpha = penalized7.model.all.df.temp.alpha %>%
        separate(factor, c("model", "pd_categoryFactorQuartile"), "_pd_") %>% 
        filter(!is.na(pd_categoryFactorQuartile))
      
      
      penalized.broom.age.alpha = penalized7.model.all.df.temp.alpha %>%
        separate(factor, c("model", "pd_categoryFactorQuartile"), "_pd_") %>%
        filter(is.na(pd_categoryFactorQuartile)) %>% 
        select(-pd_categoryFactorQuartile)  %>%
        separate(model, c("model", "AgeFactor"), "_") 
      
      
      penalized.broom.age.alpha = penalized.broom.age.alpha %>%
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
      
      
      penalized.broom.pd.alpha = penalized.broom.pd.alpha %>%
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
      
      
      
      
      
      
      
      
      
      
      
      
      
      penalized.broom.age.compressed.alpha = penalized.broom.age.alpha %>%
        group_by(model_s0, Type) %>%
        mutate(category.sum = sum(value)) %>%
        select(model_s0, Type, category.sum, alpha) %>% unique %>% as.data.frame()
      
      # clean s0 
      penalized.broom.age.compressed.alpha = penalized.broom.age.compressed.alpha %>%
        separate(model_s0, c("s", "model_number"), "s") 
      
      penalized.broom.age.compressed.alpha = penalized.broom.age.compressed.alpha %>%
        filter(category.sum > 0) %>%
        mutate(model_number = as.numeric(model_number))
      
      
      
      
      g <- ggplot(data = penalized.broom.age.compressed.alpha, aes(x = alpha , y = category.sum, group = factor(Type), color = factor(Type))) + 
        geom_line() + 
        geom_point(shape = 16, fill = "white", size = .2, stroke = 2) +
        theme_bw() + 
        xlab("Alpha") +
        geom_text(x=best.alpha + 0.1, y=0.03, label="Cross Validated Alpha", color = "gray") + 
        ylab("Sum of Coefficients in Category") +
        geom_vline(xintercept = best.alpha, linetype = "dashed", color = "gray") + 
        theme(plot.title = element_text(size = 22),
              axis.text.x= element_text(size = 22),
              axis.text.y= element_text(size = 22),
              axis.title =  element_text(size = 22),
              legend.text =  element_text(size = 14),
              legend.justification=c(1,0),
              legend.position="bottom",
              legend.title = element_blank())
      
      g
      ggsave(paste0(wkdir, fund.abbreviations[i], "_pen_7f_alpha.png"), width = 14.4/1.5, height = 7.24/1.5 )
      
      
      myname1.alpha = paste0(fund.abbreviations[i], "_", "coefs_alpha")
      assign(myname1.alpha, penalized.broom.age.compressed.alpha)
      
      
      
      
      
      
     }
    
  
    # MSE graph
    {
      
    
      
      jpeg(file=paste0(wkdir, fund.abbreviations[i], "_mse.jpg"))
      plot(penalized7)
      dev.off()

    
    }
    
    
    # Heatmap -- MSE
    {
      
      
      
      heatmap <- NULL
      for (j in 1:number.of.alphas.tested){
        
        # for this alpha
        alpha.value <- penalized7$alpha[j]
        
        # run model for this alpha, get all the lambdas
        #penalized7.model.temp <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
        #                                lower.limits = 0, 
        #                                alpha = alpha.value, intercept = FALSE)
        
        penalized7.model.loop <- cv.glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                        lower.limits = 0, 
                                        alpha = alpha.value, intercept = FALSE)
        
        

        # list of lambdas
        lambdas <- penalized7.model.loop$lambda
        
        # List of MSEs
        mse <- penalized7.model.loop$cvm
        j.value = j 
        
        # takeaway
        df <- cbind(mse, lambdas, j.value)
        df <- cbind(df, alpha.value) 
        df <- df %>% as.data.frame()
        
        heatmap = rbind(df, heatmap)
        
         
       
        
      }
      
      
      #head(heatmap$lambdas)
    
      heatmap.map = heatmap %>%
        mutate(log.lambdas = log(lambdas)) %>%
        mutate(floor.lambda = floor(log.lambdas)) %>%
        arrange(alpha.value, floor.lambda, log.lambdas) %>%
        group_by(alpha.value, floor.lambda) %>%
        slice(1) %>% as.data.frame() %>%
        mutate(alpha.value = round(alpha.value, 2))
        
      
      alpha.optimal = round(best.params.7$alpha, 3)
      lambda.optimal = round(log(best.params.7$min_lambda), 2)
      
      
      heatmap.map <- cbind(heatmap.map, alpha.optimal) %>% cbind(lambda.optimal)
      
                       
      # try to calculate limits of blocking out picked factors
      
      x.axis = abs(min(heatmap.map$floor.lambda) - floor(lambda.optimal))
      y.axis = which(abs(penalized7$alpha -alpha.optimal)==min(abs(penalized7$alpha-alpha.optimal)))
      
      

                           
      # Heatmap 
      h <- ggplot(heatmap.map, aes(as.factor(floor.lambda), as.factor(alpha.value))) + 
        geom_tile(aes(fill = mse)) +
        scale_fill_distiller(palette = "RdPu", values = c( 0.00001, 0.001, 0.01,0.03, 0.04, 0.05, 0.10,0.50, 0.75,  0.90, 0.95, 0.99, Inf), na.value="#8B007C") + 
        theme_bw() + 
        ylab("Alpha") + 
        xlab("Log(lambda)") +
        geom_rect(aes(xmin = x.axis + 0.5, xmax =  x.axis + 1.5, ymin = y.axis - 0.5, ymax = y.axis + 0.5),
                  fill = "transparent", color = "black", size = 1.5) +
        theme(text =  element_text(size = 22),
             plot.title = element_text(size = 22),
              axis.text.x= element_text(size = 22),
              axis.text.y= element_text(size = 22),
              axis.title =  element_text(size = 22),
              legend.text =  element_text(size = 14))
      
      
      h
      
   
      ggsave(paste0(wkdir, fund.abbreviations[i], "_heatmap_mse.png"), width = 14.4/1.5, height = 7.24/1.5 )
      
     
      g <- ggplot(heatmap.map, aes(as.factor(floor.lambda), as.factor(alpha.value))) + 
        geom_tile(aes(fill = mse)) +
        scale_fill_distiller(palette = "RdPu") + 
        theme_bw() + 
        ylab("Alpha") + 
        xlab("Log(lambda)") +
        geom_rect(aes(xmin = x.axis + 0.5, xmax =  x.axis + 1.5, ymin = y.axis - 0.5, ymax = y.axis + 0.5),
                  fill = "transparent", color = "black", size = 1.5) +
        theme(text =  element_text(size = 22),
              plot.title = element_text(size = 22),
              axis.text.x= element_text(size = 22),
              axis.text.y= element_text(size = 22),
              axis.title =  element_text(size = 22),
              legend.text =  element_text(size = 14))
      
      
      g
      
      
      ggsave(paste0(wkdir, fund.abbreviations[i], "_heatmap_mse_alt.png"), width = 14.4/1.5, height = 7.24/1.5 )
      
      
      
      
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
    
    # Heatmap -- Profit
    {
    
      # Main Quarterly Strip Dataset -- prices + expected returns; no div strips (names are in coefficinets; just prices)
      strip.data.vintage = left_join(price.strip.quarterly.feb, PD.initial.quarterly) %>%
        left_join(price.strip.capital.gains) %>%
        left_join(strip.return.quarterly) 
      
      
      strip.data.vintage = strip.data.vintage %>% mutate(Age.Quarter = (Transaction.Quarter - Vintage.Quarter) + 1,
                                                         AgeFactor = floor(Age.Quarter))
      
      
      strip.data.vintage = strip.data.vintage %>%
        mutate(pd_categoryFactorQuartile = pd_category.4.quarterly)
      
    
    
      heatmap.profit <- NULL
      for (j in 1:number.of.alphas.tested) {
        alpha.value2 <- penalized7$alpha[j]
        
        heatmap.map.lambda = heatmap.map %>% filter(j.value == j)
        lambda.min = min(heatmap.map.lambda$floor.lambda)
        lambda.max = max(heatmap.map.lambda$floor.lambda)
        
        for(k in lambda.min:lambda.max) {
          
          run.lambda = exp(k)
          
          penalized.profit <- glmnet(x = fund.subset.x.age.pd , y = fund.subset.y,
                                     lower.limits = 0, 
                                     lambda = run.lambda, 
                                     alpha = alpha.value2, intercept = FALSE)
          
          predictions7 <- predict(penalized.profit, fund.subset.x.age.pd, type = "response")
          
          # All Factor
          temp7 = LassoCoefs(penalized.profit, model.list7)
          penalized.7.wide.age = temp7[[1]]
          penalized.7.wide.pd = temp7[[2]]
          age.coef.pen.7 = temp7[[3]]
          pd.coef.pen.7 = temp7[[4]]
          
          # 7 Factor -- All    
          strip.data.vintage.pen.7 = left_join(strip.data.vintage, penalized.7.wide.age)
          strip.data.vintage.pen.7 = left_join(strip.data.vintage.pen.7, penalized.7.wide.pd)
          strip.data.vintage.pen.7 <- MissingVars(strip.data.vintage.pen.7)
          
          # 7 factor -- All
          strip.data.vintage.pen.q.7 <- QGenerate(strip.data.vintage.pen.7, model.list7, price.list7)
          
          
          age.coef.pen.7 = age.coef.pen.7 %>%
            mutate(specification = "Elastic Net")
          
          pd.coef.pen.7 = pd.coef.pen.7 %>%
            mutate(specification = "Elastic Net")
          
          
          
          # TVPI, cash on cash
          fund.subset = fund.subset %>% group_by(Firm.ID, Fund.ID) %>% 
            mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame()
          
          # First, link back dataset to main cash flows
          fund.subset.new = fund.subset %>%
            select(Transaction.Quarter, all.truncated.discounted.call.profit, all.truncated.call.profit, actual.plus.residual.call.profit, Age.Quarter, Fund.ID, Firm.ID, GPME, pme, tvpi, irr, irr.npv, irr.preqin, Vintage.New, year, Vintage.Quarter, net.cf.distribution.rescale)
          
          
          fund.subset.pen.7 = left_join(fund.subset.new, strip.data.vintage.pen.q.7) %>% filter(Age.Quarter != 0)
          
          
          
          # PEN 5 factor 
          temp.profit.pen.7 = ProfitAltCall(strip.data.vintage.pen.q.7,fund.subset.predictions7, fund.subset.new )
          
          
          profit.subset.pen.7 = temp.profit.pen.7[[2]] %>% filter(Vintage.New <= 2010)
          fund.subset.vintage.pen.7 = temp.profit.pen.7[[3]]
          
          
          avg_profit.pen.7 = round(mean(profit.subset.pen.7$total.profit), 3)
          
          # takeaway
          df <- cbind(avg_profit.pen.7, run.lambda)
          df <- cbind(df, alpha.value2) 
          df <- df %>% as.data.frame()
          
          heatmap.profit = rbind(df, heatmap.profit)
          
        }
        
      
      }
    
      
      #optimal alpha lambda
      alpha.optimal.profit = round(best.params.7$alpha, 3)
      lambda.optimal.profit = round(log(best.params.7$min_lambda), 2)
      
      
      
      x.axis.profit = abs(min(heatmap.map$floor.lambda) - floor(lambda.optimal.profit))
      y.axis.profit = which(abs(penalized7$alpha -alpha.optimal.profit)==min(abs(penalized7$alpha-alpha.optimal.profit)))
      
      
      
      heatmap.profit = heatmap.profit %>%
        mutate(Profit = avg_profit.pen.7)
      

      
      # Heatmap 
      h <- ggplot(heatmap.profit, aes(as.factor(log(run.lambda)), as.factor(alpha.value2), fill= Profit)) + 
        geom_tile() +
        scale_fill_distiller(palette = "RdPu", values = c( 0.00001, 0.001,0.03, 0.05, 0.10 , 0.20, .25, 0.50, 0.67, 0.75,.85, 0.90, 0.95, 0.99, Inf), na.value="#8B007C") + 
        #scale_fill_distiller(palette = "RdPu") +
        theme_bw() + 
        ylab("Alpha") + 
        xlab("Log(lambda)") +
        geom_rect(aes(xmin = x.axis.profit + 0.50, xmax =  x.axis.profit + 1.50, ymin = y.axis.profit - 0.50, ymax = y.axis.profit + .50),
                  fill = "transparent", color = "black", size = 1.5) +
        theme(text =  element_text(size = 22),
              plot.title = element_text(size = 22),
              axis.text.x= element_text(size = 22),
              axis.text.y= element_text(size = 22),
              axis.title =  element_text(size = 22),
              legend.text =  element_text(size = 14))
      
      
      h
      
      ggsave(paste0(wkdir, fund.abbreviations[i], "_heatmap_profit.png"), width = 14.4/1.5, height = 7.24/1.5 )
      
      
      
      
      # Heatmap 
      h <- ggplot(heatmap.profit, aes(as.factor(log(run.lambda)), as.factor(alpha.value2), fill= Profit)) + 
        geom_tile() +
        scale_fill_distiller(palette = "RdPu") +
        theme_bw() + 
        ylab("Alpha") + 
        xlab("Log(lambda)") +
        geom_rect(aes(xmin = x.axis.profit + 0.50, xmax =  x.axis.profit + 1.50, ymin = y.axis.profit - 0.50, ymax = y.axis.profit + .50),
                  fill = "transparent", color = "black", size = 1.5) +
        theme(text =  element_text(size = 22),
              plot.title = element_text(size = 22),
              axis.text.x= element_text(size = 22),
              axis.text.y= element_text(size = 22),
              axis.title =  element_text(size = 22),
              legend.text =  element_text(size = 14))
      
      
      h
      
      ggsave(paste0(wkdir, fund.abbreviations[i], "_heatmap_profit_alt.png"), width = 14.4/1.5, height = 7.24/1.5 )
      
      
      
    
    
    
    
  }
  
  
   
  
    # MSE Table
    {
    
      
      # OLS MSE 
      rmse2 = fund.subset.predictions2.ols %>% 
        mutate(sq_sum = (predictions - net.cf.distribution.rescale)^2)
      
      mse2 = round(sum(rmse2$sq_sum) / nrow(fund.subset.predictions2.ols), 7)
      
      
        
      # Elastic Net MSE 
      rmse = fund.subset.predictions7 %>% 
        mutate(sq_sum = (predictions - net.cf.distribution.rescale)^2)
      
      mse = round(sum(rmse$sq_sum) / nrow(fund.subset.predictions7), 7)
      
      
      
      # Lambda alpha 
      alpha = round(best.params.7$alpha, 3)
      lambda = round(log(best.params.7$min_lambda), 2)
      blank1 = 0
      
      model = rbind("2 factor OLS", "15 factor Elastic Net")
      model.mse = rbind(mse2, mse)
      model.alpha = rbind(blank1, alpha)
      model.lambda= rbind(blank1, lambda)
      
      
      r2.table = cbind(model, model.mse, model.alpha, model.lambda) %>% as.data.frame()
      r2.table = r2.table %>% rename(Model = V1)
      
      myname2 = paste0(fund.abbreviations[i], "_", "mse")
      myname3 = paste0(fund.abbreviations[i], "_", "alpha")
      myname4 = paste0(fund.abbreviations[i], "_", "log.lambda")
      
      myname5 = paste0(fund.abbreviations[i], "_", "r2.table")
      
      
      r2.table = rename(r2.table, !!myname2 := V2)
      r2.table = rename(r2.table, !!myname3 := V3)
      r2.table = rename(r2.table, !!myname4 := V4)
      
      
      assign(myname5, r2.table)
      
      
    }
      
      
  
}





#####################################  Table Model Outcomes ##################################



# Put at end now
r2.table = left_join(pe_r2.table, vc_r2.table) %>%
  left_join(re_r2.table) %>% left_join(ff_r2.table)



tab.latex = r2.table %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
tab_spanner(
  label = "Buyout",
  columns = vars(pe_mse, pe_alpha, pe_log.lambda)
) %>%
  tab_spanner(
    label = "VC",
    columns = vars(vc_mse, vc_alpha, vc_log.lambda)
  ) %>%
  tab_spanner(
    label = "Real Estate",
    columns = vars(re_mse, re_alpha, re_log.lambda)
  ) %>%
  tab_spanner(
    label = "Fund of Funds",
    columns = vars(ff_mse, ff_alpha, ff_log.lambda)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    pe_mse = "MSE",
    vc_mse = "MSE",
    re_mse = "MSE",
    ff_mse = "MSE",
    pe_alpha = "Alpha",
    vc_alpha = "Alpha",
    re_alpha = "Alpha",
    ff_alpha = "Alpha",
    pe_log.lambda = "Log(Lambda)",
    vc_log.lambda = "Log(Lambda)",
    re_log.lambda = "Log(Lambda)",
    ff_log.lambda = "Log(Lambda)") 

tab.latex

tab.latex %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(wkdir,"latex_out_cv.tex"))




# Put at end now
r2.table.alt = left_join(rs_r2.table, df_r2.table)  %>%
  left_join(in_r2.table) %>% left_join(nr_r2.table)



tab.latex.alt = r2.table.alt %>%
  gt() %>%
  tab_header(
    title = "Model Comparison"
  ) %>% 
  tab_spanner(
    label = "Restructuring",
    columns = vars(rs_mse, rs_alpha, rs_log.lambda)
  ) %>%
  tab_spanner(
    label = "Debt Fund",
    columns = vars(df_mse, df_alpha, df_log.lambda)
  ) %>%
  tab_spanner(
    label = "Infrastructure",
    columns = vars(in_mse, in_alpha, in_log.lambda)
  ) %>%
  tab_spanner(
    label = "Natural Resources",
    columns = vars(nr_mse, nr_alpha, nr_log.lambda)
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Model)
  ) %>%
  cols_label(
    rs_mse = "MSE",
    df_mse = "MSE",
    in_mse = "MSE",
    nr_mse = "MSE",
    rs_alpha = "Alpha",
    df_alpha = "Alpha",
    in_alpha = "Alpha",
    nr_alpha = "Alpha",
    rs_log.lambda = "Log(Lambda)",
    df_log.lambda = "Log(Lambda)",
    in_log.lambda = "Log(Lambda)",
    nr_log.lambda = "Log(Lambda)") 

tab.latex.alt

tab.latex.alt %>%
  as_latex() %>%
  as.character() %>%
  writeLines(paste0(wkdir,"latex_out_alt_cv.tex"))





  
 

#####################################  Plot Coefficients -- Lambda ##################################



optimal.lambda = as.numeric(pe_r2.table[2,4])
pe_coefs.alt = cbind(pe_coefs, optimal.lambda)

optimal.lambda = as.numeric(vc_r2.table[2,4])
vc_coefs.alt = cbind(vc_coefs, optimal.lambda)

optimal.lambda = as.numeric(re_r2.table[2,4])
re_coefs.alt = cbind(re_coefs, optimal.lambda)

optimal.lambda = as.numeric(in_r2.table[2,4])
in_coefs.alt = cbind(in_coefs, optimal.lambda)

optimal.lambda = as.numeric(df_r2.table[2,4])
df_coefs.alt = cbind(df_coefs, optimal.lambda)

optimal.lambda = as.numeric(rs_r2.table[2,4])
rs_coefs.alt = cbind(rs_coefs, optimal.lambda)

optimal.lambda = as.numeric(ff_r2.table[2,4])
ff_coefs.alt = cbind(ff_coefs, optimal.lambda)

optimal.lambda = as.numeric(nr_r2.table[2,4])
nr_coefs.alt = cbind(nr_coefs, optimal.lambda)


pe_lambda.plot = pe_coefs.alt %>% mutate(sector =  "Buyout")
vc_lambda.plot = vc_coefs.alt %>% mutate(sector =  "Venture Capital")
re_lambda.plot = re_coefs.alt %>% mutate(sector =  "Real Estate")
in_lambda.plot = in_coefs.alt %>% mutate(sector =  "Infrastructure")
df_lambda.plot = df_coefs.alt %>% mutate(sector =  "Debt Fund")
rs_lambda.plot = rs_coefs.alt %>% mutate(sector =  "Restructuring")
ff_lambda.plot = ff_coefs.alt %>% mutate(sector =  "Fund of Funds")
nr_lambda.plot = nr_coefs.alt %>% mutate(sector =  "Natural Resources")


total.lambda = rbind(pe_lambda.plot, 
                     vc_lambda.plot,
                     re_lambda.plot,
                     in_lambda.plot,
                     df_lambda.plot,
                     rs_lambda.plot,
                     ff_lambda.plot,
                     nr_lambda.plot) %>%
  mutate(sector.f = as.factor(sector))


total.lambda$sector.f <- factor(total.lambda$sector.f,  
                                levels = c("Buyout", "Venture Capital", "Real Estate", "Fund of Funds", "Restructuring", "Debt Fund", "Infrastructure", 'Natural Resources'))




# Change line colors by groups
g <- ggplot(total.lambda, aes(x = log(lambda) , y = category.sum, group = factor(Type), color = factor(Type))) + 
  geom_line() + 
  geom_point(shape = 16, fill = "white", size = .2, stroke = 2) +
  facet_wrap( ~ sector.f , scales = "free", nrow = 4) + 
  theme_bw() + 
  xlab("Log(Lambda)") +
  #geom_text(x=log(best.params.7$min_lambda) + 2, y=0.03, label="Cross Validated Lambda", color = "gray") + 
  ylab("Sum of Coefficients in Category") +
  geom_vline(xintercept = optimal.lambda, size = 1, linetype = "dashed", color = "darkgray") + 
  theme(plot.title = element_text(size = 22),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22),
        axis.title =  element_text(size = 22),
        legend.text =  element_text(size = 14),
        legend.justification=c(1,0),
        legend.position="bottom",
        legend.title = element_blank())



g


ggsave(paste0(wkdir, "all_lambdas.png"), width = 11.9, height = 13)





#####################################  Plot Coefficients -- Alpha ##################################





pe_lambda.plot_alpha = pe_coefs_alpha %>% mutate(sector =  "Buyout")
vc_lambda.plot_alpha = vc_coefs_alpha %>% mutate(sector =  "Venture Capital")
re_lambda.plot_alpha = re_coefs_alpha %>% mutate(sector =  "Real Estate")
in_lambda.plot_alpha = in_coefs_alpha %>% mutate(sector =  "Infrastructure")
df_lambda.plot_alpha = df_coefs_alpha %>% mutate(sector =  "Debt Fund")
rs_lambda.plot_alpha = rs_coefs_alpha %>% mutate(sector =  "Restructuring")
ff_lambda.plot_alpha = ff_coefs_alpha %>% mutate(sector =  "Fund of Funds")
nr_lambda.plot_alpha = nr_coefs_alpha %>% mutate(sector =  "Natural Resources")


total.lambda_alpha = rbind(pe_lambda.plot_alpha, 
                     vc_lambda.plot_alpha,
                     re_lambda.plot_alpha,
                     in_lambda.plot_alpha,
                     df_lambda.plot_alpha,
                     rs_lambda.plot_alpha,
                     ff_lambda.plot_alpha,
                     nr_lambda.plot_alpha) %>%
  mutate(sector.f = as.factor(sector))


total.lambda_alpha$sector.f <- factor(total.lambda_alpha$sector.f,  
                                levels = c("Buyout", "Venture Capital", "Real Estate", "Fund of Funds", "Restructuring", "Debt Fund", "Infrastructure", 'Natural Resources'))




# Change line colors by groups
g <- ggplot(total.lambda_alpha, aes(x = alpha , y = category.sum, group = factor(Type), color = factor(Type))) + 
  geom_line() + 
  geom_point(shape = 16, fill = "white", size = .2, stroke = 2) +
  facet_wrap( ~ sector.f , scales = "free", nrow = 4) + 
  theme_bw() + 
  xlab("Alpha") +
  #geom_text(x=best.alpha + 0.1, y=0.03, label="Cross Validated Alpha", color = "gray") + 
  ylab("Sum of Coefficients in Category") +
  geom_vline(xintercept = best.alpha, size = 1, linetype = "dashed", color = "darkgray") + 
  theme(plot.title = element_text(size = 22),
        strip.background = element_blank(),
        axis.text.x= element_text(size = 22),
        strip.text.x = element_text(size = 14),
        axis.text.y= element_text(size = 22),
        axis.title =  element_text(size = 22),
        legend.text =  element_text(size = 14),
        legend.justification=c(1,0),
        legend.position="bottom",
        legend.title = element_blank())



g


ggsave(paste0(wkdir, "all_alphas.png"), width = 11.9, height = 13)





