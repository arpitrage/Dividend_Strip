rm(list = ls())

library(lubridate)
library(tidyverse)
library(stargazer)
library(statar)
library(broom)
library(Hmisc)
library(zoo)


# Functions including IRR 
{
  
  calcIRR <- function(df)
  {
    # instead iterate through all funds
    fundList <- df %>% dplyr::select(Fund.ID) %>% mutate(irrCalc = NA)  %>% unique 
    
    for(i in 1:nrow(fundList))
    {
      currentFund <- df %>% filter(Fund.ID %in% fundList[i,])
      # zooData <- zoo(temp$Transaction.Amount, temp$Transaction.Date)
      
      zooData <- zoo(currentFund$Transaction.Amount, currentFund$Transaction.Date)
      fundList[i,"irrCalc"] <- irr.z(zooData)
    }
    
    df <- left_join(df, fundList, by=c("Fund.ID"))
    return(df)
  }
  
  
  irr.z = function(cf.z, gips = FALSE) {
    irr.freq = 365
    if (!is.zoo(cf.z)) {
      warning("cash flow must be zoo object")
      return(NA)
    }
    if ("Date" != class(time(cf.z))) {
      warning("need Date class for zoo index")
      return(NA)
    }
    if (any(is.na(cf.z))) 
      return(NA)
    if (length(cf.z) <= 1) 
      return(NA)
    if (all(cf.z <= 0)) 
      return(NA)
    if (all(cf.z >= 0)) 
      return(NA)
    if (sum(cf.z) == 0) 
      return(0)
    # search for an interval, 1% wide, where the sign of the NPV is not equal at
    # ends of the interval
    if (sum(cf.z) < 0) {
      rangehi = 0
      rangelo = -0.01
      i = 0
      # low range on search for negative IRR is -100%
      while (i < 100 & (sign(npv.znoadjust(rangehi, cf.z)) == sign(npv.znoadjust(rangelo, 
                                                                                 cf.z)))) {
        rangehi = rangelo
        rangelo = rangelo - 0.01
        i = i + 1
      }
    } else {
      rangehi = 0.01
      rangelo = 0
      i = 0
      # while hi range on search for positive IRR is 100,000%
      while (i < 1e+05 & (sign(npv.znoadjust(rangehi, cf.z)) == sign(npv.znoadjust(rangelo, 
                                                                                   cf.z)))) {
        rangelo = rangehi
        rangehi = rangehi + 0.01
        i = i + 1
      }
    }
    npv1 = npv.znoadjust(rangelo, cf.z)
    npv2 = npv.znoadjust(rangehi, cf.z)
    if (sign(npv1) == sign(npv2)) 
      return(NA)
    cf.n = as.numeric(cf.z)
    # calculate with uniroot if cash flow starts negative and ends positive
    # otherwise do your own search
    if ((cf.n[1] < 0) & (cf.n[length(cf.n)] > 0)) {
      ans = uniroot(npv.znoadjust, c(rangelo, rangehi), cf = cf.z)
      apr = ans$root
    } else {
      int1 = rangelo
      int2 = rangehi
      for (i in 1:40) {
        inta = mean(c(int1, int2))
        npva = npv.znoadjust(inta, cf.z)
        if (sign(npva) == sign(npv1)) {
          int1 = inta
          npv1 = npva
        } else {
          int2 = inta
          npv2 = npva
        }
      }
      apr = mean(int1, int2)
    }
    # convert IRR to compounding at irr.freq interval
    ans = ((1 + (apr/irr.freq))^irr.freq) - 1
    # convert IRR to GIPS compliant if requested
    if (gips) {
      if (cf.z[1] == 0) 
        cf.z = cf.z[-1]
      dur = lastinvec(index(cf.z)) - index(cf.z)[1]
      if (dur < irr.freq) 
        ans = (1 + ans)^((as.numeric(dur))/irr.freq) - 1
    }
    return(ans)
  }
  
  npv.znoadjust = function(i, cf.z) {
    freq = 365
    if (!is.zoo(cf.z)) {
      warning("cash flow must be zoo object")
      return(NA)
    }
    if ("Date" != class(time(cf.z))) {
      warning("need Date class for zoo index")
      return(NA)
    }
    tdif = as.numeric(index(cf.z) - (index(cf.z)[1]))
    d = (1 + (i/freq))^tdif
    sum(cf.z/d)
  }
  
  
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
}



######################################### LOAD STATIC PREQIN ############################################################################################
preqin.static = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/Preqin_PEPerf_20170919143905AllAlt.csv", stringsAsFactors = FALSE, header=T, sep=",")    
preqin.static$irr.preqin = as.numeric(preqin.static$Net.IRR....)
preqin.static$tvpi.preqin = as.numeric(preqin.static$Net.Multiple..X.)
preqin.static$Fund.ID = as.numeric(preqin.static$Fund.ID)
preqin.static$Firm.ID = as.numeric(preqin.static$Firm.ID)
preqin.static = preqin.static %>% select(Fund.ID, Firm.ID, irr.preqin, tvpi.preqin)
save(preqin.static, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/PreqinStaticPerformanceAlt.Rda")




######################################### GPMEs LOAD  ############################################################################################


gpme.pe =     read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Buyout_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.df = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Debt Fund_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.ff = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Fund of Funds_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.in = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Infrastructure_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.nr = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Natural Resources_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.re = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Real Estate_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.rs = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Restructuring_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   
gpme.vc = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/GPME/Results/Estimations/FundLevel/GPMEFundLevel_wID_Q_Venture Capital_1981-2010_1981-2019.csv", stringsAsFactors = FALSE, header=T, sep=",")   

gpme.all = rbind(gpme.pe,
                 gpme.df,
                 gpme.ff,
                 gpme.in,
                 gpme.nr,
                 gpme.re,
                 gpme.rs,
                 gpme.vc)
save(gpme.all, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/PreqinGPME.Rda")


# Generates both quarterly and yearly cash flow files

######################################### LOAD PREQIN ############################################################################################
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DiscountRatesOct20.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/PreqinStaticPerformanceAlt.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/PreqinGPME.Rda")

df = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/Preqin_PE_CashFlow_202042800_20200428031312.csv", stringsAsFactors = FALSE, header=T, sep=",")    
return.data <- read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/MASTER_Preqin_Returns.csv", stringsAsFactors = FALSE, header=T, sep=",")   
return.data <- return.data %>% select(Firm.ID, Fund.ID, pme, irr, altpme)

# Format Data - key, date and amount
df <- df %>%
  mutate(Transaction.Date       = as.Date(Transaction.Date, format = "%m/%d/%y"),
         Transaction.Amount     = gsub(" - ", NA, Transaction.Amount),
         Transaction.Amount     = gsub(" ", "", Transaction.Amount),
         Transaction.Amount     = as.numeric(gsub(",", "", Transaction.Amount) ) )%>%
  filter(!is.na(Transaction.Amount) & !is.na(Transaction.Date))

df$year_stub  = substr(df$Transaction.Date, 1, 4)
df$month_stub = substr(df$Transaction.Date, 6, 7)
df$day_stub   = substr(df$Transaction.Date, 9, 11)

df$date.char = paste0(df$month_stub, "-", 1, "-", df$year_stub)
df$monthly.date <- mdy(df$date.char)

#df$date = format(as.Date(df$Transaction.Date), "%Y-%m")

# Remove NAs in Fund Size
df = df %>% mutate(Fund.Size = Fund.Size..mn.USD.)
df = df %>% mutate(Fund.Size = ifelse(is.na(Fund.Size), 0, Fund.Size))


# Create weighted cash flows based on fund size
df = df %>% mutate(weighted.cash = Transaction.Amount/10 * Fund.Size,
                       Transaction.Month = month(Transaction.Date),
                       Transaction.Year = year(Transaction.Date))


df = df %>% mutate(Quarter = ifelse(Transaction.Month %in% c(1, 2, 3), 0, 
                                        ifelse(Transaction.Month %in% c(4, 5, 6), .25,
                                               ifelse(Transaction.Month %in% c(7, 8, 9), .50, .75))))

df = df %>% mutate(Transaction.Quarter = Transaction.Year + Quarter) 


# Vintage Quarter Origination
df = df %>% group_by(Fund.ID) %>% mutate(Vintage.Quarter = min(Transaction.Quarter)) %>% as.data.frame()



# Here, separate out the NAV Amounts 
nav = df %>% filter(Transaction.Category  == "Value") %>%  
  group_by(Fund.ID) %>% group_by(Fund.ID, Transaction.Quarter) %>% 
  mutate(avg.value = mean(Transaction.Amount, na.rm = TRUE)/(10*10^6)) %>%
  select(Fund.ID, avg.value, Transaction.Quarter) %>% unique %>% as.data.frame()
  




pecf = df %>% as.data.frame() %>% filter(Transaction.Category  != "Value")
pecf <- pecf %>%
  select(Firm.ID, Fund.ID, Fund.Name, Firm.Name, Vintage, Vintage.Quarter, Fund.Size, Fund.Status, Transaction.Date, Transaction.Amount, Transaction.Quarter, Transaction.Year, Transaction.Category, Industries, Category.Type, weighted.cash) %>%
  rename(firm.name = Firm.Name, 
         fund.name = Fund.Name) %>%
  unique

####### Simple IRR on Transactions ###########

#temp = pecf %>% filter(Fund.ID == 3)

pecf.alt = pecf %>% mutate(Transaction.Amount = ifelse(Transaction.Amount < 0, 0, Transaction.Amount))
pecf.alt = pecf.alt %>% group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount - 10000000, Transaction.Amount)) %>%
  as.data.frame()


pecf = pecf %>% mutate(fund.category = ifelse((Category.Type == "Private Equity" | Category.Type == "Buyout"), "Buyout",
                                              ifelse((Category.Type == "Balanced" | Category.Type == "Direct Secondaries" | Category.Type == "Infrastructure Secondaries" | Category.Type == "Real Estate Secondaries" | 
                                                        Category.Type == "Secondaries" | Category.Type == "Real Estate Co-Investment" | Category.Type == "Co-investment" | Category.Type == "Co-Investment Multi-Manager" | 
                                                        Category.Type == "Fund of Funds" | Category.Type == "Infrastructure Fund of Funds" | Category.Type == "Private Debt Fund of Funds" | 
                                                        Category.Type == "Real Asset Fund of Funds"| Category.Type == "Real Estate Fund of Funds"),"Fund of Funds", 
                                                     ifelse((Category.Type == "Venture Debt" |  Category.Type == "Direct Lending"), "Debt Fund", 
                                                            ifelse((Category.Type == "Infrastructure"), "Infrastructure", 
                                                                   ifelse((Category.Type == "Real Estate"), "Real Estate",
                                                                          ifelse((Category.Type == "Natural Resources"), "Natural Resources",
                                                                          ifelse((Category.Type == "Special Situations" | Category.Type == "Turnaround" | Category.Type == "Distressed Debt" ), "Restructuring",
                                                                                 ifelse((Category.Type == "Early Stage" | Category.Type == "Mezzanine" | Category.Type == "Early Stage: Seed" | Category.Type == "Early Stage: Start-up" | Category.Type == "Growth" | Category.Type == "Venture (General)" | Category.Type == "Expansion / Late Stage"), "Venture Capital",NA)))))))))



pecf = left_join(pecf, return.data)
pecf = left_join(pecf, preqin.static)

pecf = left_join(pecf, gpme.all, by = c("Fund.ID", "Firm.ID"))

# fund-vintage year-year combination
# Adjusted here to use + or - amounts to determine call/distributions; not declared type
fund.quarterly = pecf %>% mutate(Call.Amount = ifelse(Transaction.Amount < 0, Transaction.Amount, 0),
                                 Distribution.Amount = ifelse(Transaction.Amount > 0, Transaction.Amount, 0))



fund.quarterly = fund.quarterly %>% group_by(Fund.ID, Transaction.Quarter) %>%
  mutate(net.cf.weighted = sum(weighted.cash), 
         net.cf.raw = sum(Transaction.Amount),
         net.cf.call = sum(Call.Amount),
         net.cf.distribution = sum(Distribution.Amount),
         net.cf.rescale = net.cf.raw / (10 * 10^6),
         net.cf.distribution.rescale = net.cf.distribution / (10 * 10^6),
         net.cf.call.rescale = net.cf.call / (10 * 10^6)) %>% as.data.frame() %>% 
  select(Transaction.Quarter, Transaction.Year, Firm.ID, Fund.ID, net.cf.weighted, net.cf.raw, net.cf.rescale, net.cf.distribution.rescale, net.cf.call.rescale, Vintage, Vintage.Quarter, Fund.Status, fund.category, Category.Type, Fund.Size, GPME, pme, irr, irr.preqin, tvpi.preqin) %>% unique %>% 
  arrange(Fund.ID, Transaction.Quarter) %>% mutate(year = Transaction.Year, Age = Transaction.Year - Vintage)




# Fill in Gaps as Zeroes -- Quarterly Series
fund.quarterly = fund.quarterly %>% group_by(Firm.ID, Fund.ID) %>% mutate(min.quarter = min(Transaction.Quarter)) %>% as.data.frame() %>% 
  arrange(Firm.ID, Fund.ID, Transaction.Quarter) %>% mutate(Transaction.Quarter.Age = (Transaction.Quarter - min.quarter) * 4)

fund.quarterly.static = fund.quarterly %>% select(Firm.ID, Fund.ID, Fund.Size, Fund.Status, min.quarter, Vintage, Vintage.Quarter, fund.category, Category.Type, GPME, pme, irr, irr.preqin)
fund.quarterly.static = fund.quarterly.static %>% group_by(Firm.ID, Fund.ID) %>% filter(row_number() == 1) %>% as.data.frame()


fund.quarterly.dynamic = fund.quarterly %>% select(Firm.ID, Fund.ID, net.cf.weighted, net.cf.raw, net.cf.rescale, net.cf.distribution.rescale, net.cf.call.rescale, year, Age, Transaction.Quarter, Transaction.Quarter.Age, Vintage.Quarter)
fund.quarterly.dynamic = fund.quarterly.dynamic %>% group_by(Firm.ID, Fund.ID) %>% fill_gap(Transaction.Quarter.Age) %>% as.data.frame()
fund.quarterly.dynamic = fund.quarterly.dynamic %>% arrange(Fund.ID, Transaction.Quarter.Age) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter)) %>% 
  mutate(Transaction.Quarter = ifelse(is.na(Transaction.Quarter), lag(Transaction.Quarter) + .25, Transaction.Quarter))

fund.quarterly.dynamic = fund.quarterly.dynamic %>% mutate(net.cf.weighted = ifelse(is.na(net.cf.weighted), 0, net.cf.weighted),
                                                           net.cf.raw = ifelse(is.na(net.cf.raw), 0, net.cf.raw),
                                                           net.cf.rescale = ifelse(is.na(net.cf.rescale), 0, net.cf.rescale),
                                                           net.cf.distribution.rescale = ifelse(is.na(net.cf.distribution.rescale), 0, net.cf.distribution.rescale),
                                                           net.cf.call.rescale = ifelse(is.na(net.cf.call.rescale), 0, net.cf.call.rescale))



fund.quarterly.dynamic = fund.quarterly.dynamic %>% mutate(Age.Quarter = Transaction.Quarter - Vintage.Quarter)


fund.quarterly.dynamic = left_join(fund.quarterly.dynamic, late.discount.rates.quarterly)

fund.quarterly.dynamic = fund.quarterly.dynamic %>%
  mutate(discount.rate = ifelse(is.na(discount.rate), 1, discount.rate))

#Cash flows after age 15
terminal.cash = fund.quarterly.dynamic %>% 
  mutate(old.cash.discounted = ifelse(Age.Quarter >=  15.25, net.cf.distribution.rescale * discount.rate, 0),
         old.cash.undiscounted = ifelse(Age.Quarter  >=  15.25, net.cf.distribution.rescale, 0)) %>% 
  group_by(Fund.ID) %>% 
  mutate(total.old.cash = sum(old.cash.discounted, na.rm = TRUE),
         total.old.cash.undiscounted = sum(old.cash.undiscounted, na.rm = TRUE)) %>% 
  as.data.frame() %>% filter(Age.Quarter == 15.25)


terminal.cash.1 = terminal.cash %>% mutate(Age.Quarter = Age.Quarter  ,
                                           Transaction.Quarter = Transaction.Quarter  ,
                                           net.cf.distribution.rescale = total.old.cash / 4) %>%
  select(-old.cash.discounted, -old.cash.undiscounted, -total.old.cash, -total.old.cash.undiscounted)

terminal.cash.2 = terminal.cash %>% mutate(Age.Quarter = Age.Quarter + 1 * .25,
                                           Transaction.Quarter = Transaction.Quarter + 1 * .25,
                                           net.cf.distribution.rescale = total.old.cash / 4) %>%
  select(-old.cash.discounted, -old.cash.undiscounted, -total.old.cash, -total.old.cash.undiscounted)


terminal.cash.3 = terminal.cash %>% mutate(Age.Quarter = Age.Quarter + 2 * .25,
                                           Transaction.Quarter = Transaction.Quarter + 2 * .25,
                                           net.cf.distribution.rescale = total.old.cash / 4) %>%
  select(-old.cash.discounted, -old.cash.undiscounted, -total.old.cash, -total.old.cash.undiscounted)


terminal.cash.4 = terminal.cash %>% mutate(Age.Quarter = Age.Quarter + 3 * .25,
                                           Transaction.Quarter = Transaction.Quarter + 3 * .25,
                                           net.cf.distribution.rescale = total.old.cash / 4) %>%
  select(-old.cash.discounted, -old.cash.undiscounted, -total.old.cash, -total.old.cash.undiscounted)


fund.quarterly.dynamic.trim = fund.quarterly.dynamic %>% 
  filter(Age.Quarter <= 15) %>% 
  rbind(terminal.cash.1) %>%
  rbind(terminal.cash.2) %>%
  rbind(terminal.cash.3) %>%
  rbind(terminal.cash.4)



# Max
skeleton <- NULL
for(t in seq(1980,2019.75, 0.25)) {
  # number of years to go up to 
  i = min(2020.25 - t, 16) * 4
  
  temp <- data.frame("Vintage.Quarter" = rep(t, i), "Transaction.Quarter.Age" = seq(1:i), 
                     "skeleton" = rep(1, i))
  
  temp = temp %>% mutate(Transaction.Quarter.Age = Transaction.Quarter.Age)
  temp = temp %>% mutate(Transaction.Quarter = Vintage.Quarter + (Transaction.Quarter.Age * 0.25))
  
  skeleton = rbind(skeleton, temp)
}


# instead iterate through all funds
fund.quarterly.dynamic.trim <- fund.quarterly.dynamic.trim %>% 
  mutate(original.data = 1) %>% select(-Transaction.Quarter.Age)
fundList <- fund.quarterly.dynamic.trim %>% dplyr::select(Fund.ID) %>% unique 

# Issue here -- move the calls at time = 0 to time = 1 otherwise they get dropped
fund.quarterly.dynamic.trim = fund.quarterly.dynamic.trim %>%
  group_by(Fund.ID) %>%
  arrange(Transaction.Quarter) %>%
  mutate(row.number = row_number()) %>% as.data.frame() %>%
  mutate(time_zero = Age.Quarter == 0,
         time_one = row.number == 2,
         inception.cash = ifelse(time_zero == 1, net.cf.call.rescale, 0)) %>%
  group_by(Fund.ID) %>%
  mutate(ever.inception.cash = min(inception.cash, na.rm = TRUE)) %>% as.data.frame() %>%
  mutate(net.cf.call.rescale = ifelse(time_zero == 1, net.cf.call.rescale - ever.inception.cash,
                                      ifelse(time_one == 1, net.cf.call.rescale + ever.inception.cash,
                                             net.cf.call.rescale))) %>%
  select(-time_zero, -time_one, -inception.cash, -ever.inception.cash)




dynamicFunds <- NULL
for(j in 1:nrow(fundList))
{
  currentFund <- fund.quarterly.dynamic.trim %>% filter(Fund.ID %in% fundList[j,])
  CurrentVintage = currentFund %>% select(Vintage.Quarter) %>% unique 
  infillFundSkeleton = skeleton %>% filter(Vintage.Quarter == CurrentVintage[1,1])
  
  infilledFund = left_join(infillFundSkeleton, currentFund, by = c("Vintage.Quarter", "Transaction.Quarter"))
  infilledFund = infilledFund %>% 
    mutate(net.cf.distribution.rescale = ifelse(is.na(net.cf.distribution.rescale), 0, net.cf.distribution.rescale),
           net.cf.call.rescale = ifelse(is.na(net.cf.call.rescale), 0, net.cf.call.rescale),
           net.cf.rescale = ifelse(is.na(net.cf.rescale)  != 1, 0, net.cf.rescale))
  
  infilledFund = infilledFund %>%
    mutate(Fund.ID = max(Fund.ID, na.rm = TRUE),
           Firm.ID = max(Firm.ID, na.rm = TRUE))
  
  dynamicFunds <- rbind(dynamicFunds, infilledFund)
}


# tempy = anti_join(fund.quarterly.dynamic.trim, dynamicFunds)
dynamicFunds = dynamicFunds %>% select(-Vintage.Quarter)


fund.quarterly.initial  = left_join(dynamicFunds, fund.quarterly.static, by = c("Firm.ID", "Fund.ID")) %>% unique

fund.quarterly.initial = fund.quarterly.initial %>% mutate(Age.Quarter = ifelse(is.na(Age.Quarter), Transaction.Quarter - Vintage.Quarter, Age.Quarter))




# Fix Ages
fund.quarterly.initial = fund.quarterly.initial  %>% mutate(year = ifelse(is.na(year), floor(Transaction.Quarter), year),
                                                            Age  = ifelse(is.na(Age), year - Vintage, Age)) 




fund.quarterly.initial = fund.quarterly.initial %>% mutate(net.cf.weighted = ifelse(is.infinite(net.cf.weighted), 0, net.cf.weighted),
                                                           net.cf.raw = ifelse(is.infinite(net.cf.weighted), 0, net.cf.raw),
                                                           net.cf.rescale = ifelse(is.infinite(net.cf.weighted), 0, net.cf.rescale),
                                                           net.cf.call.rescale = ifelse(is.infinite(net.cf.call.rescale), 0, net.cf.call.rescale),
                                                           net.cf.distribution.rescale = ifelse(is.infinite(net.cf.distribution.rescale), 0, net.cf.distribution.rescale))  



# Here account for the initial vintage quarter
fund.quarterly.initial = fund.quarterly.initial %>%
  mutate(earliest.vintage.cash = ifelse(Age.Quarter == 0, net.cf.distribution.rescale, 0)) %>%
  group_by(Fund.ID) %>%
  mutate(ever.earliest.age.cash = max(earliest.vintage.cash)) %>% as.data.frame() %>%
  mutate(net.cf.distribution.rescale = ifelse(Age.Quarter == 1, net.cf.distribution.rescale + ever.earliest.age.cash, net.cf.distribution.rescale),
         net.cf.distribution.rescale = ifelse(Age.Quarter == 0, 0, net.cf.distribution.rescale))


fund.quarterly.initial = fund.quarterly.initial %>% filter(Age.Quarter > 0)


# Final NAV
fund.quarterly.initial = left_join(fund.quarterly.initial, nav, by = c("Fund.ID", "Transaction.Quarter"))

fund.quarterly = fund.quarterly.initial





save(fund.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/YearlyCashFlowOct20.Rda")

# load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/YearlyCashFlowOct20.Rda")



######################################### GENERATE IRRs ############################################################################################








# Generates both quarterly and yearly cash flow files

######################################### LOAD PREQIN ############################################################################################
df = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/Preqin_PE_CashFlow_202042800_20200428031312.csv", stringsAsFactors = FALSE, header=T, sep=",")    
return.data <- read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/MASTER_Preqin_Returns.csv", stringsAsFactors = FALSE, header=T, sep=",")   
return.data <- return.data %>% select(Firm.ID, Fund.ID, pme, irr, altpme)

# Format Data - key, date and amount
df <- df %>%
  mutate(Transaction.Date       = as.Date(Transaction.Date, format = "%m/%d/%y"),
         Transaction.Amount     = gsub(" - ", NA, Transaction.Amount),
         Transaction.Amount     = gsub(" ", "", Transaction.Amount),
         Transaction.Amount     = as.numeric(gsub(",", "", Transaction.Amount) ) )%>%
  filter(!is.na(Transaction.Amount) & !is.na(Transaction.Date))

df$year_stub  = substr(df$Transaction.Date, 1, 4)
df$month_stub = substr(df$Transaction.Date, 6, 7)
df$day_stub   = substr(df$Transaction.Date, 9, 11)

df$date.char = paste0(df$month_stub, "-", 1, "-", df$year_stub)
df$monthly.date <- mdy(df$date.char)

#df$date = format(as.Date(df$Transaction.Date), "%Y-%m")

# Remove NAs in Fund Size
df = df %>% mutate(Fund.Size = Fund.Size..mn.USD.)
df = df %>% mutate(Fund.Size = ifelse(is.na(Fund.Size), 0, Fund.Size))


# Create weighted cash flows based on fund size
df = df %>% mutate(weighted.cash = Transaction.Amount/10 * Fund.Size,
                   Transaction.Month = month(Transaction.Date),
                   Transaction.Year = year(Transaction.Date))


df = df %>% mutate(Quarter = ifelse(Transaction.Month %in% c(1, 2, 3), 0, 
                                    ifelse(Transaction.Month %in% c(4, 5, 6), .25,
                                           ifelse(Transaction.Month %in% c(7, 8, 9), .50, .75))))

df = df %>% mutate(Transaction.Quarter = Transaction.Year + Quarter) 


# Vintage Quarter Origination
df = df %>% group_by(Fund.ID) %>% mutate(Vintage.Quarter = min(Transaction.Quarter)) %>% as.data.frame()



# Here, separate out the NAV Amounts 
nav = df %>% filter(Transaction.Category  == "Value") %>%  
  group_by(Fund.ID) %>% group_by(Fund.ID, Transaction.Quarter) %>% 
  mutate(avg.value = mean(Transaction.Amount, na.rm = TRUE)/(10*10^6)) %>%
  select(Fund.ID, avg.value, Transaction.Quarter) %>% unique %>% as.data.frame()





pecf = df %>% as.data.frame() %>% filter(Transaction.Category  != "Value")
pecf <- pecf %>%
  select(Firm.ID, Fund.ID, Fund.Name, Firm.Name, Vintage, Vintage.Quarter, Fund.Size, Fund.Status, Transaction.Date, Transaction.Amount, Transaction.Quarter, Transaction.Year, Transaction.Category, Industries, Category.Type, weighted.cash) %>%
  rename(firm.name = Firm.Name, 
         fund.name = Fund.Name) %>%
  unique


# Adjusted here to use + or - amounts to determine call/distributions; not declared type
pecf = pecf %>% mutate(Call.Amount = ifelse(Transaction.Amount < 0, Transaction.Amount, 0),
                       Distribution.Amount = ifelse(Transaction.Amount > 0, Transaction.Amount, 0))



pecf = pecf  %>%
  mutate(net.cf.rescale = Transaction.Amount / (10 * 10^6),
         net.cf.distribution.rescale = Distribution.Amount / (10 * 10^6),
         net.cf.call.rescale = Call.Amount / (10 * 10^6)) %>% as.data.frame()


####### Generate IRRS ###########


load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/DividendStripOct20.Rda")

# Keep only first $1 capital called
bond.price = price.strip.quarterly.feb %>% select(Transaction.Quarter, Vintage.Quarter, price.bond.feb)

# Year 16 discount factor 
bond.price = bond.price %>%
  group_by(Vintage.Quarter) %>%
  mutate(is.last.bond.discount = ifelse(Transaction.Quarter == max(Transaction.Quarter), price.bond.feb, 0),
         last.bond.discount = max(is.last.bond.discount)) %>%
  as.data.frame()


# Add in bond prices
pecf = left_join(pecf, bond.price)
pecf = pecf %>%
  group_by(Fund.ID) %>%
  mutate(last.bond.discount.infill = max(last.bond.discount, na.rm = TRUE),
         price.bond.feb = ifelse(is.na(price.bond.feb), 1, price.bond.feb)) %>%
  as.data.frame()

pecf = pecf %>%
  mutate(last.bond.discount.infill = ifelse(is.infinite(last.bond.discount.infill), 1, last.bond.discount.infill))

# Discounted calls
pecf = pecf %>% group_by(Fund.ID) %>%
  mutate(discounted.call = net.cf.call.rescale * price.bond.feb,
         all.discounted.call = sum(discounted.call),
         all.call = sum(net.cf.call.rescale)) %>% as.data.frame()

# Measure truncated calls
pecf = pecf %>%
  group_by(Fund.ID) %>%
  arrange(Transaction.Date) %>%
  mutate(cumulative.calls = cumsum(net.cf.call.rescale)) %>%
  as.data.frame() %>%
  mutate(incremental.call = cumulative.calls - net.cf.call.rescale,
         excess.calls = ifelse(cumulative.calls < -1, 1, 0),
         diff.from.limit = ifelse(net.cf.call.rescale < 0, -1 - incremental.call, 0),
         net.cf.call.rescale.truncated = ifelse(excess.calls == 0, net.cf.call.rescale, pmin(diff.from.limit, 0) ),
         discounted.truncated.call = net.cf.call.rescale.truncated * price.bond.feb)

pecf = pecf %>%
  group_by(Fund.ID) %>%
  mutate(all.truncated.call = sum(net.cf.call.rescale.truncated),
         all.truncated.discounted.call = sum(discounted.truncated.call)) %>%
  as.data.frame()


# Measure that last bit of call added to end
pecf = pecf %>%
  mutate(residual.call = 1 + all.truncated.call,     # amount different from 1 that gets called
         residual.call.discounted = residual.call * last.bond.discount.infill, # discount that 
         actual.plus.residual.call = -1 + residual.call.discounted)



# pecf.alt: version with only $1 capital calls; pecf.1.call
#pecf.alt = pecf %>% mutate(Transaction.Amount = ifelse(Transaction.Amount < 0, 0, Transaction.Amount))
#pecf.alt = pecf.alt %>% group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
#  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount - 10000000, Transaction.Amount)) %>%
#  as.data.frame()

pecf.1.call = pecf %>% mutate(Transaction.Amount = net.cf.distribution.rescale) %>%
  group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount - 1, Transaction.Amount)) %>%
  as.data.frame()





# IRR 1 -- using actual cash flows
# Modify this to be up to $1 of calls
# Call Components



# (1) -- USING ACTUAL CASH FLOWS
pecf = pecf %>%
  mutate(Transaction.Amount = ifelse(Call.Amount < 0, net.cf.call.rescale.truncated, net.cf.distribution.rescale))

irr = calcIRR(pecf)
irr = irr %>% mutate(irr.actual.truncated.cash = irrCalc) %>% select(Fund.ID, irr.actual.truncated.cash) %>% unique


# (2) -- USING NPV CASH FLOWS
pecf.npv = pecf %>% mutate(Transaction.Amount = net.cf.distribution.rescale) %>%
  group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount + all.truncated.discounted.call, Transaction.Amount)) %>%
  as.data.frame()

irr.npv = calcIRR(pecf.npv)
irr.npv = irr.npv %>% mutate(irr.npv = irrCalc) %>% select(Fund.ID, irr.npv, all.truncated.discounted.call) %>% unique


# (3) -- Using actual sum of calls at time 0 
pecf.actual = pecf %>% mutate(Transaction.Amount = net.cf.distribution.rescale) %>%
  group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount + all.truncated.call, Transaction.Amount)) %>%
  as.data.frame()

irr.actual = calcIRR(pecf.actual)
irr.actual = irr.actual %>% mutate(irr.actual = irrCalc) %>% select(Fund.ID, irr.actual, all.truncated.call) %>% unique



#  (4) - IRR with actual call + npv of future pulled back
pecf.residual = pecf %>% mutate(Transaction.Amount = net.cf.distribution.rescale) %>%
  group_by(Fund.ID) %>% arrange(Transaction.Date) %>%
  mutate(Transaction.Amount = ifelse(row_number() == 1, Transaction.Amount + actual.plus.residual.call, Transaction.Amount)) %>%
  as.data.frame()

irr.residual = calcIRR(pecf.residual)
irr.residual = irr.residual %>% mutate(irr.residual = irrCalc) %>% select(Fund.ID, irr.residual, actual.plus.residual.call) %>% unique


irr.all = left_join(irr, irr.npv) %>%
  left_join(irr.actual) %>%
  left_join(irr.residual)

irr.all = irr.all %>%
  mutate(all.truncated.discounted.call.profit = all.truncated.discounted.call,
         all.truncated.call.profit = all.truncated.call, 
         actual.plus.residual.call.profit = actual.plus.residual.call) %>%
  select(-all.truncated.discounted.call, -all.truncated.call, -actual.plus.residual.call)

save(irr.all, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/IRRJuly20.Rda")







