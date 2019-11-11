rm(list = ls())

# install.packages("lme4")

library(lubridate)
library(lfe)
library(lme4)
library(R.matlab)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(statar)
library(tidyverse)
library(stargazer)
library(broom)
library(Hmisc)
library(hrbrthemes)
library(zoo)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
######################################### LOAD STATIC PREQIN ############################################################################################
preqin.static = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/Preqin/Preqin_PEPerf_20170919143905AllAlt.csv", stringsAsFactors = FALSE, header=T, sep=",")    
preqin.static$irr.preqin = as.numeric(preqin.static$Net.IRR....)
preqin.static$tvpi.preqin = as.numeric(preqin.static$Net.Multiple..X.)
preqin.static$Fund.ID = as.numeric(preqin.static$Fund.ID)
preqin.static$Firm.ID = as.numeric(preqin.static$Firm.ID)
preqin.static = preqin.static %>% select(Fund.ID, Firm.ID, irr.preqin, tvpi.preqin)
save(preqin.static, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/PreqinStaticPerformanceAlt.Rda")





# Generates both quarterly and yearly cash flow files

######################################### LOAD PREQIN ############################################################################################
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/DiscountRatesJune19.Rda")
load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/PreqinStaticPerformance.Rda")

df = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/Preqin/Preqin_PE_CashFlow_201862800_20180628154352.csv", stringsAsFactors = FALSE, header=T, sep=",")    
return.data <- read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/Preqin/MASTER_Preqin_Returns.csv", stringsAsFactors = FALSE, header=T, sep=",")   
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



## Calculate IRR
#pecf     <- calcIRR(pecf)
#
#
## Calculate PME
#pecf.pme = pecf %>% mutate(cash.weight = Transaction.Amount/ (10 * 10^6)) %>% group_by(Transaction.Date) %>%
#  mutate(total.cash.weight = sum(cash.weight, na.rm = TRUE)) %>% select(Fund.ID, Firm.ID, Transaction.Date, Vintage, total.cash.weight) %>% as.data.frame() %>% unique
#
#
#
#gspc = read.csv("/Users/agupta011/Dropbox/Research/Infrastructure/Data/Additional Benchmarks/GSPC.csv")
#gspc <- gspc %>% select(Date, Close) # %>% mutate(Date=as.yearmon(Date) ) %>% rename(month=Date) %>% arrange(-month)
#gspc <- gspc %>% mutate(Date = as.Date(Date))
#
#
#gspc <- gspc %>% rename(Transaction.Date = Date)
#
#df <- left_join(pecf.pme, gspc, by="Transaction.Date")
#
#df.unmatched1 <- df %>% filter(is.na(Close)) %>% mutate(Transaction.Date = Transaction.Date + 1) %>% select(-Close)
#df.m1 <- left_join(df.unmatched1, gspc, by="Transaction.Date")
#
#df.unmatched2 <- df.m1 %>% filter(is.na(Close)) %>% mutate(Transaction.Date = Transaction.Date + 1) %>% select(-Close)
#df.m2 <- left_join(df.unmatched2, gspc, by="Transaction.Date")
#
#df.unmatched3 <- df.m2 %>% filter(is.na(Close)) %>% mutate(Transaction.Date = Transaction.Date + 1) %>% select(-Close)
#df.m3 <- left_join(df.unmatched3, gspc, by="Transaction.Date")
#
#df.unmatched4 <- df.m3 %>% filter(is.na(Close)) %>% mutate(Transaction.Date = Transaction.Date + 1) %>% select(-Close)
#df.m4 <- left_join(df.unmatched4, gspc, by="Transaction.Date")
#
#df.unmatched5 <- df.m4 %>% filter(is.na(Close)) %>% mutate(Transaction.Date = Transaction.Date + 1) %>% select(-Close)
#df.m5 <- left_join(df.unmatched5, gspc, by="Transaction.Date")
#
#
#df <- do.call("rbind", list(df, df.m1, df.m2, df.m3, df.m4, df.m5) ) %>% filter(!is.na(Close))
#
#df <- df %>% 
#  mutate(Transaction.Date.q = as.numeric( as.yearqtr(as.yearmon(Transaction.Date, format = "%d-%b-%y") ) ) ) 
#
#
#df.pme <- df %>% 
#  arrange(Firm.ID, Fund.ID, Transaction.Date) %>%
#  mutate(call = ifelse(total.cash.weight <  0, total.cash.weight, 0) ) %>%
#  mutate(dist = ifelse(total.cash.weight >= 0, total.cash.weight, 0) ) %>%
#  group_by(Firm.ID, Fund.ID) %>% mutate(closeDivLastClose = last(Close)/Close) %>% as.data.frame %>%
#  mutate(dCall = -call * closeDivLastClose, dDist = dist * closeDivLastClose ) %>%
#  group_by(Firm.ID, Fund.ID) %>% mutate(cumCall = cumsum(dCall), cumDist = cumsum(dDist) ) %>% filter(Transaction.Date == max(Transaction.Date)) %>% as.data.frame %>%
#  mutate(pme = cumDist/cumCall ) %>% select(Vintage, Firm.ID, Fund.ID, pme) %>%
#  filter(!is.infinite(pme) & pme < 10 & pme != 0)


pecf = left_join(pecf, return.data)
pecf = left_join(pecf, preqin.static)



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
  select(Transaction.Quarter, Transaction.Year, Firm.ID, Fund.ID, net.cf.weighted, net.cf.raw, net.cf.rescale, net.cf.distribution.rescale, net.cf.call.rescale, Vintage, Vintage.Quarter, Fund.Status, fund.category, Category.Type, Fund.Size, pme, irr, irr.preqin, tvpi.preqin) %>% unique %>% 
  arrange(Fund.ID, Transaction.Quarter) %>% mutate(year = Transaction.Year, Age = Transaction.Year - Vintage)




# Fill in Gaps as Zeroes -- Quarterly Series
fund.quarterly = fund.quarterly %>% group_by(Firm.ID, Fund.ID) %>% mutate(min.quarter = min(Transaction.Quarter)) %>% as.data.frame() %>% 
  arrange(Firm.ID, Fund.ID, Transaction.Quarter) %>% mutate(Transaction.Quarter.Age = (Transaction.Quarter - min.quarter) * 4)

fund.quarterly.static = fund.quarterly %>% select(Firm.ID, Fund.ID, Fund.Size, Fund.Status, min.quarter, Vintage, Vintage.Quarter, fund.category, Category.Type, pme, irr, irr.preqin)
fund.quarterly.static = fund.quarterly.static %>% group_by(Firm.ID, Fund.ID) %>% filter(row_number() == 1) %>% as.data.frame()


fund.quarterly.dynamic = fund.quarterly %>% select(Firm.ID, Fund.ID, net.cf.weighted, net.cf.raw, net.cf.rescale, net.cf.distribution.rescale, net.cf.call.rescale, year, Age, Transaction.Quarter, Transaction.Quarter.Age)
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

fund.quarterly.initial  = left_join(fund.quarterly.dynamic, fund.quarterly.static, by = c("Firm.ID", "Fund.ID")) %>% unique





# Fix Ages
fund.quarterly.initial = fund.quarterly.initial  %>% mutate(year = ifelse(is.na(year), floor(Transaction.Quarter), year),
                       Age  = ifelse(is.na(Age), year - Vintage, Age), 
                       Age.Quarter = Transaction.Quarter - Vintage.Quarter)




fund.quarterly.initial = fund.quarterly.initial %>% mutate(net.cf.weighted = ifelse(is.infinite(net.cf.weighted), 0, net.cf.weighted),
                                             net.cf.raw = ifelse(is.infinite(net.cf.weighted), 0, net.cf.raw),
                                             net.cf.rescale = ifelse(is.infinite(net.cf.weighted), 0, net.cf.rescale),
                                             net.cf.call.rescale = ifelse(is.infinite(net.cf.call.rescale), 0, net.cf.call.rescale),
                                             net.cf.distribution.rescale = ifelse(is.infinite(net.cf.distribution.rescale), 0, net.cf.distribution.rescale))  




fund.quarterly.initial = fund.quarterly.initial %>% filter(Age.Quarter >= 0)


# Final NAV
fund.quarterly.initial = left_join(fund.quarterly.initial, nav, by = c("Fund.ID", "Transaction.Quarter"))





fund.quarterly.initial = left_join(fund.quarterly.initial, late.discount.rates.quarterly)



#Cash flows after age 15
terminal.cash = fund.quarterly.initial %>% 
 mutate(old.cash.discounted = ifelse(Age.Quarter >=  15, net.cf.distribution.rescale * discount.rate, 0),
        old.cash.undiscounted = ifelse(Age.Quarter  >=  15, net.cf.distribution.rescale, 0)) %>% 
 group_by(Fund.ID) %>% 
 mutate(total.old.cash = sum(old.cash.discounted, na.rm = TRUE),
        total.old.cash.undiscounted = sum(old.cash.undiscounted, na.rm = TRUE)) %>% 
 as.data.frame() %>% filter(Age.Quarter == 15)


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


fund.quarterly = fund.quarterly.initial %>% 
  filter(Age.Quarter <= 14.75) %>% 
  rbind(terminal.cash.1) %>%
  rbind(terminal.cash.2) %>%
  rbind(terminal.cash.3) %>%
  rbind(terminal.cash.4)
  
  



save(fund.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/Data/Clean/YearlyCashFlowOct19.Rda")

#  temp = fund.quarterly %>% filter(Firm.ID == 152327 & Fund.ID == 3) %>% select(year, Age, Transaction.Quarter, Vintage.Quarter, Age.Quarter, discount.rate, forward.rate, net.cf.distribution.rescale)

#  temp2 = fund.quarterly %>% filter(Firm.ID == 152327 & Fund.ID == 3) %>% select(year, Age, Transaction.Quarter, Vintage.Quarter, Age.Quarter, discount.rate, forward.rate, net.cf.distribution.rescale)



