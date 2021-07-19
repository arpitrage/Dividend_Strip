# Generate synthetic data

rm(list = ls())



library(synthpop)
library(tidyverse)

load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Data/YearlyCashFlowOct20.Rda")


myseed <- 20201128


# TVPI, cash on cash
fund.quarterly = fund.quarterly %>% group_by(Firm.ID, Fund.ID) %>% 
  mutate(tvpi = sum(net.cf.distribution.rescale)) %>% as.data.frame()

# Select variables
fund.quarterly.temp = fund.quarterly %>% 
  select(Fund.ID, Transaction.Quarter.Age, net.cf.distribution.rescale, net.cf.call.rescale,  Transaction.Quarter, Vintage.Quarter, fund.category, tvpi, irr.preqin, pme)

# Generate mapping to new ids 
values = fund.quarterly.temp %>%
  select(Fund.ID) %>% unique

total.funds = nrow(values)
new.ids <- sample(1:total.funds, total.funds, replace=FALSE)
id.link = cbind(values, new.ids) %>% as.data.frame()


# Synthetic Data Creation
synthetic.frame <- syn(fund.quarterly.temp, seed = myseed)

synthetic.pe = synthetic.frame$syn

synthetic.pe = left_join(synthetic.pe, id.link)

synthetic.pe = synthetic.pe %>%
  group_by(Fund.ID) %>% 
  mutate(n = row_number(),
         total = max(n)) %>% as.data.frame()


synthetic.pe.static = synthetic.pe %>% filter(n == 1) %>% select(new.ids, Vintage.Quarter, fund.category, irr.preqin, pme, tvpi)
synthetic.pe.dynamic= synthetic.pe %>% select(Transaction.Quarter.Age, net.cf.distribution.rescale, net.cf.call.rescale, new.ids)

synthetic.pe.full = left_join(synthetic.pe.static, synthetic.pe.dynamic)

synthetic.pe.full = synthetic.pe.full %>%
  mutate(Transaction.Quarter = Vintage.Quarter + Transaction.Quarter.Age*.25,
         Vintage = Vintage.Quarter) %>%
  filter(Transaction.Quarter.Age != 0)

fund.quarterly = synthetic.pe.full %>% select(-irr.preqin, -pme, -tvpi)


save(fund.quarterly, file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Synthetic Data/YearlyCashFlowOct20.Rda")




