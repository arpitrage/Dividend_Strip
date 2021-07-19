rm(list = ls())


library(gt)

library(lubridate)
library(xtable)

library(tidyverse)
library(stargazer)
library(broom)
library(Hmisc)
library(hrbrthemes)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


######################################### ANALYSIS ############################################################################################

load(file = "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Synthetic Data/MergedCashFlowOct20.Rda")

figures <- "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Synthetic Results/"
tables <- "/Users/agupta011/Dropbox/Research/Infrastructure/JFfinal/Synthetic Results/"

##################################### Table Summary Statistics ##################################### 
  # Table 1: Fund Statistics by Category
  # Panel A: Total Number
fund.quarterly.div = fund.quarterly.div %>%
  mutate(year = floor(Transaction.Quarter),
         VintageYear = floor(Vintage))

  fund.quarterly.div = fund.quarterly.div %>% filter(Vintage >= 1981, Vintage <= 2017)
  summary.profile = fund.quarterly.div %>% select(Fund.ID, fund.category, Vintage) %>% unique 
  nrow(summary.profile)
  summaryPD = fund.quarterly.div %>% select(pd_category.4.quarterly, Vintage) %>% group_by(Vintage)  %>% 
    mutate(avg.pd = round(mean(pd_category.4.quarterly, na.rm = TRUE), 2)) %>% as.data.frame() %>% select(avg.pd, Vintage) %>% unique %>% arrange(Vintage) %>% filter(!is.na(Vintage)) 
  
  table.1.A = summary.profile %>%
    group_by(Vintage, fund.category) %>%
    tally() %>% spread(fund.category, n) 
  
  table.1.A[is.na(table.1.A)] <- 0
  
  table.1.A = table.1.A %>% as.data.frame() %>% mutate(Total = rowSums(.) - Vintage)
  
  
  table.1.A = left_join(table.1.A, summaryPD)
  table.1.A[is.na(table.1.A)] <- 0
  table.1.A = table.1.A %>% mutate("PD Ratio" = avg.pd) %>% select(-avg.pd)
  
  table.1.A = table.1.A %>%
    select(Vintage, Buyout, `Venture Capital`, `Real Estate`, Infrastructure, Restructuring, `Fund of Funds`, `Debt Fund`, `Natural Resources`, Total, `PD Ratio`)
  
  
  #stargazer(table.1.A, summary = FALSE, 
  #          type="latex", digits = 1,
  #          out =  paste0(tables, "Table1_PanelA.tex"))


  table.1.A.xtable <- xtable(table.1.A)
  
  
  digits(table.1.A.xtable) <- 0
  print.xtable(table.1.A.xtable, digits = 0, include.rownames=FALSE, floating = FALSE, file =  paste0(tables, "Table1_PanelA.tex"))
  
  

 
  
  
##################################### Cash Flow Profiles ##################################### 
  
# Table 2: Cash Flow Profiles

# Cash flows after age 15
fund.quarterly.div.plot = fund.quarterly.div %>% 
    group_by(fund.category, Fund.ID, Age.Year) %>%
    mutate(yearly.cash = sum(net.cf.distribution.rescale)) %>%
    as.data.frame() %>%
    select(fund.category, fund.category.f, Age.Year, yearly.cash) %>%
    group_by(fund.category, Age.Year) %>%
    mutate(plot.cash = mean(yearly.cash)) %>% as.data.frame() %>% unique %>%
    mutate(old = ifelse(Age.Year == 16, "Future Cash Flow", "Actual Cash Flow"),
           old.factor = factor(old, levels = c("Future Cash Flow", "Actual Cash Flow"))) %>%
    filter(!is.na(fund.category))
  
  
  

 
g <- ggplot(transform(fund.quarterly.div.plot, fund.category.f = factor(fund.category.f, levels = c("Buyout", "Venture Capital", "Real Estate", "Infrastructure", "Restructuring", "Fund of Funds", "Debt Fund", "Natural Resources"))), aes(x = Age.Year, y = plot.cash, fill = as.factor(old.factor))) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  theme_bw() + scale_fill_brewer(palette = "Dark2") + 
  xlab("Cash Flows by Age") +
  ylab("Distribution Amount Relative to $1 Commitment") + 
  scale_x_continuous(limits=c(1, 16.5), breaks=seq(0,15,5)) +
  theme(plot.title = element_text(size = 16),
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 14),
        legend.justification=c(1,0), 
        legend.position="bottom",
        legend.title = element_blank()) 
  

g + facet_wrap( ~ fund.category.f, ncol = 4)

  ggsave(paste0(figures, "DistributionCategory.eps"), width = 14.4/1.5, height = 7.24/1.5 )




  


##################################### Cash Flow By Each Category ##################################### 


 

fund.subset <- fund.quarterly.div %>% filter(fund.category == "Buyout" )


# Plot Cash Flows
g <- ggplot(data = fund.subset %>% filter(Vintage >= 1990 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  #geom_rect(aes(xmin=2000, xmax=2005, ymin=0, ymax=.2), fill='pink', alpha=0.2) +
  #xlab("Cash Flows by Age") +
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(1990, 2017), breaks=seq(1990,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g



ggsave(paste0(figures, "pe", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )


fund.subset <- fund.quarterly.div %>% filter( fund.category == "Venture Capital")

g <- ggplot(data = fund.subset %>% filter(Vintage >= 1990 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(1990, 2017), breaks=seq(1990,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "vc", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )



fund.subset <- fund.quarterly.div %>% filter(  fund.category == "Real Estate"  )

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(2000, 2017), breaks=seq(2000,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "re", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )


fund.subset <- fund.quarterly.div %>% filter( fund.category == "Infrastructure")

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(2000, 2017), breaks=seq(2000,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "in", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )





fund.subset <- fund.quarterly.div %>% filter( fund.category == "Fund of Funds" )

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(1990, 2017), breaks=seq(1990,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "ff", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )




fund.subset <- fund.quarterly.div %>% filter( fund.category == "Debt Fund" )

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(2000, 2017), breaks=seq(2000,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "df", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )




fund.subset <- fund.quarterly.div %>% filter( fund.category == "Restructuring"  )

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(2000, 2017), breaks=seq(2000,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "rs", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )



fund.subset <- fund.quarterly.div %>% filter( fund.category == "Natural Resources"  )

g <- ggplot(data = fund.subset %>% filter(Vintage >= 2000 & Vintage <= 2014), aes(x=year, y = net.cf.distribution.rescale, color = factor(VintageYear))) +
  stat_summary(fun.y = "mean", geom = "line")  +
  theme_bw() + scale_fill_brewer(palette = "Greys") + 
  ylab("Capital Distribution Relative to $1 Invested") + 
  scale_color_discrete(name = "Vintage") + 
  scale_x_continuous( limits=c(2000, 2017), breaks=seq(2000,2017,5)) + 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,face="plain"),
        legend.text = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=16))  


g
ggsave(paste0(figures, "nr", "_cash_flows.eps"), width = 14.4/1.5, height = 7.24/1.5 )




