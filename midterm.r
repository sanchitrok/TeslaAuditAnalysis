library(dplyr)
library(pysch)
library(devtools)
install_github("bergant/finstr")
devtools::install_github("mkearney/rtweet")
library(finstr)
library(XBRL)
library(tidyverse)
library(knitr)
library(kableExtra)
options(knitr.table.format = "markdown")
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tseries)
library(benford.analysis)
library(readr)
library(plotluck)
library(finreportr)
library(xml2)
library(curl)
library(rtweet)
library(readr)
library(pwr)
library(tidytext)
library(textdata)
library(maps)
library(compareDF)
library(arsenal)
library(compare)
#reading files
ap_ledger <- read_csv("ap_ledger.csv")
disbursement_journal <- read_csv("disbursement_journal.csv")
expenditures <- read_csv("expenditures.csv")
fyear_begin_inventory_ledger <- read_csv("fyear_begin_inventory_ledger.csv")
fyear_end_ar_ledger <- read_csv("fyear_end_ar_ledger.csv")
perpetual_inventory_ledger <- read_csv("perpetual_inventory_ledger.csv")
purchase_journal <- read_csv("purchase_journal.csv")
real_world_cash_sales <- read_csv("real_world_cash_sales.csv")
real_world_collections <- read_csv("real_world_collections.csv")
real_world_credit_sales <- read_csv("real_world_credit_sales.csv")
real_world_fyear_end_ar_ledger <- read_csv("real_world_fyear_end_ar_ledger.csv")
real_world_ye_inventory <- read_csv("real_world_ye_inventory.csv")
receiver_journal <- read_csv("receiver_journal.csv")
sales_journal <- read_csv("sales_journal.csv")
shipments_journal <- read_csv("shipments_journal.csv")
allowance_for_uncollectable_ar <- read_csv("allowance_for_uncollectable_ar.csv")
collections_journal <- read_csv("collections_journal.csv")
customer_credit_limits <- read_csv("customer_credit_limits.csv")
daily_ar_balance <- read_csv("daily_ar_balance.csv")
deposit_daily <- read_csv("deposit_daily.csv")
# Note that .xml is the XBRL file indicator)
#Initializing balance sheet variables for 2012-2017
teslaBS_2012 <- GetBalanceSheet("TSLA", 2013)
teslaBS_2013 <- GetBalanceSheet("TSLA", 2014)
teslaBS_2014 <- GetBalanceSheet("TSLA", 2015)
teslaBS_2015 <- GetBalanceSheet("TSLA", 2016)
teslaBS_2016 <- GetBalanceSheet("TSLA", 2017)
teslaBS_2017 <- GetBalanceSheet("TSLA", 2018)

#Initializing income statement variables for 2012-2017
teslaIS_2012 <- GetIncome("TSLA", 2013)
teslaIS_2013 <- GetIncome("TSLA", 2014)
teslaIS_2014 <- GetIncome("TSLA", 2015)
teslaIS_2015 <- GetIncome("TSLA", 2016)
teslaIS_2016 <- GetIncome("TSLA", 2017)
teslaIS_2017 <- GetIncome("TSLA", 2018)
head(teslaIS_2012)
#Initializing cash flow variables for 2012-2017
teslaCF_2012 <- GetCashFlow("TSLA", 2013)
teslaCF_2013 <- GetCashFlow("TSLA", 2014)
teslaCF_2014 <- GetCashFlow("TSLA", 2015)
teslaCF_2015 <- GetCashFlow("TSLA", 2016)
teslaCF_2016 <- GetCashFlow("TSLA", 2017)
teslaCF_2017 <- GetCashFlow("TSLA", 2018)  
View(teslaCF_2012)
str(teslaBS_2012)

#Calculating Debt to equity ratio
#debt to equity ratio = total liability/shareholder's equity
teslaliability2012<-teslaBS_2012[teslaBS_2012$Metric=="Liabilities"& teslaBS_2012$endDate>"2011-12-31",]
debt<-teslaliability2012$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2012<-teslaBS_2012[teslaBS_2012$Metric=="Stockholders Equity"& teslaBS_2012$endDate>"2011-12-31",]
equity<-teslaequity2012$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2012<-(debt)/(equity)
tesladebttoequity2012
#7.934964
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2012<-teslaIS_2012[teslaIS_2012$Metric=="Sales Revenue Goods Net"& teslaIS_2012$endDate>"2011-12-31",]
netsales<-teslanetcreditsales2012$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2012<-teslaIS_2012[teslaIS_2012$Metric=="Sales Revenue Services Net"& teslaIS_2012$endDate>"2011-12-31",]
netservice<-teslanetcreditservice2012$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2012<-teslaBS_2012[teslaBS_2012$Metric=="Accounts Receivable Net Current"& teslaBS_2012$endDate>"2010-12-31",]
acr<-teslanetacr2012$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(9539000 + 26842000)
AcctRecvTurn2012 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2012
#22.72
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2012<-teslaBS_2012[teslaBS_2012$Metric=="Assets Current"& teslaBS_2012$endDate>"2011-12-31",]
ac<-teslaassets2012$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2012<-teslaBS_2012[teslaBS_2012$Metric=="Inventory Net"& teslaBS_2012$endDate>"2011-12-31",]
invn<-teslain2012$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2012<-teslaBS_2012[teslaBS_2012$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2012$endDate>"2011-12-31",]
pe<-teslape2012$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2012<-teslaBS_2012[teslaBS_2012$Metric=="Liabilities Current"& teslaBS_2012$endDate>"2011-12-31",]
lc<-teslalc2012$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2012 <- (ac - invn - pe)/(lc)	 
QuickRatio2012
#0.46
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2012<-teslaIS_2012[teslaIS_2012$Metric=="Cost Of Goods Sold"& teslaIS_2012$endDate>"2011-12-31",]
cogs<-teslalcogs2012$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2012<-teslaBS_2012[teslaBS_2012$Metric=="Inventory Net"& teslaBS_2012$endDate>"2010-12-31",]
invnet<-teslalinvnet2012$Amount
invnet<-as.data.frame(invnet)
invnet<-c(50082000+268504000)
str(invnet)
InvTurnover2012 <- cogs/ ((invnet)/2)
InvTurnover2012
#2.33
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2012<-teslaIS_2012[teslaIS_2012$Metric=="Operating Income Loss"& teslaIS_2012$endDate>"2011-12-31",]
oio<-teslaoio2012$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2012<-teslaIS_2012[teslaIS_2012$Metric=="Sales Revenue Goods Net"& teslaIS_2012$endDate>"2011-12-31",]
srgn<-teslasrgn2012$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2012<-teslaIS_2012[teslaIS_2012$Metric=="Sales Revenue Services Net"& teslaIS_2012$endDate>"2011-12-31",]
srsn<-teslasrsn2012$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2012 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2012
#-0.954
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2012<-teslaIS_2012[teslaIS_2012$Metric=="Net Income Loss"& teslaIS_2012$startDate>="2012-01-01"&teslaIS_2012$endDate>="2012-12-31",]
nil<-teslanil2012$Amount
nil<-as.data.frame(nil)
nil<- -396213000
nil<-as.numeric(nil)
str(nil)
teslawan2012<-teslaIS_2012[teslaIS_2012$Metric=="Weighted Average Number Of Shares Outstanding Basic And Diluted One"& teslaIS_2012$endDate>"2011-12-31",]
wan<-teslawan2012$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2012 <- (nil)/ (wan)
EPS2012
#-3.69088
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2012 <- 33.87 / EPS2012
#-9.15
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2012<-teslaBS_2012[teslaBS_2012$Metric=="Assets Current"& teslaBS_2012$endDate>"2011-12-31",]
ac<-teslaac2012$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2012<-teslaBS_2012[teslaBS_2012$Metric=="Liabilities Current"& teslaBS_2012$endDate>"2011-12-31",]
lc<-teslalc2012$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2012 <- ((ac) - (lc))
WorkCap2012
#-14340000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2012<-teslaIS_2012[teslaIS_2012$Metric=="Net Income Loss"& teslaIS_2012$endDate>"2011-12-31",]
ni<-teslanil2012$Amount
ni<--396213000
teslase2012<-teslaBS_2012[teslaBS_2012$Metric=="Stockholders Equity"& teslaBS_2012$endDate>"2011-12-31",]
se<-teslase2012$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-3.17733 
View(teslaBS_2013)
#Calculating Debt to equity ratio for 2013
#debt to equity ratio = total liability/shareholder's equity
teslaliability2013<-teslaBS_2013[teslaBS_2013$Metric=="Liabilities"& teslaBS_2013$endDate>"2012-12-31",]
debt<-teslaliability2013$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2013<-teslaBS_2013[teslaBS_2013$Metric=="Stockholders Equity"& teslaBS_2013$endDate>"2012-12-31",]
equity<-teslaequity2013$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2013<-(debt)/(equity)
tesladebttoequity2013
#2.62
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2013<-teslaIS_2013[teslaIS_2013$Metric=="Sales Revenue Goods Net"& teslaIS_2013$endDate>"2012-12-31",]
netsales<-teslanetcreditsales2013$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2013<-teslaIS_2013[teslaIS_2013$Metric=="Sales Revenue Services Net"& teslaIS_2013$endDate>"2012-12-31",]
netservice<-teslanetcreditservice2013$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2013<-teslaBS_2013[teslaBS_2013$Metric=="Accounts Receivable Net Current"& teslaBS_2013$endDate>"2011-12-31",]
acr<-teslanetacr2013$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(26842000 + 49109000)
AcctRecvTurn2013 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2013
#53.02
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2013<-teslaBS_2013[teslaBS_2013$Metric=="Assets Current"& teslaBS_2013$endDate>"2012-12-31",]
ac<-teslaassets2013$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2013<-teslaBS_2013[teslaBS_2013$Metric=="Inventory Net"& teslaBS_2013$endDate>"2012-12-31",]
invn<-teslain2013$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2013<-teslaBS_2013[teslaBS_2013$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2013$endDate>"2012-12-31",]
pe<-teslape2013$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2013<-teslaBS_2013[teslaBS_2013$Metric=="Liabilities Current"& teslaBS_2013$endDate>"2012-12-31",]
lc<-teslalc2013$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2013 <- (ac - invn - pe)/(lc)	 
QuickRatio2013
#1.33
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2013<-teslaIS_2013[teslaIS_2013$Metric=="Cost Of Goods Sold"& teslaIS_2013$endDate>"2012-12-31",]
cogs<-teslalcogs2013$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2013<-teslaBS_2013[teslaBS_2013$Metric=="Inventory Net"& teslaBS_2013$endDate>"2011-12-31",]
invnet<-teslalinvnet2013$Amount
invnet<-as.data.frame(invnet)
invnet<-c(340355000+268504000)
str(invnet)
InvTurnover2013 <- cogs/ ((invnet)/2)
InvTurnover2013
#5.07
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2013<-teslaIS_2013[teslaIS_2013$Metric=="Operating Income Loss"& teslaIS_2013$endDate>"2012-12-31",]
oio<-teslaoio2013$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2013<-teslaIS_2013[teslaIS_2013$Metric=="Sales Revenue Goods Net"& teslaIS_2013$endDate>"2012-12-31",]
srgn<-teslasrgn2013$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2013<-teslaIS_2013[teslaIS_2013$Metric=="Sales Revenue Services Net"& teslaIS_2013$endDate>"2012-12-31",]
srsn<-teslasrsn2013$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2013 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2013
#-0.03
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2013<-teslaIS_2013[teslaIS_2013$Metric=="Net Income Loss"& teslaIS_2013$startDate>="2012-01-01"&teslaIS_2013$endDate>="2012-12-31",]
nil<-teslanil2013$Amount
nil<-as.data.frame(nil)
nil<- -74014000
nil<-as.numeric(nil)
str(nil)
teslawan2013<-teslaIS_2013[teslaIS_2013$Metric=="Weighted Average Number Of Shares Outstanding Basic And Diluted One"& teslaIS_2013$endDate>"2012-12-31",]
wan<-teslawan2013$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2013 <- (nil)/ (wan)
EPS2013
#-0.619
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2013<-teslaBS_2013[teslaBS_2013$Metric=="Assets Current"& teslaBS_2013$endDate>"2012-12-31",]
ac<-teslaac2013$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2013<-teslaBS_2013[teslaBS_2013$Metric=="Liabilities Current"& teslaBS_2013$endDate>"2012-12-31",]
lc<-teslalc2013$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2013 <- ((ac) - (lc))
WorkCap2013
#590779000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2013<-teslaIS_2013[teslaIS_2013$Metric=="Net Income Loss"& teslaIS_2013$endDate>"2012-12-31",]
ni<-teslanil2013$Amount
ni<--74014000
teslase2013<-teslaBS_2013[teslaBS_2013$Metric=="Stockholders Equity"& teslaBS_2013$endDate>"2012-12-31",]
se<-teslase2013$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-0.11
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2013 <- 13.87 / EPS2013
#-22.37
#Calculating Debt to equity ratio for 2014
#debt to equity ratio = total liability/shareholder's equity
teslaliability2014<-teslaBS_2014[teslaBS_2014$Metric=="Liabilities"& teslaBS_2014$endDate>"2013-12-31",]
debt<-teslaliability2014$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2014<-teslaBS_2014[teslaBS_2014$Metric=="Stockholders Equity"& teslaBS_2014$endDate>"2013-12-31",]
equity<-teslaequity2014$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2014<-(debt)/(equity)
tesladebttoequity2014
#5.35
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2014<-teslaIS_2014[teslaIS_2014$Metric=="Sales Revenue Goods Net"& teslaIS_2014$endDate>"2013-12-31",]
netsales<-teslanetcreditsales2014$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2014<-teslaIS_2014[teslaIS_2014$Metric=="Sales Revenue Services Net"& teslaIS_2014$endDate>"2013-12-31",]
netservice<-teslanetcreditservice2014$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2014<-teslaBS_2014[teslaBS_2014$Metric=="Accounts Receivable Net Current"& teslaBS_2014$endDate>"2012-12-31",]
acr<-teslanetacr2014$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(226604000 + 49109000)
AcctRecvTurn2014 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2014
#23.2
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2014<-teslaBS_2014[teslaBS_2014$Metric=="Assets Current"& teslaBS_2014$endDate>"2013-12-31",]
ac<-teslaassets2014$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2014<-teslaBS_2014[teslaBS_2014$Metric=="Inventory Net"& teslaBS_2014$endDate>"2013-12-31",]
invn<-teslain2014$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2014<-teslaBS_2014[teslaBS_2014$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2014$endDate>"2013-12-31",]
pe<-teslape2014$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2014<-teslaBS_2014[teslaBS_2014$Metric=="Liabilities Current"& teslaBS_2014$endDate>"2013-12-31",]
lc<-teslalc2014$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2014 <- (ac - invn - pe)/(lc)	 
QuickRatio2014
#1.02
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2014<-teslaIS_2014[teslaIS_2014$Metric=="Cost Of Goods Sold"& teslaIS_2014$endDate>"2013-12-31",]
cogs<-teslalcogs2014$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2014<-teslaBS_2014[teslaBS_2014$Metric=="Inventory Net"& teslaBS_2014$endDate>"2012-12-31",]
invnet<-teslalinvnet2014$Amount
invnet<-as.data.frame(invnet)
invnet<-c(340355000+953675000)
str(invnet)
InvTurnover2014 <- cogs/ ((invnet)/2)
InvTurnover2014
#3.57
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2014<-teslaIS_2014[teslaIS_2014$Metric=="Operating Income Loss"& teslaIS_2014$endDate>"2013-12-31",]
oio<-teslaoio2014$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2014<-teslaIS_2014[teslaIS_2014$Metric=="Sales Revenue Goods Net"& teslaIS_2014$endDate>"2013-12-31",]
srgn<-teslasrgn2014$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2014<-teslaIS_2014[teslaIS_2014$Metric=="Sales Revenue Services Net"& teslaIS_2014$endDate>"2013-12-31",]
srsn<-teslasrsn2014$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2014 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2014
#-0.05
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2014<-teslaIS_2014[teslaIS_2014$Metric=="Net Income Loss"& teslaIS_2014$startDate>="2013-01-01"&teslaIS_2014$endDate>="2013-12-31",]
nil<-teslanil2014$Amount
nil<-as.data.frame(nil)
nil<- -294040000
nil<-as.numeric(nil)
str(nil)
teslawan2014<-teslaIS_2014[teslaIS_2014$Metric=="Weighted Average Number Of Share Outstanding Basic And Diluted"& teslaIS_2014$endDate>"2013-12-31",]
wan<-teslawan2014$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2014 <- (nil)/ (wan)
EPS2014
#-2.36
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2014<-teslaBS_2014[teslaBS_2014$Metric=="Assets Current"& teslaBS_2014$endDate>"2013-12-31",]
ac<-teslaac2014$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2014<-teslaBS_2014[teslaBS_2014$Metric=="Liabilities Current"& teslaBS_2014$endDate>"2013-12-31",]
lc<-teslalc2014$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2014 <- ((ac) - (lc))
WorkCap2014
#1091491000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2014<-teslaIS_2014[teslaIS_2014$Metric=="Net Income Loss"& teslaIS_2014$endDate>"2013-12-31",]
ni<-teslanil2014$Amount
ni<--294040000
teslase2014<-teslaBS_2014[teslaBS_2014$Metric=="Stockholders Equity"& teslaBS_2014$endDate>"2013-12-31",]
se<-teslase2014$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-0.322
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2014 <- 33.87 / EPS2014
#-14.34
#For 2015
#Calculating Debt to equity ratio
#debt to equity ratio = total liability/shareholder's equity
teslaliability2015<-teslaBS_2015[teslaBS_2015$Metric=="Liabilities"& teslaBS_2015$endDate>"2014-12-31",]
debt<-teslaliability2015$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2015<-teslaBS_2015[teslaBS_2015$Metric=="Stockholders Equity"& teslaBS_2015$endDate>"2014-12-31",]
equity<-teslaequity2015$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2015<-(debt)/(equity)
tesladebttoequity2015
#6.39
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2015<-teslaIS_2015[teslaIS_2015$Metric=="Sales Revenue Goods Net"& teslaIS_2015$endDate>"2014-12-31",]
netsales<-teslanetcreditsales2015$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2015<-teslaIS_2015[teslaIS_2015$Metric=="Sales Revenue Services And Other Net"& teslaIS_2015$endDate>"2014-12-31",]
netservice<-teslanetcreditservice2015$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2015<-teslaBS_2015[teslaBS_2015$Metric=="Accounts Receivable Net Current"& teslaBS_2015$endDate>"2013-12-31",]
acr<-teslanetacr2015$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(226604000 + 168965000)
AcctRecvTurn2015 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2015
#20.45
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2015<-teslaBS_2015[teslaBS_2015$Metric=="Assets Current"& teslaBS_2015$endDate>"2014-12-31",]
ac<-teslaassets2015$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2015<-teslaBS_2015[teslaBS_2015$Metric=="Inventory Net"& teslaBS_2015$endDate>"2014-12-31",]
invn<-teslain2015$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2015<-teslaBS_2015[teslaBS_2015$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2015$endDate>"2014-12-31",]
pe<-teslape2015$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2015<-teslaBS_2015[teslaBS_2015$Metric=="Liabilities Current"& teslaBS_2015$endDate>"2014-12-31",]
lc<-teslalc2015$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2015 <- (ac - invn - pe)/(lc)	 
QuickRatio2015
#0.49
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2015<-teslaIS_2015[teslaIS_2015$Metric=="Cost Of Goods Sold"& teslaIS_2015$endDate>"2014-12-31",]
cogs<-teslalcogs2015$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2015<-teslaBS_2015[teslaBS_2015$Metric=="Inventory Net"& teslaBS_2015$endDate>"2013-12-31",]
invnet<-teslalinvnet2015$Amount
invnet<-as.data.frame(invnet)
invnet<-c(953675000+1277838000)
str(invnet)
InvTurnover2015 <- cogs/ ((invnet)/2)
InvTurnover2015
#2.53
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2015<-teslaIS_2015[teslaIS_2015$Metric=="Operating Income Loss"& teslaIS_2015$endDate>"2014-12-31",]
oio<-teslaoio2015$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2015<-teslaIS_2015[teslaIS_2015$Metric=="Sales Revenue Goods Net"& teslaIS_2015$endDate>"2014-12-31",]
srgn<-teslasrgn2015$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2015<-teslaIS_2015[teslaIS_2015$Metric=="Sales Revenue Services And Other Net"& teslaIS_2015$endDate>"2014-12-31",]
srsn<-teslasrsn2015$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2015 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2015
#-0.177
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2015<-teslaIS_2015[teslaIS_2015$Metric=="Net Income Loss"& teslaIS_2015$startDate>="2015-01-01"&teslaIS_2015$endDate>="2015-12-31",]
nil<-teslanil2015$Amount
nil<-as.data.frame(nil)
nil<- -888663000
nil<-as.numeric(nil)
str(nil)
teslawan2015<-teslaIS_2015[teslaIS_2015$Metric=="Weighted Average Number Of Share Outstanding Basic And Diluted"& teslaIS_2015$endDate>"2014-12-31",]
wan<-teslawan2015$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2015 <- (nil)/ (wan)
EPS2015
#-6.93
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2015 <- 33.87 / EPS2015
#-4.88
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2015<-teslaBS_2015[teslaBS_2015$Metric=="Assets Current"& teslaBS_2015$endDate>"2014-12-31",]
ac<-teslaac2015$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2015<-teslaBS_2015[teslaBS_2015$Metric=="Liabilities Current"& teslaBS_2015$endDate>"2014-12-31",]
lc<-teslalc2015$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2015 <- ((ac) - (lc))
WorkCap2015
#-24706000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2015<-teslaIS_2015[teslaIS_2015$Metric=="Net Income Loss"& teslaIS_2015$endDate>"2014-12-31",]
ni<-teslanil2015$Amount
ni<--888663000
teslase2015<-teslaBS_2015[teslaBS_2015$Metric=="Stockholders Equity"& teslaBS_2015$endDate>"2014-12-31",]
se<-teslase2015$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-0.81 
#2016
#Calculating Debt to equity ratio
#debt to equity ratio = total liability/shareholder's equity
teslaliability2016<-teslaBS_2016[teslaBS_2016$Metric=="Liabilities"& teslaBS_2016$endDate>"2015-12-31",]
debt<-teslaliability2016$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2016<-teslaBS_2016[teslaBS_2016$Metric=="Stockholders Equity"& teslaBS_2016$endDate>"2015-12-31",]
equity<-teslaequity2016$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2016<-(debt)/(equity)
tesladebttoequity2016
#3.52
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2016<-teslaIS_2016[teslaIS_2016$Metric=="Sales Revenue Goods Net"& teslaIS_2016$endDate>"2015-12-31",]
netsales<-teslanetcreditsales2016$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2016<-teslaIS_2016[teslaIS_2016$Metric=="Sales Revenue Services And Other Net"& teslaIS_2016$endDate>"2015-12-31",]
netservice<-teslanetcreditservice2016$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2016<-teslaBS_2016[teslaBS_2016$Metric=="Accounts Receivable Net Current"& teslaBS_2016$endDate>"2014-12-31",]
acr<-teslanetacr2016$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(499142000 + 168965000)
AcctRecvTurn2016 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2016
#18.13
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2016<-teslaBS_2016[teslaBS_2016$Metric=="Assets Current"& teslaBS_2016$endDate>"2015-12-31",]
ac<-teslaassets2016$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2016<-teslaBS_2016[teslaBS_2016$Metric=="Inventory Net"& teslaBS_2016$endDate>"2015-12-31",]
invn<-teslain2016$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2016<-teslaBS_2016[teslaBS_2016$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2016$endDate>"2015-12-31",]
pe<-teslape2016$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2016<-teslaBS_2016[teslaBS_2016$Metric=="Liabilities Current"& teslaBS_2016$endDate>"2015-12-31",]
lc<-teslalc2016$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2016 <- (ac - invn - pe)/(lc)	 
QuickRatio2016
#0.68
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2016<-teslaIS_2016[teslaIS_2016$Metric=="Cost Of Goods Sold"& teslaIS_2016$endDate>"2015-12-31",]
cogs<-teslalcogs2016$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2016<-teslaBS_2016[teslaBS_2016$Metric=="Inventory Net"& teslaBS_2016$endDate>"2014-12-31",]
invnet<-teslalinvnet2016$Amount
invnet<-as.data.frame(invnet)
invnet<-c(2067454000+1277838000)
str(invnet)
InvTurnover2016 <- cogs/ ((invnet)/2)
InvTurnover2016
#2.55
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2016<-teslaIS_2016[teslaIS_2016$Metric=="Operating Income Loss"& teslaIS_2016$endDate>"2015-12-31",]
oio<-teslaoio2016$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2016<-teslaIS_2016[teslaIS_2016$Metric=="Sales Revenue Goods Net"& teslaIS_2016$endDate>"2015-12-31",]
srgn<-teslasrgn2016$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2016<-teslaIS_2016[teslaIS_2016$Metric=="Sales Revenue Services And Other Net"& teslaIS_2016$endDate>"2015-12-31",]
srsn<-teslasrsn2016$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2016 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2016
#-0.11
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2016<-teslaIS_2016[teslaIS_2016$Metric=="Net Income Loss Attributable To Noncontrolling Interest"& teslaIS_2016$startDate>="2016-01-01"&teslaIS_2016$endDate>="2016-12-31",]
nil<-teslanil2016$Amount
nil<-as.data.frame(nil)
nil<-as.numeric(nil)
str(nil)
teslawan2016<-teslaIS_2016[teslaIS_2016$Metric=="Weighted Average Number Of Share Outstanding Basic And Diluted"& teslaIS_2016$endDate>"2015-12-31",]
wan<-teslawan2016$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2016 <- (nil)/ (wan)
EPS2016
#-0.68
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2016 <- 8.87 / EPS2016
#-13.03
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2016<-teslaBS_2016[teslaBS_2016$Metric=="Assets Current"& teslaBS_2016$endDate>"2015-12-31",]
ac<-teslaac2016$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2016<-teslaBS_2016[teslaBS_2016$Metric=="Liabilities Current"& teslaBS_2016$endDate>"2015-12-31",]
lc<-teslalc2016$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2016 <- ((ac) - (lc))
WorkCap2016
#432791000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2016<-teslaIS_2016[teslaIS_2016$Metric=="Net Income Loss Attributable To Noncontrolling Interest"& teslaIS_2016$endDate>"2015-12-31",]
ni<-teslanil2016$Amount
ni<-as.numeric(ni)
teslase2016<-teslaBS_2016[teslaBS_2016$Metric=="Stockholders Equity"& teslaBS_2016$endDate>"2015-12-31",]
se<-teslase2016$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-0.02
#2017
#Calculating Debt to equity ratio
#debt to equity ratio = total liability/shareholder's equity
teslaliability2017<-teslaBS_2017[teslaBS_2017$Metric=="Liabilities"& teslaBS_2017$endDate>"2016-12-31",]
debt<-teslaliability2017$Amount
debt<-as.data.frame(debt)
debt<-as.numeric(debt)
str(debt)
teslaequity2017<-teslaBS_2017[teslaBS_2017$Metric=="Stockholders Equity"& teslaBS_2017$endDate>"2016-12-31",]
equity<-teslaequity2017$Amount
equity<-as.data.frame(equity)
equity<-as.numeric(equity)
str(equity)
tesladebttoequity2017<-(debt)/(equity)
tesladebttoequity2017
#5.43
#Calculating Accounts receivable turnover ratio
#Accts receivable turnover ratio = net credit sales(sales+service)/((acct receivable net prev + current)/2)
teslanetcreditsales2017<-teslaIS_2017[teslaIS_2017$Metric=="Sales Revenue Goods Net"& teslaIS_2017$endDate>"2016-12-31",]
netsales<-teslanetcreditsales2017$Amount
netsales<-as.data.frame(netsales)
netsales<-as.numeric(netsales)
str(netsales)
teslanetcreditservice2017<-teslaIS_2017[teslaIS_2017$Metric=="Sales Revenue Services And Other Net"& teslaIS_2017$endDate>"2016-12-31",]
netservice<-teslanetcreditservice2017$Amount
netservice<-as.data.frame(netservice)
netservice<-as.numeric(netservice)
str(netservice)
teslanetacr2017<-teslaBS_2017[teslaBS_2017$Metric=="Accounts Receivable Net Current"& teslaBS_2017$endDate>"2015-12-31",]
acr<-teslanetacr2017$Amount
acr<-as.data.frame(acr)
str(acr)
acr<-c(499142000 + 515381000)
AcctRecvTurn2017 <-(netsales + netservice)/ ((acr)/2)
AcctRecvTurn2017
#18.79
#Calculating Quick ratio
#quick ratio = (total current assets - Invntory - Prepaid expense)/(current liabilities)
teslaassets2017<-teslaBS_2017[teslaBS_2017$Metric=="Assets Current"& teslaBS_2017$endDate>"2016-12-31",]
ac<-teslaassets2017$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslain2017<-teslaBS_2017[teslaBS_2017$Metric=="Inventory Net"& teslaBS_2017$endDate>"2016-12-31",]
invn<-teslain2017$Amount
invn<-as.data.frame(invn)
invn<-as.numeric(invn)
str(invn)
teslape2017<-teslaBS_2017[teslaBS_2017$Metric=="Prepaid Expense And Other Assets Current"& teslaBS_2017$endDate>"2016-12-31",]
pe<-teslape2017$Amount
pe<-as.data.frame(pe)
pe<-as.numeric(pe)
str(pe)
teslalc2017<-teslaBS_2017[teslaBS_2017$Metric=="Liabilities Current"& teslaBS_2017$endDate>"2016-12-31",]
lc<-teslalc2017$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
QuickRatio2017 <- (ac - invn - pe)/(lc)	 
QuickRatio2017
#0.52
#Calculating Inventory turnover
#Inventory turns = COGS/avg inventory
teslalcogs2017<-teslaIS_2017[teslaIS_2017$Metric=="Cost Of Goods Sold"& teslaIS_2017$endDate>"2016-12-31",]
cogs<-teslalcogs2017$Amount
cogs<-as.data.frame(cogs)
cogs<-as.numeric(cogs)
str(cogs)
teslalinvnet2017<-teslaBS_2017[teslaBS_2017$Metric=="Inventory Net"& teslaBS_2017$endDate>"2015-12-31",]
invnet<-teslalinvnet2017$Amount
invnet<-as.data.frame(invnet)
invnet<-c(2067454000+2263537000)
str(invnet)
InvTurnover2017 <- cogs/ ((invnet)/2)
InvTurnover2017
#3.105
#Calculating Operating Margin
#Operating Margin = Operating Income/Net Sales(BOTH)
teslaoio2017<-teslaIS_2017[teslaIS_2017$Metric=="Operating Income Loss"& teslaIS_2017$endDate>"2016-12-31",]
oio<-teslaoio2017$Amount
oio<-as.data.frame(oio)
oio<-as.numeric(oio)
str(oio)
teslasrgn2017<-teslaIS_2017[teslaIS_2017$Metric=="Sales Revenue Goods Net"& teslaIS_2017$endDate>"2016-12-31",]
srgn<-teslasrgn2017$Amount
srgn<-as.data.frame(srgn)
srgn<-as.numeric(srgn)
str(srgn)
teslasrsn2017<-teslaIS_2017[teslaIS_2017$Metric=="Sales Revenue Services And Other Net"& teslaIS_2017$endDate>"2016-12-31",]
srsn<-teslasrsn2017$Amount
srsn<-as.data.frame(srsn)
srsn<-as.numeric(srsn)
str(srsn)
OperMargin2017 <- (oio)/ ((srgn)	+ (srsn))
OperMargin2017
#-0.17
#Calculating Earnings per share
# EPS = net income - preferred dividends/avg common shares outstanding
teslanil2017<-teslaIS_2017[teslaIS_2017$Metric=="Net Income Loss Attributable To Noncontrolling Interest"& teslaIS_2017$startDate>="2017-01-01"&teslaIS_2017$endDate>="2017-12-31",]
nil<-teslanil2017$Amount
nil<-as.data.frame(nil)
nil<-as.numeric(nil)
str(nil)
teslawan2017<-teslaIS_2017[teslaIS_2017$Metric=="Weighted Average Number Of Diluted Shares Outstanding"& teslaIS_2017$endDate>"2016-12-31",]
wan<-teslawan2017$Amount
wan<-as.data.frame(wan)
wan<-as.numeric(wan)
str(wan)
EPS2017 <- (nil)/ (wan)
EPS2017
#-1.6
#Calculating Price/Earnings ratio
#P/E = Share price / EPS
PriceEarnings2017 <- 23.87 / EPS2017
#-14.15
#Calculating Working capital
#WC = Current Assets - Current Liabilities
teslaac2017<-teslaBS_2017[teslaBS_2017$Metric=="Assets Current"& teslaBS_2017$endDate>"2016-12-31",]
ac<-teslaac2017$Amount
ac<-as.data.frame(ac)
ac<-as.numeric(ac)
str(ac)
teslalc2017<-teslaBS_2017[teslaBS_2017$Metric=="Liabilities Current"& teslaBS_2017$endDate>"2016-12-31",]
lc<-teslalc2017$Amount
lc<-as.data.frame(lc)
lc<-as.numeric(lc)
str(lc)
WorkCap2017 <- ((ac) - (lc))
WorkCap2017
#-1104150000
#Calculating Return on Equity
#ROE = Net income / Shareholders equity
teslanil2017<-teslaIS_2017[teslaIS_2017$Metric=="Net Income Loss Attributable To Noncontrolling Interest"& teslaIS_2017$endDate>"2016-12-31",]
ni<-teslanil2017$Amount
ni<-as.numeric(ni)
teslase2017<-teslaBS_2017[teslaBS_2017$Metric=="Stockholders Equity"& teslaBS_2017$endDate>"2016-12-31",]
se<-teslase2017$Amount
se<-as.data.frame(se)
se<-as.numeric(se)
str(se)
roe<-ni/se
roe
#-0.06
#similarly calculated for competitors Ford and GM and listed in their respective csv files.
#2)
#Comparing Debt to Equity Ratio for Tesla, GM, Ford
deratio <- read_csv("deratio.csv")
str(deratio)
ggplot(deratio,aes(Year))+
  geom_point(aes(y = TeslaDE, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmDE, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FDE, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "Debt to Equity ratio",
       color = "Legend")
#Comparing AC Ratio for Tesla, GM, Ford
acratio <- read_csv("acratio.csv")
str(acratio)
ggplot(acratio,aes(Year))+
  geom_point(aes(y = TeslaACR, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmACR, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FACR, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "Account Receiver Turnover",
       color = "Legend")
#Comparing Quick Ratio for Tesla, GM, Ford
qratio <- read_csv("qratio.csv")
str(qratio)
ggplot(qratio,aes(Year))+
  geom_point(aes(y = TeslaQR, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmQR, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FQR, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "Quick Ratior",
       color = "Legend")
#Comparing Inventory Turnover Ratio for Tesla, GM, Ford
itratio <- read_csv("itratio.csv")
str(itratio)
ggplot(itratio,aes(Year))+
  geom_point(aes(y = TeslaIT, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmIT, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FIT, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "Inventory Turnover Ratio",
       color = "Legend")
#Comparing OperMargin Ratio for Tesla, GM, Ford
omratio <- read_csv("omratio.csv")
str(omratio)
ggplot(omratio,aes(Year))+
  geom_point(aes(y = TeslaOM, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmOM, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FOM, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "OperMargin Ratio",
       color = "Legend")
#Comparing EPS Ratio for Tesla, GM, Ford
epsratio <- read_csv("epsratio.csv")
str(epsratio)
ggplot(epsratio,aes(Year))+
  geom_point(aes(y = TeslaEPS, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmEPS, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FEPS, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "EPS",
       color = "Legend")
#Comparing Price Ratio for Tesla, GM, Ford
perratio <- read_csv("perratio.csv") 
str(perratio)
ggplot(perratio,aes(Year))+
  geom_point(aes(y = TeslaPER, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmPER, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FPER, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "PER",
       color = "Legend")
#Comparing Working Capital Ratio for Tesla, GM, Ford
wcratio <- read_csv("wcratio.csv") 
str(wcratio)
ggplot(wcratio,aes(Year))+
  geom_point(aes(y = TeslaWC, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmWC, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FWC, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "WC",
       color = "Legend")

#Comparing ROE Ratio for Tesla, GM, Ford
roeratio <- read_csv("roeratio.csv")
str(roeratio)
ggplot(roeratio,aes(Year))+
  geom_point(aes(y = TeslaROE, colour = "Tesla"), size = 1) + 
  geom_point(aes(y = GmROE, colour = "General Motors"), size = 1)+
  geom_point(aes(y = FROE, colour = "Ford"), size = 1)+
  labs(x = "Year",
       y = "ROE",
       color = "Legend")

# 3. (10 points) Intelligence Scanning with Twitter:
# access token method: create token and save it as an environment variable
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install dev version of rtweet from github
devtools::install_github("mkearney/rtweet")
install.packages("maps")
library(twitteR)
setup_twitter_oauth(consumer_key,consumer_secret,
                    access_token,access_token_secret)

consumer_key<- "83RlJTBfqOpXcSXVknzPiSMAQ"

consumer_secret<- "bzph4h1kY5CQUVRMlclEmOHytThxePV2ZTeIosWCrO49sv39Nr"

access_token <-"14122740-1LLRNCWW7jvyTIC4p43z3TWbZtUNvYsBcoLIWrGJH"

access_token_secret :"zA9aMfugKFLz3bl681w23LLAKHSoZsQgeFuHBQ8brShIx"
token<-
create_token(
  app = "westland",
  consumer_key = api_key,
  consumer_secret =  api_secretkey,
  access_token = access_token,
  access_secret = access_tokensecret)

get_token()
key<-"6j7Ig4xzHlBr8uUJ5A4Ym0NTf"

# To test your authentication, search for 18000 tweets using the rstats hashtag

rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)

head(rt[, c("screen_name","source")])

system.time(
  stream_tweets(
    q = "auto, tesla, car, gm, electric, nissan, honda, ford, bmw, hyundai, toyota",      
    timeout = 6000,    ## the number of seconds that you will access.
    parse = FALSE,    ## do this later outside of stream_tweets
    file_name = "tweetsaboutTesla1.json"
  ))
djt <- parse_stream("tweetsaboutTesla1.json")

djt <- djt[,3:6]  ## just a few things we'd like to see
glimpse(djt)
# here is Glimpse of twitter data which includes source and text information along with time and name

rt %>%
  ts_plot("2 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #Tesla Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using two-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#From the Time Series above, we can conclude that there were relatively more Frequency of tweets are high on 15 April and during weekends
tesla_parsed <- parse_stream("tweetsaboutTesla1.json")

## plot time series of tweets
tesla_parsed %>%
  ts_plot("1 second") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tesla related statuses from past 24 hours",
    subtitle = "Twitter status (tweet) counts aggregated using 1 second intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# frequency of tesla related statuses from past 24 hours using 1 second interval where the peak is between 21:00 and 22:00

#Using the parsed dataset, performed a sentiment Analysis using the NRC lexicon and observed the following bar plot -

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- 
  tesla_parsed %>%
  select(user_id,source,created_at,text) %>% 
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

sentmnt <- inner_join(
  get_sentiments("nrc"),
  tweet_words, 
  by="word") %>% 
  count(sentiment)  

ggplot(sentmnt, aes(sentiment, n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sentiment") +
  ylab("Frequency expressed in tweets")
#Frequency of words expressed in tweets where mostly it is positive
 
# Modify the sentiment lexicon


library(tidyverse)
library(tidytext)
library(textdata)
library(plotluck)

head(get_sentiments("nrc"))
# snapshot of sentiment lexicon

lex <- matrix(
  c("tesla", "ecstacy",
  "nio", "exotic",
  "gm", "complex",
  "ford","complex", 
  "auto", "interested",
  "car", "say_what",
  "honda", "complex",
  "ev", "interested",
  "modelX", "yuss",
  "hyundai", "complex",
  "toyota", "complex"
   ),
  ncol=2,
  byrow=TRUE
  ) %>% as.data.frame()
colnames(lex) <- c("word", "sentiment")
lex <- rbind(lex, get_sentiments("nrc"))

sentmnt <- inner_join(
  lex,
  tweet_words, 
  by="word") %>% 
  count(sentiment)  
# Count for words on sentiment analysis
p <- ggplot(sentmnt, aes(sentiment, n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sentiment") +
  ylab("Frequency expressed in tweets")
 
p


#adhoc lexicon have grouped the words related to competitors of Tesla into "complex", interesting words related to electric motor vehicle into "interested"
#From histogram, most popular emotions would be positive vs negative 
#4)

require(rvest)
require(wordcloud)
require(RColorBrewer)
require(xml2)
require(tm)
require(slam)

#WordCloud 
url <- "https://www.consumeraffairs.com/automotive/tesla_motors.html"
reviews <- url %>%
  read_html() %>%
  html_nodes(".rvw-bd , p")
quote <- reviews %>%  html_text()
data.frame(quote, stringsAsFactors = FALSE) %>% matrix()
pal2 <- brewer.pal(8,"Dark2") ## from RColorBrewer
wordcloud(quote, colors = pal2)

#5)

# I have two R files named as ui and server for creating risk assessment matrix dashboard and included in zip folder
#  **Sample size and statistical confidence in discovery sampling**

confidence <- seq(.99,.7,-.01)
n <- (log(1-confidence))/log(1-.05)
plot(confidence,n, type="l")



#For example, a 5% intolerable error rate at 95% confidence the result is -

confidence <- .95
n <- (log(1-confidence))/log(1-.05)
cat("\n Discovery sample size = ", ceiling(n))



setwd("~/R Projects/523_midterm_files")

sales_journal <-
  read_csv("sales_journal.csv",
           col_types = cols(X1 = col_skip()))

purchase_journal <-
  read_csv("purchase_journal.csv",
           col_types = cols(X1 = col_skip()))

perpetual_inventory_ledger <-
  read_csv("perpetual_inventory_ledger.csv",
           col_types = cols(X1 = col_skip()))

fyear_end_ar_ledger <-
  read_csv("fyear_end_ar_ledger.csv",
           col_types = cols(X1 = col_skip()))

fyear_begin_inventory_ledger <-
  read_csv("fyear_begin_inventory_ledger.csv",
           col_types = cols(X1 = col_skip()))

expenditures <-
  read_csv("expenditures.csv",
           col_types = cols(X1 = col_skip()))

disbursement_journal <-
  read_csv("disbursement_journal.csv",
           col_types = cols(X1 = col_skip()))

deposit_daily <-
  read_csv("deposit_daily.csv",
           col_types = cols(X1 = col_skip()))

daily_ar_balance <-
  read_csv("daily_ar_balance.csv",
           col_types = cols(X1 = col_skip()))

collections_journal <- read_csv("collections_journal.csv",
                                col_types = cols(X1 = col_skip()))

ap_ledger <- read_csv("ap_ledger.csv",
                      col_types = cols(X1 = col_skip()))

Sales <- sum(sales_journal$collection_amount)
Sales_avg <- mean(sales_journal$collection_amount)

Sales_Returns <-sum(sales_journal$sales_return * sales_journal$sales_unit_price)
Returns_avg <- mean(sales_journal$sales_return * sales_journal$sales_unit_price)

Purchases <- sum(purchase_journal$po_count * purchase_journal$unit_cost)

Misc_Expenses <- sum(expenditures$amount)

YE_Inventory <- perpetual_inventory_ledger %>% group_by(sku) %>%
  inner_join(fyear_begin_inventory_ledger, by="sku") %>% slice(n()) %>%
  mutate(inv_extended = unit_cost * stock_on_hand.x) %>%
  ungroup() %>% select(inv_extended) %>% sum()

Begin_Inventory <-fyear_begin_inventory_ledger %>% group_by(sku) %>%
  mutate(inv_extended = unit_cost * stock_on_hand) %>%
  ungroup() %>% select(inv_extended) %>% sum()

Cost_of_Goods_Sold <- sum(sales_journal$sales_count * sales_journal$unit_cost)
+ YE_Inventory -Begin_Inventory
YE_Inventory_avg <- (Begin_Inventory - YE_Inventory )/2

Cash <- sum(deposit_daily$deposit_amount) - Purchases - Misc_Expenses 

Cash_avg <- Cash/nrow(deposit_daily$deposit_amount)

AR <- sum(fyear_end_ar_ledger$amount)


#The Risk matrix thus construsted is shown below and has the ability to dynamically calculate sample sizes for corresponding confidence and risk parameters - 

#
#  See http://shiny.rstudio.com/ for documentation
#
library(knitr)
library(shiny)

# Define UI for application 
ui <- fluidPage(
  
  
  titlePanel("Risk Assessment Matrix"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: statistical confidence level of the audit tests 
      sliderInput("confidence", "Confidence:",
                  min = .7, max = .999,
                  value = .95),
      
      # Input: cost of auditing per transaction sampled  
      sliderInput("cost", "Audit $ / transaction:",
                  min = 0, max = 500,
                  value = 100),
      
      
      # Input: Text for providing a caption for the RAM 
      textInput(inputId = "caption",
                label = "Client:",
                value = "Tesla")
      
    ),
    
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: slider values entered 
      tableOutput("values"),
      
      # Output: Formatted text for caption 
      h3(textOutput("caption", container = span)),
      
      # Output: total cost of the audit 
      textOutput("view"), 
      
      # Output: RAM summary with sample sizes (scope) and cost
      verbatimTextOutput("summary"),
      
      
      h6("Risk choices are:  1 = Low,  2=Medium, 3=High"),
      h6("Risk_intel = the risk level indicated by business intelligence scanning"),
      h6("Risk_prior = the risk level indicated by audits in prior years"),
      h6("Account Amount and the Ave. Transaction size are in $ without decimals or 000 dividers"),
      h6("Scope = estimated discovery sample size that will be needed in the audit of this account"),
      h6("Audit cost = audit labor dollars per sampled transaction"),      
      h6("Confidence = statistical confidence")
      
      
    )
  )
)


# Define server logic required to draw a histogram
    server <- function(input, output) {
  
  # auditors risk assessment matrix generated from prior years workpapers, etc.
  ram <- read.csv("risk_asst_matrix.csv")
  
  
  # Reactive expression to create data frame of slider input values 
  sliderValues <- reactive({
    
    data.frame(
      Audit_Parameter = c("confidence",
                          "cost"),
      Value = as.character(c(input$confidence,
                             input$cost)),
      stringsAsFactors = FALSE)
  })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  
  output$caption <- renderText({
    input$caption
  })
  
  
  # Recompute scope and cost whenever input$confidence or input$cost change
  
  output$summary <- renderPrint({
    ram <- ram
    conf <- input$confidence
    cost <- input$cost
    risk <- (10 - (as.numeric(ram[,2]) * as.numeric(ram[,3])) )/100
    Scope <-  ceiling( log(1-conf) / log( 1- risk))
    ram <- cbind(ram[,1:5], Scope)
    Min_cost <- Scope * cost
    ram <- cbind(ram[,1:6], Min_cost)
    ram
  })
  
  
  # Recompute minimum audit cost whenever input$confidence or input$cost change
  
  output$view <- renderText({
    ram <- ram
    conf <- input$confidence
    cost <- input$cost
    risk <- (10 - (as.numeric(ram[,2]) * as.numeric(ram[,3])) )/100
    Scope <-  ceiling( log(1-conf) / log( 1 - risk))
    ram <- cbind(ram[,1:5], Scope)
    Min_cost <- Scope * cost
    minimum_audit_cost <- sum(Min_cost)
    c("Minimum estimated audit cost = ",minimum_audit_cost)
  })
}

#{r eval=FALSE, include=FALSE}
#
#  See http://shiny.rstudio.com/ for documentation
#

# Define UI for application 
ui

# Define server logic required to draw a histogram
server


# Run the application 
shinyApp(ui = ui, server = server)
#6)
#By running the app we can calculate minimum audit cost for given parameters or conditions.

#7) (5 points) Preliminary Tests of Transactions Controls

#Used ggplot for visualizing plot and ggarrange function under pubrr package for plotting multiple ggplots.
#Our general assumption is that auditing has normal distribution for the different transactions throughout the companys activities.
#Plotting the transaction distributions:


Sales <- sum(sales_journal$collection_amount)
YE_Inventory <- perpetual_inventory_ledger %>% group_by(sku) %>%
                inner_join(fyear_begin_inventory_ledger, by="sku") %>% slice(n()) %>%
                mutate(inv_extended = unit_cost * stock_on_hand.x) %>%
                ungroup() %>% select(inv_extended)
#Sales
p1 <- plotluck(sales_journal, collection_amount~1) + 
      ggtitle("Plot of Sales") +
      xlab("Sales amount") + ylab("Density") 
#Cash
p2 <- plotluck(deposit_daily,deposit_amount~1) + 
      ggtitle("Plot of Cash") +
      xlab("Cash Amount") + ylab("Density") 
#AR
p3 <- plotluck(fyear_end_ar_ledger,amount~1) +
      ggtitle("Plot of Accounts Receivable") +
      xlab("Amount") + ylab("Density")
p4 <- plotluck(YE_Inventory, inv_extended~1) + ggtitle("Plot of Inventory")

#Returns
p5<-plot(density(sales_journal$sales_return*sales_journal$sales_unit_price),main="Returns")
#CoGs
p6<-plot(density(sales_journal$sales_count*sales_journal$unit_cost),main="COGS")
ggarrange(
  ggarrange(p1, p2, ncol = 2),
  ggarrange(p3, p4, ncol = 2),
  nrow = 2, 
  common.legend = TRUE, legend = "bottom"
  )

hist(sales_journal$sales_return * sales_journal$sales_unit_price)

#Here are the final conclusions based on AICPA's aspects to sampling risk when performing tests of controls:
#1. Assessing control risk is too low represents the risk that an audit sample supports the conclusion that the design and operation of an internal control is effective when in fact it is not.
#2. Assessing control risk too high represents the risk that an audit sample supports the conclusion that the design and operation of an internal control is not effective when in fact it is effective.
#Attribute estimation will help us decide on actual error rate of system that the process is transaction stream and attribute sampling used for correcting samples. 
#If discovery sampling gives that particular transaction stream is out of control

#8) (5 points) Employee Expenditures Audit
#identify omissions or duplicates for Expense Receipt numbers.
expenditures <-read.csv("~/R Projects/523_midterm_files/expenditures.csv", na.strings="0", stringsAsFactors=FALSE)
duplicates <- duplicated(expenditures$exp_no)
raw <- seq(1,333)
exp_no <- expenditures$exp_no
expense_dups <- cbind.data.frame(exp_no,raw,duplicates)
ggplot(expense_dups, aes(x=exp_no, y=raw, col=duplicates)) +
  geom_point()

expense <- as.numeric(substring(expenditures$exp_no, 2))
expense_min <- as.numeric(min(expense))
expense_max <- as.numeric(max(expense))

omit <- as.data.frame(setdiff(expense_min:expense_max, expense))
n <- nrow(omit)
dups <- length(duplicates[duplicates==TRUE])
cat("\n # of duplicate expense records = ", dups)
cat("\n # of omitted expense records = ", n)

#Reading expenditure data to get idea to do Benford test
hist(expenditures$amount)

#Benford Test is conducted for employee expenditure auditing.

#For Workers and Staff, the authorised limit is $50 and therefore has taken no of digits as 1 for first test.

lead_digit <- extract.digits(expenditures$amount, number.of.digits=1)
lead_digit <- lead_digit[,2]
hist(lead_digit, breaks=9)
ben <- benford(expenditures$amount, number.of.digits=1)
plot(ben)

#Result suggests that digits distribution second order test for 5 remains under controlled observations of Benford Test 
#and therefore suggests that the number 50 occurred well within its limits.
#For Line supervisors, the authorized limit is $500   
#Here, we have taken the number of digits as 2 being consistent to the Benford Testing logic.

lead_digit <- extract.digits(expenditures$amount, number.of.digits=2)
lead_digit <- lead_digit[,2]
hist(lead_digit, breaks=9)
ben <- benford(expenditures$amount, number.of.digits=2)
plot(ben)

#Results suggests that the digits 50 were in the data more often than expected and hence, 
#classified on the employees for all contracts with 50 as the first two digits to find several line supervisors who
#submitted an excessive number of expense reports for $500.

lead_digit <- extract.digits(expenditures$amount, number.of.digits=2)
employee_check <- cbind(expenditures,lead_digit)
employee_check[employee_check$data.digits ==50,] %>%
arrange(employee_no) %>%
select(employee_no, date, amount)
#displays the amount for each employee_no with respective to date

#9)
#Invoice Number
dup_sales <- sales_journal[duplicated(sales_journal$invoice_no), ]
n <- nrow(dup_sales)
cat("\n # of duplicate sales = ", n)
dup_sales <- (n/nrow(sales_journal))
#4.7% of duplicates of invoice number

invoice <- as.numeric(substring(sales_journal$invoice_no, 2))
invoice_min <- as.numeric(min(invoice))
invoice_max <- as.numeric(max(invoice))
omit_sales <- as.data.frame(setdiff(invoice_min:invoice_max, invoice))
n <- nrow(omit)
cat("\n # of omitted sales records = ", n)
omit_sales <- (n/nrow(sales_journal))
#5.02% of omissions for invoice number

# Shipping numbers - 
dup_shipment <- sales_journal[duplicated(sales_journal$shipper_no), ]
n <- nrow(dup_shipment)
cat("\n # of duplicate shipments = ", n)
dup_shipment <- (n/nrow(sales_journal))
#4.7% of duplicate shipping numbers 
shipments <- as.numeric(substring(sales_journal$shipper_no, 2))
shipments_min <- as.numeric(min(shipments))
shipments_max <- as.numeric(max(shipments))
omit_ship <- as.data.frame(setdiff(shipments_min:shipments_max, shipments))
n <- nrow(omit_ship)
cat("\n # of omitted shipments = ", n)
omit_ship <- (n/nrow(sales_journal))
#5.02% of omission for shipping numbers

# Collection Receipt numbers - 
dup_collections <- collections_journal[duplicated(collections_journal$collection_no), ]
n <- nrow(dup_collections)
cat("\n # of duplicate receipts = ", n)
dup_collections <- (n/nrow(collections_journal))
#4.7% of duplicate of collection receipt number
collections <- as.numeric(substring(collections_journal$collection_no, 2))
collections_min <- as.numeric(min(collections))
collections_max <- as.numeric(max(collections))
omit_collections <- as.data.frame(setdiff(collections_min:collections_max, collections))
n <- nrow(omit_collections)
cat("\n # of omitted receipts = ", n)
omit_collections <- (n/nrow(collections_journal))
#0% of omissions of collection receipt numbers

# Customers with credit balances
custwithcreditBal <- collections_journal[collections_journal$sales_extended<0,]
dup_custCreditBal <- custwithcreditBal[duplicated(custwithcreditBal$sales_extended), ]
n <- nrow(dup_custCreditBal)
cat("\n # of duplicate Customers with credit balances = ", n)
dup_custCreditBal <- (n/nrow(collections_journal))
# 0% duplicates for customer with credit balances
#Calculating omissions
n <- sum(is.na(custwithcreditBal$customer_no))
cat("\n # of omitted Customers with credit balance = ", n)
omit_custwithcreditBal <- (n/nrow(collections_journal))
# 0% omissions for customers with credit balance 

#Summary for duplicate
Dups <- data.frame("Transaction" = c("Invoice","Shipment","CollectionReceipt","CustomerWCredit"), "Duplicates" = c(dup_sales,dup_shipment,dup_collections,dup_custCreditBal))
Dups <- Dups %>% mutate(
Txn_Status = ifelse(`Duplicates`>0.01, "Out-of-control", "In-control"),
Duplicates = round(Duplicates, digits = 4))
print(Dups)
#Summary for ommissions
Omits <- data.frame("Transaction" = c("Invoice","Shipment","CollectionReceipt","CustomerWCredit"), "Omissions" = c(omit_sales,omit_ship,omit_collections, omit_custwithcreditBal))
Omits <- Omits %>% mutate( Txn_Status = ifelse(`Omissions`>0.01, "Out-of-control", "In-control"),
                           Omissions = round(Omissions, digits = 4))
print(Omits)

# 10. (10 points) Error Rates in Sales Amounts

#Computing the error rates in Sales Amounts 
# set the sample size based on the discovery sample algorithm
sample_size = 59 

real_world_sales <- read.csv("~/R Projects/523_midterm_files/real_world_credit_sales.csv", na.strings="0", stringsAsFactors=FALSE) %>%
  select(invoice_no,sales_unit_price,sales_count)

sales_journal <-read_csv("~/R Projects/523_midterm_files/sales_journal.csv", col_types = cols(X1 = col_skip()))

sales_temp <- split(sales_journal,sales_journal$cash_not_ar)
sales_journal <- sales_temp$`0` ## select only credit sales

discovery_sample_sales <- sales_journal[runif(sample_size,1,nrow(sales_journal)),] %>%
  select(invoice_no, sales_unit_price, sales_count)

audited_sample <- left_join(discovery_sample_sales, real_world_sales, by="invoice_no") %>%
  select(invoice_no, sales_unit_price.x, sales_count.x, sales_unit_price.y, sales_count.y)

exceptions <- audited_sample %>%
  filter(sales_unit_price.x != sales_unit_price.y |
           sales_count.x != sales_count.y 
  )

error_rate_sales <- nrow(exceptions) / nrow(audited_sample)
cat("\n \n \n Discovery: error rate in sales:", error_rate_sales)
error_sales_amt <-
  100 * sum(exceptions$sales_count.y * exceptions$sales_count.y ) /
  sum(audited_sample$sales_unit_price.x * audited_sample$sales_count.x)
cat("\n \n Discovery: amount for sales sample is overstated (i.e., is in error):",
    prettyNum(error_sales_amt, big.mark=","),
    " %")

#Performing tests with discovery sampling, we see that the error rates are 0%, which indicates tolerable error amounts in 
#sales and to be sure of that have performed attribute sampling t-tests which is usually performed when the error rates are 
#identified to be 'out-of-control'

#Attribute Sampling with t-Tests
  
#Error estimates from attribute samples may either be rates of erroneous transactions or from a monetary unit sampling perspective, can be rates of monetary error in the transaction stream. 

# Modify directory Path 

real_world_sales <- read.csv("~/R Projects/523_midterm_files/real_world_credit_sales.csv", na.strings="0", stringsAsFactors=FALSE)
sales_journal <-
  read_csv("~/R Projects/523_midterm_files/sales_journal.csv",
           col_types = cols(X1 = col_skip()))
sales_temp <- split(sales_journal,sales_journal$cash_not_ar)

sales_journal <- sales_temp$`0`
# data set size
size <- as.numeric(nrow(sales_journal)) 
# detect 5% occurrence error
Delta <- .05*size 
# variability (guess ~1/3 rd)
sigma <- .3*size 
effect <- Delta/sigma
# look for overstatement of earnings
sample <- pwr.t.test( d=effect,sig.level = 0.05,power = .8, type="one.sample", alternative="greater") 
cat("\n \n Attribute sample size for occurrence of error = ", ceiling(sample$n))
# data set size
size <- as.numeric(sum(sales_journal$sales_unit_price*sales_journal$sales_count)) 
# average value of transaction
mu <- mean(sales_journal$sales_unit_price*sales_journal$sales_count) 
# detect 5% amount intolerable error
Delta <- .05*mu 
# variability
sigma <- sd(sales_journal$sales_unit_price*sales_journal$sales_count) 
effect <- Delta/sigma
# look for sales value too large
sample <- pwr.t.test(d=effect,sig.level = 0.05,power = .8,type="one.sample",alternative="greater") 
cat("\n\n Attribute sample size for amount of error = ", ceiling(sample$n))

#The discovery sample size was 59, an additional $224- 58 = 166 records are required for attribute tests of occurrence rate
#and $636 - 58 = $578 records are required for attribute tests of amount rate.
discovery_sample_sales_add <- sales_journal[runif(166,1,nrow(sales_journal)),] %>% 
  select(invoice_no, sales_unit_price, sales_count)

discovery_sample_sales_occ <- rbind( data.frame(discovery_sample_sales_add), data.frame(discovery_sample_sales))

audited_sample <-left_join(discovery_sample_sales_occ,real_world_sales, by="invoice_no") %>%
  select(invoice_no,sales_unit_price.x,sales_count.x,sales_unit_price.y,sales_count.y)
exceptions <- audited_sample %>% filter(sales_unit_price.x != sales_unit_price.y |sales_count.x != sales_count.y)
error_rate_sales <- nrow(exceptions) / nrow(audited_sample)
cat("\n \n \n Attribute: error rate in sales: ", error_rate_sales)

discovery_sample_sales_add <- sales_journal[runif(578,1,nrow(sales_journal)),] %>%
  select(invoice_no,sales_unit_price, sales_count)
discovery_sample_sales_amt <- rbind( data.frame(discovery_sample_sales_add),data.frame(discovery_sample_sales))

audited_sample <- left_join(discovery_sample_sales_amt,real_world_sales,by="invoice_no") %>%
  select(invoice_no,sales_unit_price.x,sales_count.x,sales_unit_price.y,sales_count.y)
exceptions <-audited_sample %>% filter(sales_unit_price.x != sales_unit_price.y |sales_count.x != sales_count.y)
error_sales_amt <-100 * sum(exceptions$sales_count.y * exceptions$sales_count.y ) /
  sum(audited_sample$sales_unit_price.x * audited_sample$sales_count.x)
cat("\n \n \n Attribute: amount sales sample is overstated (i.e., is in error) ",
    prettyNum(error_sales_amt, big.mark=","),
    " %")

#The out-of-control critical value with a confidence of 95%  is 0.05 error rate, thus both in occurrence and 
#amount auditor determines sales system to be in-control.

#11 A part)
receiver_journal<-read.csv('receiver_journal.csv')
View(receiver_journal)
receiver_journal %>% distinct(ro_no, .keep_all = TRUE)
View(receiver_journal)
duplicated(receiver_journal)
receiver_journal[duplicated(receiver_journal$ro_no),]
a<-dim(receiver_journal[duplicated(receiver_journal$ro_no),])[1]
#there are 0 duplicate rows in receiver_journal
percent<-a/nrow(receiver_journal)
percent
# there are 0 % of duplicate rows in receiver_journal
receiver<-as.numeric(substring(receiver_journal$receiver_no, 4))
receiver_min<-as.numeric(min(receiver))
receiver_max<-as.numeric(max(receiver))
omit<-as.data.frame(setdiff(receiver_min:receiver_max, receiver))
n<-nrow(omit)
cat("\n # of omitted receiver records = ", n)
n<-n/nrow(receiver_journal)
n
# there are no omissions in receiver_journal ie 0%

purchase_journal<-read.csv('purchase_journal.csv')
View(purchase_journal)
purchase_journal %>% distinct(po_no, .keep_all = TRUE)
View(purchase_journal)
duplicated(purchase_journal)
purchase_journal[duplicated(purchase_journal$po_no),]
b<-dim(purchase_journal[duplicated(purchase_journal$po_no),])[1]
b
# there are 7 duplicate rows in purchase_journal
percent1<-b/nrow(purchase_journal)
percent1
# there are 4.6% of duplicate rows in purchase_journal
po<-as.numeric(substring(purchase_journal$po_no, 2))
po_min<-as.numeric(min(po))
po_max<-as.numeric(max(po))
omit<-as.data.frame(setdiff(po_min:po_max, po))
n1<-nrow(omit)
cat("\n # of omitted purchase records = ", n1)
# there are 6 omissions in purchase_journal
n1<-n1/nrow(purchase_journal)
n1
# 3.9% of omissions for purchase journal

#summarise for duplicates of receiver number and purchase order
Dups <- data.frame("Transaction" = c("ReceiverNumbers","PurchaseOrders"), "Duplicates" = c(percent,percent1))
Dups <- Dups %>% mutate(
  Txn_Status = ifelse(`Duplicates`>0.01, "Out-of-control", "In-control"),
  Duplicates = round(Duplicates, digits = 4))
print(Dups)

#summarise for omissions of receiver number and purchase order
Omits <- data.frame("Transaction" = c("ReceiverNumbers","PurchaseOrders"), "Omissions" = c(n, n1))

Omits <- Omits %>% mutate( Txn_Status = ifelse(`Omissions`>0.01, "Out-of-control", "In-control"),
                           Omissions = round(Omissions, digits = 4))
print(Omits)

#11 B part)
perpetual_inventory_ledger <-
  read_csv("~/R Projects/523_midterm_files/perpetual_inventory_ledger.csv",
           col_types = cols(X1 = col_skip()))

fyear_begin_inventory_ledger <- read_csv("~/R Projects/523_midterm_files/fyear_begin_inventory_ledger.csv",
                                         col_types = cols(X1 = col_skip()))

real_world_ye_inventory <- read_csv("~/R Projects/523_midterm_files/real_world_ye_inventory.csv",
                                    col_types = cols(X1 = col_skip()))

perpetual_inventory_ledger$date <- as.Date(perpetual_inventory_ledger$date)

Interim_Inventory <-perpetual_inventory_ledger %>% group_by(sku) %>%
  inner_join(fyear_begin_inventory_ledger, by="sku") %>% 
  filter(date >= "2020-10-01" & date >= "2020-10-31") %>%
  mutate(inv = unit_cost * stock_on_hand.x)%>%
  summarise(AnnualInv = sum(inv))

Annual_Inventory <-perpetual_inventory_ledger %>% group_by(sku) %>%
  inner_join(fyear_begin_inventory_ledger, by="sku") %>%
  filter(date < "2020-12-01") %>%
  mutate(inv = unit_cost * stock_on_hand.x)%>%
  summarise(AnnualInv = sum(inv))

table <- bind_cols(Interim_Inventory,Annual_Inventory[,2],.id=NULL)
names(table)[names(table) == "AnnualInv"] <-  "InterimInventory"
names(table)[names(table) == "AnnualInv1"] <-  "AnnualInventory"
TimesFive = 5 * Interim_Inventory$AnnualInv
table<-data.frame(Interim_Inventory$sku,Interim_Inventory$AnnualInv,Annual_Inventory$AnnualInv, TimesFive)
mutate(table, LowTurnover = ifelse(TimesFive > Annual_Inventory$AnnualInv, "Yes", "No"))
table

#12) (5 points) Inventory Stock on Hand Lower of Cost or Market
real_world_ye_inventory <- read_csv("~/R Projects/523_midterm_files/real_world_ye_inventory.csv",
                                    col_types = cols(X1 = col_skip()))

real_world_ye_inventory[is.na(real_world_ye_inventory)] <- 0
inventory_count_differences <- perpetual_inventory_ledger %>%
  group_by(sku) %>%
  slice(n()) %>% ## the final slice, by SKU, will be what is in-stock at year end
  left_join(real_world_ye_inventory, by="sku") %>%
  filter(exception != "No exception, count is accurate") %>%
  mutate(err_perpetual = stock_on_hand - ye_stock_on_hand) %>%
  select(sku,stock_on_hand,ye_stock_on_hand,unit_cost,actual_unit_market,err_perpetual,count_exception,
         exception) %>%
  mutate(ye_cost = stock_on_hand * unit_cost,
         ye_market = ye_stock_on_hand * actual_unit_market,
         inv_markdown = ye_cost - ye_market) %>%
  select(sku, ye_cost, ye_market, inv_markdown) %>%
  as.data.frame() 

total_inv_markdown <- sum(inventory_count_differences$inv_markdown)
cat("\n Total Amount to Markdown Client's Trial Balance Inventory Amount(by LOCOM rule) = ",total_inv_markdown)
#By LOCOM rule following inventory items where net realizable value is less than 110% of cost and arrived at the Total Markdown for LOCOM rule.
kable(inventory_count_differences, caption = "Market and Cost of Inventory by SKU", booktabs = T) %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote (general = "",
            symbol = c(round(total_inv_markdown,2)),
            general_title = "Total Markdown for LOCOM rule",
            title_format = c("italic", "underline")
  )
#Here is the market and cost of inventory by sku mentioning total markdown for LOCOM rule

#13)
#Included in word document