## get trade data
library("tidyverse")
setwd('uncomtrade')

temp = list.files(pattern="*.zip")
myfiles = lapply(temp, read_csv)
trad<-dplyr::bind_rows(myfiles) %>%
  select('Classification','Year','Trade Flow','Partner','Partner ISO','Commodity Code',
         'Commodity','Netweight (kg)','Trade Value (US$)') %>%
  rename(Flow=`Trade Flow`,ISO=`Partner ISO`,HS=`Commodity Code`,
         Kg=`Netweight (kg)`,USD=`Trade Value (US$)`)
setwd('..')
ukraine<-filter(trad,Partner == "Ukraine")
ukraine<-filter(ukraine,nchar(HS)==2)
russia<-filter(trad,Partner == "Russian Federation")
russia<-filter(russia,nchar(HS)==4)


ux5<-ukraine %>% filter(Flow=="Export") %>% group_by(Year) %>% slice_max(order_by = USD, n = 4)
um5<-ukraine %>% filter(Flow=="Import") %>% group_by(Year) %>% slice_max(order_by = USD, n = 4)
rx5<-russia %>% filter(Flow=="Export") %>% group_by(Year) %>% slice_max(order_by = USD, n = 4)
rm5<-russia %>% filter(Flow=="Import") %>% group_by(Year) %>% slice_max(order_by = USD, n = 4)

## UX: 2604, 1511, 1517

## UM
