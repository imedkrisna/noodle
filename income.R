## devtools::install_github("insongkim/concordance", dependencies = TRUE)
library("tidyverse")
library("concordance")
library("patchwork")
library("ggthemes")
library("sjPlot")
library("ARDL")
library("dynlm")
library("kableExtra")
## Concordance

## Cite
### Steven Liao, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance.
### R package version 2.0.0. https://CRAN.R-project.org/package=concordance



con10<-data.frame(concord_hs_isic(sourcevar = "10",
                                  origin = "ISIC4", destination = "HS1",
                                  dest.digit = 6, all = TRUE)) %>%
  rename(HS=X10.match, weight=X10.weight)
con10$ISIC<-10

con11<-data.frame(concord_hs_isic(sourcevar = "11",
                                  origin = "ISIC4", destination = "HS1",
                                  dest.digit = 6, all = TRUE)) %>%
  rename(HS=X11.match, weight=X11.weight)
con11$ISIC<-11

con01<-data.frame(concord_hs_isic(sourcevar = "01",
                                  origin = "ISIC4", destination = "HS1",
                                  dest.digit = 6, all = TRUE)) %>%
  rename(HS=X01.match, weight=X01.weight)
con01$ISIC<-01

con<-rbind(con01,con10,con11,c("250100","08","08")) %>% select(-weight)


## get trade data

setwd('uncomtrade_HS1')

temp = list.files(pattern="*.zip")
myfiles = lapply(temp, read_csv)
trad<-dplyr::bind_rows(myfiles) %>%
  select('Classification','Year','Trade Flow','Partner','Partner ISO','Commodity Code',
         'Commodity','Netweight (kg)','Trade Value (US$)') %>%
  rename(Flow=`Trade Flow`,ISO=`Partner ISO`,HS=`Commodity Code`,
         kg=`Netweight (kg)`,USD=`Trade Value (US$)`)
trad$milUSD<-trad$USD/1000000
trad$ton<-trad$kg/1000
setwd('..')
trade<-filter(trad,HS %in% con$HS)%>%
  filter(nchar(HS)==6)

## ADDS BEC & final
trade$BEC<-concord_hs_bec(trade$HS,origin = "HS1" ,destination = "BEC4",dest.digit = 3)
## Filter out non food BEC
trade<-trade%>%filter(!BEC %in% c(31,42,63,322)) %>%
  filter(!HS %in% c(852390,520100,382200,400122,230660,140490))
trade$final<-ifelse(trade$BEC==122,1,0)
#trade<-trade %>% filter(str_detect(BEC, "^1"))

## Turn on to check what HS is in each BEC
#gobz<-twm%>%filter(BEC==21)

## Separate export & import
### Finxx=finx without CPO
tw<-trade%>%filter(Partner == "World")
ta<-trade%>%filter(Partner != "World")
twx<-filter(tw,Flow=="Export") %>% arrange(-USD)
twxx<-twx %>% mutate(final=replace(final, str_detect(HS,"^15")==TRUE,2))
twxxx<-twxx %>% filter(final!=2)
twm<-filter(tw,Flow=="Import") %>% arrange(-USD)

### By USD
finx<-twxx%>%group_by(Year,final)%>%summarise(wew=sum(milUSD))
finx$Flow<-"Export"
finxx<-finx%>%filter(final!=2)
finxx$Flow<-"Export"
finm<-twm%>%group_by(Year,final)%>%summarise(wew=sum(milUSD))
finm$Flow<-"Import"
fin<-rbind(finxx,finm) ## Without CPO

## ADDS BEC & final
trade$BEC<-concord_hs_bec(trade$HS,origin = "HS1" ,destination = "BEC4",dest.digit = 3)
## Filter out non food BEC
trade<-trade%>%filter(!BEC %in% c(31,42,63,322)) %>%
  filter(!HS %in% c(852390,520100,382200,400122,230660,140490))
trade$final<-ifelse(trade$BEC==122,1,0)

## add income

income<-readxl::read_excel('income.xlsx') %>% rename(ISO=Code) %>% select(ISO,Income)
trade<-left_join(trade,income,by='ISO')
ta<-trade%>%filter(Partner!="World")

tax<-filter(ta,Flow=="Export") %>% arrange(-USD)
taxx<-tax %>% mutate(final=replace(final, str_detect(HS,"^15")==TRUE,2))
taxxx<-taxx %>% filter(final!=2)
tam<-filter(ta,Flow=="Import") %>% arrange(-USD)

fanx<-taxx%>%group_by(Year,final,Income)%>%summarise(wew=sum(milUSD))
fanx$Flow<-"Export"
fanxx<-fanx%>%filter(final!=2)
fanxx$Flow<-"Export"
fanm<-tam%>%group_by(Year,final,Income)%>%summarise(wew=sum(milUSD))
fanm$Flow<-"Import"
fan<-rbind(fanxx,fanm)%>%na.omit()

ggplot(fan,aes(Year,wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.5)+
facet_grid(Flow~Income,as.table = FALSE) +
  theme_test()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(title="Indonesian Food Exports and Imports, 2002-2020",
       subtitle="BEC rev4 classification, without palm oil products",
       caption = "Source: UN Comtrade Database",
       y="USD")+
  scale_color_discrete(labels=c("intermediate goods","final goods"))+
  scale_linetype_discrete(labels=c("intermediate goods","final goods"))
ggsave("income.png")
###### REG ###########
lol<-fin %>% pivot_wider(names_from = c(final,Flow), values_from = wew) %>%
  rename(intx=`0_Export`,intm=`0_Import`,fx=`1_Export`,fm=`1_Import`)
lel<-fan %>% pivot_wider(names_from = c(final,Flow,Income), values_from = wew) %>%
  rename(intxh=`0_Export_High income`,
         fxh=`1_Export_High income`,
         intmh=`0_Import_High income`,
         fmh=`1_Import_High income`)
#%>%
#  mutate(fx=log(fx),intm=log(intm),fm=log(fm),intx=log(intx))

dfx<-diff(log(lol$fx))*100
dintx<-diff(log(lol$intx))*100
dintm<-diff(log(lol$intm))*100
dfm<-diff(log(lol$fm))*100
dfxh<-diff(log(lel$fxh))*100
dintxh<-diff(log(lel$intxh))*100
dfmh<-diff(log(lel$fmh))*100
dintmh<-diff(log(lel$intmh))*100

Year<-c(2003:2020)

lold<-tibble(Year,dfx,dintx,dintm,dfm,dfxh,dintxh,dfmh,dintmh)
lolt<-ts(tibble(dfx,dintx,dintm,dfm,dfxh,dintxh,dfmh,dintmh),start=2003)



model<-ARDL::auto_ardl(dintxh~dintm+dfm, data=lold, max_order = 5)
model1<-ARDL::ardl(dintxh~dintmh+dfmh,data=lold,order=c(3,4,3))
