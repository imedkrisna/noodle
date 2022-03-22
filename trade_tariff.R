## devtools::install_github("insongkim/concordance", dependencies = TRUE)
library("tidyverse")
library("concordance")
library("patchwork")
library("ggthemes")

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
trade2<-filter(trad,HS %in% c(16:24,2501))
trade<-filter(trad,HS %in% con$HS)%>%
  filter(nchar(HS)==6)

## ADDS BEC & final
trade$BEC<-concord_hs_bec(trade$HS,origin = "HS" ,destination = "BEC4",dest.digit = 3)
trade$final<-ifelse(trade$BEC==112,1,ifelse(trade$BEC==122,1,0))
trade<-trade %>% filter(str_detect(BEC, "^1"))

## Separate export & import
### Finxx=finx without CPO
tw<-trade%>%filter(Partner == "World")
ta<-trade%>%filter(Partner != "World")
twx<-filter(tw,Flow=="Export") %>% arrange(-USD)
twxx<-twx %>% mutate(final=replace(final, str_detect(HS,"^15")==TRUE,2))
twxxx<-twxx %>% filter(final!=2)
twm<-filter(tw,Flow=="Import") %>% arrange(-USD)

## Cek food import
twm2<-trade2%>%filter(Partner == "World")%>%filter(Flow=="Import") %>% arrange(-USD)

### By USD
finx<-twxx%>%group_by(Year,final)%>%summarise(wew=sum(milUSD))
finx$Flow<-"Export"
finxx<-finx%>%filter(final!=2)
finxx$Flow<-"Export"
finm<-twm%>%group_by(Year,final)%>%summarise(wew=sum(milUSD))
finm$Flow<-"Import"
fin<-rbind(finxx,finm)

finm2<-twm2%>%group_by(Year)%>%summarise(wew=sum(milUSD))
finm2$Flow<-"Import"
fin2<-rbind(finxx,finm2)

###### REG ###########
lol<-fin %>% pivot_wider(names_from = c(final,Flow), values_from = wew) %>%
  rename(intx=`0_Export`,intm=`0_Import`,fx=`1_Export`,fm=`1_Import`) %>%
  mutate(fx=log(fx),intm=log(intm),fm=log(fm))
dfx<-diff(lol$fx)*100
dintm<-diff(lol$intm)*100
dfm<-diff(lol$fm)*100
Year<-c(2003:2020)
lold<-tibble(Year,dfx,dintm,dfm)
lold$dum<-0
lold<-lold%>%mutate(dum=ifelse(Year>=2012,1,0))
model1<-auto_ardl(dfx~dintm+dfm, data=lold, max_order = 5)
ggplot()+
  geom_line(aes(lold$Year,lold$dfx),color='red')+
  geom_line(aes(lold$Year,lold$dintm),color='blue')+
  geom_line(aes(lold$Year,lold$dfm),color='green')
model2<-lm(dfx~dintm+dfm, data=lold)
summary(model2)

loldb<-lold%>%filter(Year<2012)
lolda<-lold%>%filter(Year>2012)
model3<-lm(dfx~dintm+dfm, data=loldb)
model4<-lm(dfx~dintm+dfm, data=lolda)

### By ton
tfinx<-twxx%>%group_by(Year,final)%>%summarise(wew=sum(ton))
tfinx$Flow<-"Export"
tfinxx<-tfinx%>%filter(final!=2)
tfinxx$Flow<-"Export"
tfinm<-twm%>%group_by(Year,final)%>%summarise(wew=sum(ton))
tfinm$Flow<-"Import"
tfin<-rbind(tfinxx,tfinm)

### Without CPO, exports & imports side-by-side, USD
ggplot(fin,aes(Year,wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.5)+
  facet_wrap(~Flow,as.table = FALSE) +
  theme_test()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(title="Indonesian Food Exports and Imports, 2002-2020",
       subtitle="BEC rev4 classification, without palm oil products",
       caption = "Source: UN Comtrade Database",
       y="Million USD")+
  scale_color_discrete(labels=c("intermediate goods","final goods"))+
  scale_linetype_discrete(labels=c("intermediate goods","final goods"))
ggsave("food_trade_USD.png")

### Without CPO, exports & imports side-by-side, ton
ggplot(tfin,aes(Year,wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.5)+
  facet_wrap(~Flow,as.table = FALSE) +
  theme_test()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(title="Indonesian Food Exports and Imports, 2002-2020",
       subtitle="BEC rev4 classification, without palm oil products",
       caption = "Source: UN Comtrade Database",
       y="Ton")+
  scale_color_discrete(labels=c("intermediate goods","final goods"))+
  scale_linetype_discrete(labels=c("intermediate goods","final goods"))
ggsave("food_trade_kg.png")

### Exports only, intermediate, final, CPO
ggplot(finx,aes(x=Year,y=wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.2)+
  theme_classic()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(title="Indonesian Food Exports, 2002-2020",
       subtitle="BEC rev4 + HS 15",
       caption = "Source: UN Comtrade Database",
       y="Million USD")+
  scale_color_discrete(labels=c("intermediate goods","final goods","Palm oil related goods"))+
  scale_linetype_discrete(labels=c("intermediate goods","final goods","Palm oil related goods"))
ggsave("food_exports_CPO.png")

xxx<-ggplot(finxx,aes(x=Year,y=wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.2)
mmm<-ggplot(finm,aes(x=Year,y=wew,color=factor(final),linetype=factor(final)))+geom_line(size=1.2)
food<-xxx+mmm+plot_layout(ncol=2,heights = unit(200, "point")) &
  theme_classic()+
  theme(legend.position = "bottom",legend.title = element_blank())

## Cek persentase impor
twm2020<-filter(twm,Year==2020) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twx2020<-filter(twx,Year==2020) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twm2010<-filter(twm,Year==2010) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twx2010<-filter(twx,Year==2010) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twm2005<-filter(twm,Year==2005) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twx2005<-filter(twx,Year==2005) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twm2011<-filter(twm,Year==2011) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twx2011<-filter(twx,Year==2011) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))
twm2012<-filter(twm,Year==2012) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))%>%
  rename(newUSD=USD)
twx2012<-filter(twx,Year==2012) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))

## Top 5 in 2020
twx2020$millUSD<-twx2020$USD/1000000
twm2020$millUSD<-twm2020$USD/1000000
fx<-twx2020 %>% group_by(final) %>% slice_max(order_by = USD, n = 5)
fm<-twm2020 %>% group_by(final) %>% slice_max(order_by = USD, n = 5)
ffx<-fx%>%filter(final==1)
fix<-fx%>%filter(final==0)
ffm<-fm%>%filter(final==1)
fim<-fm%>%filter(final==0)

# THE PLOT!
aa<-labs(y="Million USD",x="Products")
bb<-ylim(0,13000)

a<-ggplot(ffx, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods exports")+
  aa+bb+
  scale_x_discrete(breaks=c("210690","190230","160414","151790","041000"),labels=c(
    "Tofu etc",
    "Noodle",
    "Fisn,can",
    "Margarine",
    "Other \n animal"
  ))

b<-ggplot(ffm, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods imports")+
  aa+bb+
  scale_x_discrete(breaks=c("210690","080810","080610","070320","020230"),labels=c(
    "Food prep",
    "Apples",
    "Grapes",
    "Garlic",
    "Beef"
  ))
c<-ggplot(fix, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods exports")+
  aa+bb+
  scale_x_discrete(breaks=c("180400","151329","151190","151110","090111"),labels=c(
    "Cocoa\nbutter",
    "Palm kernel\noil",
    "Palm oil",
    "CPO",
    "Coffee"
  ))
d<-ggplot(fim, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods imports")+
  aa+bb+
  scale_x_discrete(breaks=c("190190","180100","170230","040410","040210"),labels=c(
    "Flour",
    "Cocoa\nbeans",
    "Sugar",
    "Whey",
    "Milk"
  ))

f<-a+b+c+d
f<-f+plot_annotation(
  title = "Top 5 Indonesia's food trade in 2020",
  caption = 'Source: UN Comtrade Database'
) &
  theme_classic()
f
## Saving the plot
ggsave("final.png",f)

## TARIFF WTO

readxl::read
tariff1<-readxl::read_excel("tariff_use.xlsx",sheet = 1)
tariff2<-readxl::read_excel("tariff_use.xlsx",sheet = 2)
tariff1%>%distinct(HSV)
tarifff1<-tariff1 %>%
  mutate(HS = case_when(
    HSV == "HS17" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS5", destination = "HS1", dest.digit = 6),
    HSV == "HS12" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS4", destination = "HS1", dest.digit = 6),
    HSV == "HS07" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS3", destination = "HS1", dest.digit = 6),
    HSV == "HS02" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS2", destination = "HS1", dest.digit = 6),
  ))
