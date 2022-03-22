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
ggsave("fig/9food_trade_USD.png")

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
ggsave("fig/7food_exports_CPO.png")

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
twxxx2020<-filter(twxxx,Year==2020) %>%
  mutate(USDPCT=paste0(round(USD/sum(USD)*100,2),"%"))


## Top 5 in 2020
twx2020$millUSD<-twx2020$USD/1000000
twxxx2020$millUSD<-twxxx2020$USD/1000000
twm2020$millUSD<-twm2020$USD/1000000
fx<-twx2020 %>% group_by(final) %>% slice_max(order_by = USD, n = 5)
fxx<-twxxx2020 %>% group_by(final) %>% slice_max(order_by = USD, n = 5)
fm<-twm2020 %>% group_by(final) %>% slice_max(order_by = USD, n = 5)
ffx<-fx%>%filter(final==1)
fix<-fx%>%filter(final==0)
ffm<-fm%>%filter(final==1)
fim<-fm%>%filter(final==0)
ffxx<-fxx%>%filter(final==1)
fixx<-fxx%>%filter(final==0)

# THE PLOT with CPO!
aa<-labs(y="Million USD",x="Products")
bb<-ylim(0,13000)

a<-ggplot(ffx, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods exports")+
  aa+bb+
  scale_x_discrete(breaks=c("210690","190530","160520","151790","041000"),labels=c(
    "Food prep",
    "Biscuits",
    "canned seafood",
    "Margarine",
    "Bird's nest"
  ))

b<-ggplot(ffm, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods imports")+
  aa+bb+
  scale_x_discrete(breaks=c("210690","170199","100640","040221","020230"),labels=c(
    "Food prep",
    "Sucrose",
    "Rice",
    "Milk powder",
    "Beef"
  ))

c<-ggplot(fix, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods exports")+
  aa+bb+
  scale_x_discrete(breaks=c("151329","151190","151110","090111","030613"),labels=c(
    "Refined palm\nkernel oil",
    "Refined\npalm oil",
    "Crude palm\noil",
    "Coffee",
    "Frozen\nsea food"
  ))
d<-ggplot(fim, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods imports")+
  aa+bb+
  scale_x_discrete(breaks=c("230400","170111","120100","100190","070320"),labels=c(
    "Animal feed\n(soybean)",
    "Raw sugar",
    "Soybean",
    "Wheat",
    "Garlic"
  ))

f<-a+b+c+d
f<-f+plot_annotation(
  title = "Top 5 Indonesia's food trade in 2020",
  caption = 'Source: UN Comtrade Database'
) &
  theme_classic()
f
## Saving the plot
ggsave("fig/6final.png",f)

# THE PLOT without CPO!

bxb<-ylim(0,2650)

aaa<-ggplot(ffxx, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods exports")+
  aa+bxb+
  scale_x_discrete(breaks=c("210690","190530","160520","030420","041000"),labels=c(
  "Food prep",
  "Biscuits",
  "canned seafood",
  "Frozen fish\nfillet",
  "Bird's nest"
))

bbb<-ggplot(ffm, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Final goods imports")+
  aa+bxb+
  scale_x_discrete(breaks=c("210690","170199","100640","040221","020230"),labels=c(
    "Food prep",
    "Sucrose",
    "Rice",
    "Milk powder",
    "Beef"
  ))


ccc<-ggplot(fixx, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods exports")+
  aa+bxb+
  scale_x_discrete(breaks=c("180400","080290","090111","030799","030613"),labels=c(
  "Cocoa\nbutter",
  "Candle nut",
  "Coffee",
  "Dried seafood",
  "Frozen seafood"
))
ddd<-ggplot(fim, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Intermediate goods imports")+
  aa+bxb+
  scale_x_discrete(breaks=c("230400","170111","120100","100190","070320"),labels=c(
    "Animal feed\n(soybean)",
    "Raw sugar",
    "Soybean",
    "Wheat",
    "Garlic"
  ))


fff<-aaa+bbb+ccc+ddd
fff<-fff+plot_annotation(
  title = "Top 5 Indonesia's food trade in 2020",
  caption = 'Source: UN Comtrade Database'
) &
  theme_classic()
fff
## Saving the plot
ggsave("fig/8final_noCPO.png",fff)


###### REG ###########
lol<-fin %>% pivot_wider(names_from = c(final,Flow), values_from = wew) %>%
  rename(intx=`0_Export`,intm=`0_Import`,fx=`1_Export`,fm=`1_Import`)
summary(lol)
#%>%
#  mutate(fx=log(fx),intm=log(intm),fm=log(fm),intx=log(intx))

dfx<-diff(log(lol$fx))*100
dintx<-diff(log(lol$intx))*100
dintm<-diff(log(lol$intm))*100
dfm<-diff(log(lol$fm))*100

Year<-c(2003:2020)

lold<-tibble(Year,dfx,dintx,dintm,dfm)
lolt<-ts(tibble(dfx,dintx,dintm,dfm),start=2003)

model<-ARDL::auto_ardl(dfx~dintm+dfm, data=lold, max_order = 5)
modell<-ARDL::auto_ardl(dintx~dintm+dfm, data=lold, max_order = 5)
modelll<-ARDL::auto_ardl(dintm~dfx+dintx, data=lold, max_order = 5)
model1<-ARDL::ardl(dfx~dintm+dfm,data=lold,order=c(1,5,1))
model1a<-ARDL::ardl(dintx~dintm+dfm,data=lold,order=c(3,4,3))

## Save regression results in csv and doc (same content)
write_csv(broom::tidy(model1),"model1.csv")
write_csv(broom::tidy(model1a),"model2.csv")
sjPlot::tab_model(model1,
                  dv.labels = "growth of final good exports",
                  file="tabel2.html")
sjPlot::tab_model(model1a,
                  dv.labels = "growth of intermediate good exports",
                  file="tabel3.html")

sjPlot::tab_model(model1,model1a,
                  dv.labels = c("growth of final good exports","growth of intermediate good exports"),
                  file="fig/3tabel2a.html")

ARDL::bounds_f_test(model1,case=3) ## F=26.728, p~0, hence reject no cointegration
ARDL::bounds_f_test(model1a,case=3) ## F=2.0816, p=0.5193

loldg<-lold%>%pivot_longer(!Year,names_to = "what",values_to = "values")
breakzz<-c("dfx","dintx","dfm","dintm")
labzz<-c(
  "final good exports",
  "intermediate good exports",
  "final good imports",
  "intermediate good impots"
  )
loldg%>%ggplot(aes(x=Year,y=values))+geom_line(aes(color=what,linetype=what),size=1)+
  labs(
    x="Year",
    y="%",
    title = "Percent change of export and import of final and intermediate goods",
    caption="source: UN Comtrade database"
  )+theme_classic()+
  scale_color_discrete(breaks=breakzz,labels=labzz)+
  scale_linetype_discrete(breaks=breakzz,labels=labzz)+
  theme(legend.position = "bottom",legend.title = element_blank())
ggsave("fig/10growth.png")



#======================= EXPERIMENTS =====================================
#model5<-BVAR::bvar(lolt,lags=2)
#xtable::xtable(model5)
#plot(lolt)
#data5<-ts(lold%>%dplyr::select(!Year),start=2003)

#model2<-dynlm::dynlm(dfx~L(dfx,1:2)+L(dintm,0:5)+L(dfm,0:0), data=data5)

#model3<-ARDL::ardl(dintx~dintm+dfm,data=lold,order=c(3,4,3))
#model4<-dynlm::dynlm(dintx~L(dfx,1:2)+L(dintm,0:2)+L(dfm,0:2), data=data5)

#model5<-BVAR::bvar(data5,lags=5)
#model5a<-vars::VAR(data5,lag=2)
#predict5<-predict(model5,horizon=5)
#irf5<-BVAR::irf(model5,horizon=5,identification=TRUE)

## TARIFF WTO

#readxl::read
#tariff1<-readxl::read_excel("tariff_use.xlsx",sheet = 1)
#tariff2<-readxl::read_excel("tariff_use.xlsx",sheet = 2)
#tariff1%>%distinct(HSV)
#tarifff1<-tariff1 %>%
#  mutate(HS = case_when(
#    HSV == "HS17" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS5", destination = "HS1", dest.digit = 6),
#    HSV == "HS12" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS4", destination = "HS1", dest.digit = 6),
#    HSV == "HS07" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS3", destination = "HS1", dest.digit = 6),
#    HSV == "HS02" ~ concord_hs(tariff1,sourcevar =  HSC, origin = "HS2", destination = "HS1", dest.digit = 6),
#  ))

