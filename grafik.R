## devtools::install_github("insongkim/concordance", dependencies = TRUE)
library("tidyverse")
library("concordance")
library("patchwork")
## Plot TiVA database

ctr<-c('year','IDN','VNM','THA','MYS','SGP')
labz<-c("Indonesia","Malaysia","Singapore","Thailand","Vietnam")

share <- readxl::read_excel("share.xlsx")[ctr] %>% pivot_longer(!year, names_to="countries", values_to="values")
ekspor<-readxl::read_excel('ekspor.xlsx')[ctr] %>% pivot_longer(!year, names_to="countries", values_to="values")
## va<-readxl::read_excel('valueadded.xlsx')[ctr] %>% pivot_longer(!year, names_to="countries", values_to="values")
prod<-readxl::read_excel('produksi.xlsx')[ctr] %>% pivot_longer(!year, names_to="countries", values_to="values")


## Plotting share of foreign value added to exports
ggplot(share, aes(x=year,y=values,color=countries,group=countries))+
  geom_line(size=1.1,aes(linetype=countries)) +
  scale_x_discrete(breaks=c(1995,2000,2005,2010,2015,2018)) +
  labs(title="Share of foreign value added in a countries' food,beverage and tobacco export",
       caption="source: OECD",
       x="Year",
       y="Million USD") +
  theme_classic() +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  scale_color_discrete(labels=labz) +
  scale_linetype_discrete(labels=labz)
ggsave("share.png")

## Plotting gross production
ggplot(prod, aes(x=year,y=values,color=countries,group=countries))+
  geom_line(size=1.1,aes(linetype=countries)) +
  scale_x_discrete(breaks=c(1995,2000,2005,2010,2015,2018)) +
  labs(title="Gross production of food,beverage and tobacco export",
       caption="source: OECD",
       x="Year",
       y="Million USD") +
  theme_classic() +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  scale_color_discrete(labels=labz) +
  scale_linetype_discrete(labels=labz)
ggsave("prod.png")

## Plotting gross export
ggplot(ekspor, aes(x=year,y=values,color=countries,group=countries))+
  geom_line(size=1.1,aes(linetype=countries)) +
  scale_x_discrete(breaks=c(1995,2000,2005,2010,2015,2018)) +
  labs(title="Gross exports of food,beverage and tobacco",
       caption="source: OECD",
       x="Year",
       y="Million USD") +
  theme_classic() +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  scale_color_discrete(labels=labz) +
  scale_linetype_discrete(labels=labz)
ggsave("ekspor.png")

## iot table, top 5 value added, percent of total intermediate input

iot<-read_excel("iot.xlsx")
iotagg<-aggregate(cbind(`2002`,`2003`,`2004`,`2005`,
                        `2006`,`2007`,`2008`,`2009`,
                        `2010`,`2011`,`2012`,`2013`,`2014`) ~ ctr, data = iot, FUN = sum, na.rm = TRUE)

iotpct<-iotagg %>%
  mutate(`2002`=paste0(round(`2002`/sum(`2002`)*100,2),"%")) %>%
  mutate(`2003`=paste0(round(`2003`/sum(`2003`)*100,2),"%")) %>%
  mutate(`2004`=paste0(round(`2004`/sum(`2004`)*100,2),"%")) %>%
  mutate(`2005`=paste0(round(`2005`/sum(`2005`)*100,2),"%")) %>%
  mutate(`2006`=paste0(round(`2006`/sum(`2006`)*100,2),"%")) %>%
  mutate(`2007`=paste0(round(`2007`/sum(`2007`)*100,2),"%")) %>%
  mutate(`2008`=paste0(round(`2008`/sum(`2008`)*100,2),"%")) %>%
  mutate(`2009`=paste0(round(`2009`/sum(`2009`)*100,2),"%")) %>%
  mutate(`2010`=paste0(round(`2010`/sum(`2010`)*100,2),"%")) %>%
  mutate(`2011`=paste0(round(`2011`/sum(`2011`)*100,2),"%")) %>%
  mutate(`2012`=paste0(round(`2012`/sum(`2012`)*100,2),"%")) %>%
  mutate(`2013`=paste0(round(`2013`/sum(`2013`)*100,2),"%")) %>%
  mutate(`2014`=paste0(round(`2014`/sum(`2014`)*100,2),"%"))
iotpct %>%
  arrange(desc(`2014`)) %>%
  head(5) %>%
  write_csv("iotpct.csv")

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

setwd('uncomtrade')

temp = list.files(pattern="*.zip")
myfiles = lapply(temp, read_csv)
trad<-dplyr::bind_rows(myfiles) %>%
  select('Classification','Year','Trade Flow','Partner','Partner ISO','Commodity Code',
          'Commodity','Netweight (kg)','Trade Value (US$)') %>%
  rename(Flow=`Trade Flow`,ISO=`Partner ISO`,HS=`Commodity Code`,
         Kg=`Netweight (kg)`,USD=`Trade Value (US$)`)
setwd('..')
trade<-filter(trad,HS %in% con$HS)%>%
  filter(nchar(HS)==6)

## ADDS BEC & final
trade$BEC<-concord_hs_bec(trade$HS,origin = "HS1" ,destination = "BEC4",dest.digit = 3)
trade$final<-ifelse(trade$BEC==112,1,ifelse(trade$BEC==122,1,0))
trade<-trade%>% filter(str_detect(BEC, "^1"))

## Separate export & import
tw<-trade%>%filter(Partner == "World")
ta<-trade%>%filter(Partner != "World")
twx<-filter(tw,Flow=="Export") %>% arrange(-USD)
twm<-filter(tw,Flow=="Import") %>% arrange(-USD)

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

kantal<-tw%>%distinct(Classification,Year)
kintil<-tariff1%>%distinct(HSV,Year)
kntl<-left_join(kantal,kintil)

#===================== OLD CODE FOR TARIFF FROM WITS ========================

## Tariff


#tariff<-read_csv('tariff1.zip') %>%
#  filter(DutyType == "MFN") %>%
#  select("Native Nomen","Product","Partner Name","Tariff Year",
#         "Simple Average","Weighted Average") %>%
#  rename("HSC"="Native Nomen","Partner"="Partner Name","Year"="Tariff Year", "HSO"="Product",
#         "simple"="Simple Average","weighted"="Weighted Average")

#if (tariff$HSC=="H2") {
#  tariff$HS<-concord_hs(tariff,sourcevar =  tariff$HSO, origin = "HS2", destination = "HS1", dest.digit = 6)
#} else if (tariff$HSC=="H3") {
#  tariff$HS<-concord_hs(tariff,sourcevar =  tariff$HSO, origin = "HS3", destination = "HS1", dest.digit = 6)
#} else if (tariff$HSC=="H4") {
#  tariff$HS<-concord_hs(tariff,sourcevar =  tariff$HSO, origin = "HS4", destination = "HS1", dest.digit = 6)
#} else {
#  tariff$HS<-concord_hs(tariff,sourcevar =  tariff$HSO, origin = "HS5", destination = "HS1", dest.digit = 6)
#}

#tariff<-tariff%>%
#  filter(Partner=="Australia") %>%
#  arrange(HS)


#tariff<-tariff%>%
#  filter(HS %in% con$HS)

#hadeh<-left_join(ta,tariff)
