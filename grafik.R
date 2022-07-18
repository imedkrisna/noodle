## devtools::install_github("insongkim/concordance", dependencies = TRUE)
library("tidyverse")
library("concordance")
library("patchwork")
library("ggthemes")

## Plot Indonesian manufacturing growth
growth<-read_csv("growth.csv") %>%
  pivot_longer(!year, names_to = "type", values_to = "growth")
ggplot(growth,aes(x=year,y=growth,color=type))+
  geom_line(size=1.1,aes(linetype=type)) +
  scale_x_continuous(breaks=seq(2002,2021,2))+
  scale_y_continuous(breaks=seq(-2,12,2))+
  labs(title="GDP growth of food & beverage, manufacturing, and the economy of Indonesia",
       caption="source: BPS\nnote: BPS provides two sets of growth data: 2002-2014 using 2000 base year and \n2011-2021 using 2010 base year. We use geometric mean for overlapping year 2011-2014.",
       x="Year",
       y="%") +
  scale_color_discrete(labels=c("Food and beverage","Total","Manufacturing")) +
  scale_linetype_discrete(labels=c("Food and beverage","Total","Manufacturing"))+
  theme_classic() +
  theme(legend.position = "bottom",legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"))
ggsave("fig/1growth.png")

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
       y="%") +
  theme_classic() +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  scale_color_discrete(labels=labz) +
  scale_linetype_discrete(labels=labz)
ggsave("fig/5share.png")

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
ggsave("fig/2prod.png")

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
ggsave("fig/3ekspor.png")

## iot table for making table 1

iot<-readxl::read_excel("iot.xlsx")
iotagg<-aggregate(cbind(`2002`,`2003`,`2004`,`2005`,
                        `2006`,`2007`,`2008`,`2009`,
                        `2010`,`2011`,`2012`,`2013`,`2014`) ~ ctr, data = iot, FUN = sum, na.rm = TRUE)
iotagg1<-aggregate(cbind(`2002`,`2003`,`2004`,`2005`,
                        `2006`,`2007`,`2008`,`2009`,
                        `2010`,`2011`,`2012`,`2013`,`2014`) ~ indx, data = iot, FUN = sum, na.rm = TRUE)

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
  mutate(`2014`=paste0(round(`2014`/sum(`2014`)*100,2)))
iotpct %>%mutate(`2014`=as.numeric(`2014`))%>%
  arrange(desc(`2014`)) %>%
  head(5) %>%mutate(`2014`=paste0(`2014`,"%"))%>%
  write_csv("iotpct.csv")

iotpct1<-iotagg1 %>%
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
  mutate(`2014`=paste0(round(`2014`/sum(`2014`)*100,2)))
iotpct1 %>%mutate(`2014`=as.numeric(`2014`))%>%
  arrange(desc(`2014`)) %>%
  head(5) %>%mutate(`2014`=paste0(`2014`,"%"))%>%
  write_csv("iotpct1.csv")

t1<-iotpct%>%mutate(`2014`=as.numeric(`2014`))%>%
  arrange(desc(`2014`)) %>%
  head(5) %>%mutate(`2014`=paste0(`2014`,"%"))%>%
  select(`ctr`,`2011`,`2012`,`2013`,`2014`)%>%
  mutate(ctr = ifelse(ctr == "IDN", "Indonesia",ctr))%>%
  mutate(ctr = ifelse(ctr == "ROW", "Rest of the World",ctr))%>%
  mutate(ctr = ifelse(ctr == "AUS", "Australia",ctr))%>%
  mutate(ctr = ifelse(ctr == "BRA", "Brazil",ctr))%>%
  mutate(ctr = ifelse(ctr == "USA", "United States",ctr))%>%
  rename(`country`=ctr)
t2<-iotpct1%>%mutate(`2014`=as.numeric(`2014`))%>%
  arrange(desc(`2014`)) %>%
  head(5) %>%mutate(`2014`=paste0(`2014`,"%"))%>%
  select(`indx`,`2011`,`2012`,`2013`,`2014`)%>%
  rename(industry=indx)

sjPlot::tab_dfs(list(t1,t2),file="fig/2table2.html")

#===================== OLD CODE FOR TARIFF FROM WITS ========================

## Tariff
#kbl(c(t1,t2),"html") %>% cat(.,file="tabbb.html")

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
