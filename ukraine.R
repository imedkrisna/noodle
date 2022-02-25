## get trade data
library("tidyverse")
library("patchwork")

# Reading the trade data

setwd('uncomtrade')

temp = list.files(pattern="*.zip")
myfiles = lapply(temp, read_csv)
trad<-dplyr::bind_rows(myfiles) %>%
  select('Classification','Year','Trade Flow','Partner','Partner ISO','Commodity Code',
         'Commodity','Netweight (kg)','Trade Value (US$)') %>%
  rename(Flow=`Trade Flow`,ISO=`Partner ISO`,HS=`Commodity Code`,
         Kg=`Netweight (kg)`,USD=`Trade Value (US$)`)
setwd('..')

## Getting just Ukraine and Russia with 2 digit HS Code
trad$millUSD<-trad$USD/1000000
ukraine<-filter(trad,Partner == "Ukraine")
ukraine<-filter(ukraine,nchar(HS)==2)
russia<-filter(trad,Partner == "Russian Federation")
russia<-filter(russia,nchar(HS)==2)

## Getting top 5 exports & imports with Ukraine & Russia
ux5<-ukraine %>% filter(Flow=="Export") %>% filter(Year == 2020) %>% group_by(Year) %>% slice_max(order_by = USD, n = 5)
um5<-ukraine %>% filter(Flow=="Import") %>% filter(Year == 2020) %>% group_by(Year) %>% slice_max(order_by = USD, n = 5)
rx5<-russia %>% filter(Flow=="Export") %>% filter(Year == 2020) %>% group_by(Year) %>% slice_max(order_by = USD, n = 5)
rm5<-russia %>% filter(Flow=="Import") %>% filter(Year == 2020) %>% group_by(Year) %>% slice_max(order_by = USD, n = 5)

## Listing the main exports & imports
### UX: 15
### UM: 10, 72
### RX: 15
### RM: 72, 31, 27

## Checking the top export & import

## Export
### palm oil
filter(trad,HS == "15") %>% filter(Year=="2020") %>% filter(Partner != "World") %>%
  filter(Flow == "Export") %>%
  mutate(pct=paste0(round(USD/sum(USD)*100,2),"%")) %>%
  filter(Partner==c("Ukraine","Russian Federation"))

## Import
### 10
filter(trad,HS == "10") %>% filter(Year=="2020") %>% filter(Partner != "World") %>%
  filter(Flow == "Import") %>%
  mutate(pct=paste0(round(USD/sum(USD)*100,2),"%")) %>%
  filter(Partner=="Russian Federation" | Partner=="Ukraine")
### 72
filter(trad,HS == "72") %>% filter(Year=="2020") %>% filter(Partner != "World") %>%
  filter(Flow == "Import") %>%
  mutate(pct=paste0(round(USD/sum(USD)*100,2),"%")) %>%
  filter(Partner=="Russian Federation" | Partner=="Ukraine")
### 31
filter(trad,HS == "31") %>% filter(Year=="2020") %>% filter(Partner != "World") %>%
  filter(Flow == "Import") %>%
  mutate(pct=paste0(round(USD/sum(USD)*100,2),"%")) %>%
  filter(Partner=="Russian Federation" | Partner=="Ukraine")
### 27
filter(trad,HS == "31") %>% filter(Year=="2020") %>% filter(Partner != "World") %>%
  filter(Flow == "Import") %>%
  mutate(pct=paste0(round(USD/sum(USD)*100,2),"%")) %>%
  filter(Partner=="Russian Federation" | Partner=="Ukraine")

# THE PLOT!
aa<-labs(y="Million USD",x="Products")

a<-ggplot(ux5, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Export to Ukraine")+
  scale_x_discrete(breaks=c("15","48","64","40","24"),labels=c(
   "Palm Oil",
   "Paper",
   "Footwear",
   "Rubber",
   "Tobacco"
  ))+ylim(0,720)+aa

b<-ggplot(um5, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Import from Ukraine")+aa+
  scale_x_discrete(breaks=c("10","72","17","11","84"),labels=c(
    "Wheat",
    "Iron &\nsteel",
    "Sugar",
    "Flour",
    "Machinary"
  ))+ylim(0,720)
c<-ggplot(rx5, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Export to Russia")+aa+
  scale_x_discrete(breaks=c("15","09","64","85","40"),labels=c(
    "Palm Oil",
    "Coffee,\ntea",
    "Footwear",
    "Electrical",
    "Rubber"
  ))+ylim(0,720)
d<-ggplot(rm5, aes(x=HS,y=millUSD))+
  geom_bar(size=1.1,stat="identity") + coord_flip()+
  ggtitle("Import from Russia")+aa+
  scale_x_discrete(breaks=c("72","31","27","88","93"),labels=c(
    "Iron &\nsteel",
    "Fertilizer",
    "Coal\nproducts",
    "Aircraft\nparts",
    "Arms"
  ))+ylim(0,720)

p<-a+b+c+d
p<-p+plot_annotation(
  title = "Top 5 of Indonesia's trade with Ukraine and Russia, 2020",
  caption = 'Source: UN Comtrade Database'
) &
  theme_classic()
p
## Saving the plot
ggsave("ukraine.png",p)
