library("tidyverse")
library("patchwork")

amne2<-readxl::read_excel("amne2.xlsx",sheet = 1)%>%filter(ind=="C10T12")

IDN<-amne2%>%filter(cou=="IDN")%>%ggplot(aes(x=year,y=go,color=own))+geom_line()+ggtitle("IDN")
MYS<-amne2%>%filter(cou=="MYS")%>%ggplot(aes(x=year,y=go,color=own))+geom_line()+ggtitle("MYS")
THA<-amne2%>%filter(cou=="THA")%>%ggplot(aes(x=year,y=go,color=own))+geom_line()+ggtitle("THA")
VNM<-amne2%>%filter(cou=="VNM")%>%ggplot(aes(x=year,y=go,color=own))+geom_line()+ggtitle("VNM")
SGP<-amne2%>%filter(cou=="SGP")%>%ggplot(aes(x=year,y=go,color=own))+geom_line()+ggtitle("SGP")
IDN+MYS+THA+SGP+VNM

IDN<-amne2%>%filter(cou=="IDN")%>%ggplot(aes(x=year,y=gva,color=own))+geom_line()+ggtitle("IDN")
MYS<-amne2%>%filter(cou=="MYS")%>%ggplot(aes(x=year,y=gva,color=own))+geom_line()+ggtitle("MYS")
THA<-amne2%>%filter(cou=="THA")%>%ggplot(aes(x=year,y=gva,color=own))+geom_line()+ggtitle("THA")
VNM<-amne2%>%filter(cou=="VNM")%>%ggplot(aes(x=year,y=gva,color=own))+geom_line()+ggtitle("VNM")
SGP<-amne2%>%filter(cou=="SGP")%>%ggplot(aes(x=year,y=gva,color=own))+geom_line()+ggtitle("SGP")
IDN+MYS+THA+SGP+VNM

IDN<-amne2%>%filter(cou=="IDN")%>%ggplot(aes(x=year,y=exgr,color=own))+geom_line()+ggtitle("IDN")
MYS<-amne2%>%filter(cou=="MYS")%>%ggplot(aes(x=year,y=exgr,color=own))+geom_line()+ggtitle("MYS")
THA<-amne2%>%filter(cou=="THA")%>%ggplot(aes(x=year,y=exgr,color=own))+geom_line()+ggtitle("THA")
VNM<-amne2%>%filter(cou=="VNM")%>%ggplot(aes(x=year,y=exgr,color=own))+geom_line()+ggtitle("VNM")
SGP<-amne2%>%filter(cou=="SGP")%>%ggplot(aes(x=year,y=exgr,color=own))+geom_line()+ggtitle("SGP")
IDN+MYS+THA+SGP+VNM

IDN<-amne2%>%filter(cou=="IDN")%>%ggplot(aes(x=year,y=imgr,color=own))+geom_line()+ggtitle("IDN")
MYS<-amne2%>%filter(cou=="MYS")%>%ggplot(aes(x=year,y=imgr,color=own))+geom_line()+ggtitle("MYS")
THA<-amne2%>%filter(cou=="THA")%>%ggplot(aes(x=year,y=imgr,color=own))+geom_line()+ggtitle("THA")
VNM<-amne2%>%filter(cou=="VNM")%>%ggplot(aes(x=year,y=imgr,color=own))+geom_line()+ggtitle("VNM")
SGP<-amne2%>%filter(cou=="SGP")%>%ggplot(aes(x=year,y=imgr,color=own))+geom_line()+ggtitle("SGP")
IDN+MYS+THA+SGP+VNM
