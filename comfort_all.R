library("XML")
library(xml2)
library("stringr")
library("RCurl")
library("dplyr")
library("rvest")
library(sf)
library(ggplot2)
library(jsonlite)
library(httr)
library(rlist)
library('pipeR')
setwd('C:/Users/zhouq/Documents/R/comfort-index')

get.index<-function(i){
  date.url<-paste('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',Sys.Date(),'&releasedate=',sep = '')
  time<-c('9:30','11:30','13:30','15:30')
  index.table<- paste(date.url,time[i],sep='') %>% getURL(.encoding="utf-8")  %>% readHTMLTable(stringAsFactors = FALSE)
  index.list<-list()
  for(i in 1:4){
    index.list[[i]]<-list.merge(index.table[[(i-1)*7+4]],index.table[[(i-1)*7+5]]) %>% lapply(as.character)
    if(length(names(index.list[[i]]))==4){
      names(index.list[[i]])<-c("Tag",'Name',"Visitor",'index')  
      index.list[[i]]$Tag<-index.list[[i]]$Tag %>% str_replace('发布时间：','') %>% strsplit('\\s')
    }
     
  }
  return(index.list)
}
p<-get.index()
jqname_full <- list.cases(p,Name)
jqname <- list.cases(p,Name) %>% str_replace('（4A）|（5A）','') %>%str_replace('）','') %>% str_split('（')
n<-sapply(jqname,length)

for(i in 1:length(jqname)){
  if(n[i]>1)
  jqname[[i]]<- jqname[[(i)]][2]
  else
  jqname[[i]]<-jqname[[i]][1]
}
jqname<-unlist(jqname)
jqgeo  <- read.csv('jqgeo0217.csv',stringsAsFactors = FALSE)[,-1]

t<-sapply(jqgeo$keyword,grep,jqname) %>% unlist() 
t_number<-as.integer(t)
jq<-data.frame(jqname_full[t_number],keyword)
comfor_posi<-merge(jq,jqgeo,by='keyword',all = F)
keyword<-names(t)
t<-data.frame(keyword=as.character(names(t)),posi=as.character(t))
t[1]
sapply(t,length)
heritage_Current<-heritage.parsed[[2]]  %>% .[,setdiff(1:ncol(.),c(2,5,7,9))]
heritage_Previous<-heritage.parsed[[4]] %>% .[,setdiff(1:ncol(.),c(2,5,7,9))]
heritage_Current$Address<- heritage_Current$Location %>% as.character %>% strsplit(',') %>% sapply('[[',1)
heritage_Current$lat<- heritage_Current$Location %>% str_extract("-?\\d{1,2}\\.\\d{1,}; -?\\d{1,3}\\.\\d{1,}") %>% strsplit(';') %>% sapply('[[',1) %>% as.numeric()
heritage_Current$lng<- heritage_Current$Location %>% str_extract("-?\\d{1,2}\\.\\d{1,}; -?\\d{1,3}\\.\\d{1,}") %>% strsplit(';') %>% sapply('[[',2) %>% as.numeric()
heritage_Current$Criteria<-heritage_Current$Criteria %>% as.character()%>% strsplit(":") %>% sapply("[[",1)
world_region<-st_read('C:/Users/zhouq/Documents/R/map/world_region.shp')

for(i in 1:dim(heritage_Current)[1]){
 heritage_Current$geo[[i]]<-st_point(c(heritage_Current$lng[i],heritage_Current$lat[i]))
}
heritage_Current$geo<-st_sfc(heritage_Current$geo,crs = 4326)
heritage_Current.sf<-st_sf(heritage_Current)
p<-ggplot()+geom_sf(data = world_region)+geom_sf(data = heritage_Current.sf,aes(shape=Criteria,fill=Criteria),size=3,colour="red")+ scale_shape_manual(values=c(21,22))+
  labs(title="世界濒危文化遗产分布图（当前）",caption="数据来源：维基百科")+   
  theme_void(base_size=15) %+replace%
  theme(
    plot.title=element_text(size=25,hjust=0),
    plot.caption=element_text(hjust=0),       
    legend.position = c(0.05,0.55),
    plot.margin = unit(c(1,0,1,0), "cm")
  )
t<-ggplot()+geom_point(data=heritage_Current.sf,aes(x=lng,y=lat,shape=Criteria,fill=Criteria),size=3,colour="red")
p
t
