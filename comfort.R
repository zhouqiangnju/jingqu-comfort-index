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
library(leaflet)
library(RColorBrewer)
setwd('C:/Users/zhouq/Documents/R/comfort-index')
#�жϵ�ǰʱ��Ӧ��ȡ��һ��ʱ�������ʶ�ָ����
time.diff<-function(){
  start.time  <- as.POSIXlt(paste(Sys.Date(),'09:30:00 CST'))
  diff        <- difftime(Sys.time(),start.time,units='mins') %>% as.data.frame.difftime() %>% as.numeric()
  i           <- floor(diff/120)+1
  if(i<1)    i<- 1
  if(i>4)    i<- 4
  return(i)
}
#��ȡ��ǰʱ������ʶ�ָ����
get.index<-function(){
  date.url          <- paste('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',Sys.Date(),'&releasedate=',sep = '')
  time              <- c('9:30','11:30','13:30','15:30')
  index.content     <- paste(date.url,time[time.diff()],sep='') %>% 
                       getURL(.encoding="utf-8")  %>% 
                       readHTMLTable(stringAsFactors = FALSE)
  index.list        <- list.merge(index.content[[4]],index.content[[5]]) %>% lapply(as.character)
  names(index.list) <- c("Tag",'Name',"Visitor",'index')  
  index.list$Visitor<- as.integer(index.list$Visitor)
  index.list$index  <- substr(index.list$index,1,1) %>% factor(levels=c('5','4','3','2','1'),labels=c('����','������','һ��','��ӵ��','ӵ��'))
  index.list$Tag    <- index.list$Tag %>% str_replace('����ʱ�䣺','') %>% strsplit('\\s')
  return(index.list)
}
#�������ʶ�ָ����
index.list          <- get.index()
index.data          <- index.list %>% list.skip(1) %>% data.frame() 
index.data$Name     <- as.character(index.data$Name)
index.data$Time     <- paste(index.list$Tag[[1]][1],index.list$Tag[[1]][2])
keyword.list        <- index.data$Name %>% str_replace('��4A��|��5A��','') %>%str_replace('��','') %>% str_split('��')
n<-sapply(keyword.list,length)
for(j in 1:length(keyword.list)){
  if(n[j]>1)
  index.data$keyword[j]<- keyword.list[[j]][2]
  else
  index.data$keyword[j]<- keyword.list[[j]][1]
}

jqgeo              <- read.csv('jqgeo0217.csv',stringsAsFactors = FALSE)[,c(5,8,9,10,11)]
index.data$keyword <- sapply(jqgeo$keyword,grep,index.data$keyword) %>% unlist() %>% sort() %>% names
index.data         <- merge(jqgeo,index.data,by='keyword',all=F)

pal                <- colorFactor(brewer.pal(11,'RdYlGn')[c(1,4,7,9,11)],domain = c('ӵ��','��ӵ��','һ��','������','����'),ordered=TRUE)
index.order        <- factor(c('1','2','3','4','5'),levels=c('ӵ��','��ӵ��','һ��','������','����'),ordered=TRUE)
p                  <- leaflet(index.data) %>%
                      addTiles(
                        'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
                         options=tileOptions(tileSize=256, minZoom=7, maxZoom=17, subdomains="1234"),
                         attribution = '&copy; <a href="http://ditu.amap.com/">�ߵµ�ͼ</a>',
                         group="Road Map") %>% 
                      setView(index.data$lng[1],index.data$lat[1] ,zoom = 10)%>%
                      addCircles(color=~pal(index.data$index),weight=25,popup=paste(index.data$Name.y,"<br/>",'����:',index.data$Visitor,br(),index.data$index,sep=''))%>%
                      addLegend("bottomleft",pal=pal,values=~index,title=paste(index.data$Time[1],'�����ص㾰�����ʶ�'))
p
