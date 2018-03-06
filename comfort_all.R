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

  date.series<- paste('2018-02-',c(15:21),sep = '')
  date.url<-paste('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',date.series,'&releasedate=',sep = '')
  time.series<-c('9:30','11:30','13:30','15:30')
  time.matrix<-sapply(date.series,paste,time.series)
  search.url<-sapply(date.url,paste,time.series,sep='') 
  index.data<-sapply(search.url,getURL,.encoding='utf-8') %>% readHTMLTable(stringAsFactors = FALSE) %>% 
               list.filter(length(.)>0) %>% list.filter(!'V1' %in% names(.))
  comfort.index<-list()
  for (i in 1:length(index.data)){
    if (length(index.data[[i]])==3){
      comfort.index[[ceiling(i/2)]]<-list.merge(index.data[[i-1]],index.data[[i]])
      names(comfort.index[[ceiling(i/2)]])<-c("Tag",'Name',"Visitor",'index')  
      comfort.index[[ceiling(i/2)]]$index  <- substr(comfort.index[[ceiling(i/2)]]$index,1,1) %>% factor(levels=c('5','4','3','2','1'),labels=c('舒适','较舒适','一般','较拥挤','拥挤'))
      comfort.index[[ceiling(i/2)]]$Tag    <- comfort.index[[ceiling(i/2)]]$Tag %>% str_replace('发布时间：','') %>% strsplit('\\s')
    }
  }

  comfort.index[[11]]<-data.frame()
  comfort.index[[11]] <- data.frame(Tag=time.matrix[11%%4,floor(11/4)],Name=NA,Visitor=NA,index=NA)
  data<-comfort.index
  list.map(data,Name)
  x<-list.map(data,Name) %>% list.apply(str_replace,'（4A）|（5A）','') %>% list.apply(str_replace,'）','') %>% list.apply(str_split,'（')
#clean data to get location
  list.map(data,Name)
ui<-comfort.index[[12]]%>% list.skip(1) %>% data.frame() 
names(data[[12]])

data[[11]]<-as.data.frame(data[[11]],col.names=names(data[[12]]))
data[[11]]$Tag<-time.matrix[11%%4,floor(11/4)]

data[[11]]$Tag
list.names(data[[11]])
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

x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
          p2 = list(type='B',score=list(c1=9,c2=9)),
          p3 = list(type='B',score=list(c1=9,c2=7)))
list.order(x, type, (score$c2)) # order by type (ascending) and score$c2 (descending)
list.order(x, min(score$c1,score$c2))
list.order(x, min(score$c1,score$c2), keep.names=TRUE)
list.order(comfort.index,length(.)==0)
list.first(comfort.index,length(.)<4)
saveRDS(comfort.index,'SFCI.rds')
data<-readRDS('SFCI.rds')
