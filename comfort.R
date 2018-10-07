library(pacman)
p_load('XML','xml2','stringr','RCurl','tidyverse','sf','rvest','jsonlite','httr','rlist','leaflet','showtext')
library(magrittr)
setwd('C:/Users/zhouq/Documents/R/comfort-index')

jq_vr_name=c("景区名称","发布时间","景区瞬间最大容量","在园人数","景区停车位","剩余停车位",
             "舒适度指数",'发布日期')   
#判断当前时间应提取哪一个时间点的舒适度指数表
time.diff<-function(){
  start.time  <- as.POSIXlt(paste(Sys.Date(),'09:30:00 CST'))
  diff        <- difftime(Sys.time(),start.time,units='mins') %>% as.data.frame.difftime() %>% as.numeric()
  i           <- floor(diff/120)+1
  if(i<1)    i<- 1
  if(i>4)    i<- 4
  return(i)
}
#提取当前时点的舒适度指数表
get.index<-function(){
  date.url          <- paste('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',Sys.Date(),'&releasedate=',sep = '')
  time              <- c('9:30','11:30','13:30','15:30')
  index.content     <- paste(date.url,time[time.diff()],sep='') %>% 
                       getURL(.encoding="utf-8")  %>% 
                       readHTMLTable(stringAsFactors = FALSE)
  index.list        <- list.merge(index.content[[4]],index.content[[5]]) %>% lapply(as.character)
  names(index.list) <- c("Tag",'Name',"Visitor",'index')  
  index.list$Visitor<- as.integer(index.list$Visitor)
  index.list$index  <- substr(index.list$index,1,1) %>% factor(levels=c('5','4','3','2','1'),labels=c('舒适','较舒适','一般','较拥挤','拥挤'))
  index.list$Tag    <- index.list$Tag %>% str_replace('发布时间：','') %>% strsplit('\\s')
  return(index.list)
}
#整理舒适度指数表
index.list          <- get.index()
index.data          <- index.list %>% list.skip(1) %>% data.frame() 
index.data$Name     <- as.character(index.data$Name)
index.data$Time     <- paste(index.list$Tag[[1]][1],index.list$Tag[[1]][2])



keyword.list        <- index.data$Name %>% str_replace('（4A）|（5A）','') %>%str_replace('）','') %>% str_split('（')
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

pal                <- colorFactor(brewer.pal(11,'RdYlGn')[c(1,4,7,9,11)],domain = c('拥挤','较拥挤','一般','较舒适','舒适'),ordered=TRUE)
index.order        <- factor(c('1','2','3','4','5'),levels=c('拥挤','较拥挤','一般','较舒适','舒适'),ordered=TRUE)
p                  <- leaflet(index.data) %>%
                      addTiles(
                        'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
                         options=tileOptions(tileSize=256, minZoom=7, maxZoom=17, subdomains="1234"),
                         attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
                         group="Road Map") %>% 
                      setView(index.data$lng[1],index.data$lat[1] ,zoom = 10)%>%
                      addCircles(color=~pal(index.data$index),weight=25,popup=paste(index.data$Name.y,"<br/>",'人数:',index.data$Visitor,br(),index.data$index,sep=''))%>%
                      addLegend("bottomleft",pal=pal,values=~index,title=paste(index.data$Time[1],'江苏重点景区舒适度'))
p

date=Sys.Date()
date.url          <- paste('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',date,'&releasedate=',sep = '')
time              <- c('9:30','11:30','13:30','15:30')
#to get the search_url for the last time_point
url_time=paste0(date.url,time[time.diff()],sep='')
#to get the serach_url for a whole day
url_day=sapply(date.url,paste0,time) %>% as.vector()
#to get the detail information html page (for single time point)
get_jq_table_day=function(date){
  
  date_url=paste0('http://61.155.108.87/jslyzw/front/show/all_show.do?releasenyr=',date,'&releasedate=',time)  
  get_jq_href=function(date_url){
    read_html(date_url) %>% html_nodes('a') %>% html_attr('href')
  }
  jq_href=sapply(date_url,get_jq_href) %>% unlist
   jq_prefix='http://61.155.108.87/jslyzw/front/show/'
   jq_url=paste0(jq_prefix,jq_href)
   get_jq_detail=function(jq_url){ 
              x=jq_url[1] %>% 
              getURL(.encoding="utf-8") %>% 
              readHTMLTable(which=5,header = F,stringAsFactors = F) %>% '['(,2)
              }
jq_table=sapply(jq_url,get_jq_detail) %>%t %>% as.data.frame(row.names=1:nrow(.),stringAsFactors=F)
names(jq_table)=jq_vr_name
jq_table$发布日期=date
numeric_col=names(jq_table)[3:6]
jq_table=jq_table %>%mutate_at(numeric_col,str_replace,'人|个','')%>%
         mutate_at(numeric_col,as.numeric)
return(jq_table)
}
jq_table_time=get_jq_table_day(date)
jq_table_all=map(date,get_jq_table_day)  

jq_table_time %>% group_by(发布时间)%>% 

jq=jq_table_all %>% list.rbind
write.csv(jq,'2018年中秋假日江苏省重点景区舒适度及停车位情况.csv')
#backup
jq=read.csv('9.22日重点景区舒适度及停车位情况.csv')
jq$停车位占比=jq$剩余停车位/jq$景区停车位*100;

showtext_auto()
ggplot(data=jq)+geom_point(mapping = aes(x=景区名称,y=停车位占比,shape=发布时间))
ggplot(data=mpg)
plot(order(jq$停车位占比))
ungroup(jq)
jq%>% 
   ggplot()+
   geom_point(mapping = aes(x=景区名称,y=停车位占比,color=发布时间),size=2)+
     facet_wrap(~在园人数)
 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~.)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
