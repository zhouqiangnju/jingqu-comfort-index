library(tidyverse)
library(rlist)
library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
library(sf)
library('httr')
library('jsonlite')
library(Rgctc2)
#
jq=readRDS('C:/Users/zhouq/Documents/GitHub/data/boundry_of_js_highrate_jq/jq_fanwei.rds')
setwd('C:/Users/zhouq/Documents/GitHub/jingqu-comfort-index/transportation')
js<-readRDS('js_sf.rds')
options(digits=11)
rec=st_bbox(jq[1,]) %>% as.numeric()

get_transportation_status<- function(name){
  key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
  rectangle=paste(rec[1],',',rec[2],';',rec[3],',',rec[4],sep='')
  url = 'https://restapi.amap.com/v3/traffic/status/rectangle?rectangle='%>%
        paste0(
        rectangle,
        '&key=',key,
        '&extensions=all'
        )

 status<-GET(url)%>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)
  return(city)
}
y=status$trafficinfo$roads
y%>% class
poly=y$polyline
y$poly1=poly%>%str_split(';') %>% lapply(str_split,',') %>% lapply(list.rbind)%>%
      lapply(apply,2,as.numeric)%>% lapply(st_linestring) %>% st_sfc(crs=4326)
y$geo=poly1 %>% st_sfc
y$poly1 %>% st_sfc(crs=4326
y =y %>% st_as_sf                   
plot(y$poly1)
#单一行政区边界及中心
js<-get_location('江苏�?')[['districts']]  %>% '['('districts') %>% '[['(1) %>% '[['(1)


js$polyline<-js$polyline %>% str_split('//|') %>% lapply(str_split,';')%>% '[['(1)%>%
          lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% lapply(list.rbind)%>%
          lapply(gcj02_wgs84_matrix_matrix) %>% lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
center<-js$center %>% str_split(',') %>% sapply(as.numeric) %>% gcj02_wgs84_point
js$center_lng_wgs84<-center[1]
js$center_lat_wgs84<-center[2]
js<-select(js,adcode,name,polyline,center,level,center_lng_wgs84,center_lat_wgs84)
js.sf<-st_sf(js)

saveRDS(js.sf,'js_amap.rds')
saveRDS(js_center.sf,'js_center.rds')

#行政区内所有行政单元行政中�?
js_districts<-js %>% select(districts) %>% '[['(1)

district_center<-lapply(js_districts,select,-districts) %>% list.rbind
town_center<-lapply(js_districts,select,districts) %>% lapply('[[',1) %>% lapply(lapply,select,-districts) %>%
             lapply(list.rbind) %>% list.rbind
js_center<-rbind(district_center,town_center)
js_center$center<-str_split(js_center$center,',') %>% lapply(as.numeric) %>% list.rbind %>% gcj02_wgs84_matrix_df %>% apply(1,list)%>%
        lapply(unlist) %>% lapply(st_point) %>% st_sfc(crs=4326)
js_center<-cbind(js_center,center)
js_center.sf<-st_sf(js_center)
saveRDS(js_center,'js_center.rds')
#行政区内所有下级单元行政区划边�?
admin<-get_location('025')[['districts']]  
admin_city<-admin %>% "["('districts') %>% '[['(1) %>% '[['(1)  #提取各城市adcode
admin_city<-lapply(admin_city$adcode,get_location)  %>%         #利用lapply提取所有城市信�?       
  list.map(districts) %>% 
  lapply(select,-districts) %>% 
  list.rbind
admin_city$polyline <- admin_city$polyline %>%                    
  str_split('//|') %>% 
  lapply(str_split,';') %>% 
  lapply(lapply,str_split,',') %>%
  lapply(lapply,lapply,as.numeric) %>% 
  lapply(lapply,list.rbind) %>%
  lapply(lapply,gcj02_wgs84_matrix_matrix) %>% 
  lapply(lapply,list) %>%
  lapply(st_multipolygon) %>%st_sfc(crs=4326)

admin_city       <-admin_city$center %>% 
  str_split(';') %>% 
  lapply(str_split,',') %>% 
  lapply(lapply,as.numeric) %>% 
  lapply(list.rbind) %>% list.rbind %>% 
  gcj02_wgs84_matrix_df %>%
  bind_cols(admin_city)
admin_city       <-st_sf(admin_city)
saveRDS(admin_city,'nj_town.rds')

ggplot()+geom_sf(data=admin_city) +geom_sf(data=nj_poi[which(nj_poi$大类=='餐饮服务'),])
nj_poi<-read_rds('C:/Users/zhouq/Documents/R/map/20个城市的POI数据/POI数据整理城市/南京/CSV版本/nj_poi_sf.rds')
library(showtext)
font_add('fzfs', regular = '方正仿宋_GBK.TTF')
showtext_auto()   

ggplot()+geom_sf(data=js_city) +
  geom_text(data=js_city,aes(x=wgs84_lng,y=wgs84_lat,label=name),family='fzfs')
saveRDS(js_city,'js_city.rds') 
#
cn<-get_location('?й?')[['districts']]
cn$polyline<-cn$polyline %>% str_split('//|') %>% lapply(str_split,';')%>% '[['(1)%>%
  lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% lapply(list.rbind)%>%
  lapply(gcj02_wgs84_matrix_matrix) %>% lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
cn.sf<-st_sf(cn)
cn.sf<-select(cn.sf,-districts)
