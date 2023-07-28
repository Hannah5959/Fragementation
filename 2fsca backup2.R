install.packages(c("matrixStats", "SpatialAcc"))
library(sf)
library(tidyverse)
library(matrixStats)
library(SpatialAcc) 
library(tmap)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(rgdal)


##기존 qgis에서 행정동 정보가 기입된 격자(centroid) 가져오기

teen_pop_centroid <- st_read("C:\\Users\\82108\\Desktop\\R directory\\ursp2023\\centroid with teenpop.shp")

class(teen_pop_centroid)






##격자에 인구 배분하기: centeroid가 있는 곳 (청소년 인구/)

#행정동별로 묶기
tract_centroid <-
  teen_pop_centroid %>%
  group_by(ADM_NM) %>%
  summarise(count = n())   # 행정동에 해당하는 격자 수'

#행정동 청소년 인구 정보만 가져오기
teen_pop <-
  teen_pop_centroid %>%
  select('area', 'teenagerpo') %>%
  group_by(area) %>%
  summarise(teen_pop = mean(teenagerpo))

#merge 하기
distribute_pop <-st_join(tract_centroid, teen_pop, by = "area") %>%
  mutate(per_teen_pop = teen_pop/count) 

distribute_pop$per_teen_pop <- as.integer(distribute_pop$per_teen_pop)
  
#인구 분배한 걸 dataframe으로 바꿔서 centroid랑 merge 하기

typeof(distribute_pop)
distribute_pop <- select(as.data.frame(distribute_pop), -geometry)


teen_pop_centroid <- teen_pop_centroid %>%
  rename("area.y" = "area")
  
per_pop_centroid <- merge(teen_pop_centroid, distribute_pop,by = c("area.y"),all.x=T)
plot(per_pop_centroid['per_teen_pop'])



class(per_pop_centroid,")  

# Create sf object 

pop_centroid_sf <- sf::st_as_sf(per_pop_centroid, coords = c("lon", "lat"))  
class(pop_centroid_sf )
writeOGR(per_pop_centroid, ".", "per_pop_centroid.shp",  driver = "ESRI Shapefile")
write.csv(per_pop_centroid, "per_pop_centroid.csv")

WGScoor<- per_pop_centroid
coordinates(WGScoor)=~long+lat
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
raster::shapefile(LLcoor, "MyShapefile.shp")


#r에서 병원이랑 점의 거리를 재서 1km 내에 있는 곳을 select한다.
#유클리드 거리구하기

#이 격자 다시 qgis로 가져와서 버퍼 그리기

#if문 사용해서 버퍼 내에 들어가는 네모에 거리별로 점수 부여하기

#population 점수+ 거리 점수 새로운 column 형성하기

