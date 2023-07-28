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


##���� qgis���� ������ ������ ���Ե� ����(centroid) ��������

teen_pop_centroid <- st_read("C:\\Users\\82108\\Desktop\\R directory\\ursp2023\\centroid with teenpop.shp")

class(teen_pop_centroid)






##���ڿ� �α� ����ϱ�: centeroid�� �ִ� �� (û�ҳ� �α�/)

#���������� ����
tract_centroid <-
  teen_pop_centroid %>%
  group_by(ADM_NM) %>%
  summarise(count = n())   # �������� �ش��ϴ� ���� ��'

#������ û�ҳ� �α� ������ ��������
teen_pop <-
  teen_pop_centroid %>%
  select('area', 'teenagerpo') %>%
  group_by(area) %>%
  summarise(teen_pop = mean(teenagerpo))

#merge �ϱ�
distribute_pop <-st_join(tract_centroid, teen_pop, by = "area") %>%
  mutate(per_teen_pop = teen_pop/count) 

distribute_pop$per_teen_pop <- as.integer(distribute_pop$per_teen_pop)
  
#�α� �й��� �� dataframe���� �ٲ㼭 centroid�� merge �ϱ�

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


#r���� �����̶� ���� �Ÿ��� �缭 1km ���� �ִ� ���� select�Ѵ�.
#��Ŭ���� �Ÿ����ϱ�

#�� ���� �ٽ� qgis�� �����ͼ� ���� �׸���

#if�� ����ؼ� ���� ���� ���� �׸� �Ÿ����� ���� �ο��ϱ�

#population ����+ �Ÿ� ���� ���ο� column �����ϱ�
