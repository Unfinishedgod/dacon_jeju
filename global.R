# 제주도 공간정보 탐색적 데이터 분석 경진대회
library(data.table)
library(tidyverse)
library(lubridate)
library(fs)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(leaflet)
library(plotly)
library(formattable)
library(shiny)
library(leaflet)
library(DT)

data_jeju <- map_dfr(dir_ls("data"), function(x) {
  read_csv(x)
}) %>% 
  select(-X, -Y)

# 좌표 변환기
convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}



df <- data_jeju

coord <- data.frame(utmk.long=c(954677.6, 958869.4), utmk.lat=c(1951583, 1945669))
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
coord <- cbind(df, convertCoordSystem(df$POINT_X, df$POINT_Y, from.crs, to.crs))

coord <- coord %>% 
  filter(Time != "x시") %>% 
  as_tibble()



YM_list <- coord$YM %>% unique()
FranClass_list <- coord$FranClass %>% unique()
Time_list <- coord$Time %>% unique()
Type_list <- coord$Type %>% unique()

leaflet_df <- coord %>% 
  filter(YM == 202005) %>%
  filter(FranClass == "영세") %>%
  filter(Time == "18시")


asdf <- coord %>% 
  group_by(Time, FranClass) %>% 
  summarise(TotalSpent_1 = mean(TotalSpent))

asdf$Time <- asdf$Time %>% 
  str_remove_all("시")

asdf$FranClass <- asdf$FranClass %>% as.factor()

asdf %>% 
  ggplot(aes(x=Time, y = TotalSpent_1, group = FranClass, color= FranClass)) + 
  geom_line()

asdf %>% 
  # spread(key = "FranClass", value = "TotalSpent_1") %>%
  plot_ly() %>%
  # plot_ly(x = ~Time, y = ~`영세`, 
  #         line = list(shape = "spline", color = "red", width = 2.5)) %>%
  add_trace(x = ~Time, y = ~영세, type = 'scatter', mode = 'lines') %>%
  layout(
    title = "Line Chart",
    xaxis = list(title = "x축 이름"),
    yaxis = list(title = "y축 이름")
  )

Orange %>% 
  as_tibble()

Orange %>% 
  plot_ly() %>% 
  add_trace(x = ~age, y = ~circumference, color = ~Tree, mode = "lines") %>% 
  layout(
    title = "Line Chart",
    xaxis = list(title = "x축 이름"),
    yaxis = list(title = "y축 이름")
  )
