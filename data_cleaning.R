# 제주도 공간정보 탐색적 데이터 분석 경진대회
# 데이터 cleaning
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
library(RColorBrewer)

# 데이콘 제주도 데이터 하나의 데이터 프레임으로 병합
data_jeju <- map_dfr(dir_ls("data", glob = "*.txt"), function(x) {
  read_csv(x)
}) %>%
  select(-X, -Y)

# 좌표 변환기 함수 위셩도 변환용도
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


# 지리정보 API를 사용한 위경도에서 주소 추출 함수
Lat_lon_to_address_fun <- function(long, lat) {
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
        # query = list(x = long, y= lat),
        query = list(x = long, y= lat),
        add_headers(Authorization = paste0("KakaoAK ", "2ecacbafd523802f293b245103346b06"))) %>%
    content(as = 'text') %>%
    fromJSON()

  lon_lat_df <- tibble(
    long = long,
    lat = lat,
    주소 = data_list$documents$address_name,
    구 = data_list$documents$region_2depth_name,
    동 = data_list$documents$region_3depth_name
  ) %>%
    head(1)

  return(lon_lat_df)
}


# 좌표 변환
coord <- data.frame(utmk.long=c(954677.6, 958869.4), utmk.lat=c(1951583, 1945669))
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# convertCoordSystem() 함수 사용하여 좌표체계와 위경도를 구성하는 df 생성, lat_long_df
lat_long_df <- convertCoordSystem(data_jeju$POINT_X, data_jeju$POINT_Y, from.crs, to.crs) %>%
  as_tibble()

# 위경도에서 주소 변환 카카오 api사용하며, 하루 할당량 약 10만개로, 위경도 unique하여 주소 변환
# 이를 lat_long_df_unique라는 df 생성
lat_long_df$long <- as.character(lat_long_df$long)
lat_long_df$lat <- as.character(lat_long_df$lat)
lat_long_df_unique <- lat_long_df %>%
  unique()

# # 위경도에서 주소로 변환시키는 Lat_lon_to_address_fun()함수를 사용하여 위경도, 주소를 담고 있는 df 변환
# # 이를 lat_long_addr_df라는 df 생성
lat_long_addr_df  <- map_dfr(1:nrow(lat_long_df_unique), function(x) {
  Lat_lon_to_address_fun(lat_long_df_unique$long[x], lat_long_df_unique$lat[x])
})

# lat_long_addr_df <- read_csv("data/long_lat_addr_matching_csv")
lat_long_addr_df$long <- lat_long_addr_df$long %>% as.character()
lat_long_addr_df$lat <- lat_long_addr_df$lat %>% as.character()

jeju_lat_long_addr_df  <- lat_long_df %>%
  left_join(lat_long_addr_df)

# 기존 data_jeju와 jeju_lat_long_addr_df와 column bind, 이를 total_df
total_df <- data_jeju %>%
  bind_cols(jeju_lat_long_addr_df)
