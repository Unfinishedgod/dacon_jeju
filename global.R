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
library(RColorBrewer)

# law 데이터 로드
# total_df <- read_csv("data/total_df.csv")
total_df <- read_csv("../total_df.csv")

# Time중 "x시"는 제거
total_df <- total_df %>% 
  filter(Time != "x시") %>% 
  as_tibble()

# Franclass에 따른 컬러 코드 작성
color_index <- total_df$FranClass %>% unique()
colors_list <- brewer.pal(length(color_index), "Set1")

# 년월, 소상공인구분, 시간대, 업종명 구분
YM_list <- total_df$YM %>% unique()
FranClass_list <- total_df$FranClass %>% unique()
Time_list <- total_df$Time %>% unique()
Type_list <- total_df$Type %>% unique()


## Shiny ui, server
print("=================ui.R=================")
source("./ui.R" , local = TRUE)
print("=================server.R=================")
source("./server.R" , local = TRUE)

shinyApp(ui, server)
