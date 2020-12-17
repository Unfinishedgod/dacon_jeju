# 제주도 공간정보 탐색적 데이터 분석 경진대회
library(tidyverse)
library(lubridate)
library(plotly)
library(formattable)
library(shiny)
library(leaflet)
library(DT)
library(RColorBrewer)
library(shinydashboard)
library(reactable)
library(rsconnect)

# law 데이터 로드
total_df <- read_csv("data/total_df.csv")
# total_df <- read_csv("../total_df.csv")

# Time중 "x시"는 제거
total_df <- total_df %>% 
  filter(Time != "x시") %>% 
  as_tibble()

# Franclass에 따른 컬러 코드 작성
color_index <- total_df$FranClass %>% unique()
colors_list <- brewer.pal(length(color_index), "Set1")

# Select Box ----
YM_list <- total_df$YM %>% unique()
FranClass_list <- total_df$FranClass %>% unique()
Time_list <- total_df$Time %>% unique()
Type_list <- total_df$Type %>% unique()

YM_list_2 <- c("ALL", YM_list)
FranClass_list_2 <- c("ALL", FranClass_list)
Time_list_2 <- c("ALL", Time_list)

# 시간에 따른 조정 ----
timeline_raw <- total_df %>% 
  group_by(Time, Type) %>% 
  summarise(aaa = mean(TotalSpent), 
            bbb = mean(DisSpent)) 

timeline_raw <- crossing("Time"= Time_list, "Type"= Type_list) %>% 
  left_join(timeline_raw)

timeline_raw <- timeline_raw %>% 
  group_by(Time, Type) %>% 
  summarise(aaa_sum = mean(aaa), 
            bbb_sum = mean(bbb))

timeline_raw[is.na(timeline_raw)] <- 0

timeline_total_df <- timeline_raw %>% 
  filter(Time %in% c("02시", "06시", "11시", "15시", "18시", "22시")) %>% 
  select(-bbb_sum) %>% 
  spread(key = "Time", value = "aaa_sum") %>% 
  mutate(`심야~새벽` = round((`06시` - `02시`),2), 
         `새벽~오전` = round((`11시` - `06시`),2),
         `오전~점심` = round((`15시` - `11시`),2),
         `점심~오후` = round((`18시` - `15시`),2),
         `오후~저녁` = round((`22시` - `18시`),2),
         `저녁~심야` = round((`02시` - `22시`),2))

timeline_dis_df <- timeline_raw %>% 
  filter(Time %in% c("02시", "06시", "11시", "15시", "18시", "22시")) %>% 
  select(-aaa_sum) %>% 
  spread(key = "Time", value = "bbb_sum") %>% 
  mutate(`심야~새벽` = round((`06시` - `02시`),2), 
         `새벽~오전` = round((`11시` - `06시`),2),
         `오전~점심` = round((`15시` - `11시`),2),
         `점심~오후` = round((`18시` - `15시`),2),
         `오후~저녁` = round((`22시` - `18시`),2),
         `저녁~심야` = round((`02시` - `22시`),2))


diff_time_list <- c("심야~새벽", "새벽~오전","오전~점심","점심~오후", "오후~저녁", "저녁~심야")


# vline function 
vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash = 'dot', width = 1)
  )
}

## Shiny ui, server
print("=================ui.R=================")
source("./ui.R" , local = TRUE)
print("=================server.R=================")
source("./server.R" , local = TRUE)

shinyApp(ui, server)
