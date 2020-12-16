# 제주도 공간정보 탐색적 데이터 분석 경진대회
# 데이터 시각화 샘플

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


asdf <- total_df %>% 
  group_by(Time, FranClass) %>% 
  summarise(aaa = mean(TotalSpent), 
            bbb = mean(DisSpent))

asdf %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~aaa, type="scatter", mode="line", color = ~FranClass, line = list(shape = "spline")) 


asdf %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~bbb, type="scatter", mode="line", color = ~FranClass, line = list(shape = "spline")) 



asdf_2 <- total_df %>% 
  group_by(Time, Type) %>% 
  summarise(aaa = mean(TotalSpent), 
            bbb = mean(DisSpent))


asdf_2 %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~aaa, type="scatter", mode="line", color = ~Type, line = list(shape = "spline")) 


asdf_2 %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~bbb, type="scatter", mode="line", color = ~Type, line = list(shape = "spline")) 




total_df %>% 
  group_by(Type) %>% 
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>% 
  arrange(TotalSpent_1) %>% 
  head(20) %>% 
  plot_ly() %>% 
  add_trace(x = ~TotalSpent_1, y = ~Type, type = 'bar', 
            text = ~paste0(round(comma(TotalSpent_1),0), "원"),
            hoverinfo = "text",
            hovertemplate = ~paste0(Type, "\n", comma(TotalSpent_1), "원"),
            textposition = "inside"
            # ,
            # marker = list(color = colors_list)
            
  ) %>%
  layout(
    title = "업종별 사용금액",
    xaxis = list(title = ""),
    yaxis = list(title = "",
                 categoryorder = "array",
                 categoryarray = ~TotalSpent_1,
                 size=5),
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    fig_bgcolor   = "rgba(0, 0, 0, 0)"
  )





asdfasdf <- total_df %>% 
  group_by(Type) %>% 
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>% 
  arrange(desc(DisSpent_1)) %>% 
  head(20) %>% 
  arrange(DisSpent_1) 

asdfasdf %>%
  plot_ly() %>% 
  add_trace(x = ~DisSpent_1, y = ~Type, type = 'bar', 
            text = ~paste0(round(comma(DisSpent_1),0), "원"),
            hoverinfo = "text",
            hovertemplate = ~paste0(Type, "\n", comma(DisSpent_1), "원"),
            textposition = "inside"
            # ,
            # marker = list(color = colors_list)
            
  ) %>%
  layout(
    title = "업종별 사용금액",
    xaxis = list(title = ""),
    yaxis = list(title = "",
                 categoryorder = "array",
                 categoryarray = ~DisSpent_1,
                 size=5),
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    fig_bgcolor   = "rgba(0, 0, 0, 0)"
  )


asdfasdf

asdf_2 %>% 
  filter(Type %in% asdfasdf$Type) %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~aaa, type="scatter", mode="line", color = ~Type, line = list(shape = "spline")) 

asdf_2 %>% 
  filter(Type %in% asdfasdf$Type) %>% 
  plot_ly() %>% 
  add_trace(x = ~Time, y = ~bbb, type="scatter", mode="line", color = ~Type, line = list(shape = "spline")) 



aaaasdf <- total_df %>% 
  group_by(FranClass, Type) %>% 
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>% 
  select(-DisSpent_1)

asdf <- aaaasdf %>% 
  spread(key = "FranClass", value = "TotalSpent_1")

asdf[is.na(asdf)] <- 0

asdf %>% 
  as.data.frame()

fig <- plot_ly(
  # x = c("a", "b", "c"), 
  # y = c("d", "e", "f"),
  # z = m, type = "heatmap"
  x = colnames(asdf[,-1]),
  y = asdf$Type,
  z = as.matrix(asdf[,-1]),
  type = "heatmap"
)

fig

m <- matrix(rnorm(9), nrow = 3, ncol = 3)
fig <- plot_ly(
  x = c("a", "b", "c"), y = c("d", "e", "f"),
  z = m, type = "heatmap"
)

fig
