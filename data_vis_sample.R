# 제주도 공간정보 탐색적 데이터 분석 경진대회
# 데이터 시각화 샘플
#
# library(data.table)
# library(tidyverse)
# library(lubridate)
# library(fs)
# library(sp)
# library(sf)
# library(rgeos)
# library(rgdal)
# library(leaflet)
# library(plotly)
# library(formattable)
# library(shiny)
# library(leaflet)
# library(DT)
# library(RColorBrewer)

# law 데이터 로드
# total_df <- read_csv("data/total_df.csv")
total_df <- read_csv("../total_df.csv")

# Time중 "x시"는 제거
total_df <- total_df %>%
  filter(Time != "x시") %>%
  as_tibble()

total_df %>%
  group_by(동) %>%
  summarise(aaa = mean(TotalSpent),
            bbb = mean(DisSpent)) %>%
  arrange((aaa)) %>%
  plot_ly() %>%
  add_trace(x = ~aaa, y = ~동, mode="bar") %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = "",
                      hoverformat = '.0f',
                      categoryorder = "array",
                      categoryarray = ~(aaa),
                      size=10))




# 시간에 따른 소상공인별 시각화 ----
asdf <- total_df %>%
  group_by(Time, FranClass) %>%
  summarise(aaa = mean(TotalSpent),
            bbb = mean(DisSpent))

# _시간에 따른 소상공인별 총 사용금액 ----
asdf %>%
  plot_ly() %>%
  add_trace(x = ~Time, y = ~aaa, type="scatter", mode="line", color = ~FranClass, line = list(shape = "spline")) %>%
  add_segments(x = "09시", xend = "09시", y = 0, yend = ~max(1.1*aaa), mode="line",
               line = list(color = 'rgb(22, 96, 167)', width = 1, dash = 'dot'))

# _시간에 따른 소상공인별 재난지원금 ----
asdf %>%
  plot_ly() %>%
  add_trace(x = ~Time, y = ~bbb, type="scatter", mode="line", color = ~FranClass, line = list(shape = "spline")) %>%
  layout(shapes = list(vline("02시"),
                       vline("06시"),
                       vline("11시"),
                       vline("15시"),
                       vline("18시"),
                       vline("22시"))) %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "04시",
      y = max(asdf$bbb),
      xanchor = 'meddle',
      yanchor = 'bottom',
      text = "새벽",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  ) %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "08시",
      y = max(asdf$bbb),
      xanchor = 'left',
      yanchor = 'bottom',
      text = "오전",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  ) %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "13시",
      y = max(asdf$bbb),
      xanchor = 'meddle',
      yanchor = 'bottom',
      text = "점심",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  )  %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "16시",
      y = max(asdf$bbb),
      xanchor = 'left',
      yanchor = 'bottom',
      text = "오후",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  )  %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "20시",
      y = max(asdf$bbb),
      xanchor = 'meddle',
      yanchor = 'bottom',
      text = "저녁",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  )  %>%
  layout(
    annotations = list(
      xref = 'x',
      yref = 'y',
      x = "00시",
      y = max(asdf$bbb),
      xanchor = 'meddle',
      yanchor = 'bottom',
      text = "심야",
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
  )

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

# 시간에 따른 업종별 시각화 ----
asdf_2 <- total_df %>%
  group_by(Time, Type) %>%
  summarise(aaa = mean(TotalSpent),
            bbb = mean(DisSpent))

asdf_2 %>%
  group_by(Time, Type) %>%
  summarise(max_aaa = max(aaa),
            max_bbb = max(aaa))



fafafafafa <- asdf_2 %>%
  mutate(time_class = case_when(
    Time %in% c("09시", "10시", "11시", "12시", "13시", "14시", "15시", "16시", "18시") ~ "오전~오후",
    Time %in% c("19시", "20시", "21시", "22시", "23시") ~ "저녁",
    Time %in% c("06", "07", "08") ~ "이른아침",
    TRUE ~ "심야"
  ))


fafafafafa_2 <- fafafafafa %>%
  group_by(time_class, Type) %>%
  summarise(aaa_sum = mean(aaa),
            bbb_sum = mean(aaa))


asdf222 <- fafafafafa_2 %>%
  group_by(Type) %>%
  summarise(aaa_max = max(aaa_sum))

fafafafafa_2 %>%
  filter(aaa_sum %in% asdf222$aaa_max) %>%
  arrange(time_class, aaa_sum) %>%
  as.data.frame()

fafafafafa_2 %>%
  filter(Type == "1급호텔")


f3f3f$class %>% unique()
max(1,2,3)

aaaa <- asdf_2 %>%
  group_by(Type) %>%
  summarise(aaa_1 = mean(aaa),
            bbb_1 = mean(bbb)) %>%
  arrange(desc(aaa_1)) %>%
  head(10) %>%
  select(Type)

bbbb <- asdf_2 %>%
  group_by(Type) %>%
  summarise(aaa_1 = mean(aaa),
            bbb_1 = mean(bbb)) %>%
  arrange(desc(bbb_1)) %>%
  head(10) %>%
  select(Type)


# _총 사용금액 ----
asdf_2 %>%
  filter(Type %in% aaaa$Type) %>%
  plot_ly() %>%
  add_trace(x = ~Time, y = ~aaa, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"))


# _재난지원금 사용금액 ----
asdf_2 %>%
  filter(Type %in% bbbb$Type) %>%
  plot_ly() %>%
  add_trace(x = ~Time, y = ~bbb, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"))




total_df %>%
  group_by(Type) %>%
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>%
  arrange(TotalSpent_1) %>%
  tail(7) %>%
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


total_df %>%
  group_by(Type) %>%
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>%
  arrange(DisSpent_1) %>%
  tail(7) %>%
  plot_ly() %>%
  add_trace(x = ~DisSpent_1, y = ~Type, type = 'bar',
            text = ~paste0(round(comma(DisSpent_1),0), "원"),
            hoverinfo = "text",
            hovertemplate = ~paste0(Type, "\n", comma(DisSpent_1), "원"),
            textposition = "inside",
            marker = list(color = colors_list)

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


# 지역별 ----
addr_vis_df <- total_df %>%
  group_by(구, 동) %>%
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>%
  mutate(주소_2 = paste(구, 동))


# 지리정보 API를 사용한 위경도 추출
Lat_lon_fun <- function(addr) {
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", "2ecacbafd523802f293b245103346b06"))) %>%
    content(as = 'text') %>%
    fromJSON()


  lon_lat_df <- tibble(주소 = addr,
                         long = as.numeric(data_list$documents$x),
                         lat = as.numeric(data_list$documents$y))

  return(lon_lat_df)
}

addr_lat_lon_2 <- map_dfr(addr_vis_df$주소_2, function(x) {
  Lat_lon_fun(x) %>%
    unique()
})


addr_vis_df_2 <- addr_vis_df %>%
  bind_cols(addr_lat_lon_2[,-1])


leaflet() %>%
  setView(lng = 126.7229, lat = 33.40035, zoom = 11) %>%
  addTiles() %>%
  addCircleMarkers(
    data = addr_vis_df_2,
    lng = ~long,
    lat = ~lat,
    radius=~(TotalSpent_1)/100000,
    color = "red",
    label = ~paste0(주소_2, ": ",round(comma(TotalSpent_1, 0), 0)),
    # stroke = FALSE,
    fillOpacity = 0.15
  )



leaflet() %>%
  setView(lng = 126.7229, lat = 33.40035, zoom = 11) %>%
  addTiles() %>%
  addCircleMarkers(
    data = addr_vis_df_2,
    lng = ~long,
    lat = ~lat,
    radius=~(DisSpent_1)/10000,
    color = "red",
    popup = ~paste0(주소_2, "<br/>",round(comma(DisSpent_1 , 0), 0), "원"),
    # stroke = FALSE,
    fillOpacity = 0.15
  )




total_df %>%
  group_by(Type) %>%
  summarise(TotalSpent_1 = mean(TotalSpent),
            DisSpent_1 = mean(DisSpent)) %>%
  arrange(desc(TotalSpent_1))


# 지역별
asdf <- total_df %>% 
  group_by(FranClass) %>% 
  summarise(TotalSpent_2 = mean(TotalSpent),
            DisSpent_2= mean(DisSpent))

asdf %>%
  gather(key = "asdff", value = "fff", `TotalSpent_2`:`DisSpent_2`) %>% 
  plot_ly() %>% 
  add_trace(x = ~FranClass, y = ~fff, color = ~asdff) %>% 
  layout(
    title = "Bar Chart",
    xaxis = list(title = "x축 이름"),
    yaxis = list(title = "y축 이름")
  )

total_df %>% 
  group_by(FranClass, 구, 동) %>% 
  summarise(TotalSpent_2 = mean(TotalSpent),
            DisSpent_2= mean(DisSpent))


sleep %>% 
  plot_ly() %>% 
  add_trace(x = ~ID, y = ~extra, color= ~group,  type = "bar") %>% 
  layout(
    title = "Bar Chart",
    xaxis = list(title = "x축 이름"),
    yaxis = list(title = "y축 이름")
  )

