# 제주도 공간정보 탐색적 데이터 분석 경진대회

print("------------server------------")


server <- shinyServer(function(input, output) {
  
  # leaflet 반응형 데이터 ----
  leaflet_data <- reactive({
    
    total_df <- total_df[sample(1:nrow(total_df), nrow(total_df)/100), ]
    
    
    if(input$selectTime == "ALL") {
      fixed_selectTime <- Time_list
    } else {
      fixed_selectTime <- input$selectTime 
    }
    
    if(input$selectYM == "ALL") {
      fixed_selectYM <- YM_list
    } else {
      fixed_selectYM <- input$selectYM
    }
    
    if(input$selectFranClass == "ALL") {
      fixed_selectFranClass <- FranClass_list
    } else {
      fixed_selectFranClass <- input$selectFranClass
    }

    color_index <-  total_df$FranClass %>% unique()
    colors_list <- brewer.pal(length(color_index), "Set1")
    
    total_df <- total_df %>% 
      mutate(color = case_when(
        FranClass == color_index[1] ~ colors_list[1], 
        FranClass == color_index[2] ~ colors_list[2], 
        FranClass == color_index[3] ~ colors_list[3], 
        FranClass == color_index[4] ~ colors_list[4], 
        TRUE ~ colors_list[5]
      ))
    
    
    total_df %>%
      filter(Time %in% fixed_selectTime) %>%
      filter(YM %in% fixed_selectYM) %>%
      filter(FranClass %in% fixed_selectFranClass)
  })
  

  
  # Map Tab ----
  # _leaflet 제주도  ----
  output$bbmap <- renderLeaflet({
    
    test_df <- leaflet_data()
    
    # # Franclass에 따른 컬러 코드 작성
    # color_index_leaflet <- test_df$FranClass %>% unique()
    # colors_list_leaflet <- brewer.pal(length(color_index_leaflet), "Set1")
    
    
    leaflet() %>%
      setView(lng = 126.7229, lat = 33.40035, zoom = 11) %>% 
      addTiles() %>%
      addCircleMarkers(test_df$long,
                       test_df$lat,
                       color = test_df$color,
                       radius = 3,
                       fill = T,
                       fillOpacity = 1,
                       opacity = 0.5,
                       popup = paste0("소상공인구분: ",test_df$FranClass,"",
                                      "<br/> 업종명: ", test_df$Type,
                                      "<br/> 기준년월: ", test_df$YM, 
                                      "<br/> 시간대: ", test_df$Time,
                                      "<br/> 주소: ", test_df$주소, 
                                      "<br/> 총사용금액(재난지원금 포함): ",round(comma(test_df$TotalSpent , 0), 0) ,"",
                                      "<br/> 재난지원금 사용금액: ", round(comma(test_df$DisSpent , 0), 0),"")) %>%
      addLegend("topleft",
                colors = unique(test_df$color),
                labels = unique(test_df$FranClass),
                opacity = 1)
  })
  
  # _Type별 총 금액 ----
  output$histCentile <- renderPlotly({
    leaflet_data() %>% 
      group_by(Type) %>% 
      summarise(TotalSpent_1 = mean(TotalSpent),
                DisSpent_1 = mean(DisSpent)) %>% 
      arrange(desc(TotalSpent_1)) %>% 
      head(5) %>% 
      plot_ly() %>% 
      add_trace(x = ~TotalSpent_1, y = ~Type, type = 'bar', 
                text = ~paste0(round(comma(TotalSpent_1),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Type, "\n", comma(TotalSpent_1), "원"),
                textposition = "inside",
                marker = list(color = colors_list)
                
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
  })
  
  # _Type별 재난지원금 사용 금액 ----
  output$scatterCollegeIncome <- renderPlotly({
    leaflet_data() %>% 
      group_by(Type) %>% 
      summarise(TotalSpent_1 = mean(TotalSpent),
                DisSpent_1 = mean(DisSpent)) %>% 
      arrange(desc(DisSpent_1)) %>% 
      head(5) %>% 
      plot_ly() %>% 
      add_trace(x = ~DisSpent_1, y = ~Type, 
                text = ~paste0(round(comma(DisSpent_1),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Type, "\n", comma(DisSpent_1), "원"),
                textposition = "inside" ,
                marker = list(color = colors_list)
      ) %>% 
      layout(
        title = "업종별 재난지원금 사용금액",
        xaxis = list(title = ""),
        yaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~DisSpent_1,
                     size=5),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)"
      ) 
  })
  
  # 시각화 Tab ----
  
  # leaflet 반응형 데이터 ----
  vis_data <- reactive({
    total_df
  })
  
  # _시간에 따른 업종별 총 사용금액 증가  ----
  output$total_spend_increase <- renderPlotly({
    
    if(input$selectType_2 %in% diff_time_list) {
      fix_selectType <- timeline_total_df %>% 
        select(Type, input$selectType_2) %>%
        rename(`증감` = input$selectType_2) %>% 
        arrange(desc(`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_2
    }
    
    # leaflet_data()
    type_time_df <- total_df %>%
      filter(Type %in% fix_selectType) %>% 
      group_by(Time, Type) %>% 
      summarise(TotalSpent_mean = mean(TotalSpent), 
                DisSpent_mean = mean(DisSpent))
    
    type_time_df <- crossing("Time"= Time_list, "Type"= fix_selectType) %>% 
      left_join(type_time_df) %>% 
      fill(Type) %>% 
      replace_na(list(TotalSpent_mean = 0, DisSpent_mean = 0))
    
    type_time_df %>% 
      plot_ly() %>% 
      add_trace(x = ~Time, y = ~TotalSpent_mean, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"),
                text = ~paste0(round(comma(TotalSpent_mean),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Time, "\n", Type, "\n", comma(TotalSpent_mean), "원"),
                textposition = "inside"
      ) %>% 
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
          text = "심야",
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
      )

    
  })
  
  # _시간에 따른 업종별 총 사용금 증가 비율 ----
  output$total_spend_increase_table <- renderReactable({
    if(input$selectType_2 %in% diff_time_list) {
      fix_selectType <- timeline_total_df %>% 
        select(Type, input$selectType_2) %>%
        rename(`증감` = input$selectType_2) %>% 
        arrange(desc(`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_2
    }
    
    timeline_total_df %>% 
      select(Type, input$selectType_2) %>%
      rename(`증감` = input$selectType_2) %>% 
      arrange(desc(`증감`)) %>% 
      head(10) %>% 
      reactable(
        columns = list(
        `증감` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ))
  })
  
  
  # _시간에 따른 업종별 총 사용금액 감소  ----
  output$total_spend_decrease <- renderPlotly({
    
    if(input$selectType_2 %in% diff_time_list) {
      fix_selectType <- timeline_total_df %>% 
        select(Type, input$selectType_2) %>%
        rename(`증감` = input$selectType_2) %>% 
        arrange((`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_2
    }
    
    # leaflet_data()
    type_time_df <- total_df %>%
      filter(Type %in% fix_selectType) %>% 
      group_by(Time, Type) %>% 
      summarise(TotalSpent_mean = mean(TotalSpent), 
                DisSpent_mean = mean(DisSpent))
    
    type_time_df <- crossing("Time"= Time_list, "Type"= fix_selectType) %>% 
      left_join(type_time_df) %>% 
      fill(Type) %>% 
      replace_na(list(TotalSpent_mean = 0, DisSpent_mean = 0))
    
    type_time_df %>% 
      plot_ly() %>% 
      add_trace(x = ~Time, y = ~TotalSpent_mean, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"),
                text = ~paste0(round(comma(TotalSpent_mean),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Time, "\n", Type, "\n", comma(TotalSpent_mean), "원"),
                textposition = "inside"
      ) %>% 
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$TotalSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
          text = "심야",
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
      )
    
    
  })
  
  # _시간에 따른 업종별 총 사용금 감소 비율 ----
  output$total_spend_decrease_table <- renderReactable({
    if(input$selectType_2 %in% diff_time_list) {
      fix_selectType <- timeline_total_df %>% 
        select(Type, input$selectType_2) %>%
        rename(`증감` = input$selectType_2) %>% 
        arrange((`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_2
    }
    
    
    timeline_total_df %>% 
      select(Type, input$selectType_2) %>%
      rename(`증감` = input$selectType_2) %>% 
      arrange((`증감`)) %>% 
      head(10) %>% 
      reactable(
        columns = list(
          `증감` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
        ))
  })
  
  

  
  # _시간에 따른 업종별 재난지원금 사용금액 증가  ----
  output$dis_spend_increase <- renderPlotly({
    
    if(input$selectType_3 %in% diff_time_list) {
      fix_selectType <- timeline_dis_df %>% 
        select(Type, input$selectType_3) %>%
        rename(`증감` = input$selectType_3) %>% 
        arrange(desc(`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_3
    }
    
    # leaflet_data()
    type_time_df <- total_df %>%
      filter(Type %in% fix_selectType) %>% 
      group_by(Time, Type) %>% 
      summarise(TotalSpent_mean = mean(TotalSpent), 
                DisSpent_mean = mean(DisSpent))
    
    type_time_df <- crossing("Time"= Time_list, "Type"= fix_selectType) %>% 
      left_join(type_time_df) %>% 
      fill(Type) %>% 
      replace_na(list(TotalSpent_mean = 0, DisSpent_mean = 0))
    
    type_time_df %>% 
      plot_ly() %>% 
      add_trace(x = ~Time, y = ~DisSpent_mean, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"),
                text = ~paste0(round(comma(DisSpent_mean),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Time, "\n", Type, "\n", comma(DisSpent_mean), "원"),
                textposition = "inside"
      ) %>% 
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
          text = "심야",
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
      )
    
    
  })
  
  # _시간에 따른 업종별 재난지원금 사용금 증가 비율 ----
  output$dis_spend_increase_table <- renderReactable({
    if(input$selectType_3 %in% diff_time_list) {
      fix_selectType <- timeline_dis_df %>% 
        select(Type, input$selectType_3) %>%
        rename(`증감` = input$selectType_3) %>% 
        arrange(desc(`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_3
    }
    
    timeline_dis_df %>% 
      select(Type, input$selectType_3) %>%
      rename(`증감` = input$selectType_3) %>% 
      arrange(desc(`증감`)) %>% 
      head(10) %>% 
      reactable(
        columns = list(
          `증감` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
        ))
  })
  
  
  # _시간에 따른 업종별 재난지원금 사용금액 감소  ----
  output$dis_spend_decrease <- renderPlotly({
    
    if(input$selectType_3 %in% diff_time_list) {
      fix_selectType <- timeline_dis_df %>% 
        select(Type, input$selectType_3) %>%
        rename(`증감` = input$selectType_3) %>% 
        arrange((`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_3
    }
    
    # leaflet_data()
    type_time_df <- total_df %>%
      filter(Type %in% fix_selectType) %>% 
      group_by(Time, Type) %>% 
      summarise(TotalSpent_mean = mean(TotalSpent), 
                DisSpent_mean = mean(DisSpent))
    
    type_time_df <- crossing("Time"= Time_list, "Type"= fix_selectType) %>% 
      left_join(type_time_df) %>% 
      fill(Type) %>% 
      replace_na(list(TotalSpent_mean = 0, DisSpent_mean = 0))
    
    type_time_df %>% 
      plot_ly() %>% 
      add_trace(x = ~Time, y = ~DisSpent_mean, type="scatter", mode="line", color = ~Type, line = list(shape = "spline"),
                text = ~paste0(round(comma(DisSpent_mean),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(Time, "\n", Type, "\n", comma(DisSpent_mean), "원"),
                textposition = "inside"
      ) %>% 
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'left',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
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
          y = max(type_time_df$DisSpent_mean),
          xanchor = 'meddle',
          yanchor = 'top',
          text = "심야",
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
      )
    
    
  })
  
  # _시간에 따른 업종별 재난지원금 사용금 감소 비율 ----
  output$dis_spend_decrease_table <- renderReactable({
    if(input$selectType_3 %in% diff_time_list) {
      fix_selectType <- timeline_dis_df %>% 
        select(Type, input$selectType_3) %>%
        rename(`증감` = input$selectType_3) %>% 
        arrange((`증감`)) %>% 
        head(10) %>% 
        select(Type) %>% 
        t() %>% 
        as.character()
    } else {
      fix_selectType <- input$selectType_3
    }
    
    
    timeline_dis_df %>% 
      select(Type, input$selectType_3) %>%
      rename(`증감` = input$selectType_3) %>% 
      arrange((`증감`)) %>% 
      head(10) %>% 
      reactable(
        columns = list(
          `증감` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
        ))
  })
  
  
  
  
  # Row data ----
  output$data <- renderDataTable({
    leaflet_data() %>%
      select(-OBJECTID, -Field1) %>% 
      DT::datatable(
        filter = 'top',
        escape = FALSE, 
        selection = 'none', 
        rownames = FALSE,
        options = list(
          pageLength =20
        )
      )
  })
  
})