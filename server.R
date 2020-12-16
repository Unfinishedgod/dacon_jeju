# 제주도 공간정보 탐색적 데이터 분석 경진대회

print("------------server------------")


server <- shinyServer(function(input, output) {
  
  leaflet_data <- reactive({
    total_df
    # total_df %>% 
    #   filter(Time == Time_list[input$selectTime + 1]) %>% 
    #   filter(YM == paste0("20200", input$selectYM)) %>% 
    #   filter(Type == input$selectType)
  })
  

  
  
  output$histCentile <- renderPlotly({
    leaflet_data() %>% 
      group_by(FranClass) %>% 
      summarise(TotalSpent_1 = mean(TotalSpent),
                DisSpent_1 = mean(DisSpent)) %>% 
      arrange(TotalSpent_1) %>% 
      head(20) %>% 
      plot_ly() %>% 
      add_trace(x = ~TotalSpent_1, y = ~FranClass, type = 'bar', 
                text = ~paste0(round(comma(TotalSpent_1),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(FranClass, "\n", comma(TotalSpent_1), "원"),
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
  
  
  output$scatterCollegeIncome <- renderPlotly({
    leaflet_data() %>% 
      group_by(FranClass) %>% 
      summarise(TotalSpent_1 = mean(TotalSpent),
                DisSpent_1 = mean(DisSpent)) %>% 
      arrange(DisSpent_1) %>% 
      head(20) %>% 
      plot_ly() %>% 
      add_trace(x = ~DisSpent_1, y = ~FranClass, 
                text = ~paste0(round(comma(DisSpent_1),0), "원"),
                hoverinfo = "text",
                hovertemplate = ~paste0(FranClass, "\n", comma(DisSpent_1), "원"),
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
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    
    test_df <- leaflet_data()
    
    test_df <- test_df %>%
      filter(Time == Time_list[input$selectTime + 1]) %>%
      filter(YM == paste0("20200", input$selectYM)) %>%
      filter(Type == input$selectType)
    
    color_index <-  test_df$FranClass %>% unique()
    colors_list <- brewer.pal(length(color_index), "Set1")
    
    leaflet() %>%
      setView(lng = 126.7229, lat = 33.40035, zoom = 11) %>% 
      addTiles() %>%
      addCircleMarkers(test_df$long,
                       test_df$lat,
                       color = colors_list, 
                       # radius = 3,
                       radius = ifelse(test_df$FranClass == "제주공항", 100, 3),
                       fill = ifelse(test_df$FranClass == "제주공항", F, T),
                       fillOpacity = 1,
                       opacity = 0.5,
                       popup = paste0("<br/> 전화번호: ",test_df$FranClass,"",
                                      
                                      "<br/> 주소: ", test_df$Type,"")) %>%
      addLegend("topleft",
                colors = colors_list,
                labels = unique(test_df$FranClass),
                opacity = 1)
  })
  
  
  
  
  
  #create a data object to display data
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