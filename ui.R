# 제주도 공간정보 탐색적 데이터 분석 경진대회
print("------------ui------------")

ui <-   
  navbarPage("DACON: 공간정보 탐색적 데이터 분석 경진대회", id="nav",
             # # Main ----
             # tabPanel("Dacon",
             #          tags$a(
             #            href="https://dacon.io/competitions/official/235682/overview/", 
             #            tags$img(src="img_main.PNG", 
             #                     title="Example Image Link", 
             #                     width="100%",height="25%"))
             # ), 
             
             # Map ----
             tabPanel("소상공인 구분 지도 분포",
                      div(class="outer",
                          tags$head(
                            shiny::tags$link(rel = "shortcut icon", href = "https://unfinishedgod.netlify.app/Gareth_Frank_Bale_circle_index.ico"), 
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          
                          # leaflet
                          leafletOutput("bbmap", width="100%", height="100%"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 450, height = 1000,
                                        
                                        h2("Option"),
                                        h4("총 1,072,439개중 표본으로 약 10724개 사용"), 
                                        
                                        selectInput(inputId = "selectTime",
                                                    label = h4("시간"),
                                                    choices = Time_list_2,
                                                    selected = "ALL",
                                                    width = 400),
                                        
                                        selectInput(inputId = "selectYM",
                                                    label = h4("년/월"),
                                                    choices = YM_list_2,
                                                    selected = "ALL",
                                                    width = 400),
                                        
                                        selectInput(inputId = "selectFranClass",
                                                    label = h4("소상공인구분"),
                                                    choices = FranClass_list_2,
                                                    selected = "ALL",
                                                    width = 400),
                                        
                                        
                                        
                                        plotlyOutput("histCentile", height = 300),
                                        plotlyOutput("scatterCollegeIncome", height = 300)
                                        
                          )
                      )
             ),
             
             # 시간에 따른 업종별 총 사용금액----
             tabPanel("총사용 사용금액",
                      fluidRow(
                        h3("시간에 따른 총 사용금액 증감 현황"), 
                        box(
                          width = 4, status = "info", solidHeader = TRUE,
                          selectInput(inputId = "selectType_2",
                                      label = h4("업종명"),
                                      choices = diff_time_list,
                                      selected = c("면세점","항공사","인터넷Mall","대형할인점","농협하나로클럽","건강진단","농축협직영매장","종합병원"),
                                      width = 500)
                        ),
                        box(
                        )
                        
                      ),
                      fluidRow(
                        h3("시간에 따른 총 사용금액 증가 상위 10개 업종"), 
                        box(
                          width = 9, status = "info", solidHeader = TRUE,
                          plotlyOutput("total_spend_increase")
                        ),
                        box(
                          width = 3, status = "info", solidHeader = TRUE,
                          reactableOutput("total_spend_increase_table")
                        )
                      ),
                      fluidRow(
                        h3("시간에 따른 총 사용금액 감소 하위 10개 업종"), 
                        box(
                          width = 9, status = "info", solidHeader = TRUE,
                          plotlyOutput("total_spend_decrease")
                        ),
                        box(
                          width = 3, status = "info", solidHeader = TRUE,
                          reactableOutput("total_spend_decrease_table")
                        )
                        
                      )
             ), 
             
             # 시간에 따른 업종별 재난 지원금 사용금액----
             tabPanel("재난지원금 사용금액",
                      fluidRow(
                        h3("시간에 따른 재난지원금 사용금액 증감 현황"), 
                        box(
                          width = 4, status = "info", solidHeader = TRUE,
                          selectInput(inputId = "selectType_3",
                                      label = h4("업종명"),
                                      choices = diff_time_list,
                                      selected = c("면세점","항공사","인터넷Mall","대형할인점","농협하나로클럽","건강진단","농축협직영매장","종합병원"),
                                      width = 500)
                        ),
                        box(
                        )
                      ),
                      fluidRow(
                        h3("시간에 따른 재난지원금 사용금액 증가 상위 10개 업종"), 
                        box(
                          width = 9, status = "info", solidHeader = TRUE,
                          plotlyOutput("dis_spend_increase")
                        ),
                        box(
                          width = 3, status = "info", solidHeader = TRUE,
                          reactableOutput("dis_spend_increase_table")
                        )
                      ),
                      fluidRow(
                        h3("시간에 따른 재난지원금 사용금액 감소 하위 10개 업종"), 
                        box(
                          width = 9, status = "info", solidHeader = TRUE,
                          plotlyOutput("dis_spend_decrease")
                        ),
                        box(
                          width = 3, status = "info", solidHeader = TRUE,
                          reactableOutput("dis_spend_decrease_table")
                        )
                      ), 
             ),
             
             # Data ----
             tabPanel("데이터",
                      DT::dataTableOutput("data")
             )
             # # About ----
             # tabPanel("About",
             # )
  )
# )

