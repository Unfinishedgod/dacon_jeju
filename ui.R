# 제주도 공간정보 탐색적 데이터 분석 경진대회
print("------------ui------------")

ui <- navbarPage("DACON: 공간정보 탐색적 데이터 분석 경진대회", id="nav",
           tabPanel("Interactive",
                    tags$a(
                      href="https://dacon.io/competitions/official/235682/overview/", 
                      tags$img(src="img_main.PNG", 
                               title="Example Image Link", 
                               width="100%",height="30%"))
                    ), 
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          shiny::tags$link(rel = "shortcut icon", href = "https://dacon.io/favicon.ico"), 
                          # Include our custom CSS
                          includeCSS("styles.css")
                          # includeScript("gomap.js")
                        ),
                        # leaflet
                        leafletOutput("bbmap", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 450, height = 1000,
                                      
                                      h2("Option"),
                                      
                                      sliderInput(inputId = "selectTime", 
                                                  label = h4("시간"),
                                                  min = 0, max = 23,
                                                  value = 0, step = 1,
                                                  pre = "", sep = ",",
                                                  width = 400, 
                                                  animate = animationOptions(interval = 500, loop = TRUE)), 
                                      
                                      sliderInput(inputId = "selectYM", 
                                                  label = h4("년/월"),
                                                  min = 5, max = 8,
                                                  value = 0, step = 1,
                                                  pre = "", sep = ",",
                                                  width = 400, 
                                                  animate = TRUE), 
                                      
                                      selectInput(inputId = "selectType",
                                                  label = h4("업종명"),
                                                  choices = Type_list,
                                                  selected = "일반한식",
                                                  width = 400),
                                      
                                      plotlyOutput("histCentile", height = 300),
                                      plotlyOutput("scatterCollegeIncome", height = 300)
                                      
                        )
                    )
           ),
          
            tabPanel("data",
                    DT::dataTableOutput("data")
           )
)
# )

