rm(list = ls())
library(plotly)
library(shiny)
library(shinythemes)

color = "color:#0099cc"

shinyUI(fluidPage(
    
    # Some custom CSS
    tags$head(
        tags$style(HTML("
            /* Smaller font for preformatted text */
            /*pre, table.table {
            font-size: smaller;
             }*/
            
            body {
            min-height: 2000px;
            font-size: 18px;
            font-family: Arial, Helvetica, sans-serif;
 
            }
            
            .option-group {
            border: 1px solid #ccc;
            border-radius: 10px;
            margin-left: 40px;
            margin-right: 40px;
            margin-top: 10px;
            margin-bottom: 30px;
            background-color: #f5f5f5;
            }
            
            .option-header1 {
            color: #3383FF;
            margin-bottom: 15px;
            font-size: 20px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }

            .option-header2 {
            color: #FF5533;
            margin-bottom: 15px;
            font-size: 20px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }

            .option-header3 {
            color: #FFDA33;
            margin-bottom: 15px;
            font-size: 20px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }

             .option-header01 {
            margin-bottom: 18px;
            font-size: 16px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }
            
             .option-header11 {
            color: #3383FF;
            margin-bottom: 5px;
            font-size: 16px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }
            
            .option-header21 {
            color: #FF5533;
            margin-bottom: 5px;
            font-size: 16px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }
            
            .option-header31 {
            color: #FFDA33;
            margin-bottom: 5px;
            font-size: 16px;
            font-family: Arial, Helvetica, sans-serif;
            font-weight: bold;
            align: center
            }
            
            "))
        ),
    # set theme
    theme = shinytheme("yeti"),
    
    tabsetPanel(
        #------------home panel -----------------
            tabPanel("Home", 
                # title 
                fixedRow(
                    align = "center", 
                    br(),
                    img(src="logo1.png", height = 150, width = 650),
     
                    fixedRow(
                        br(),
                        column(4,align = "center", 
                            div(class = "option-group",
                                div(class = "option-header1", "Level 1"),
                                p("8 diseases"), 
                                p(),
                                p("604 million cases"),
                                p("0 million deaths"),
                                br(),
                                strong("US location:"),
                                p("51 states"),
                                p("116 cities"),
                                p("0 territorries"), 
                                br(),
                                strong("year:"),
                                p("1916 - 2011")
                            )
                            
                            
                        ),
                        
                        column(4,align = "center", 
                            div(class = "option-group",
                                div(class = "option-header2", "Level 2"),
                                p("50 diseases"), 
                                p(),
                                p("65 million cases"),
                                p("5 million deaths"),
                                br(),
                                strong("US location:"),
                                p("51 states"),
                                p("573 cities"),
                                p("6 territorries"), 
                                br(),
                                strong("year:"),
                                p("1887 - 2014")
                            )
                            
                        ),
                        
                        column(4,align = "center", 
                            div(class = "option-group",
                                div(class = "option-header3", "Level 3"),
                                p("55 diseases"), 
                                p(),
                                p("2598 million cases"),
                                p("6 million deaths"),
                                br(),
                                strong("US location:"),
                                p("51 states"),
                                p("2019 cities"),
                                p("6 territorries"), 
                                br(),
                                strong("year:"),
                                p("1887 - 2014")
                            )
                            
                        )
                    )   
                )
        #        
        ), 
                
        #---------------level 1----------------------
            tabPanel("Level 1", 
                
                # summary of diseases 
                fixedRow(
                column(2, 
                    br(),
                    br(),
                    br(),
                    
                    strong("click bar to select disease", style = color), 
                    br(),
                    verbatimTextOutput("L1_disease_summary")
                ),
                column(10, align = "center", 
                    plotlyOutput("plot1", height = "700px")
                )
                ), 
                verbatimTextOutput("click1"),
                
                # heatmap 
                h2("Heatmap for data at state level", style = color, align = "center"), 
                br(), 
                fixedRow(
                    column(2,
                        strong("select plot option", style = color), 
                        br(), 
                        uiOutput("data_list1"), 
                        uiOutput("time_list1"), 
                        actionButton("plot1", "generate plots")
                        ),
                    column(10, align = "center", 
                        plotlyOutput("plot1_1", height = "600px"),
                        plotlyOutput("plot1_2", height = "600px")  
                        
                        )
                    
                ), 
                
                # bubble map 
                
                h2("Bubble map for data at city level", style = color, align = "center"), 
                br(), 
                fixedRow(column(2),
                         column(8,
                             plotlyOutput("plot1_4", height = "450px"),
                             plotlyOutput("plot1_3", height = "200px"),
                             uiOutput("plot1_3_slider")
                         )
                    )
               
        #        
        ), 
        #--------------------level 2 ----------------------
        tabPanel("Level 2", 
            
            # summary of diseases 
            fixedRow(
                column(2, 
                    br(),
                    br(),
                    br(),
                    strong("click bar to select disease", style = color), 
                    br(),
                    verbatimTextOutput("L2_disease_summary")
                ),
                column(10, align = "center", 
                    plotlyOutput("plot2", height = "700px")
                )
            ), 
            
            verbatimTextOutput("click2"), 

            # heatmap 
            h2("Heatmap for data at state level", style = color, align = "center"), 
            br(), 
            fixedRow(
                column(2,
                    strong("select plot option", style = color), 
                    br(), 
                    uiOutput("data_list2"), 
                    uiOutput("time_list2"), 
                    actionButton("plot2", "generate plots")
                ),
                column(10, align = "center", 
                    plotlyOutput("plot2_1", height = "600px"),
                    plotlyOutput("plot2_2", height = "600px")  
                    
                )
                
            ), 
            
            # bubble map 
            h2("Bubble map for data at city level", style = color, align = "center"), 
            br(), 
            fixedRow(column(2),
                column(8,
                    plotlyOutput("plot2_4", height = "450px"),
                    plotlyOutput("plot2_3", height = "200px"),
                    uiOutput("plot2_3_slider")
                )
            )
      
        ), 
        #-------------------level 3--------------------------
        tabPanel("Level 3", 
            # summary of diseases 
            fixedRow(
                column(2, 
                    br(),
                    br(),
                    br(),
                    
                    strong("click bar to select disease", style = color), 
                    br(),
                    verbatimTextOutput("L3_disease_summary")
                ),
                column(10, align = "center", 
                    plotlyOutput("plot3", height = "700px")
                )
            ), 
            
            verbatimTextOutput("click3"),
            
            # heatmap 
            h2("Heatmap for data at state level", style = color, align = "center"), 
            br(), 
            fixedRow(
                column(2,
                    strong("select plot option", style = color), 
                    br(), 
                    uiOutput("data_list3"), 
                    uiOutput("time_list3"), 
                    actionButton("plot3", "generate plots")
                ),
                column(10, align = "center", 
                    plotlyOutput("plot3_1", height = "600px"),
                    plotlyOutput("plot3_2", height = "600px")  
                    
                )
                
            ), 
        
       
            # bubble map 
            
            h2("Bubble map for data at city level", style = color, align = "center"), 
            br(), 
            fixedRow(column(2),
                column(8,
                    plotlyOutput("plot3_4", height = "450px"),
                    plotlyOutput("plot3_3", height = "200px"),
                    uiOutput("plot3_3_slider")
                )
            )
            
            #        
        )
    )    
    
  
))   
    
    
  
 
   
  

    
    
    
 
    
  