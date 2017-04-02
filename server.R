rm(list = ls())

#install packages if missing
list.of.packages <- c("shiny","plotly","shinythemes","data.table","reshape2",
    "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(plotly)
library(shiny)
library(data.table)
source("helper.R")



# read data frames
L1= readRDS("L1.rds")
L2 = readRDS("L2.rds")
L3= readRDS("L3.rds")
vaccine <- readRDS("vaccine.rds")
state_list = readRDS("state_list.rds")


# convert to data.table format 
L1 = data.table(L1)
vaccine = data.table(vaccine)
setkey(L1, disease, year, state)

#---------------------------
L2 = data.table(L2)
L3 = data.table(L3)
L3 = L3[!is.na(year)]
setkey(L2, disease, year, state)
setkey(L3, disease, year, state)



shinyServer(function(input, output, session) {
    
    output$plot1 <- renderPlotly({plot1(L1, source = "L1_disease")})
    output$plot2 <- renderPlotly({plot1(L2, source = "L2_disease")})
    output$plot3 <- renderPlotly({plot1(L3, source = "L3_disease")})
    
    output$L1_disease_summary <- renderText({
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L1_sub <- droplevels(L1[disease_select])
            state_num = length(levels(droplevels(L1_sub[loc_type == "STATE"])$state))
            city_num = length(levels(droplevels(L1_sub[loc_type == "CITY"])$loc))
            terr_num = length(levels(droplevels(L1_sub[loc_type == "TERR"])$loc))
            year_range = range(L1_sub$year)
            paste(
                paste( disease_select), 
                '\n', 
                "location:", 
                paste(state_num, "states"),
                paste(city_num, "cities"),
                paste(terr_num, "territories"), 
                '\n',
                "year:",
                paste(year_range[1], "-", year_range[2]), 
                sep = '\n')
            
        }
    })
    
    
    
    output$L2_disease_summary <- renderText({
        click = event_data("plotly_click",source = "L2_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L2_sub <- droplevels(L2[disease_select])
            state_num = length(levels(droplevels(L2_sub[loc_type == "STATE"])$state))
            city_num = length(levels(droplevels(L2_sub[loc_type == "CITY"])$loc))
            terr_num = length(levels(droplevels(L2_sub[loc_type == "TERR"])$loc))
            year_range = range(L2_sub$year)
            paste(
                paste(disease_select), 
                '\n', 
                "location:", 
                paste(state_num, "states"),
                paste(city_num, "cities"),
                paste(terr_num, "territories"), 
                '\n',
                "year:",
                paste(year_range[1], "-", year_range[2]), 
                sep = '\n')
            
        }
    })
    
    
    
    
    output$L3_disease_summary <- renderText({
        click = event_data("plotly_click",source = "L3_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L3_sub <- droplevels(L3[disease_select])
            state_num = length(levels(droplevels(L3_sub[loc_type == "STATE"])$state))
            city_num = length(levels(droplevels(L3_sub[loc_type == "CITY"])$loc))
            terr_num = length(levels(droplevels(L3_sub[loc_type == "TERR"])$loc))
            year_range = range(L3_sub$year)
            paste(
                paste(disease_select), 
                '\n', 
                "location:", 
                paste(state_num, "states"),
                paste(city_num, "cities"),
                paste(terr_num, "territories"), 
                '\n',
                "year:",
                paste(year_range[1], "-", year_range[2]), 
                sep = '\n')
            
        }
    })
    
    
    output$data_list1 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            radioButtons("data1","data type", choices = c("cases", "incidence per 100K"),
                         selected = NULL, inline = FALSE)
        }
     })
    
    output$data_list2 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L2_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L2_sub <- droplevels(L2[disease_select])
            L2_sub <- L2_sub[loc_type %in% c("CITY", "STATE")]
            event_list = rev(L2_sub[, unique(event)])
            if(length(event_list ) == 0 ){ return(NULL)}
            else{
            radioButtons("data2","data type", choices = event_list,
                selected = NULL, inline = FALSE)
            }
        }
    })
    
    
    output$data_list3 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L3_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L3_sub <- droplevels(L3[disease_select])
            L3_sub <- L3_sub[loc_type %in% c("CITY", "STATE")]
            event_list = rev(L3_sub[, unique(event)])
            if(length(event_list ) == 0 ){ return(NULL)}
            else{
                radioButtons("data3","data type", choices = event_list,
                    selected = NULL, inline = FALSE)
            }
        }
    })
    
    
    output$time_list1 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            radioButtons("time1","time unit", choices = c("week", "month","year"),
                selected = NULL, inline = FALSE)
        }
    })
    
    
    output$time_list2 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L2_disease")

        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L2_sub <- droplevels(L2[disease_select])
            L2_sub <- L2_sub[loc_type %in% c("CITY", "STATE")]
            event_list = L2_sub[, unique(event)]
            if(length(event_list ) == 0 ){ return(NULL)}
            else{
            radioButtons("time2","time unit", choices = c("week", "month","year"),
                selected = NULL, inline = FALSE)}
        }
    })
    
    output$time_list3 <- renderUI({ 
        
        click = event_data("plotly_click",source = "L3_disease")
        
        if(is.null(click)){
            return(NULL)
        }else{
            disease_select = click$x[1]
            L3_sub <- droplevels(L3[disease_select])
            L3_sub <- L3_sub[loc_type %in% c("CITY", "STATE")]
            event_list = L3_sub[, unique(event)]
            if(length(event_list ) == 0 ){ return(NULL)}
            else{
                radioButtons("time3","time unit", choices = c("week", "month","year"),
                    selected = NULL, inline = FALSE)}
        }
    })
    
    
     plots_1 <- eventReactive(input$plot1, {
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data1
            time = input$time1
            
            # disease = "SMALLPOX"
            # data = "cs"
            # time = "year"
            if(data == "cases"){ 
                df = L1[disease == disease & loc_type == "STATE", 
                    lapply(.SD, sum), by = c(time,"state"),
                    .SDcols = c( "number")]
                setnames(df,(time), "time")
            }else{
                df = L1[disease == disease & loc_type == "STATE", 
                    lapply(.SD, sum), by = c(time,"state"),
                    .SDcols = c("incidence_per_100000", "number")]
                setnames(df,(time), "time")
            }
    
            list = heatmap(df, range(L1$year), vaccine, time, data, disease, source="L1")
            
            if(data == "cases"){ 
                df2 = L1[disease == disease & loc_type == "CITY", 
                    lapply(.SD, sum), by = c(time,"loc", "lon","lat", "state"),
                    .SDcols = c( "number")]
                setnames(df2,(time), "time")
            }else{
                df2 = L1[disease == disease & loc_type == "CITY", 
                    lapply(.SD, sum), by = c(time,"loc", "lon","lat", "state"),
                    .SDcols = c("incidence_per_100000", "number")]
                setnames(df2,(time), "time")
            }
            
            list[[3]]<- df2
            list[[4]] <- c(time, data, disease)
                
            return(list)
        } 
    })
    
    
    plots_2 <- eventReactive(input$plot2, {
        click = event_data("plotly_click",source = "L2_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data2
            time = input$time2
            
            df = L2[disease == disease & loc_type == "STATE" & event == data, 
                lapply(.SD, sum), by = c(time,"state"),
                .SDcols = c( "number")]
            setnames(df,(time), "time")
            
            list = heatmap(df, range(L2$year), vaccine, time, data, disease,source="L2")
            
            df2 = L2[disease == disease & loc_type == "CITY" & event == data, 
                lapply(.SD, sum), by = c(time,"loc", "lon","lat", "state"),
                .SDcols = c( "number")]
            setnames(df2,(time), "time")
       
            list[[3]]<- df2
            list[[4]] <- c(time, data, disease)
            
            return(list)
    
        } 
    })
    
    
    
    
    
    plots_3 <- eventReactive(input$plot3, {
        click = event_data("plotly_click",source = "L3_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data3
            time = input$time3
            
            df = L3[disease == disease & loc_type == "STATE" & event == data, 
                lapply(.SD, sum), by = c(time,"state"),
                .SDcols = c( "number")]
            setnames(df,(time), "time")
    
            list = heatmap(df, range(L3$year), vaccine, time, data, disease,source="L3")
            df2 = L3[disease == disease & loc_type == "CITY" & event == data, 
                lapply(.SD, sum), by = c(time,"loc", "lon","lat", "state"),
                .SDcols = c( "number")]
            setnames(df2,(time), "time")
            
            list[[3]]<- df2
            list[[4]] <- c(time, data, disease)
            
            return(list)
            
        } 
    })
    
    
    output$click1 <- renderText({plots_1()[[4]]})
    output$click2 <- renderText({plots_2()[[4]]})
    output$click3 <- renderText({plots_3()[[4]]})
    
    
# L1 ----------------------------------------------------------    
    output$plot1_1 <- renderPlotly({
        subplot(plots_1()[[1]])
    })
    
    output$plot1_2 <- renderPlotly({
        subplot(plots_1()[[2]])
    })
    
    output$plot1_3<- renderPlotly({
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            data = input$data1
        bar_chart(plots_1()[[3]], input$t1, data, "L1-3")
        }
        })
    
    output$plot1_4<- renderPlotly({
        click = event_data("plotly_click",source = "L1_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data1
        subplot(bubble(plots_1()[[3]],input$t1, data, disease, "L1-4"))
      
        }
    })
    
    
    
    output$plot1_3_slider <- renderUI({
        time = plots_1()[[3]][,unique(time)]
        min = 1
        max = length(time)
        sliderInput("t1", "time", 
            min=min, max=max, value=1,  step=1, width = "75%", animate=animationOptions(loop=TRUE))
        
    })
    
# L2----------------------------------------------------------------------    
    output$plot2_1 <- renderPlotly({
        subplot(plots_2()[[1]])
    })
    
    output$plot2_2 <- renderPlotly({
        subplot(plots_2()[[2]])
    })
    
 
    output$plot2_3<- renderPlotly({
        click = event_data("plotly_click",source = "L2_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            data = input$data2
            bar_chart(plots_2()[[3]], input$t2, data, "L2-3")
        }
    })
    
    
    output$plot2_3_slider <- renderUI({
        time = plots_2()[[3]][,unique(time)]
        min = 1
        max = length(time)
        sliderInput("t2", "time", 
        min=min, max=max, value=1,  step=1, width = "75%", animate=animationOptions(loop=TRUE))
    })   
        
    output$plot2_4<- renderPlotly({
        click = event_data("plotly_click",source = "L2_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data2
            subplot(bubble(plots_2()[[3]],input$t2, data, disease, "L2-4"))
            }
        })
    
    
    # L3  
    output$plot3_1 <- renderPlotly({
        subplot(plots_3()[[1]])
    })
    
    output$plot3_2 <- renderPlotly({
        subplot(plots_3()[[2]])
    })
    
    output$plot3_3<- renderPlotly({
        click = event_data("plotly_click",source = "L3_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            data = input$data3
            bar_chart(plots_3()[[3]], input$t3, data, "L3-3")
        }
    })
    
    
    
    output$plot3_3_slider <- renderUI({
        time = plots_3()[[3]][,unique(time)]
        min = 1
        max = length(time)
        sliderInput("t3", "time", 
            min=min, max=max, value=1,  step=1, width = "75%", animate=animationOptions(loop=TRUE))
    })   
    
    output$plot3_4<- renderPlotly({
        click = event_data("plotly_click",source = "L3_disease")
        if(is.null(click)){
            return(NULL)
        }else{
            disease = click$x[1]
            data = input$data3
            
            subplot(bubble(plots_3()[[3]],input$t3, data, disease, "L3-4"))
            
        }
    })
    
################

})    
    
 