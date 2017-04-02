
rm(list = ls())
library(reshape2)
library(RColorBrewer)
library(data.table)
library(plotly)

col1 = "Turquoise Blue"
col2 = "Pink Sherbert"
col3 = '#1f77b4'
col4 = '#ff7f0e'


# plot cases/deaths for disease
plot1<- function(df, source){

    if(df[,length(unique(event))]==1){
        summary = df[,.(number = sum(number)), by= disease][order(-number)]
        plot_ly(summary, x = ~disease, y = ~number, type ="bar",
                color = I('#1f77b4'), source = source)%>%
        layout(yaxis = list(title = "cases"),
                dragmode = "click",
                title = "number of cases by disease",
                margin = list(b = 250, t = 100))
    }else{
        colors = c('#1f77b4', '#ff7f0e')
        summary <- df[, .(number = sum(number)),
                    by = .(disease, event) ][order(-event,-number)]
        summary %>%
            plot_ly(x = ~disease, y = ~number, type ="bar",
                    color = ~event, colors = colors, source = source)%>%
            layout(yaxis = list(type = "log",
                    title = "number (in log scale)"),
                    xaxis = list(tickangle = 45),
                    dragmode = "click",  barmode = 'group',
                    title = "number of cases and deaths by disease",
                    margin = list(b = 250, t = 100))
        
    }
    
}
    
    
heatmap <- function(df, year_range, vaccine, time, data, disease_select,source){
    # -----------------------------vaccine---------------------------------
    # vaccine line 
    vaccine_year = vaccine[disease == disease_select, year]
    if(length(vaccine_year)>0){
        x_pos = (vaccine_year-year_range[1])/(year_range[2]-year_range[1])*0.75
        lines2 = list()
        lines2[[1]] <- list(type = "line", line = list(color = "saddlebrown"), 
            xref = "paper", yref = "paper",
            x0 = x_pos, x1 = x_pos, width = 6, 
            y0 = 0.8, y1 = 1)
        # annotation for vaccine 
        a = list(xref = "paper",
            yref = "paper",
            x = 0.9,
            y = 0.92,
            ay = 0, 
            ax = 40, 
            text = paste("<= First vaccine in", vaccine_year), 
            # Styling annotations' text:
            font = list(family = 'sans serif',
                color = "saddlebrown", 
                size = 16)
        )
        
    }
    
    #----------------------------mat-------------------------------------
    
    df = df[!duplicated(df[,c("time","state")]),]
    # create a categorical variable based on quntiles of inc_per >0
    if(data == "incidence per 100K"){
        
        breaks = c(0, unique(quantile(df[incidence_per_100000>0,incidence_per_100000], seq(0,1,0.1))))

        num_breaks = length(breaks)
        df$new = as.numeric(cut(df[,incidence_per_100000], breaks = c(-1,breaks), right = TRUE, labels = 1:num_breaks))
        
        range1 = range(df[,incidence_per_100000])
        breaks1 = round(seq(range1[1], range1[2], length.out = 12))
        
        df$new1 = as.numeric(cut(df[,incidence_per_100000], breaks = c(-1,breaks1), right = TRUE, labels = 1:12))
        
        
        # actual value 
        mat0 = acast(df[, .(state, time,new1)], state~time, value.var = "new1")
        # quantile 
        mat1 = acast(df[, .(state, time,new)], state~time, value.var = "new")
        # actual value 
        mat2 = acast(df[, .(state, time, incidence_per_100000)], state~time, value.var = "incidence_per_100000")
        
    }else{
        breaks = c(0, unique(quantile(df[number>0,number], seq(0,1,0.1))))
        num_breaks = length(breaks)
        df$new = as.numeric(cut(df[,number], breaks = c(-1,breaks), right = TRUE, labels = 1:num_breaks))
        
        range1 = range(df[,number])
        breaks1 = round(seq(range1[1], range1[2], length.out = 12))
        df$new1 = as.numeric(cut(df[,number], breaks = c(-1,breaks1), right = TRUE, labels = 1:12))
        
        
        # actual value 
        mat0 = acast(df[, .(state, time,new1)], state~time, value.var = "new1")
        # quantile 
        mat1 = acast(df[, .(state, time,new)], state~time, value.var = "new")
         # actual value 
        mat2 = acast(df[, .(state, time, number)], state~time, value.var = "number")
    }
        
    # cases 
        mat3 = acast(df[, .(state, time, number)], state~time, value.var = "number")

    
    # hover 
    f1 <- function(x){
        return(paste((data), "<br>", as.character(x)))
    }

    # --------------------------quantile ---------------------------------
    
    if(data == "DEATHS"){
        name1 <- paste("deaths by", time)
        name2 <- "deaths by state"
        
    }else{
        name1 <- paste("cases by", time)
        name2 <- "cases by state"
    }
    
    
    q <- plot_ly()
    q <- add_trace(q,
        x = colnames(mat1), y = rownames(mat1), 
        z = mat1, text =  apply(mat2, 2, f1),  hoverinfo = "y+x+text" ,type = "heatmap", zauto = FALSE, source = source, 
        colors = rev(c("black", brewer.pal(num_breaks-1,"Spectral"))), 
        colorbar = list(title = data , tickmode = "array", tickvals = 1:num_breaks, ticktext = breaks, y = 0.35, len = 0.7))
    # bar plot of cases by week 
    q <- add_trace(q,
        x=colnames(mat3), y=colSums(mat3, na.rm = TRUE),  type="bar", hoverinfo = "x+y", 
        name = name1, 
        xaxis ="x1", yaxis = "y2", color = I('#1f77b4'))
    
    # bar plot of cases by state 
    q <- add_trace(q, 
        y = rownames(mat3), x = rowSums(mat3, na.rm = TRUE),  type = "bar", hoverinfo = "y+x", 
        orientation = "h", 
        name = name2, 
        xaxis = "x2", yaxis = "y1", color = I('skyblue'))
    
    
    if(length(vaccine_year)>0){
    # add layout 
    q <- layout(q, yaxis = list(title = "state", domain = c(0, 0.79), zeroline = FALSE, side = "right"),
        yaxis2 = list(domain = c(0.8, 1)), 
        xaxis = list(title = time, domain = c(0, 0.75), zeroline = TRUE, side = "bottom"),
        xaxis2 = list(domain = c(0.8, 1)), 
        margin = list(b = 100),
        title = paste(disease_select, "by", time, ", quantile color scale"), 
        legend = list(x = 0.85, y = 0.9),
        shapes = lines2,
        annotations = a) 
    }else{
        
    q <- layout(q, yaxis = list(title = "state", domain = c(0, 0.79), zeroline = FALSE, side = "right"),
            yaxis2 = list(domain = c(0.8, 1)), 
            xaxis = list(title = time, domain = c(0, 0.75), zeroline = TRUE, side = "bottom"),
            xaxis2 = list(domain = c(0.8, 1)), 
            margin = list(b = 100),
            title = paste(disease_select, "by", time, ", quantile color scale"), 
            legend = list(x = 0.85, y = 0.9)
    )
    }
    
    
    # --------------------------actual value ---------------------------------
    p <- plot_ly()
    # add heatmap 
    p <- add_trace(p,
        x = colnames(mat2), y = rownames(mat2), 
        z = mat0, text =  apply(mat2, 2, f1),  hoverinfo = "y+x+text",
        type = "heatmap", zauto = FALSE, source = source, 
        colors = rev(c("black", brewer.pal(11,"Spectral"))), 
        colorbar = list(title = data , tickmode = "array", tickvals = 1:12, ticktext = breaks1, y = 0.35, len = 0.7))
    # bar plot of cases by week 
    p <- add_trace(p,
        x=colnames(mat3), y=colSums(mat3, na.rm = TRUE),  type="bar", hoverinfo = "x+y",
        name = name1, 
        xaxis ="x1", yaxis = "y2", color = I('#1f77b4'))
    
    # bar plot of cases by state 
    p <- add_trace(p, 
        y = rownames(mat3), x = rowSums(mat3, na.rm = TRUE),  type = "bar", hoverinfo = "y+x", 
        orientation = "h", 
        name = name2, 
        xaxis = "x2", yaxis = "y1", color = I('skyblue'))
    
    # add layout 
    if(length(vaccine_year)>0){
    p <- layout(p, yaxis = list(title = "state", domain = c(0, 0.79), zeroline = FALSE, side = "right"),
        yaxis2 = list(domain = c(0.8, 1)), 
        xaxis = list(title = "week", domain = c(0, 0.75), zeroline = TRUE, side = "bottom"),
        xaxis2 = list(domain = c(0.8, 1)), 
        margin = list(b = 100),
        title = paste(disease_select, "by", time),  
        legend = list(x = 0.85, y = 0.9),
        shapes = lines2,
        annotations = a) 
    }else{
        p <- layout(p, yaxis = list(title = "state", domain = c(0, 0.79), zeroline = FALSE, side = "right"),
            yaxis2 = list(domain = c(0.8, 1)), 
            xaxis = list(title = "week", domain = c(0, 0.75), zeroline = TRUE, side = "bottom"),
            xaxis2 = list(domain = c(0.8, 1)), 
            margin = list(b = 100),
            title = paste(disease_select, "by", time), 
            legend = list(x = 0.85, y = 0.9)) 
        
    }
    plot_list = list()
    plot_list[[1]] = q
    plot_list[[2]] = p
    return(plot_list)
    
}
    

    # -------------------------- value ---------------------------------
    

bubble <- function(df, t,  data , disease_select, src){
    
    if(data %in% c("CASES","cases")){
        col = '#1f77b4'
    }else{
        col = '#ff7f0e'
    }
    
    
    if(nrow(df) == 0){return(NULL)}
    
    times = df[,sort(unique(time))]
    time_select= times[t]
    df_sub = df[time == time_select]
    
    g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray85"),
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white")
    )
    
    if(data == "incidence per 100K"){
        num_range = df[, range(incidence_per_100000)]
        num1 = num_range[1]
        num2 = num_range[2]
        slope = (1500-100)/(num2-num1)
        size_list = 100+ slope*(df_sub$incidence_per_100000 - num1)
        p <-plot_geo(df_sub, locationmode = 'USA-states', source = src, 
                x = ~lon, y = ~lat, size =size_list, 
                hoverinfo = "text",
                text = ~paste(df_sub$loc, ",", df_sub$state,  "<br />", df_sub$incidence_per_100000, "cases"), color = I(col))%>% 
    layout(title = paste(disease_select, "in", time_select ), geo = g)
        
    }else{
        num_range = df[, range(number)]
        num1 = num_range[1]
        num2 = num_range[2]
        slope = (1500-100)/(num2-num1)
        size_list = 100+ slope*(df_sub$number - num1)
        p<-plot_geo(df_sub, locationmode = 'USA-states', source = src, 
                x = ~lon, y = ~lat, size =size_list, hoverinfo = "text",
                text = ~paste(df_sub$loc, ",", df_sub$state,  "<br />", df_sub$number, "cases"), color = I(col))%>% 
            layout(title = paste(disease_select, "in", time_select, data), geo = g)
    }
    return(p)
    
}



bar_chart <- function(df, t, data, src){
    df = data.table(df)
    if(data %in% c("CASES","cases")){
        col = '#1f77b4'
    }else{
        col = '#ff7f0e'
    }
    
    times = df[,sort(unique(time))]
    x_pos = t/length(times)
    lines2 = list()
    lines2[[1]] <- list(type = "line", line = list(color = "black"), 
        xref = "paper", yref = "paper",
        x0 = x_pos, x1 = x_pos, width = 6, 
        y0 = 0, y1 = 1)
    
    summary = df[,.(number = sum(number)),by = time]
    
    plot_ly(summary, x = ~time, y = ~number, type ="bar",
        marker = list(color = col), source = src) %>% 
        layout(shapes = lines2,title = paste(0,data,0, sep =""), margin = list(b =100)) 
    
}
    
    
    
    

    
    
    
    
    
    
    