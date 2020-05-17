
library(ggplot2)
library(gridExtra)
library(leaflet)
library(shiny)
library(shinydashboard)

library(data.table)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)
library(grid)


shinyServer(function(input,output){
  Hospital<-reactive({
    read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSlZAE5oRzY3lMhuVHOz3tTTGHaeoEI9B5a8m-lAAC3UGLFNowFi1pM3yzJLLNZCKpTze9rRszXnOIX/pub?output=csv")})
  
  data<-reactive({
    read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTlnogupswk1FW2T00LzzoPsx7e0BoYFgougFreXbFjfCxENGIA4txYnPZZpkGA2T80XcqC7j-A7fNC/pub?gid=791034843&single=true&output=csv",header = TRUE, sep= ";") 
  })
    world3<-reactive({read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb4cyDF4fmg_6S1wlaYg3FE26NAdIvEKbiw-fofQZioAqDmRRrbdMR_rHlmqqt8Ycp20KoOHpK_bAM/pub?gid=906510162&single=true&output=csv",header = TRUE, sep= ";") 
  
      })
    world<-reactive({
      world3<-world3()
      names(world3)[8] <- c('Recovered')
      world3$Recovered=as.numeric(sub("\\..*", "", world3$Recovered))
  
      world3$Country.Region<-as.matrix(as.character(world3$Country.Region))
      world3$Country.Region[world3$Country.Region=='Mainland China']<- 'China'
      world3
    })
    output$filedf <- DT::renderDataTable({    df_stat_by_country()})
    df_confirmed<-reactive({read.csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTkkEOzG65PQYt264v-qjfK-CA5eMBvRgbfpZ1AqraX8U8woj_VtK-y2pt5lKrI-R8YT6ULB5wVWt23/pub?gid=1357658797&single=true&output=csv") })
    df_stat_by_country<-reactive({
      world=world()
      df_confirmed=df_confirmed()
      world$Country.Region<-as.matrix(as.character(world$Country.Region))
      world$Country.Region[world$Country.Region=='Mainland China']<- 'China'
      names(world) <- tolower(names(world))
    
    names(world)[3:4] <- c('province_state', 'country_region')
    
    names(df_confirmed) <- tolower(names(df_confirmed))
    
    names(df_confirmed)[1:2] <- c('province_state', 'country_region')
    
    df_confirmed_province <- df_confirmed
    
    df_confirmed_country <- df_confirmed %>%
      group_by(country_region) %>%
      summarise(lat = mean(lat),
                long = mean(long))
    
    
    
    
    world <- world %>%
      left_join(df_confirmed_country)
    
    world$date <- as.Date(world$observationdate, format = '%m/%d/%y')
    
    world <- world %>%
      na.omit()
     
  
    world_province <-  world
    
    world_country <-  world %>%
      group_by(country_region, observationdate) %>%
      summarise(confirmed = sum(confirmed),
                deaths = sum(deaths),
                recovered = sum(recovered))
    
    world_country$date <- as.Date( world_country$observationdate, format = '%m/%d/%y')
    
    world_country <-  world_country %>%
      left_join(df_confirmed_country)
    
    df_stat_by_country <-  world_country %>%
      filter(country_region != 'Others') %>%
      group_by(country_region) %>%
      summarise(cum_confirmed = max(confirmed),
                cum_deaths = max(deaths),
                cum_recovered = max(recovered)) %>%
      mutate(death_rate = round(cum_deaths/(cum_confirmed + cum_recovered + cum_deaths), 3)) %>%
      left_join( world_country %>%
                   select(country_region, lat, long)) %>%
      unique() %>%
      ungroup()
    
    df_stat_by_country$id <- seq(nrow(df_stat_by_country))   
    
    df_stat_by_country$mean_incre_confirmed <-  world_country %>%
      filter(country_region != 'Others') %>%
      group_by(country_region) %>%
      summarise(mean_incre_confirmed = round((sort(confirmed, decreasing = T)[1] - sort(confirmed, decreasing = T)[4])/3, 1)) %>%
      as.data.frame() %>%
      select(mean_incre_confirmed) %>%
      unlist()
    for (i in 1:dim(df_stat_by_country)[1]){
      if(df_stat_by_country[i,1]=='France'){
        df_stat_by_country[i,6]=46.2276
        df_stat_by_country[i,7]=2.2137
        
      }
      if(df_stat_by_country[i,1]=='China'){
        df_stat_by_country[i,6]=35.00
        df_stat_by_country[i,7]=105.00
        
      }
    }
    
    df_stat_by_country
    })
  
   confirmed<-reactive({df_stat_by_country<- df_stat_by_country()
   
   somme<-sum( df_stat_by_country$cum_confirmed)
   somme}) 
  
 
   output$chart <- flexdashboard::renderGauge({
     confirmed<-confirmed()
     df_stat_by_country<- df_stat_by_country()
     
     recoverd<-sum(df_stat_by_country$cum_recovered)
     
     flexdashboard::gauge(recoverd, min = 0, max = confirmed)
   })
   output$chart2 <- flexdashboard::renderGauge({
     confirmed<-confirmed()
     df_stat_by_country<- df_stat_by_country()
     
     deaths<-sum( df_stat_by_country$cum_deaths)
     
     flexdashboard::gauge(deaths, min = 0, max = confirmed)
   })
   
   output$chart3 <- flexdashboard::renderGauge({
     confirmed<-confirmed()

     
     flexdashboard::gauge(confirmed, min = 0, max = confirmed)
   })
   output$mymap <- renderLeaflet({
     df_stat_by_country<-df_stat_by_country()
     palette <- colorBin('YlOrRd', domain = df_stat_by_country$cum_confirmed, bins = c(0, 50, 100, 500, 1000, 2500, Inf))
     leaflet(data = df_stat_by_country) %>%
       setView(lat = 35, lng = 102.5, zoom = 2.5) %>%
       addProviderTiles(providers$Stamen.TonerLite) %>%
       addCircleMarkers(lng = ~long, lat = ~lat, layerId = ~id,
                        popup = ~paste0('<b>', 'Confirmed cases: ',country_region, ':','</b>',cum_confirmed,'<br>','<b>', 'Number of Deaths : ' , '</b>', cum_deaths, '<br>','<b>', 'Recovered : ' , '</b>', cum_recovered, '<br>'),
                        radius = ~sqrt(cum_confirmed)/10,
                        color = ~palette(cum_confirmed)) %>%
       
       addLegend(position = 'bottomright',
                 pal <- palette,
                 values = ~cum_confirmed,
                 title = 'Num. of Confirmed',
                 opacity = 0.75)})
   
   output$leaflet <- renderLeaflet({
     df_stat_by_country<-df_stat_by_country()
     palette <- colorBin('YlOrRd', domain = df_stat_by_country$cum_confirmed, bins = c(0, 50, 100, 500, 1000, 2500, Inf))
   leaflet(data = df_stat_by_country) %>%
     setView(lat = 32, lng = 53, zoom = 2.5) %>%
     addProviderTiles(providers$Stamen.TonerLite) %>%
     addCircleMarkers(lng = ~long, lat = ~lat, layerId = ~id,
                      popup = ~paste0('<b>', 'Number of Confirmed cases ',country_region, ':','</b>',cum_confirmed,'<br>','<b>', 'Number of deaths ' , '</b>', cum_deaths, '<br>','<b>', 'Number of Recovered : ' , '</b>', cum_recovered, '<br>'),
                      radius = ~sqrt(cum_confirmed)/10,
                      color = ~palette(cum_confirmed)) %>%
     
     addLegend(position = 'bottomright',
               pal <- palette,
               values = ~cum_confirmed,
               title = 'Num. of Confirmed',
               opacity = 0.75)})
   
  Tunisia<-reactive({read.csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd-dbraIVeJa758lIsTUuWeH_BAo5b8RZH2UAJezxRlhKt0tN9FK-tBSe448_3zwR7Q4wAjy0CYSQ0/pub?output=csv",header=TRUE,sep=";") })
  output$mymap2 <- renderLeaflet({
    Tunisia<-Tunisia()
    palette <- colorBin('YlOrRd', domain = Tunisia$confirme, bins = c(0, 10,20 ,30, 40, 50, Inf))
    leaflet(data = Tunisia) %>%
      setView(lat = 33, lng = 10, zoom = 5) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircleMarkers(lng = ~long, lat = ~lat, layerId = ~id,
                       popup = ~paste0('<b>', 'Number of Confirmed cases: ',gouv, ':','</b>',confirme,'<br>','<b>', 'Number of deaths ' , '</b>', deces, '<br>','<b>', 'Number of recovered: ' , '</b>', retablis, '<br>'),
                       radius = ~sqrt(confirme)*2,
                       color = ~palette(confirme)) %>%
      
      addLegend(position = 'bottomright',
                pal <- palette,
                values = ~confirme,
                title = 'Num. of Confirmed',
                opacity = 0.75)})
  output$filedf2 <- DT::renderDataTable({   Tunisia()})
  output$Total <- renderInfoBox({
    world<-world()
    p<-subset(world,Country.Region=="Tunisia")
    p
    confirmed<-p[nrow(p),6]
    infoBox(
      "Confirmed cases", paste0(confirmed),
      color = "olive",icon= icon("medical" ,class = "fa-briefcase-medical")
    )
  })
  output$Recovred <- renderInfoBox({
    world<-world()
    p<-subset(world,Country.Region=="Tunisia")
    p
    recovered<-p[nrow(p),8]
    infoBox(
      "Recovred Cases", paste0(recovered),
      color = "orange", icon= icon("bible" ,class = "fa-bible"),
      
    )
  })
  output$Death <- renderInfoBox({
    world<-world()
    p<-subset(world,Country.Region=="Tunisia")
    p
    deaths<-p[nrow(p),7]
    infoBox(
      "Dead cases", paste0(deaths),icon= icon("ambulance" ,class = "fa-ambulance"),
      color = "green",
    )
  })
  output$pie <- renderPlotly({
    data<-data()
    data2<-as.data.frame(table(data$return_from))
    p <- plot_ly( data, labels = ~data2[,1] , values = ~data2[,2], type = 'pie') %>%
      layout(title = 'Percentage of imported cases by country ',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p})
  output$bar<- renderPlotly({
    data<-data()
    world<-world()
    p<-subset(world,Country.Region=="Tunisia")
    p
    z<-plot_ly(p,x=~ObservationDate,y=~Confirmed, type='bar')%>%
      layout(title ='Confirmed cases',
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    z
    
    
  })
  output$pie2 <- renderPlotly({
    data<-data()
    data2<-as.data.frame(table(data$city))
    p2 <- plot_ly( data, labels = ~data2[,1] , values = ~data2[,2], type = 'pie') %>%
      layout(title = 'Percentage of cases in Tunisia by region',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p2
    
    
  })
  output$bar2<- renderPlot({
    data<-data()
    p<- ggplot(data=data, aes(x=city, y=case, fill=gender)) +
      geom_bar(stat="identity", position=position_dodge())
    p + coord_flip()
    
  })
  
  clicked_leaflet <- reactiveValues(clickedMarker=NULL)
  observeEvent(input$leaflet_marker_click,{
    clicked_leaflet$clickedMarker <- input$leaflet_marker_click
  })
  
  
  selected_coordinates= reactive(({
    c(clicked_leaflet$clickedMarker$lng,clicked_leaflet$clickedMarker$lat)
  }))
  
  world2<-reactive({ 
    world<-world()
    df_confirmed<-df_confirmed()
    names(world) <- tolower(names(world))
  
  names(world)[3:4] <- c('province_state', 'country_region')
  
  names(df_confirmed) <- tolower(names(df_confirmed))
  
  names(df_confirmed)[1:2] <- c('province_state', 'country_region')
  
  df_confirmed_province <- df_confirmed
  
  df_confirmed_country <- df_confirmed %>%
    group_by(country_region) %>%
    summarise(lat = mean(lat),
              long = mean(long))
  
  
  
  
  world <- world %>%
    left_join(df_confirmed_country)
  
  world$date <- as.Date(world$observationdate, format = '%m/%d/%y')
  
  world <- world %>%
    na.omit()
  for (i in 1:dim(world)[1]){
    if(world[i,4]=='France'){
      world[i,9]=46.2276
      world[i,10]=2.2137
      
    }
    if(world[i,4]=='China'){
      world[i,9]=35.00
      world[i,10]=105.00
      
    }
    if(world[i,4]=='Netherlands'){
      world[i,9]=52.370216
      world[i,10]=4.895168
      
    }
    if(world[i,4]=='Denmark'){
      world[i,9]=55.676098
      world[i,10]=12.568337
      
    }
  }
  world
  })
  
  selected_data= reactive(({
    if(is.null(clicked_leaflet$clickedMarker))
      return(NULL)
    world<-world2()
  data=  filter(world, long == as.numeric(as.character(selected_coordinates()[1])),lat==as.numeric(as.character(selected_coordinates()[2])))
  data[,c(2,4,6,7,8)]   
  }))

  output$filedf3 <- DT::renderDataTable({ selected_data()})
  
  output$tout <- renderPlotly({
    
    data<-selected_data()
    if(is.null(data))
      return()
    date<-data[,1]
    cases<-data[,3]
    fig <- plot_ly(data, x = ~date, y = ~cases, name = 'Confirmed cases', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~data[,4], name = 'Dead cases', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~data[,5], name = 'Recovered cases', mode = 'markers')
    
    fig
 
    
    })
  output$value <- shinydashboard::renderValueBox({
    data<-selected_data()
    df_stat_by_country<-df_stat_by_country()
    b=as.matrix(subset(df_stat_by_country,country_region==data[1,2]))
      
    shinydashboard::valueBox(paste("Number of cases for ",data[1,2]), b[2],color="green")
  })
  output$value4 <- shinydashboard::renderValueBox({
    data<-selected_data()
    df_stat_by_country<-df_stat_by_country()
    b=as.matrix(subset(df_stat_by_country,country_region==data[1,2]))
    shinydashboard::valueBox("Deaths ", b[3],color="red")
  })
  output$value5 <- shinydashboard::renderValueBox({
    data<-selected_data()
    df_stat_by_country<-df_stat_by_country()
    b=as.matrix(subset(df_stat_by_country,country_region==data[1,2]))
    shinydashboard::valueBox("Recovered cases ",b[4],color="yellow")
  })
  df4<-reactive({
    df_stat_by_country<-df_stat_by_country()
    world<-world2()
    nom<-df_stat_by_country$country_region
    date<-world$date[1:length(nom)]
    for (i in 1:length(nom)){
      for (j in 1:length(world$country_region)){
        if( nom[i]== world$country_region[j]  ){
          date[i]<- world$date[j] 
          break 
        }
        
      }}
    df4<-data.frame(nom,date)
    date_num<-vector("numeric",nrow(df4))
    
    for (i in 1:nrow(df4)){
      date_num[i]<- time_length(interval(start = ymd(df4[i,2]), end = today()), unit = "days") 
    }
    
    df4<-data.frame(df4,date_num)
    df4<-data.frame(df4,df_stat_by_country$cum_deaths)
    ratio<-vector("numeric",nrow(df4))
    ratio<-df_stat_by_country$cum_deaths/date_num
    df4<-data.frame(df4,ratio)
   
    df4
  })
  output$plotlyy<-renderPlotly({
    df4<-df4()
    df_1<-subset(df4,ratio>19)
    df_2<-subset(df4,ratio<19 & ratio>1)
    df_3<-subset(df4,ratio<1 & ratio>0.1)
    df_4<-subset(df4,ratio<0.1 & ratio != 0.0)
  
    fig <- plot_ly(x = df_4$ratio, y =as.matrix(df_4$nom)[,1], type = 'bar', orientation = 'h',marker = list(color = 'tomato'))
    
    
  })
  output$plotlyy1<-renderPlotly({
    df4<-df4()
    df_1<-subset(df4,ratio>19)
    df_2<-subset(df4,ratio<19 & ratio>1)
    df_3<-subset(df4,ratio<1 & ratio>0.1)
    df_4<-subset(df4,ratio<0.1 & ratio != 0.0)
    
    fig <- plot_ly(x = df_3$ratio, y =as.matrix(df_3$nom)[,1], type = 'bar', orientation = 'h',marker = list(color = 'orangered'))
    
    fig
  })
  output$plotlyy2<-renderPlotly({
    df4<-df4()
    df_1<-subset(df4,ratio>19)
    df_2<-subset(df4,ratio<19 & ratio>1)
    df_3<-subset(df4,ratio<1 & ratio>0.1)
    df_4<-subset(df4,ratio<0.1 & ratio != 0.0)
    fig <- plot_ly(x = df_2$ratio, y =as.matrix(df_2$nom)[,1], type = 'bar', orientation = 'h',marker = list(color = 'firebrick'))
    
    fig
    })
  output$plotlyy3<-renderPlotly({
    df4<-df4()
    df_1<-subset(df4,ratio>19)
    df_2<-subset(df4,ratio<19 & ratio>1)
    df_3<-subset(df4,ratio<1 & ratio>0.1)
    df_4<-subset(df4,ratio<0.1 & ratio != 0.0)
    fig <- plot_ly(x = df_1$ratio, y =as.matrix(df_1$nom)[,1], type = 'bar', orientation = 'h',marker = list(color = 'darkred'))
    
    fig
    })
  output$filedf5 <- DT::renderDataTable({   df4()})
  output$pie3 <- renderPlotly({
    data<-data()
    fig <- data %>% plot_ly(labels = ~city, values = ~case)
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Distribution of cases according to governorates ",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
    
    
  })
  output$pie4 <- renderPlotly({
    data<-data()
    data2<-as.data.frame((table(data$gender)))
    fig <- data2 %>% plot_ly(labels = ~data2[,1], values = ~data2[,2])
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Distribution of cases by gender",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  output$line <- renderPlotly({
    world<-world()
    
    p<-subset(world,Country.Region=="Tunisia")
    p
    
    fig <- plot_ly(p, x = ~ObservationDate, y = ~Confirmed, name = 'confirmed-cases', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~Recovered, name = 'Rercovered', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~Deaths, name = 'Deaths', mode = 'markers')
    
    fig
  })
  output$pred <- renderPlot({
    world<-world()
    covid<-world
    covid$ObservationDate <- as.Date(covid$ObservationDate, "%m/%d/%Y")
    covid$Confirmed <- as.integer(as.character(covid$Confirmed))
    covid$Deaths <- as.integer(as.character(covid$Deaths))
    covid$Recovered <- as.integer(as.character(covid$Recovered))
    covid_sp <- covid %>% filter(Country.Region=="Tunisia") 
    model_start_date <- covid_sp %>% filter(row_number()==1) %>% .$ObservationDate
    covid_sp$Day <- c(-(min(which(covid_sp$ObservationDate>model_start_date))-1):0, 1:(nrow(covid_sp)-min(which(covid_sp$ObservationDate>model_start_date))))
    
    forecast_days <-7
    covid_sp <- merge(data.frame(Day=min(covid_sp$Day):max(covid_sp$Day)+forecast_days), covid_sp, by="Day", all=TRUE)
    covid_sp$ObservationDate[which(is.na(covid_sp$ObservationDate))] <- covid_sp$ObservationDate[min(which(is.na(covid_sp$ObservationDate)))-1] + 1:forecast_days
    library(drc)
    
    model <- drm(formula=Confirmed~Day, data=covid_sp %>% filter(Day>=-1), fct=L.3())
    
    suppressWarnings({
      prediction <- predict(model, covid_sp %>% filter(Day>=0), interval="confidence", level=0.95) %>% as.data.frame()
    })
    
    prediction <- prediction %>% mutate(Day=row_number()-1)
    prediction$Date <- covid_sp$ObservationDate[match(prediction$Day, covid_sp$Day)]
    suppressWarnings({
      prediction50 <- predict(model, covid_sp %>% filter(Day>=0), interval="confidence", level=0.50) %>% as.data.frame()
    })
    
    prediction50 <- prediction50 %>% mutate(Day=row_number()-1)
    prediction50$Date <- covid_sp$ObservationDate[match(prediction50$Day, covid_sp$Day)]
    library(repr)
    options(repr.plot.width=9, repr.plot.height=5)
    
    
    fills <- c("CI95"="red", "IQR"="purple")
    colors <- c("Prediction"="orange", "Data"="blue")
    prediction$Prediction<-round(prediction$Prediction+25)
    prediction$Lower=round(prediction$Lower+30)
    prediction$Upper= round(prediction$Upper+30)
    prediction50$Prediction<- round(prediction50$Prediction+25)
    prediction50$Lower= round(prediction50$Lower+30)
    prediction50$Upper=round( prediction50$Upper+30)
    ggplot() + 
      geom_ribbon(data=tail(prediction, forecast_days+1), aes(x=Date, ymin=Lower, ymax=Upper, fill="CI95"), alpha=0.2) + 
      geom_ribbon(data=tail(prediction50, forecast_days+1), aes(x=Date, ymin=Lower, ymax=Upper, fill="IQR"), alpha=0.2) + 
      geom_point(data=tail(prediction, forecast_days) %>% head(3), aes(x=Date, y=Prediction+15, color="Prediction"), size=1) + 
      geom_text(data=tail(prediction, forecast_days) %>% head(3), aes(x=Date, y=Prediction+15, label=round(Prediction+15)), size=2.5) + 
      geom_line(data=covid_sp %>% filter(ObservationDate>="2020-03-04"), aes(x=ObservationDate, y=Confirmed, color="Data"), na.rm=TRUE) + 
      
      
      scale_fill_manual(values=fills, labels=c("95% CI", "IQR"), guide=guide_legend(title=NULL, order=2)) + 
      scale_color_manual(values=colors, guide=guide_legend(override.aes=list(shape=c(NA, 16), linetype=c("solid", "blank")), title=NULL, order=1)) + 
      scale_x_date(date_minor_breaks="1 days") + 
      scale_y_continuous(labels=scales::comma) + 
      theme_bw() + 
      theme(plot.caption=element_text(size=7), legend.justification="top") + 
      labs(x="Date", y="Total cases", title="Coronavirus in Tunisia", subtitle="Three-parameter logistic model.")
    
    
    
    
  })
  output$bed <- renderPlotly({
    Hospital <- Hospital()
    fig <- plot_ly(Hospital, x = ~city, y = ~beds, type = 'bar', color = I("pink"))
    fig <- fig %>% layout(title = "Distribution of hospital bed by region",
                          xaxis = list(title = "City"),
                          yaxis = list(title = "Beds"))
    
    fig
    
    
  })
})