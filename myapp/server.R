######################################################
#
# Spatiotemporal Data Visualization (NTU Geographhy)
#
# 3. Interactive Data Visualization
#
######################################################

#rsconnect::setAccountInfo(name='r09228001',
#                          token='CBC8DD1670E4AE4D90D49B4023E73BD3',
#                          secret='<SECRET>')




rm(list=ls(all=TRUE))
#setwd("~/Desktop/110-1/110-1 data visualization/w8_midterm/myapp")
library(xts)
library(shiny)
library(readxl)
library(sf)
library(magrittr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(leafpop)
library(tmap)
library(htmltools)
library(cartography)
library(mapview)
library(lubridate)
library(TSstudio)
options(scipen=999)

#remotes::install_github("mtennekes/tmaptools")
#remotes::install_github("mtennekes/tmap")
#setwd("~/Desktop/110-1/110-1 data visualization/w8_midterm/myapp")
#Loading xls files
n_data <- read_excel("nationals.xlsx")
f_data <- read_excel("foreigners.xlsx")
b_data <- read_excel("bothnations.xlsx")
spots <- st_read(dsn = "point2.shp",options = "encoding=utf-8")
#both=n_data[,c(3:12)]+f_data[,c(3:12)]
#b_data=cbind(n_data[,1:2],both)
#library("writexl")
#write_xlsx(b_data,"~/Desktop/110-1/110-1 data visualization/w8_midterm/myapp/bothnations.xlsx")




# Plotting
shinyServer(function(input, output) {
  
  

  
  ### Time Series
    

    # plot seasonal
    library(TSstudio)
    library(plotly)
    

#   if (input$tourist=="nationals") data=national
 #    else data=foreinger

  
      
     
      
 #install.packages("shinyalert")    
  library(shinyalert)
    output$plot1 <- renderPlotly({
      
      ####################### Data #############################
      if (input$tourist=="nationals") data=n_data
      else if (input$tourist=="foreigners") data=f_data
      else if (input$tourist=="all") data=b_data
      
      location=c("All","Palace Museum","Salt mountain","Water AmusementPark"
                 ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                 "Little man world","Aboriginal tribes",  
                 "Lugon Dragon Temple","Clean Farm")
      
      colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                       ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                       "Little man world","Aboriginal tribes",  
                       "Lugon Dragon Temple","Clean Farm")
      
      
      
      
      #######################條件式#############################
      
      if (input$trend=="longterm"){
        
        if( input$location=="All") {
          my_tsf <- xts(data[,-c(1:2)] , order.by = as.Date(data$Date))
          ts_plot(my_tsf,slider = TRUE,Xgrid = T,title=paste("Time-Series in All for",input$tourist),
                  Xtitle="time",Ytitle="ppl")
        }
        
        else if (input$location !="All"){
          my_tsf <- xts(data[,-c(1:2)] , order.by = as.Date(data$Date))
          my_tsf = my_tsf[,which( colnames(my_tsf)==input$location)]
          ts_plot(my_tsf,slider = TRUE,Xgrid = T,title=paste("Time-Series in",input$location,"for",input$tourist),
                  Xtitle="time",Ytitle="ppl")
          
        }
        
      }
      
      
      ################## seasonal plot #########################
      else if (input$trend=="seasonal"){
        
        if( input$location=="All") {
          ###########################################################
          library(dplyr)
          library(tidyr)
          long=gather(data,spot,people,`Palace Museum`:`Clean Farm`,factor_key = TRUE)
          nd=long[,-c(1,3)] %>% group_by(Date) %>% summarise_all(sum)
          year=substr(nd$Date,1,4)
          month=substr(nd$Date,6,7)
          t=data.frame("year"=year,"month"=month,"people"=round(nd$people/1000,1))
          xtable <- xtabs(formula = people~year+month,data = t)
          d=t(as.matrix(xtable))
          
          #轉成ts的格式 #注意矩陣要轉置，這樣才會 byrow
          my_ts <- ts(as.vector(d), start=2018, frequency = 12)
          ts_seasonal(my_ts,Xgrid = T, type = "normal") %>%
            layout(title =paste("Seansonal time series in,",input$location,", for",input$tourist), xaxis = list(title = "Month"), yaxis = list(title = "Thousand People"),legend = list(title=list(text='<b> Year </b>')))
        }
          
        
        else if (input$location !="All"){
          d1=data[,which(colnames(data)==input$location)]  
          d2=cbind(data[,c(1:2)],d1)
          data=d2
          long=gather(data,spot,people,input$location,factor_key = TRUE)
          nd=long[,-c(1,3)] %>% group_by(Date) %>% summarise_all(sum)
          year=substr(nd$Date,1,4)
          month=substr(nd$Date,6,7)
          t=data.frame("year"=year,"month"=month,"people"=round(nd$people/1000,1))
          xtable <- xtabs(formula = people~year+month,data = t)
          d=t(as.matrix(xtable))  
          
          #轉成ts的格式 #注意矩陣要轉置，這樣才會 byrow
          my_ts <- ts(as.vector(d), start=2018, frequency = 12)
          
           ts_seasonal(my_ts,Xgrid = T, type = "normal") %>%
            layout(title =paste("Seansonal time series in,",input$location,", for",input$tourist), xaxis = list(title = "Month"), yaxis = list(title = "Thousand People"),legend = list(title=list(text='<b> Year </b>')))
        
        }
      }
          
          
     })
    
    output$plot2 <- renderPlotly({
      
      
      
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      ################  Heat Map    ###########################
      
      #######################條件式#############################
      if (input$tourist=="nationals") data=n_data
      else if (input$tourist=="foreigners") data=f_data
      else if (input$tourist=="all") data=b_data
      
      #######################條件式#############################
      
      colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                       ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                       "Little man world","Aboriginal tribes",  
                       "Lugon Dragon Temple","Clean Farm")
      data

      library(dplyr)
      library(tidyr)
      
      if( input$location=="All") {
      
      long=gather(data,spot,people,`Palace Museum`:`Clean Farm`,factor_key = TRUE)
      nd=long[,-c(1,3)] %>% group_by(Date) %>% summarise_all(sum)
      year=substr(nd$Date,1,4)
      month=substr(nd$Date,6,7)
      t=data.frame("year"=year,"month"=month,"people"=round(nd$people/1000,1))
      xtable <- xtabs(formula = people~year+month,data = t)
      d=t(as.matrix(xtable))
      
      #轉成ts的格式 #注意矩陣要轉置，這樣才會 byrow
      my_ts <- ts(as.vector(d), start=2018, frequency = 12)
    
      ts_heatmap(my_ts)%>%
      layout(title = paste("Time Series Heatmap in",input$location,"for",input$tourist,"1000ppl"), 
             xaxis = list(title = "Year"), yaxis = list(title = "Month"),legend = list(title='Thousand People'))
      
      }
      
      else if (input$location !="All"){
        d1=data[,which(colnames(data)==input$location)]  
        d2=cbind(data[,c(1:2)],d1)
        data=d2
        long=gather(data,spot,people,input$location,factor_key = TRUE)
        nd=long[,-c(1,3)] %>% group_by(Date) %>% summarise_all(sum)
        year=substr(nd$Date,1,4)
        month=substr(nd$Date,6,7)
        t=data.frame("year"=year,"month"=month,"people"=round(nd$people/1000,1))
        xtable <- xtabs(formula = people~year+month,data = t)
        d=t(as.matrix(xtable))  
        
        #轉成ts的格式 #注意矩陣要轉置，這樣才會 byrow
        my_ts <- ts(as.vector(d), start=2018, frequency = 12)
        
        ts_heatmap(my_ts)%>%
          layout(title = paste("Time Series Heatmap in",input$location,"for",input$tourist,"1000ppl"), 
                 xaxis = list(title = "Year"), yaxis = list(title = "Month"),legend = list(title='Thousand People'))
        
      }
      
      
    
  })
    
  ### Map

    output$map <- renderLeaflet({

      
 
      
      #tmap_mode("view")
      #tm_shape(spots) +
      # tm_dots(size="Capacity", col="typesEN", alpha = 0.6, id = "NameEN")#,popup.vars=c(
      #   "NAME: "="NameEN",""
      
      
      # year-month
  #    ggplot(data = data, aes(x = Date, y = `Palace Museum`))+ 
   #     geom_line()+
    #    theme(text=element_text(family="黑體-繁 中黑"))+
     #   ggtitle("Palace Museum") +
      #  xlab('Time')+
       # ylab("ppl")+
        #stat_smooth(method = "loess", formula = y ~ x, size = 1)
      
      # year-quater
     
      if (input$trend=="longterm"){
        #######################條件式#############################
        if (input$tourist=="nationals") {
          data=n_data
          colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                           ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                           "Little man world","Aboriginal tribes",  
                           "Lugon Dragon Temple","Clean Farm")
          p = list()
          for(i in 1:nrow(spots)){
            df <- data.frame(data[,2],data[,i+2])
            colnames(df)=c("time","location")
            p[[i]] = ggplot(data = df, aes(x = time, y = location ),
                            family = "黑體-繁 中黑")+ 
              geom_line(col="#009E73")+
              theme(text=element_text(family="黑體-繁 中黑"))+
              labs(title = paste("Month-Year trend in",colnames(data)[i+2]) ,x = "time",y = paste("ppl for",input$tourist))
       
            
          }
          }
      
          
          else if (input$tourist=="foreigners") 
            {
            data=f_data
            colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                             ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                             "Little man world","Aboriginal tribes",  
                             "Lugon Dragon Temple","Clean Farm")
            p = list()
            for(i in 1:nrow(spots)){
              df <- data.frame(data[,2],data[,i+2])
              colnames(df)=c("time","location")
              p[[i]] = ggplot(data = df, aes(x = time, y = location ),
                              family = "黑體-繁 中黑")+ 
                geom_line(color="#D55E00")+
                theme(text=element_text(family="黑體-繁 中黑"))+
                labs(title = paste("Month-Year trend in",colnames(data)[i+2]) ,x = "time",y = paste("ppl for",input$tourist))
          
              
            }
          }
      
        
        
        else if (input$tourist=="all"){
          data=b_data
          p = list()
          for(i in 1:nrow(spots)){
            df <- data.frame(n_data[,2],n_data[,i+2],f_data[,i+2])
            colnames(df)=c("time","nationals","foreigners")
            library(reshape2)
            long<- melt(df, id = "time") 
            p[[i]] = ggplot(data = long, aes(x = time, y = value,col=variable))+ 
              geom_line()+ #+
              theme(text=element_text(family="黑體-繁 中黑"))+
              labs(title = paste("Month-Year trend in",colnames(data)[i+2]) ,x = "time",y = paste("ppl for",input$tourist)) 
          
          
        }
        
        }
      }
      #################################
      else if (input$trend=="seasonal"){
        #######################條件式#############################
        if (input$tourist=="nationals") {
          data=n_data
        colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                         ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                         "Little man world","Aboriginal tribes",  
                         "Lugon Dragon Temple","Clean Farm")
        #######################條件式#############################
        monthly <- ts(data, start = c(2018, 1), frequency = 12)
        #ts_plot(  monthly[,"Palace Museum"],Xgrid = T,title=paste("Monthly time-Series in","input$location"),
        #          Xtitle="time",Ytitle="ppl")
        quarterly <- aggregate(monthly, nfrequency = 4)
        #  ts_plot( quarterly[,"Palace Museum"],Xgrid = T,title=paste("Quarterly time-Series in","input$location"),
        #         Xtitle="time",Ytitle="ppl")
        date1=as.Date(time( quarterly))
        r1=data.frame(quarterly)
        r2=cbind(date1,r1[,-c(1:2)])
        colnames(r2)=c("Date","Palace Museum","Salt mountain","Water AmusementPark"
                       ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                       "Little man world","Aboriginal tribes",  
                       "Lugon Dragon Temple","Clean Farm")
        data=r2
        
        p = list()
        for(i in 1:nrow(spots)){
          df <- data.frame(data[,1],data[,i+1])
          colnames(df)=c("time","location")
          p[[i]] = ggplot(data = df, aes(x = time, y = location ),
                          family = "黑體-繁 中黑")+ 
            geom_line(color="#009E73")+
            theme(text=element_text(family="黑體-繁 中黑"))+
            labs(title = paste("Quater-Year trend in",colnames(data)[i+1],"for",input$tourist) ,x = "time",y =  paste("ppl for",input$tourist))
        }
      }
        
        else if (input$tourist=="foreigners") {
          data=f_data
          colnames(data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                           ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                           "Little man world","Aboriginal tribes",  
                           "Lugon Dragon Temple","Clean Farm")
          #######################條件式#############################
          monthly <- ts(data, start = c(2018, 1), frequency = 12)
          #ts_plot(  monthly[,"Palace Museum"],Xgrid = T,title=paste("Monthly time-Series in","input$location"),
          #          Xtitle="time",Ytitle="ppl")
          quarterly <- aggregate(monthly, nfrequency = 4)
          #  ts_plot( quarterly[,"Palace Museum"],Xgrid = T,title=paste("Quarterly time-Series in","input$location"),
          #         Xtitle="time",Ytitle="ppl")
          date1=as.Date(time( quarterly))
          r1=data.frame(quarterly)
          r2=cbind(date1,r1[,-c(1:2)])
          colnames(r2)=c("Date","Palace Museum","Salt mountain","Water AmusementPark"
                         ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                         "Little man world","Aboriginal tribes",  
                         "Lugon Dragon Temple","Clean Farm")
          data=r2
          
          p = list()
          for(i in 1:nrow(spots)){
            df <- data.frame(data[,1],data[,i+1])
            colnames(df)=c("time","location")
            p[[i]] = ggplot(data = df, aes(x = time, y = location ),
                            family = "黑體-繁 中黑")+ 
              geom_line(color="#D55E00")+
              theme(text=element_text(family="黑體-繁 中黑"))+
              labs(title = paste("Quater-Year trend in",colnames(data)[i+1],"for",input$tourist) ,x = "time",y =  paste("ppl for",input$tourist))
          }
        }    
          else if (input$tourist=="all"){
            n_data <- read_excel("nationals.xlsx")
            
            colnames(n_data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                               ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                               "Little man world","Aboriginal tribes",  
                               "Lugon Dragon Temple","Clean Farm")
            #######################條件式#############################
            monthly <- ts(n_data, start = c(2018, 1), frequency = 12)
            #ts_plot(  monthly[,"Palace Museum"],Xgrid = T,title=paste("Monthly time-Series in","input$location"),
            #          Xtitle="time",Ytitle="ppl")
            quarterly <- aggregate(monthly, nfrequency = 4)
            #  ts_plot( quarterly[,"Palace Museum"],Xgrid = T,title=paste("Quarterly time-Series in","input$location"),
            #         Xtitle="time",Ytitle="ppl")
            date1=as.Date(time( quarterly))
            r1=data.frame(quarterly)
            r2=cbind(date1,r1[,-c(1:2)])
            colnames(r2)=c("Date","Palace Museum","Salt mountain","Water AmusementPark"
                           ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                           "Little man world","Aboriginal tribes",  
                           "Lugon Dragon Temple","Clean Farm")
            r2
            f_data <- read_excel("foreigners.xlsx")
            colnames(f_data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                               ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                               "Little man world","Aboriginal tribes",  
                               "Lugon Dragon Temple","Clean Farm")
            #######################條件式#############################
            monthly <- ts(f_data, start = c(2018, 1), frequency = 12)
            #ts_plot(  monthly[,"Palace Museum"],Xgrid = T,title=paste("Monthly time-Series in","input$location"),
            #          Xtitle="time",Ytitle="ppl")
            quarterly <- aggregate(monthly, nfrequency = 4)
            #  ts_plot( quarterly[,"Palace Museum"],Xgrid = T,title=paste("Quarterly time-Series in","input$location"),
            #         Xtitle="time",Ytitle="ppl")
            date1=as.Date(time( quarterly))
            r3=data.frame(quarterly)
            r4=cbind(date1,r3[,-c(1:2)])
            colnames(r4)=c("Date","Palace Museum","Salt mountain","Water AmusementPark"
                           ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                           "Little man world","Aboriginal tribes",  
                           "Lugon Dragon Temple","Clean Farm")
            data=b_data
            p = list()
            for(i in 1:nrow(spots)){
              df <- data.frame(r2[,1],r2[,i+1],r4[,i+1])
              colnames(df)=c("time","nationals","foreigners")
              library(reshape2)
              long<- melt(df, id = "time") 
              p[[i]] = ggplot(data = long, aes(x = time, y = value,col=variable))+ 
                geom_line()+
                theme(text=element_text(family="黑體-繁 中黑"))+
                labs(title = paste("Quater-Year trend in",colnames(data)[i+2]) ,x = "time",y = paste("ppl for",input$tourist)) 
              
            }
          }
            
     
      
        
      }
      ###########################
    
      m=mapview(spots,zcol="typesEN",cex="Capacity",label="NameEN", popup = popupGraph(p))
      m@map
    })   
    
      colnames(n_data)=c("Column1","Date","Palace Museum","Salt mountain","Water AmusementPark"
                         ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
                         "Little man world","Aboriginal tribes",  
                         "Lugon Dragon Temple","Clean Farm")
    level=rep(15,nrow(spots))
    zoomtable=data.frame(x=spots$x,y=spots$y,brick=level,location=colnames(n_data)[-c(1:2)])

    a=c( "121.6285","23.80202","7","All")
    zoomtable2=rbind(zoomtable,a)
    zoomtable2
    
    Zooming <-reactive({
      subset(zoomtable2, location == input$location)
  
      })
    
    observe({
      leafletProxy("map") %>%
        setView(lng=Zooming()$x,lat=Zooming()$y,zoom = Zooming()$brick)
    })

    
    
    output$image <- renderImage({
      
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images',"cat3.jpg"))
      # Return a list containing the filename
      list(src = filename,
           width = 400,
           height = 300
           )
      
    }, deleteFile = FALSE)

    
})
    

    
    
    
    