
######################################################
#
# Spatiotemporal Data Visualization (NTU Geographhy)
#
# 3. Interactive Data Visualization
#
######################################################

location=c("All","Palace Museum","Salt mountain","Water AmusementPark"
           ,"Kaohsiung Art Museum","Seven star lake","Monkey cave cat village",
           "Little man world","Aboriginal tribes",  
           "Lugon Dragon Temple","Clean Farm")


library(shiny)

shinyUI(fluidPage(
  
  titlePanel("2021-NTU Prof. Wen's Data Visualiztion Midterm Exam"),
  helpText("Yang, Yu-Hsiang, NTU GEOG M.S. 2, R09228001. "),
  helpText("Contact Info : r09228001@ntu.edu.tw.        Complete Time : 2021/11/13"),
  sidebarLayout(
  
    sidebarPanel(
      titlePanel("Desired  Characteristics"),
      #shinythemes::themeSelector(),
      #fluidRow(column(3,
             
      
     
      selectInput(inputId = "location", label = h5("Scenic Spots"), 
                  choices = location, 
                  selected = "All"),
      helpText("time-series analysis at orginal zoom level are highly recommended."), 
      
      
      radioButtons(inputId ="trend",
                   label = "Trend Type:",
                   choices = c("Long-term Trend"="longterm", "Seasonal Trend"="seasonal"),
                   selected = "longterm"),
     
      
      #   Select which Gender(s) to plot
      radioButtons(inputId ="tourist",
                         label = "Tourist Nationality:",
                         choices = c("Nationals" = "nationals", "Foreigners" = "foreigners","All"="all"),
                         selected = "nationals"),
      helpText("Note: Please be sure to at least pick one")


    ),
    
    mainPanel(
      #plotOutput("distPlot")
      tabsetPanel(
       
        tabPanel("InteractivePlot",leafletOutput("map")),
        tabPanel("TimeSeriesComparison",
                 fluidRow(
                   column(12, plotlyOutput("plot1")),
                   column(12, plotlyOutput("plot2"))
                 )),
        tabPanel("For TAs",imageOutput("image"))
       
        #h4("Thoughtful reminder : time-series analysis at orginal zoom level are highly recommended.", align = "left")    
      )
    )
  )
))
