library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(ECharts2Shiny)
library("readxl")
library(shinyWidgets)
library(leaflet)
library(rstatix)
library(tidyverse)
library(dplyr)
library(dslabs)
library(datasets)

#df <- read_excel("C:/MSc (BDA)/R Shiny project/GTD/reduced global terrorism dataset.xlsx")
#india_df<-data.frame()
#for (i in 1:nrow(df)){
#  if (df$country[i]==92 & df$provstate[i]!="Unknown"){
#    india_df <- rbind(india_df,df[i,])
#}
#}

Dataset <- data.frame(
  "Type" = c(df$attacktype1),
  "Value" = c(df$iyear),
  "Lat" = c(df$latitude), 
  "Long" = c(df$longitude)
)

ui <- dashboardPage(
  skin='red',
  
  dashboardHeader(
    title = "The Global Terrorism Database", titleWidth = 450), #dashboard header
  
  dashboardSidebar(width = 350,
                   
                   #sidebar menu
                   sidebarMenu(id="tabs", 
                               menuItem(h3("Home"), tabName = "home", selected = T),
                               menuItem(strong("Dashboards"), tabName = "India", icon = icon("dashboard")),
                               #selection of SUccessful/failed/all attacks
                               radioButtons("success", label = h4(strong("Success / Failure")),
                                            choices = list("Failed attacks" = 0, "Successful Attacks" = 1, "All Attacks" = 2), 
                                            selected = 2),
                               
                               #selection of time interval
                               h4(" Select the time period",style = "text-align: center"),
                               sliderInput(inputId = "years",
                                           label = "Years:",
                                           min = 1970,
                                           max = 2017,
                                           value =  c(1970,1990)),
                               menuItem("World", tabName="World", icon=icon("globe"), 
                                        radioButtons("regions", label = h3("Regions"),
                                                     choices = list("North America" = 1, "Central America & Caribbean" = 2, 
                                                                    "South America" = 3, "East Asia" = 4, "Southeast Asia" = 5, 
                                                                    "South Asia" = 6, "Central Asia"=7, "Western Europe" = 8, 
                                                                    "Eastern Europe" = 9, "Middle East & North Africa" = 10, 
                                                                    "Sub-Saharan Africa"= 11, "Australasia & Oceania"= 12, 
                                                                    "All Regions" = 13 ), 
                                                     selected = 1)),#menuitem
                               
                               menuItem("Types of attack", icon = icon("th"), tabName = "Type",
                                        # badgeLabel = "new", badgeColor = "green",
                                        radioButtons("Type", "Type", choices = c("Assassination" = 1,
                                                                                 "Armed Assault" = 2,
                                                                                 "Bombing/Explosion" = 3,
                                                                                 "Hijacking" = 4,
                                                                                 "Hostage Taking (Barricade Incident)" = 5,
                                                                                 "Hostage Taking (Kidnapping)" = 6,
                                                                                 "Facility/Infrastructure Attack" = 7,
                                                                                 "Unarmed Assault" = 8,
                                                                                 "Unknown" = 9), selected = 1))#menuitem
                   )#sidebarmenu
  ),#dashboardsidebar
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Old English Text MT";
        font-weight: normal;
        font-size: 30px;
      }
    '))),
    fluidPage(
    tabItems(
      tabItem(
        tabName = "home",
          mainPanel(
            h1(span("The Global Terrorism Database", style = "color:red")),
            h3(p("The Global Terrorism Database (GTD) is the most comprehensive unclassified database of terrorist attacks in the world. 
            The National Consortium for the Study of Terrorism and Responses to Terrorism (START) makes the GTD available via", 
            strong(em("https://www.start.umd.edu/gtd/ ")),
            "in an effort to improve understanding of terrorist violence, so that it can be more readily studied and defeated. 
            The GTD is produced by a dedicated team of researchers and technical staff."),align ="justify"),
            br(),
            h3("The GTD is an open-source database, which provides information on domestic and international terrorist attacks around the 
            world since 1970, and includes more than 180,000 events. For each event, a wide range of information is available, 
            including the date and location of the incident, the weapons used, nature of the target, the number of casualties, 
            and - when identifiable - the group or individual responsible. ", align ="justify"), 
              
            br(),
            h2(em(span("Characteristics of the GTD ", style = 'color:red'))),
            br(),
            h3("     - Contains information on over 180,000 terrorist attacks"),
            h3("     - Currently the most comprehensive unclassified database on terrorist attacks in the world"),
            h3("     - Includes information on more than 95,000 bombings, 20,000 assassinations, and 15,000 kidnappings and hostage 
            events since 1970"),
            h3("     - Includes information on at least 45 variables for each case, with more recent incidents including information on 
            more than 120 variables"),
            h3("     - More than 4,000,000 news articles and 25,000 news sources were reviewed to collect incident data from 1998 to 2017 alone"),
         )#mainPanel
      ),#tabitem
      tabItem(
        tabName = "India",
              mainPanel(
                tabsetPanel(#type = "tabs",
                            tabPanel("INDIA I", 
                                     # Value Box for killed and injured
                                       column(width=12, valueBoxOutput("KilledBox1", width = 6),
                                                     valueBoxOutput("InjuredBox1", width = 6)),
                                     box(width =7, status = "warning", title = "STATE-WISE ATTACKS",
                                         solidHeader = TRUE, plotlyOutput(outputId = "India_state_wise")), #box
                                     box(width=5, status = "warning", title = "TYPES OF ATTACK",
                                         solidHeader = TRUE, plotlyOutput(outputId = "India_attacktype"))#Box
                            ),#tabpanel
                            
                            tabPanel("INDIA II", 
                                     # Value Box for killed and injured
                                       column(width=12, valueBoxOutput("KilledBox2", width = 6),
                                                     valueBoxOutput("InjuredBox2", width = 6)),
                                     box( status = "warning", title = "TOP 10 TERRORIST GROUPS : NUMBER OF ATTACKS",
                                         solidHeader = TRUE, tableOutput("India_terr_gp")),#box
                                     box( status = "warning", title = "WEAPONS USED",
                                         solidHeader = TRUE, plotlyOutput("India_weapon"))#BOX
                            ),#tabpanel
                            
                            tabPanel("WORLD I", 
                                     # Value Box for killed and injured
                                       column(width=12, valueBoxOutput("WorldKilledBox1", width = 6),
                                                     valueBoxOutput("WorldInjuredBox1", width = 6)),
                                       box(width = 6, status = "warning", title = "REGION-WISE COUNT : NUMBER OF ATTACKS",
                                                  solidHeader = TRUE, plotlyOutput(outputId = "region")),#box
                                              box(width = 6, status = "warning", title = "TYPES OF ATTACK",
                                                  solidHeader = TRUE, plotlyOutput(outputId = "attacktype"))#Box
                                              
                            ),#tabpanel
                            
                            tabPanel("WORLD II", 
                                     # Value Box for killed and injured
                                     #fluidRow(
                                       column(width=12, valueBoxOutput("WorldKilledBox2", width = 6),
                                                     valueBoxOutput("WorldInjuredBox2", width = 6)),
                                       box(width = 6, status = "warning", title = "TOP 10 TERRORIST GROUPS : NUMBER OF ATTACKS",
                                                  solidHeader = TRUE, tableOutput("terr_gp_data_by_region")),#box
                                              box(width = 6, status = "warning", title = "WEAPONS USED",
                                                  solidHeader = TRUE, plotlyOutput("weapon"))#BOX
                                              
                            ),#tabpanel
                            
                            tabPanel("MAP", 
                                     absolutePanel(bottom = 150, right = 625),
                                     leafletOutput("map")
                            )#tabpanel
                )#tabsetpanel
              )#mainpanel
      )#tabitem
    )#tabitems
    )#fluidrow
  )#dashboardBody
)#dashboardPage

server <- function(input, output, session) {
  
  ##### WORLD VISUALISATIONS
  
  ###Region wise count of attacks
  output$region <- renderPlotly({
    #par(mar=c(12,3,4,2))
    if (input$success==2) {
      y<-data.frame(table(df$region_txt[df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(df$region_txt[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("magenta"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  
  ###Top 10 terror groups in the world region-wise
  output$terr_gp_data_by_region <- renderTable({
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        z    <- data.frame(table(df$gname[(df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {#All regions
        z    <- data.frame(table(df$gname[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  
  ###Weapons used in the attacks worldwide
  output$weapon <- renderPlotly({
    #par(mar=c(14,3,2,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$weaptype1_txt[(df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$weaptype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("red"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  ### Types of Attacks : worldwide
  output$attacktype <- renderPlotly({
    # par(mar=c(15,3,4,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$attacktype1_txt[(df$region==input$regions & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$attacktype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$region==input$regions &
                                                  df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("black"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  output$WorldKilledBox1 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Killed", icon = icon("tint", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$WorldInjuredBox1 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Injured", icon = icon("tint", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$WorldKilledBox2 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Killed", icon = icon("tint", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$WorldInjuredBox2 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Injured", icon = icon("tint", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  ##### INDIA VISUALISATIONS
  
  ###State wise details for India
  output$India_state_wise <- renderPlotly({
    #par(mar=c(9,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("magenta"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    ggplotly(fig)
  })
  
  
  ### Types of Attacks : India
  output$India_attacktype <- renderPlotly({
    #par(mar=c(15,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("black"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 180), xaxis = list(tickangle = 45))
    fig
  })
  
  ###Top 10 terror groups in the India
  output$India_terr_gp <- renderTable({
    if (input$success==2) {
      z    <- data.frame(table(india_df$gname[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    else {#If input$success are 0 or 1
      z    <- data.frame(table(india_df$gname[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  
  ###Weapons used in the attacks in India
  output$India_weapon <- renderPlotly({
    #par(mar=c(14,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("green"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  
  output$KilledBox1 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Killed", icon = icon("tint", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$InjuredBox1 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Injured", icon = icon("tint", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$KilledBox2 <- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Killed", icon = icon("tint", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$InjuredBox2<- renderInfoBox({
    
    if (input$success==2) {
      kaam_sum<-round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    else {
      kaam_sum<-round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0)
    }
    valueBox(
      kaam_sum, "Injured", icon = icon("tint", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  ######### MAP VISUALISATION
  filteredData <- reactive({
    
    
    Dataset %>% 
      filter(Type %in% input$Type) %>%
      filter(Value >= input$years[1]) %>% 
      filter(Value <= input$years[2])
  })
  output$map <- renderLeaflet({
    leaflet(Dataset) %>% addTiles() %>%
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions())
    
    leaflet(Dataset)%>% addTiles() %>%
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions(freezeAtZoom = 4))
    leaflet(Dataset) %>%  addTiles() %>%
      addLabelOnlyMarkers(data = filteredData(),
                          lng = ~Long, lat = ~Lat,
                          label = ~as.character(filteredData()),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(multiple = T,
                                                      direction = "auto"))
  })
  
  
  
  # columns <- reactiveVal()
  # 
  # rawdata <- reactive({
  #   uploaded <- input$file
  #   if(is.null(uploaded)){
  #     return()
  #   }
  #   read.csv(file = uploaded$datapath,
  #            header = input$dataheader)
  #   
  # })
  
  # output$showData <- renderDataTable(
  #   rawdata(),
  #   options = list(
  #     pageLength = 10,
  #     scrollY="400p"
  #   )
  #   
  # )
  # 
  # output$showSummary <- renderPrint(summary(rawdata())) 
  # 
  # observe(columns(colnames(Filter(is.numeric, rawdata()))))
  # 
  # output$colX <- renderUI({
  #   selectInput(inputId = "XAxis",
  #               label = "Select X Axis:",
  #               choices = columns())
  #   
  # })
  
  # output$colY <- renderUI({
  #   selectInput(inputId = "YAxis",
  #               label = "Select Y Axis:",
  #               choices = columns())
  #   
  # })
  # output$showPlot <- renderPlotly({
  #   g <- ggplot(rawdata(), aes_string(x = input$XAxis, y = input$YAxis))+
  #     geom_point(colour = "blue") +
  #     theme_bw()
  #   ggplotly(g)
  # }) 
}

shinyApp(ui = ui, server = server)
