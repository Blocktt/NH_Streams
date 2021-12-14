# RShiny code for basic data visualization of NH Streams bug and nuts data
# Code adapted by Ben Block from Diane Allen
# Written 12/13/2021
#
# Call libraries ####
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(scales)
library(leaflet)

# import data ####
df_nuts<-read.csv("Data/NH_All_Nuts_IBI_Data_20211214.csv")

# Create app

# ui ####

ui <- fluidPage(
  tabsetPanel(
    ## Macroinvert tab ####
    tabPanel("Macroinvertebrates and Nutrients"
      , sidebarLayout(
        sidebarPanel(
          helpText("The information presented here represents",
                   "macroinvertebrate and nutrient data paired by site.",
                   "Note: IBI scores are site averages over time and data",
                   "were not paired by sampling date.")
          ,selectInput(inputId = "Nut_Choice1"
                      ,label = "Nutrient Parameter:"
                      ,choices = c("TP_mgL"
                                   ,"PO4_mgL"
                                   ,"NOx_mgL"
                                   ,"TKN_mgL"
                                   ,"NO3_mgL"
                                   ,"NO2_mgL"
                                   ,"TN_mgL")) # selectInput
          
        ) # sidebarPanel
        , mainPanel(
          plotOutput(outputId = "S_R_Plot")
          , h4("Summary statistics for all samples:")
          , verbatimTextOutput("stats1")
          , h4("Summary statistics for Ref samples:")
          , verbatimTextOutput("stats2")
          , h4("Summary statistics for linear model of all samples:")
          , verbatimTextOutput("lm")
        ) # mainPanel
      ) # sidebarLayout
    ) # tabPanel
    ## Nutrient tab ####
    , tabPanel("Nutrient Distributions",   
               fluidRow(
                 column(width=4,
                        # Input: parameter
                        selectInput(inputId = "Nut_Choice2",
                                    label = "Nutrient Parameter",
                                    choices = c("TP_mgL"
                                                ,"PO4_mgL"
                                                ,"NOx_mgL"
                                                ,"TKN_mgL"
                                                ,"NO3_mgL"
                                                ,"NO2_mgL"
                                                ,"TN_mgL")) # selectInput
                        ) # column
                 )
               , fluidRow(
                 column(width=4
                        , helpText("The information presented here represents",
                                   "nutrient data sampling locations. Note: not all data",
                                   "are shown in the map because some sites had",
                                   "missing lat/long info.")
                        ,leafletOutput(outputId = "Map")
                        ,height=800
                        )# column
                 , column(width=4
                          ,plotOutput(outputId = "cdf")
                          ,tableOutput(outputId = "QuantTable")
                          ) # column
                 , column(width=3
                          ,plotOutput(outputId = "boxplot")
                          ) # column
               ) # fluidRow
    ) # tabPanel
  ) # tabsetPanel
) # fluidPage


# server ####

server <- function(input, output, session){
  
  ## Stressor Response tab ####
  
  myData <- reactive({
    df_nuts %>%
      select(input$Nut_Choice1, Avg_NH_IBI_Score, Type) %>%
      filter(complete.cases(.))
  }) # reactive
  
  ### stressor response plot ####
  output$S_R_Plot <- renderPlot({
    
    # filter data
    plot_data <- myData()
    plot_data$Type <- as.factor(plot_data$Type)
    
    # get number of observations
    myCount <- nrow(plot_data)
    myRefCount <- sum(plot_data$Type == "Ref")
    
    # create color scale
    pal <- c("Ref" = "#0570b0"
             ,"Non-Ref" = "#cccccc"
             ,"Imp" = "#cccccc"
             ,"Not_Available" = "#cccccc")
    # create plot
    ggplot(data = plot_data
           , aes(x = log10(.data[[input$Nut_Choice1]])
                 , y = Avg_NH_IBI_Score))+
      geom_point(aes(color = Type))+
      geom_smooth(method='lm', formula= y~x, color = "Red", linetype = "dashed"
                  , size = 2)+
      labs(title = paste0("NH IBI vs ", input$Nut_Choice1, "; n ="
                          , myCount, "; n Ref = ", myRefCount)
           , y = "Site Average IBI Score"
           , x = paste0("Log10(",input$Nut_Choice1,")")
           , color = "Site Type")+
      # scale_x_continuous(trans = "log10")+
      scale_color_manual(values = pal)+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "right",
            legend.text=element_text(size = 14))
    
  }) # renderPlot 

  ### Stats ####
  output$stats1 <- renderPrint({
    stat1_data <- myData()
    stat1_data <- stat1_data %>% 
      select(input$Nut_Choice1, Avg_NH_IBI_Score)
    
    summary(stat1_data)
    # df_4table <- myData()
    # df_4table <- df_4table %>% 
    #   rename(Parameter = input$Nut_Choice1)
    # 
    # # ref only stats
    # df_ref <- df_4table %>% 
    #   filter(Type == "Ref")
    # df_ref$Group <- "Ref sites"
    # 
    # df_ref_stats <- df_ref %>% 
    #   group_by(Group) %>% 
    #   summarize(N = n()
    #             , min = min(Parameter)
    #             , `25%` = quantile(Parameter, 0.25)
    #             , `50%` = quantile(Parameter, 0.50)
    #             , `75%` = quantile(Parameter, 0.75)
    #             , max = max(Parameter))
    # 
    # # all stats
    # df_All <- df_4table
    # df_All$Group <- "All sites"
    # 
    # df_All_stats <- df_All %>% 
    #   group_by(Group) %>% 
    #   summarize(N = n()
    #             , min = min(Parameter)
    #             , `25%` = quantile(Parameter, 0.25)
    #             , `50%` = quantile(Parameter, 0.50)
    #             , `75%` = quantile(Parameter, 0.75)
    #             , max = max(Parameter))
    # 
    # # bind data together and return
    # 
    # df_stats <- rbind(df_ref_stats, df_All_stats)
    # 
    # df_stats <- df_stats %>% 
    #   select(Group, everything())
    # 
    # return(df_stats)
  
  }) # renderPrint
  
  output$stats2 <- renderPrint({
    stat2_data <- myData()
    stat2_data <- stat2_data %>%
      filter(Type == "Ref") %>% 
      select(input$Nut_Choice1, Avg_NH_IBI_Score)
    
    summary(stat2_data)
   
    
  }) # renderPrint
  
  ### LM summary ####
  
  # build linear model
  Linear_Model <- reactive({
    lm_data <- myData()
    lm_data <- lm_data %>% 
      rename(Parameter = input$Nut_Choice1)
    
    lm(Avg_NH_IBI_Score ~ Parameter, data = lm_data)
  }) # reactive
  
  output$lm <- renderPrint({
    summary(Linear_Model())
  })

  
  ## Nutrient tab ####
  
  myNutData <- reactive({
    df_nuts %>%
      select(input$Nut_Choice2) %>%
      na.omit()
  }) # reactive
  
  myMapData <- reactive({
    df_nuts %>%
      select(StationID, Latitude, Longitude, input$Nut_Choice2) %>%
      filter(complete.cases(.))
  }) # reactive
  
  ### Map ####
  
  output$Map <- renderLeaflet({
    
    df_4Map <- myMapData()
    
    df_4Map <- df_4Map %>%
      select(StationID, Latitude, Longitude) %>% 
      distinct(.keep_all = TRUE)
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>%
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
      addCircleMarkers(data = df_4Map, lat = ~Latitude, lng = ~Longitude
                       , group = "NH WQ Sites"
                       , popup = paste("StationID:", df_4Map$StationID)
                       , color = "black", fillColor = "orange"
                       , fillOpacity = 0.5, radius = 1, stroke = TRUE
      ) %>%
      # addLegend(pal = qpal
      #           ,values = scale_range
      #           ,position = "bottomright"
      #           ,title = "Index Scores"
      #           ,opacity = 1) %>%
      addLayersControl(overlayGroups = c("NH WQ Sites"),
                       baseGroups = c("Esri WSM", "Positron", "Toner Lite"),
                       options = layersControlOptions(collapsed = TRUE))%>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
  }) ##renderLeaflet~END
  
  ### cdf plot ####
  output$cdf <- renderPlot({
    
    # filter data
    cdf_data <- myNutData()
    
    # get number of observations
    myCDFCount <- nrow(cdf_data)

    # create plot
    
    ggplot(data = cdf_data
           , aes(x = log10(.data[[input$Nut_Choice2]])))+
      stat_ecdf(geom = "point", pad = FALSE, color = "orange")+
      labs(title = paste0("n = ", myCDFCount), y = "Cumulative Percent"
           , x = paste0("Log10(",input$Nut_Choice2,")"))+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "right",
            legend.text=element_text(size = 14))
    
  }) # renderPlot 
  
  ### boxplot ####
  
  output$boxplot <- renderPlot({
    
    # format data
    box_data <- myNutData()
    box_data$Parameter <- input$Nut_Choice2
    
    # get number of observations
    myBoxCount <- nrow(box_data)
    # myBoxCount <- len(box_x)
    # create plot
    
    ggplot(data = box_data
           , aes(x = Parameter
                 , y = log10(.data[[input$Nut_Choice2]])))+
      geom_boxplot(fill = "orange")+
      labs(title = paste0("n = ", myBoxCount)
           , y = paste0("Log10(",input$Nut_Choice2,")")
           , x = input$Nut_Choice2)+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "right",
            legend.text=element_text(size = 14))
    
  }) # renderPlot 
  
  ### Quantile table ####
  output$QuantTable <- renderTable({
    
    # format data
    Table_Data <- myNutData()
    Table_Data <- Table_Data %>% 
      rename(Parameter = input$Nut_Choice2)
    
    Table_Data$Group <- "All sites"
    Table_Data %>% 
      group_by(Group) %>% 
      summarize(N = n()
                , min = min(Parameter)
                , `25%` = quantile(Parameter, 0.25)
                , `50%` = quantile(Parameter, 0.50)
                , `75%` = quantile(Parameter, 0.75)
                , max = max(Parameter))
    
  }) # renderTable
  

} # End of server

shinyApp(ui, server)