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
df_nuts<-read.csv("Data/nutrients_wide01102022.csv")
df_strs_respon <-read.csv("Data/NH_Strs_Resp_Data_20220124.csv")

# duplicate nutrient data for distinct groups
df_ref <- df_nuts %>% 
  filter(DistCat_Tt == "Ref") %>% 
  mutate(Group = "Ref_Samps")

df_above_thresh <- df_nuts %>% 
  filter(BugThreshGroup == "Above_Threshold") %>% 
  mutate(Group = "Above_Threshold")

df_nuts$Group <- "All_Samps"

df_nuts_updated <- rbind(df_nuts, df_ref, df_above_thresh)

df_nuts_updated$Group <- factor(df_nuts_updated$Group,
                                levels = c("All_Samps","Ref_Samps", "Above_Threshold"))

# duplicate stressor response data for distinct groups

# df_ref_respon <- df_strs_respon %>%
#   filter(DistCat_Tt == "Ref") %>%
#   mutate(Group = "Ref_Samps")
# 
# df_strs_respon$Group <- "All_Samps"
# 
# df_strs_respon_updated <- rbind(df_strs_respon, df_ref_respon)
# 
# df_strs_respon_updated$Group <- as.factor(df_strs_respon_updated$Group)

# Create app

# ui ####

ui <- fluidPage(
  tabsetPanel(
    ## Macroinvert tab ####
    tabPanel("Macroinvertebrates and Nutrients"
      , sidebarLayout(
        sidebarPanel(
          helpText("The information presented here represents",
                   "macroinvertebrate and nutrient data paired by site and year.",
                   "Note: IBI (and component metric) scores and nutrient data are annual medians.")
          ,selectInput(inputId = "Nut_Choice1"
                      ,label = "Nutrient Parameter:"
                      ,choices = c("Med_NOx_mgL"
                                   ,"Med_TP_mgL"
                                   ,"Med_TKN_mgL"
                                   ,"Med_NO3_mgL"
                                   ,"Med_TN_mgL"
                                   ,"Med_NO2_mgL"
                                   ,"Med_Chla_uncorr"
                                   ,"Med_NH3_mgL"
                                   ,"Med_PO4_mgL")) # selectInput
          
          ,selectInput(inputId = "Bug_Choice1"
                       ,label = "Macroinvertebrate Parameter:"
                       ,choices = c("Med_IBI_Score"
                                    ,"Med_TotTaxSc"
                                    ,"Med_PlecTaxSc"
                                    ,"Med_ChirPctSc"
                                    ,"Med_NonInsPctSc"
                                    ,"Med_ClingPctSc"
                                    ,"Med_IntolPctSc"
                                    ,"Med_TolerTaxSc")) # selectInput
          
        ) # sidebarPanel
        , mainPanel(
          plotOutput(outputId = "S_R_Plot")
          , h5("Solid line = 90th quantile regression; dashed line = linear regression")
          , br()
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
                 column(width=4, offset = 1, 
                        # Input: parameter
                        selectInput(inputId = "Nut_Choice2",
                                    label = "Nutrient Parameter",
                                    choices = c("NOx_mgL"
                                                ,"TP_mgL"
                                                ,"TKN_mgL"
                                                ,"NO3_mgL"
                                                ,"TN_mgL"
                                                ,"NO2_mgL"
                                                ,"Chla_Uncorr"
                                                ,"Chla_corr"
                                                ,"NH3_mgL"
                                                ,"PO4_mgL"
                                                ,"TDN_mgL"
                                                ,"NH4_ugL")) # selectInput
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
    df_strs_respon %>%
      select(input$Nut_Choice1, input$Bug_Choice1, DistCat_Tt) %>%
      filter(complete.cases(.))
  }) # reactive
  
  ### stressor response plot ####
  output$S_R_Plot <- renderPlot({
    
    # filter data
    plot_data <- myData()
    plot_data$DistCat_Tt <- as.factor(plot_data$DistCat_Tt)
    
    # get number of observations
    myCount <- nrow(plot_data)
    myRefCount <- sum(plot_data$DistCat_Tt == "Ref")
    
    # create color scale
    pal <- c("Ref" = "#0570b0"
             ,"Non-Ref" = "#969696"
             ,"Not_Eval" = "#cccccc")
    
    # create plot
    ggplot(data = plot_data
           , aes(x = log10(.data[[input$Nut_Choice1]])
                 , y = (.data[[input$Bug_Choice1]])))+
      geom_smooth(method='lm', formula= y~x, color = "Red", linetype = "dashed"
                  , size = 2)+
      geom_quantile(quantiles = 0.90, formula= y~x, color = "light blue"
                    , linetype = "solid", size = 1)+
      geom_point(aes(color = DistCat_Tt))+
      labs(title = paste0(input$Nut_Choice1," vs ", input$Bug_Choice1, "; n = "
                          , myCount, "; n Ref = ", myRefCount)
           , y = paste0(input$Bug_Choice1)
           , x = paste0("Log10(",input$Nut_Choice1,")")
           , color = "DistCat_Tt")+
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
      select(input$Nut_Choice1, input$Bug_Choice1)
    
    summary(stat1_data)
  }) # renderPrint
  
  output$stats2 <- renderPrint({
    stat2_data <- myData()
    stat2_data <- stat2_data %>%
      filter(DistCat_Tt == "Ref") %>% 
      select(input$Nut_Choice1, input$Bug_Choice1)
    
    summary(stat2_data)
   
    
  }) # renderPrint
  
  ### LM summary ####
  
  # build linear model
  Linear_Model <- reactive({
    lm_data <- myData()
    lm_data <- lm_data %>% 
      rename(Nut_Parameter = input$Nut_Choice1
             , Bug_Parameter = input$Bug_Choice1)
    
    lm(Bug_Parameter ~ Nut_Parameter, data = lm_data)
  }) # reactive
  
  output$lm <- renderPrint({
    summary(Linear_Model())
  })

  
  ## Nutrient tab ####
  
  myNutData <- reactive({
    df_nuts_updated %>%
      select(input$Nut_Choice2, DistCat_Tt, Group) %>%
      filter(complete.cases(.))
  }) # reactive
  
  myMapData <- reactive({
    df_nuts %>%
      select(StationID, Latitude, Longitude, input$Nut_Choice2) %>%
      filter(complete.cases(.))
  }) # reactive
  
  # create color scale
  nutpal <- c("Ref_Samps" = "#0570b0"
              ,"All_Samps" = "#969696"
              ,"Above_Threshold" = "#04b571")
  
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
    myCDFCount <- sum(cdf_data$Group == "All_Samps")
    myCDFRefCount <- sum(cdf_data$Group == "Ref_Samps")
    myCDFThreshCount <- sum(cdf_data$Group == "Above_Threshold")

    # create plot
    
    ggplot(data = cdf_data
           , aes(x = log10(.data[[input$Nut_Choice2]]), color = Group))+
      stat_ecdf(geom = "point", pad = FALSE)+
      # stat_ecdf(geom = "point", pad = FALSE, color = "orange")+
      labs(title = paste0(input$Nut_Choice2,"; n = "
                          , myCDFCount,"; n Ref = ", myCDFRefCount
                          , "; \nn Above Threshold = ", myCDFThreshCount)
           , y = "Cumulative Percent"
           , x = paste0("Log10(",input$Nut_Choice2,")"))+
      scale_color_manual(values = nutpal)+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
    
  }) # renderPlot 
  
  ### boxplot ####
  
  output$boxplot <- renderPlot({
    
    # format data
    box_data <- myNutData()
    box_data$Parameter <- input$Nut_Choice2
    
    # get number of observations
    myBoxCount <- nrow(box_data)
    
    # get number of observations
    myBoxCount <- sum(box_data$Group == "All_Samps")
    myBoxRefCount <- sum(box_data$Group == "Ref_Samps")
    myBoxThreshCount <- sum(box_data$Group == "Above_Threshold")
    
    # create plot
    
    ggplot(data = box_data
           , aes(x = Parameter
                 , y = log10(.data[[input$Nut_Choice2]])
                 , fill = Group))+
      geom_boxplot()+
      labs(title = paste0(input$Nut_Choice2,"; n = "
                          , myBoxCount,"; n Ref = ", myBoxRefCount
                          , "; \nn Above Threshold = ", myBoxThreshCount)
           , y = paste0("Log10(",input$Nut_Choice2,")")
           , x = input$Nut_Choice2)+
      scale_fill_manual(values = nutpal)+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
    
  }) # renderPlot 
  
  ### Quantile table ####
  output$QuantTable <- renderTable({
    
    # format data
    Table_Data <- myNutData()
    Table_Data <- Table_Data %>% 
      rename(Parameter = input$Nut_Choice2)
    
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