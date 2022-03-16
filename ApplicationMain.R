# 1. input location
# 2. extract BioClim within a radius (severel radius choices)
# 3. Dowload plant observation data--present or `70-00`?
# 4. RF accuracy
# 5. Partial dependence
# 6. Predictions
# (just need to do these for non-native plants--native is not that necessary in this case)
# 7. Pick out EXACT SPECIES that are especially ranpant 
# 8. ****IDEA: once found which group increase the most, give an option to run procedures on 
#              the most populous SPECIES of this group (these groups)
# 9. Or: offer something like that for each group
# 10. can use JSDM on those individual species
setwd("D:/Application")

library(shiny)
library(shinyWidgets)
library(leaflet)
# install.packages("leaflet")
library(plotly)
library(rootSolve)
library(greekLetters)
library(numDeriv)
require(raster)
require(sp)
require(rgdal)
require(rfUtilities)
require(randomForest)
require(dismo)
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(maptools)
library(rgeos)
library(viridis)
library(rasterVis)
# install.packages("magrittr")
library(magrittr)
library(mrdwabmisc)



#########TESTING AREA##############
# m = leaflet() %>% addTiles()
# df = data.frame(
#   lat = rnorm(100),
#   lng = rnorm(100),
#   size = runif(100, 5, 20),
#   color = sample(colors(), 100)
# )
# m = leaflet(df) %>% addTiles()
# m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
# m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

# IVCAEcoregions <- shapefile("D:/Application/data/ca_eco_l4/ca_eco_l4.shp")


#Load data here?
Grasslike_NonNative_with_envi <- readRDS("D:/Application/data/Grasslike_NonNative_with_envi.rds")
Grasslike_NonNative_background <- readRDS("D:/Application/data/Grasslike_NonNative_background.rds")
Herb_NonNative_with_envi <- readRDS("D:/Application/data/Herb_NonNative_with_envi.rds")
Herb_NonNative_background <- readRDS("D:/Application/data/Herb_NonNative_background.rds")
Shrub_NonNative_with_envi <- readRDS("D:/Application/data/Shrub_NonNative_with_envi.rds")
Shrub_NonNative_background <- readRDS("D:/Application/data/Shrub_NonNative_background.rds")
Tree_NonNative_with_envi <- readRDS("D:/Application/data/Tree_NonNative_with_envi.rds")
Tree_NonNative_background <- readRDS("D:/Application/data/Tree_NonNative_background.rds")
Fern_NonNative_with_envi <- readRDS("D:/Application/data/Fern_NonNative_with_envi.rds")
Fern_NonNative_background <- readRDS("D:/Application/data/Fern_NonNative_background.rds")
Vine_NonNative_with_envi <- readRDS("D:/Application/data/Vine_NonNative_with_envi.rds")
Vine_NonNative_background <- readRDS("D:/Application/data/Vine_NonNative_background.rds")
Annual_NonNative_with_envi <- readRDS("D:/Application/data/Annual_NonNative_with_envi.rds")
Annual_NonNative_background <- readRDS("D:/Application/data/Annual_NonNative_background.rds")
Biennial_NonNative_with_envi <- readRDS("D:/Application/data/Biennial_NonNative_with_envi.rds")
Biennial_NonNative_background <- readRDS("D:/Application/data/Biennial_NonNative_background.rds")
Perennial_NonNative_with_envi <- readRDS("D:/Application/data/Perennial_NonNative_with_envi.rds")
Perennial_NonNative_background <- readRDS("D:/Application/data/Perennial_NonNative_background.rds")

template <- raster("D:/Application/data/amt.tif")
CalEcoRegionsOutlines <- shapefile("D:/Application/data/CalEcoRegionsOutlines2.shp")



###LEAVE THE SHAPEFILE AND ECOREGIONS FOR NOW... GET THE BASIC STRUCTURE DOWN FIRST!!!!!
#ADD THOSE NICE, EXTRA DETAILS LATER





# neStates <- subset(IVCAEcoregions, states$STUSPS %in% c(
#   "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
# ))

# leaflet(IVCAEcoregions) %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE))
# 
# 


###########FUNCTIONS##############
process_observations_with_envi <- function (radius, center_lat, center_lng, group) {
  #No need to match observation with BioClim data again
  #all I need is to select the "with_envi" observations from the data(base) that has coordinates within the selected radius
  
  #This function is for processing one group at a time; to do all 18, just call this function multiple times (more flexible that way too)
  
  #Find with_envi within the selected radius
  # Grasslike_NonNative_abundance_list <- rbind(Grasslike_NonNative_abundance_list, Grasslike_NonNative_abundance_at_site)
  eval(parse(text = paste(
    group,'_with_envi_filtered <- data.frame()
    xmin <- center_lng - radius
    xmax <- center_lng + radius
    ymin <- center_lat - radius
    ymax <- center_lat + radius
  
    for (i in 1:nrow(',group,'_with_envi)){
      temp_x <- ',group,'_with_envi[i,2]
      temp_y <- ',group,'_with_envi[i,3]
      if((temp_x >= xmin) & (temp_x <= xmax)){
        if((temp_y >= ymin) & (temp_y <= ymax)){
          ',group,'_with_envi_filtered <- rbind(',group,'_with_envi[i,], ',group,'_with_envi_filtered)        
        }
      }
    }',
    
  sep='')))
  
  eval(parse(text = paste(
  
  'if (nrow(',group,'_with_envi_filtered) < 3) {
    output$too_little_data <- renderText({"There are less than 3 observations of ',group,'. Insufficient data for assessment. 
      Please ensure that the selected area is within California, increase radius, or choose another location."})
  }
  if (nrow(',group,'_with_envi_filtered) >= 3) {
   return(',group,'_with_envi_filtered)}',
    
    sep='')))
}


#eval(parse(text = paste('',sep='')))
  

process_observations_background <- function (radius, center_lat, center_lng, group) {
  #No need to match observation with BioClim data again
  #all I need is to select the "with_envi" observations from the data(base) that has coordinates within the selected radius
  
  #This function is for processing one group at a time; to do all 18, just call this function multiple times (more flexible that way too)
  
  #Find with_envi within the selected radius
  # Grasslike_NonNative_abundance_list <- rbind(Grasslike_NonNative_abundance_list, Grasslike_NonNative_abundance_at_site)
  eval(parse(text = paste(
    group,'_background_filtered <- data.frame()
    xmin <- center_lng - radius
    xmax <- center_lng + radius
    ymin <- center_lat - radius
    ymax <- center_lat + radius


    for (i in 1:nrow(',group,'_background)){
      temp_x <- ',group,'_background[i,2]
      temp_y <- ',group,'_background[i,3]
      if((temp_x >= xmin) & (temp_x <= xmax)){
        if((temp_y >= ymin) & (temp_y <= ymax)){
          ',group,'_background_filtered <- rbind(',group,'_background[i,], ',group,'_background_filtered)        
        }
      }
    }
  
  if (nrow(',group,'_background_filtered) < 3) {
    output$too_little_data <- renderText({"There are less than 3 observations of ',group,'. Insufficient data for assessment. 
      Please ensure that the selected area is within California, increase radius, or choose another location."})
  }
  if (nrow(',group,'_background_filtered) >= 3) {
  
   return(',group,'_background_filtered)}',
    
    sep='')))
}

  
  
  
  

##Make sure to have this line before each set of loops to collect TOTAL output


return_envi_train <- function (group_with_envi, group_background) {
  envi_filename <- c("amt", "ap", "isoth", "mdr", "mtcm", "mtcq", "mtdq", 
                     "mtwarmq", "mtwetq", "mtwm", "pcq", "pdm", "pdq", 
                     "ps", "pwarmq", "pwetq", "pwm", "tar", "ts")
  
  
  sample_num <- sample.size(nrow(group_with_envi), samp.size = NULL, c.lev = 95, c.int = 4,
                            what = "sample", distribution = 50)
  
  sample_num <- as.numeric(sample_num[5])
  
  
  #Convert dataframe to vectors to make index calls easier--same some amount of runtime
  nrow_pres_subset <- nrow(group_with_envi)
  nrow_abs_subset <- nrow(group_background)
  
  pres_subset <- group_with_envi[,colnames(group_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- group_background[,colnames(group_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  
  testpres <- pres_test
  testbackgr <- backgr_test
  return(envi_train)
}

run_a_random_forest_model <- function (return_envi_train) {
  #This is the version with rf.modelSel
  #Calculate Sample size now based on each popoluations size
  
  
  
  #https://rdrr.io/github/mrdwab/mrdwabmisc/man/sample.size.html
  #^^http://www.raosoft.com/samplesize.html
  # envi_filename <- gsub("D:/Application/data/","", gsub(".tif", "", envi_map_layers))
  envi_filename <- c("amt", "ap", "isoth", "mdr", "mtcm", "mtcq", "mtdq", 
                     "mtwarmq", "mtwetq", "mtwm", "pcq", "pdm", "pdq", 
                     "ps", "pwarmq", "pwetq", "pwm", "tar", "ts")
  
  
  rf_regress <- suppressWarnings(randomForest(return_envi_train[,colnames(return_envi_train) %in% envi_filename],return_envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  
  return(rf_regress)
  
  
  #again, this function should
  #Need to think about how to return the most crucial part so that all outputs can be derived from that returned part
  #Do I need to make separate functions for the graph-storing objects?
}


#not need to run_jSDM1 bc the site would be too small to divide into even further "sites"
#and the interaction between plants (/plant groups) won't drastically change anyways


#Separate functions woult not work, because EACH of these outputs need to use the same model,
#but if they use the same model in one function, it is impossible to output all the different kinds of returns





##########IMPORTANT REVELATIONS################
# (should have investigated this long ago...) the function randomForest can to it all
# I just need to write a function that returnes an object that is the output of randomForest(), then 
# I can do all the other processing in the server (including loops) <---
#   
  
















############UI####################
#use leaflet as a way to input lat and long

# number_of_prediction_graphs_rows <- 2
# number_of_prediction_graphs_cols <- 2

ui <- fluidPage(
  #Given penals for components of the app
  titlePanel(h1("Which Non-native Plants Occupy the Changing Environment?", style = "font-family: 'Georgia'")),
  
  fluidRow(
    column(10, 
      leafletOutput("sel_map", height=250)
    ),
    column(2, 
      
      tags$p("Selected Location Coordinates", style = "font-family: 'Georgia'"),
      #Figure out why the formatting for the textoutputs are not working
      ##!!! I've figured it out now!! p<> will not work with any font--only the headlines can be changed to other fonts in this case. Hmm..
      tags$h6(strong(textOutput("selected_point"), style = "font-family: 'Georgia'")),
      
      # tags$p("Area of Region Assessed (degrees)", style = "font-family: 'Georgia'"),
      #Maybe add a conversation message as a courtesy
      numericInput(inputId = "radius", label =p("Area of Region Assessed (degrees)", style = "font-family: 'Georgia'"), value = 0.5),
      
      
      actionButton("calc", h5("Confirm",style = "font-family: 'Georgia'")),
      h4(textOutput("test_clicks"),style = "font-family: 'Georgia'"),
      
      
    )
  ),
  

  #Fix the weird incosistency in ability to use font later
  navlistPanel(
    "Influence of Climate Change",
    
    tabPanel(title=tags$p("Bioclimate Variable Importance", style = "font-family: 'Georgia'"),
             
            
             h3("The importance of bioclimatic variables to the spread of each plant group",style = "font-family: 'Georgia'"),
             
             # actionButton("test", h5("Will this give a text?",style = "font-family: 'Georgia'")),
             # h4(textOutput("test_give_text"),style = "font-family: 'Georgia'"),
             h4(textOutput("too_little_data"),style = "font-family: 'Georgia'"),
             h6(textOutput("reading_data"),style = "font-family: 'Georgia'; color:blue"),
             h6(textOutput("progress_message"),style = "font-family: 'Georgia'; color:blue"),
             
             selectInput(
               "var_group",
               "Non-native Plant Group:",
               c("Grasslike", "Herb","Shrub","Tree","Fern","Vine","Annual","Biennial","Perennial"),
               selected = NULL,
               multiple = FALSE,
               selectize = TRUE,
               width = NULL,
               size = NULL
             ),
             
             fluidRow(
               column(4, 
                      tableOutput("Grasslike_NonNative_var_imp")
               ),
             #   column(8, 
             #          
             #          
             #   ),
             #   column(12
             #          
             # ),
             # fluidRow(
             #   column(4, 
             #          
             #   ),
             #   column(8, 
             #          
             #          
             #   ),
             #   column(12
             #          
             #   ),
             #   fluidRow(
             #     column(4, 
             #            
             #     ),
             #     column(8, 
             #            
             #            
             #     ),
             #     column(12
             #            
                 ),
             actionButton("var_imp", h5("Assess",style = "font-family: 'Georgia'")),   
               
             ),
    
    
    
    
    tabPanel(title=tags$p("Response to Changes in Bioclimate", style = "font-family: 'Georgia'"),
             h3("Partial Dependence of Plant Presence on Climate: How changes in bioclimatic variables influences 
                response in plant group's probability of presence",style = "font-family: 'Georgia'"),
             fluidRow(
               column(8, 
                      plotOutput("Grasslike_NonNative_partial_depend", width = 600, height = 6000))
               ),
    ),
    
    
    tabPanel(title=tags$p("Model Accuracy Evaluation", style = "font-family: 'Georgia'"),
             h3("Evaluate Accuracy and Reliability of Random Forest Model Trained to Assess
                Non-native Plant Spread in this region",style = "font-family: 'Georgia'"),
             tableOutput("Grasslike_NonNative_eval")),
    
    "Future Prediction",
    
    tabPanel(title=tags$p("Observation Likelihood", style = "font-family: 'Georgia'"),
             h5("Please check if the selected region is still the target and click \"confirm\" to proceed",style = "font-family: 'Georgia';color: blue"),
             h3("Prediction of the Spread of Plants with Selected Traits",style = "font-family: 'Georgia'"),
             
             actionButton("future_predict", h5("Assess",style = "font-family: 'Georgia'")),
             fluidRow(
               column(3, 
                      selectInput(
                        "group",
                        "Non-native Plant Group:",
                        c("Grasslike", "Herb","Shrub","Tree","Fern","Vine","Annual","Biennial","Perennial"),
                        selected = NULL,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL
                      )
               ),
               column(3,selectInput(
                 "SSP",
                 "Shared Socio-economic Pathway*:",
                 c("126","245","370","585","All"),
                 selected = NULL,
                 multiple = FALSE,
                 selectize = TRUE,
                 width = NULL,
                 size = NULL
               )),
               column(3,selectInput(
                 "time",
                 "Time Period:",
                 c("2021-2040","2041-2060","2061-2080","2081-2100","All"),
                 selected = NULL,
                 multiple = FALSE,
                 selectize = TRUE,
                 width = NULL,
                 size = NULL
               ))),
    
             

             fluidRow(
               column(8, 
                      plotOutput("Grasslike_NonNative_Observation_Likelihood", width = 600, height=600)
             )),
             
    
    ),
  
    tabPanel(tags$p("Presence/Absence", style = "font-family: 'Georgia'"),
             h3("Predicted Presence (Green) of Plants with Selected Traits",style = "font-family: 'Georgia'"),
             fluidRow(
               column(8, 
                      plotOutput("PA", width = 600, height=600)
               ))),
             
    tabPanel(tags$p("Quantification", style = "font-family: 'Georgia'"),
             h3("Average Predicted Observation Likelihood in Each Scenario",style = "font-family: 'Georgia'"),
             
             tableOutput("OL_Quantification"))
    # "Header B",
    # tabPanel("Component 3"),
    # tabPanel("Component 4"),
    # "-----",
    # tabPanel("Component 5")
  )
)







# ui <- fluidPage(
#   #Grid layout--for graphs
#   titlePanel("Hello Shiny!"),
#   
#   fluidRow(
#     
#     column(4,
#            wellPanel(
#              sliderInput(
#                "bins", label = "Number of bins:",
#                min = 1, value = 30, max = 50
#              )
#            )       
#     ),
#     
#     column(8,
#            plotOutput("distPlot")
#     )
#   )
# )
# 
# 
# ####Figure out if table set can coexist with side penals
# ui <- fluidPage(
#   #Tablesets--for outputs of the same topic
#   titlePanel("Tabsets"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("plot")), 
#         tabPanel("Summary", verbatimTextOutput("summary")), 
#         tabPanel("Table", tableOutput("table"))
#       )
#     )
#   )
# )


##########SERVER#################

server <- function(input, output, session) {
  # IVCAEcoregions <- readOGR("D:/Application/data/ca_eco_l4/ca_eco_l4.shp",
  #                           layer = "ca_eco_l4", GDAL1_integer64_policy = TRUE)
  
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  #Maybe read in data outside of observe event?
  
  
  
  
  
  
  
  
  output$sel_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -118, lat = 36, zoom = 5)
      # fitBounds(-126.3139, 30.88767, -111.6354, 43.47037) - redundent; setView provides the same effect
  })
  

  observeEvent(input$sel_map_click, {
    
    

    click <- input$sel_map_click
    
    text <-paste("Latitude: ", round(click$lat,6), "Longtitude: ", round(click$lng,6))
    
    proxy <- leafletProxy("sel_map")
    
    ## This displays the pin drop circle
    proxy %>% 
      clearGroup("new_point") %>%
      #clearMarkers(layerId=input$sel_map_click$id) %>%
      #addPopups(click$lng, click$lat) %>%
      addCircles(click$lng, click$lat, radius=100, color="red", group = "new_point")
    
    output$selected_point <- renderText({text})
    
    proxy2 <- leafletProxy("sel_map")
    proxy %>% 
      clearGroup("new_rectangle") %>%
      #clearMarkers(layerId=input$sel_map_click$id) %>%
      #addPopups(click$lng, click$lat) %>%
      addRectangles(
        lng1= click$lng - input$radius, lat1= click$lat - input$radius,
        lng2= click$lng + input$radius, lat2= click$lat + input$radius,
        fillColor = "transparent", group = "new_rectangle"
        
      )

  

  
  observeEvent(input$calc, {
        
        radius <- input$radius
        center_lat <- click$lat
        center_lng <- click$lng
        


  
  
  # observeEvent(input$test, {
  #   output$test_give_text <- renderText({"will this print?"})
  # })
  # 


    observeEvent(input$var_imp,{
    
# #loading data... unsure if necessary here; is it OK if it's already in global environment?-------------------
    # output$reading_data <- renderText({"Loading original data. This will take about"})
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Loading plant observation data',
                 detail = "Hopefully, it'll be quick!")
    
    
    #input$var_group
    eval(parse(text = paste('
    ', input$var_group, '_NonNative_with_envi <- readRDS("D:/Application/data/', input$var_group, '_NonNative_with_envi.rds")
    ', input$var_group, '_NonNative_background <- readRDS("D:/Application/data/', input$var_group, '_NonNative_background.rds")

    colnames_', input$var_group, '_NonNative_with_envi <- colnames(', input$var_group, '_NonNative_with_envi)

    colnames_', input$var_group, '_NonNative_background <- colnames(', input$var_group, '_NonNative_background)


    envi_filename <- c("amt",     "ap",      "isoth",   "mdr",     "mtcm",    "mtcq",    "mtdq",    "mtwarmq", "mtwetq",  "mtwm",    "pcq",     "pdm",     "pdq",    "ps",      "pwarmq",  "pwetq",   "pwm",     "tar",     "ts")

    Only_BioClim_Names_and_pa <- c(envi_filename,"pa")


    progress$set(message = \'Finished loading data!\',
                 detail = \'Now matching plant observations to selected region...\')
    
    ', input$var_group, '_NonNative <- "', input$var_group, '_NonNative"
    ', input$var_group, '_NonNative_with_envi_filtered <- process_observations_with_envi(radius, center_lat, center_lng, ', input$var_group, '_NonNative)
    ', input$var_group, '_NonNative_background_filtered <- process_observations_background(radius, center_lat, center_lng, ', input$var_group, '_NonNative)
    progress$set(message = \'', input$var_group,' non-native plants observation data filtered and processed\',detail = \'These processes may take another while...\')
    

    
    
    
    
    
    #Running Random Forest
    envi_filename <- c(\"amt\", \"ap\", \"isoth\", \"mdr\", \"mtcm\", \"mtcq\", \"mtdq\", 
                       \"mtwarmq\", \"mtwetq\", \"mtwm\", \"pcq\", \"pdm\", \"pdq\", 
                       \"ps\", \"pwarmq\", \"pwetq\", \"pwm\", \"tar\", \"ts\")
    
    ', input$var_group, '_NonNative_rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
    
    
    
    Only_BioClim_Names_and_pa <- c(envi_filename,"pa")
    ', input$var_group, '_RFp1Total <- data.frame()
    ', input$var_group, '_RFp2Total <- data.frame()
    ', input$var_group, '_RFp3Total <- data.frame()
    ', input$var_group, '_RFp4Total <- data.frame()
    ', input$var_group, '_RFp5Total <- data.frame()
    ', input$var_group, '_RFp6Total <- data.frame()
    ', input$var_group, '_RFp7Total <- data.frame()
    ', input$var_group, '_RFp8Total <- data.frame()
    ', input$var_group, '_RFp9Total <- data.frame()
    ', input$var_group, '_RFp10Total <- data.frame()
    ', input$var_group, '_RFp11Total <- data.frame()
    ', input$var_group, '_RFp12Total <- data.frame()
    ', input$var_group, '_RFp13Total <- data.frame()
    ', input$var_group, '_RFp14Total <- data.frame()
    ', input$var_group, '_RFp15Total <- data.frame()
    ', input$var_group, '_RFp16Total <- data.frame()
    ', input$var_group, '_RFp17Total <- data.frame()
    ', input$var_group, '_RFp18Total <- data.frame()
    ', input$var_group, '_RFp19Total <- data.frame()
    
    
    ', input$var_group, '_NonNative_randomF_evaluation_total <- data.frame()

    
        
    nrow_pres_subset <- nrow(', input$var_group, '_NonNative_with_envi)
    # res_abs_subset <- numeric(nrow(', input$var_group, '_NonNative_background))
    nrow_abs_subset <- nrow(', input$var_group, '_NonNative_background)
    
    
    ', input$var_group, '_NonNative_with_envi <- as.vector(', input$var_group, '_NonNative_with_envi)
    ', input$var_group, '_NonNative_background <- as.vector(', input$var_group, '_NonNative_background)
    
    
    

    

    
    
    
    
    # For now it\'s 3 models\' iteration; consider allowing user input
    for(i in 1:3){
      ', input$var_group, '_envi_train <- return_envi_train(', input$var_group, '_NonNative_with_envi_filtered, ', input$var_group, '_NonNative_background_filtered)
      ', input$var_group, '_rf_regress <- run_a_random_forest_model(', input$var_group, '_envi_train)
      rf_importance <- importance(', input$var_group, '_rf_regress)
      rf_importance <- data.frame(names=row.names(rf_importance),rf_importance)
      ', input$var_group, '_NonNative_rf_importance_total <- rbind(', input$var_group, '_NonNative_rf_importance_total,rf_importance)
      
      
      envi_filename <- c("amt", "ap", "isoth", "mdr", "mtcm", "mtcq", "mtdq", 
                         "mtwarmq", "mtwetq", "mtwm", "pcq", "pdm", "pdq", 
                         "ps", "pwarmq", "pwetq", "pwm", "tar", "ts")
      
      
      
      ', input$var_group, '_RFp1 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="amt"))
      ', input$var_group, '_RFp1Total <- rbind(', input$var_group, '_RFp1Total,', input$var_group, '_RFp1)
      ', input$var_group, '_RFp2 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="ap"))
      ', input$var_group, '_RFp2Total <- rbind(', input$var_group, '_RFp2Total,', input$var_group, '_RFp2)
      ', input$var_group, '_RFp3 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="isoth"))
      ', input$var_group, '_RFp3Total <- rbind(', input$var_group, '_RFp3Total,', input$var_group, '_RFp3)
      ', input$var_group, '_RFp4 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mdr"))
      ', input$var_group, '_RFp4Total <- rbind(', input$var_group, '_RFp4Total,', input$var_group, '_RFp4)
      ', input$var_group, '_RFp5 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtcm"))
      ', input$var_group, '_RFp5Total <- rbind(', input$var_group, '_RFp5Total,', input$var_group, '_RFp5)
      ', input$var_group, '_RFp6 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtcq"))
      ', input$var_group, '_RFp6Total <- rbind(', input$var_group, '_RFp6Total,', input$var_group, '_RFp6)
      ', input$var_group, '_RFp7 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtdq"))
      ', input$var_group, '_RFp7Total <- rbind(', input$var_group, '_RFp7Total,', input$var_group, '_RFp7)
      ', input$var_group, '_RFp8 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtwarmq"))
      ', input$var_group, '_RFp8Total <- rbind(', input$var_group, '_RFp8Total,', input$var_group, '_RFp8)
      ', input$var_group, '_RFp9 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtwetq"))
      ', input$var_group, '_RFp9Total <- rbind(', input$var_group, '_RFp9Total,', input$var_group, '_RFp9)
      ', input$var_group, '_RFp10 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="mtwm"))
      ', input$var_group, '_RFp10Total <- rbind(', input$var_group, '_RFp10Total,', input$var_group, '_RFp10)
      ', input$var_group, '_RFp11 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pcq"))
      ', input$var_group, '_RFp11Total <- rbind(', input$var_group, '_RFp11Total,', input$var_group, '_RFp11)
      ', input$var_group, '_RFp12 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pdm"))
      ', input$var_group, '_RFp12Total <- rbind(', input$var_group, '_RFp12Total,', input$var_group, '_RFp12)
      ', input$var_group, '_RFp13 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pdq"))
      ', input$var_group, '_RFp13Total <- rbind(', input$var_group, '_RFp13Total,', input$var_group, '_RFp13)
      ', input$var_group, '_RFp14 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="ps"))
      ', input$var_group, '_RFp14Total <- rbind(', input$var_group, '_RFp14Total,', input$var_group, '_RFp14)
      ', input$var_group, '_RFp15 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pwarmq"))
      ', input$var_group, '_RFp15Total <- rbind(', input$var_group, '_RFp15Total,', input$var_group, '_RFp15)
      ', input$var_group, '_RFp16 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pwetq"))
      ', input$var_group, '_RFp16Total <- rbind(', input$var_group, '_RFp16Total,', input$var_group, '_RFp16)
      ', input$var_group, '_RFp17 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="pwm"))
      ', input$var_group, '_RFp17Total <- rbind(', input$var_group, '_RFp17Total,', input$var_group, '_RFp17)
      ', input$var_group, '_RFp18 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="tar"))
      ', input$var_group, '_RFp18Total <- rbind(', input$var_group, '_RFp18Total,', input$var_group, '_RFp18)
      ', input$var_group, '_RFp19 <- as.data.frame(partialPlot(', input$var_group, '_rf_regress, pred.data=', input$var_group, '_envi_train[,colnames(', input$var_group, '_envi_train) %in% envi_filename],x.var="ts"))
      ', input$var_group, '_RFp19Total <- rbind(', input$var_group, '_RFp19Total,', input$var_group, '_RFp19)
      
      #detete "', input$var_group, '" to let program overwrite this
      for (i in 1:nrow(', input$var_group, '_envi_train)){
        if(', input$var_group, '_envi_train$pa[i] < 1){
          ', input$var_group, '_div_pt <- i
          break
        }
      }
      ', input$var_group, '_testpres <- ', input$var_group, '_envi_train[1:(', input$var_group, '_div_pt-1),]
      ', input$var_group, '_testbackgr <- ', input$var_group, '_envi_train[', input$var_group, '_div_pt:nrow(', input$var_group, '_envi_train),]
      
      ', input$var_group, '_NonNative_erf <- suppressWarnings(evaluate(', input$var_group, '_testpres,', input$var_group, '_testbackgr,', input$var_group, '_rf_regress))
      ', input$var_group, '_NonNative_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
      colnames(', input$var_group, '_NonNative_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
      ', input$var_group, '_NonNative_randomF_rf_evaluation$AUC <- ', input$var_group, '_NonNative_erf@auc
      ', input$var_group, '_NonNative_randomF_rf_evaluation$cor <- ', input$var_group, '_NonNative_erf@cor
      ', input$var_group, '_NonNative_randomF_rf_evaluation$kappa <- max(', input$var_group, '_NonNative_erf@kappa)
      
      # Calculate Yules Q.
      tmp <- ', input$var_group, '_NonNative_erf@OR
      tmp[!is.finite(tmp)] <- NA 
      ', input$var_group, '_NonNative_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
      ', input$var_group, '_NonNative_randomF_rf_evaluation$TSS <- mean(', input$var_group, '_NonNative_erf@TPR,na.rm=T)+mean(', input$var_group, '_NonNative_erf@TNR,na.rm=T)-1
      ', input$var_group, '_NonNative_randomF_evaluation_total <- rbind(', input$var_group, '_NonNative_randomF_evaluation_total,', input$var_group, '_NonNative_randomF_rf_evaluation)
      
      
    } 
    
    
    #outputting var imp
    tmp <- as.data.frame(table(', input$var_group, '_NonNative_rf_importance_total$names))
    colnames(tmp) <- c("Variable","Freq")
    
    ', input$var_group, '_NonNative_rf_importance_total_sum <- ddply(', input$var_group, '_NonNative_rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))

    colnames(', input$var_group, '_NonNative_rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
    ', input$var_group, '_NonNative_rf_importance_total_sum <- left_join(tmp,', input$var_group, '_NonNative_rf_importance_total_sum)


    ', input$var_group, '_NonNative_rf_importance_total_sum <- ', input$var_group, '_NonNative_rf_importance_total_sum[order(-', input$var_group, '_NonNative_rf_importance_total_sum$MeanIncNodePurity),]
    
    
    ', input$var_group, '_NonNative_rf_importance_total_sum <- ', input$var_group, '_NonNative_rf_importance_total_sum[,c(1,3,4)]
    
    output$', input$var_group, '_NonNative_var_imp <- renderTable(', input$var_group, '_NonNative_rf_importance_total_sum)
    
    
    
    
    
    #outputting partial depend
    #Rename partial response dataframe variables for plotting.
    colnames(', input$var_group, '_RFp1Total) <- c("amt","Detection Probability")
    colnames(', input$var_group, '_RFp2Total) <- c("ap","Detection Probability")
    colnames(', input$var_group, '_RFp3Total) <- c("isoth","Detection Probability")
    colnames(', input$var_group, '_RFp4Total) <- c("mdr","Detection Probability")
    colnames(', input$var_group, '_RFp5Total) <- c("mtcm","Detection Probability")
    colnames(', input$var_group, '_RFp6Total) <- c("mtcq","Detection Probability")
    colnames(', input$var_group, '_RFp7Total) <- c("mtdq","Detection Probability")
    colnames(', input$var_group, '_RFp8Total) <- c("mtwarmq","Detection Probability")
    colnames(', input$var_group, '_RFp9Total) <- c("mtwetq","Detection Probability")
    colnames(', input$var_group, '_RFp10Total) <- c("mtwm","Detection Probability")
    colnames(', input$var_group, '_RFp11Total) <- c("pcq","Detection Probability")
    colnames(', input$var_group, '_RFp12Total) <- c("pdm","Detection Probability")
    colnames(', input$var_group, '_RFp13Total) <- c("pdq","Detection Probability")
    colnames(', input$var_group, '_RFp14Total) <- c("ps","Detection Probability")
    colnames(', input$var_group, '_RFp15Total) <- c("pwarmq","Detection Probability")
    colnames(', input$var_group, '_RFp16Total) <- c("pwetq","Detection Probability")
    colnames(', input$var_group, '_RFp17Total) <- c("pwm","Detection Probability")
    colnames(', input$var_group, '_RFp18Total) <- c("tar","Detection Probability")
    colnames(', input$var_group, '_RFp19Total) <- c("ts","Detection Probability")
    
    
    
    #Create 2d histograms with best-fit splines for the partial response curves.
    #Understand the default inputs (or if they are defaults)
    ', input$var_group, '_RFp1Plot <- ggplot(', input$var_group, '_RFp1Total, aes(x=amt, y=`Detection Probability`) )+xlab("amt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp1Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp2Plot <- ggplot(', input$var_group, '_RFp2Total, aes(x=ap, y=`Detection Probability`) )+xlab("ap")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp2Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp3Plot <- ggplot(', input$var_group, '_RFp3Total, aes(x=isoth, y=`Detection Probability`) )+xlab("isoth")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp3Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp4Plot <- ggplot(', input$var_group, '_RFp4Total, aes(x=mdr, y=`Detection Probability`) )+xlab("mdr")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp4Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp5Plot <- ggplot(', input$var_group, '_RFp5Total, aes(x=mtcm, y=`Detection Probability`) )+xlab("mtcm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp5Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp6Plot <- ggplot(', input$var_group, '_RFp6Total, aes(x=mtcq, y=`Detection Probability`) )+xlab("mtcq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp6Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp7Plot <- ggplot(', input$var_group, '_RFp7Total, aes(x=mtdq, y=`Detection Probability`) )+xlab("mtdq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp7Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp8Plot <- ggplot(', input$var_group, '_RFp8Total, aes(x=mtwarmq, y=`Detection Probability`) )+xlab("mtwarmq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp8Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp9Plot <- ggplot(', input$var_group, '_RFp9Total, aes(x=mtwetq, y=`Detection Probability`) )+xlab("mtwetq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp9Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp10Plot <- ggplot(', input$var_group, '_RFp10Total, aes(x=mtwm, y=`Detection Probability`) )+xlab("mtwm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp10Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp11Plot <- ggplot(', input$var_group, '_RFp11Total, aes(x=pcq, y=`Detection Probability`) )+xlab("pcq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp11Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp12Plot <- ggplot(', input$var_group, '_RFp12Total, aes(x=pdm, y=`Detection Probability`) )+xlab("pdm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp12Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp13Plot <- ggplot(', input$var_group, '_RFp13Total, aes(x=pdq, y=`Detection Probability`) )+xlab("pdq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp13Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp14Plot <- ggplot(', input$var_group, '_RFp14Total, aes(x=ps, y=`Detection Probability`) )+xlab("ps")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp14Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp15Plot <- ggplot(', input$var_group, '_RFp15Total, aes(x=pwarmq, y=`Detection Probability`) )+xlab("pwarmq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp15Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp16Plot <- ggplot(', input$var_group, '_RFp16Total, aes(x=pwetq, y=`Detection Probability`) )+xlab("pwetq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp16Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp17Plot <- ggplot(', input$var_group, '_RFp17Total, aes(x=pwm, y=`Detection Probability`) )+xlab("pwm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp17Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp18Plot <- ggplot(', input$var_group, '_RFp18Total, aes(x=tar, y=`Detection Probability`) )+xlab("tar")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp18Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    ', input$var_group, '_RFp19Plot <- ggplot(', input$var_group, '_RFp19Total, aes(x=ts, y=`Detection Probability`) )+xlab("ts")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Count`),method="auto",color="violet",fill="red",n=0.1*nrow(', input$var_group, '_RFp19Total))+theme_bw(base_size=25)+ theme(text=element_text(size=30))
    
    ', input$var_group, '_RFplots <- plot_grid(', input$var_group, '_RFp1Plot,', input$var_group, '_RFp2Plot,', input$var_group, '_RFp3Plot,', input$var_group, '_RFp4Plot,', input$var_group, '_RFp5Plot,', input$var_group, '_RFp6Plot,
                         ', input$var_group, '_RFp7Plot,', input$var_group, '_RFp8Plot,', input$var_group, '_RFp9Plot,', input$var_group, '_RFp10Plot,', input$var_group, '_RFp11Plot,', input$var_group, '_RFp12Plot,
                         ', input$var_group, '_RFp13Plot,', input$var_group, '_RFp14Plot,', input$var_group, '_RFp15Plot,', input$var_group, '_RFp16Plot,', input$var_group, '_RFp17Plot,', input$var_group, '_RFp18Plot,
                         ', input$var_group, '_RFp19Plot, ncol=1,labels="AUTO")
    
    output$', input$var_group, '_NonNative_partial_depend <- renderPlot(plot(', input$var_group, '_RFplots))
    
    
    
    
    #outputting model evaluation
    #Summarie Evaluations and save as an aggregated table
    tmpMean <- colMeans(', input$var_group, '_NonNative_randomF_evaluation_total)
    tmpSD <-apply(', input$var_group, '_NonNative_randomF_evaluation_total,2,sd)
    ', input$var_group, '_NonNative_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
    
    output$', input$var_group, '_NonNative_eval <- renderTable(', input$var_group, '_NonNative_randomF_evaluation_total_summarized)
    
    ',sep='')))
    
  })
  # display_click <- reactive({input$sel_map_click})
  # display_sel_lat <- display_click$lat
  # display_sel_long <- display_click$lng
  # 
  
  # output$selected_point_lat <- renderPrint({
  #   display_sel_lat})
  # output$selected_point_long <- renderPrint({
  #   display_sel_long})



  # observeEvent(input$calc,{
  #   click <- input$sel_map_click
  #   sel_lat <- click$lat
  #   sel_lng <- click$lng
  # })
  
  
  
  ##############Future Predictions###############
  
  
  observeEvent(input$future_predict,{
    
    #eval(parse(text = paste('',sep='')))
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Loading and Prossessing Future Bioclimate and Plant Data',
                 detail = "It won\'t take too long, hopefully...")



    template <- raster("D:/Application/data/amt.tif")
    CalEcoRegionsOutlines <- shapefile("D:/Application/data/CalEcoRegionsOutlines2.shp")
    CalEcoRegionsOutlines_WGS84 <- spTransform(CalEcoRegionsOutlines, crs(template))
    
    envi_filename <- c("amt",     "ap",      "isoth",   "mdr",     "mtcm",    "mtcq",    "mtdq",    "mtwarmq", "mtwetq",  "mtwm",    "pcq",     "pdm",     "pdq",    "ps",      "pwarmq",  "pwetq",   "pwm",     "tar",     "ts")
    
    
    
    
    #read in which group to assess and if all scenarios are selected
    group <- input$group
    SSP <- input$SSP
    time <- input$time
    #Convert time to format used in code
    if(time == "2021-2040"){
      time <- "2140"
    }
    
    if(time == "2041-2060"){
      time <- "4160"
    }
    
    if(time == "2061-2080"){
      time <- "6180"
    }
    
    if(time == "2081-2100"){
      time <- "8100"
    }
    
    
    if(input$SSP == "126"){
      SSP <- "126"
    }
    
    if(input$SSP == "245"){
      SSP <- "245"
    }
    
    
    if(input$SSP == "370"){
      SSP <- "370"
    }
    
    
    if(input$SSP == "585"){
      SSP <- "585"
    }
    

    
    
    eval(parse(text = paste('
    
    #For individual situations (no \"All\" option selected)
    if(SSP != \"All\" & time != \'All\'){
      
      
      
      number_of_prediction_graphs_rows <- 1
      number_of_prediction_graphs_cols <- 1
      
      
      #Train model for predictions
      #Read in and process data
      ', group, '_NonNative_with_envi <- readRDS(\"D:/Application/data/', group, '_NonNative_with_envi.rds\")
      ', group, '_NonNative_background <- readRDS(\"D:/Application/data/', group, '_NonNative_background.rds\")

      colnames_', group, '_NonNative_with_envi <- colnames(', group, '_NonNative_with_envi)

      colnames_', group, '_NonNative_background <- colnames(', group, '_NonNative_background)
      
      

      envi_filename <- c(\"amt\",     \"ap\",      \"isoth\",   \"mdr\",     \"mtcm\",    \"mtcq\",    \"mtdq\",    \"mtwarmq\", \"mtwetq\",  \"mtwm\",    \"pcq\",     \"pdm\",     \"pdq\",    \"ps\",      \"pwarmq\",  \"pwetq\",   \"pwm\",     \"tar\",     \"ts\")
      
      Only_BioClim_Names_and_pa <- c(envi_filename,\"pa\")
      
      ', group, '_NonNative <- \"', group, '_NonNative\"
      ', group, '_NonNative_with_envi_filtered <- process_observations_with_envi(radius, center_lat, center_lng, ', group, '_NonNative)
      ', group, '_NonNative_background_filtered <- process_observations_background(radius, center_lat, center_lng, ', group, '_NonNative)

      
      
      
      #Load rasters
      SSP', SSP, '', time, '_envi_map_layers <- list.files(\"D:/Application/data/CNRM-CM6-1/ssp', SSP, '/SSP', SSP, '', time, '/.\", pattern=\'.tif\',full.names = TRUE)
      

      # other_data <- list.files(\"D:/Application/data/CNRM-CM6-1/ssp', SSP, '/otherdata\", pattern=".tif", full.names = TRUE)
      SSP', SSP, '', time, '_envi_layers_stack <- stack(c(SSP', SSP, '', time, '_envi_map_layers))#,other_data))
      
      
      
      
      
      
      
      progress$set(message = \'Now training model for prediction...\',
                   detail = "Should be quick!")
      
      #Training Model for Prediction (remember, model is trained based on current data! No need to imput future rasters, not yet)
      ', group, '_envi_train_predict <- return_envi_train(', group, '_NonNative_with_envi_filtered, ', group, '_NonNative_background_filtered)
      ', group, '_rf_regress_predict <- run_a_random_forest_model(', group, '_envi_train_predict)
      

      for (i in 1:nrow(', group, '_envi_train_predict)){
        if(', group, '_envi_train_predict$pa[i] < 1){
          ', group, '_div_pt_predict <- i
          break
        }
      }
      
      
      ', group, '_testpres_predict <- ', group, '_envi_train_predict[1:(', group, '_div_pt_predict-1),]
      ', group, '_testbackgr_predict <- ', group, '_envi_train_predict[', group, '_div_pt_predict:nrow(', group, '_envi_train_predict),]
      
      
      erf_', group, '_predict <- suppressWarnings(evaluate(', group, '_testpres_predict,', group, '_testbackgr_predict, ', group, '_rf_regress_predict))
      
      

      
      #Predict Observation likelihood & P/A
      
      progress$set(message = \'Graphing predictions right now...\',
                   detail = "This entire process may repeat several times!")
      SSP', SSP, '', time, '_', group, '_NonNative_future_prediction <- predict(SSP', SSP, '', time, '_envi_layers_stack,', group, '_rf_regress_predict)

      
      
      
      
      ###Maybe the key to not repeating loops is this! Do NOT do functions INSIDE the render__({}) syntax! Just store what you
      ###want to render OUTSIDE the render function so that it can be output very simply without extra calculations
      
      # output$', group, '_NonNative_Observation_Likelihood <- renderPlot({
      #   # png("', group, ' NonNative SSP ', SSP, ' ', input$time, ' Prediction.png")
      #   plot(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction, main=\'', group, ' NonNative SSP ', SSP, ' ', input$time, ' Prediction\', col = viridis(100))
      #   plot(CalEcoRegionsOutlines_WGS84, add=TRUE, border=\'dark grey\')
      #   # dev.off()
      # 
      # })

      # output$', group, '_NonNative_Observation_Likelihood <- renderPlot(
      #   # png("', group, ' NonNative SSP ', SSP, ' ', input$time, ' Prediction.png")
      #   plot(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction, main=\'', group, ' NonNative SSP ', SSP, ' ', input$time, ' Prediction\', col = viridis(100), 
      #        xlim = c((center_lng - radius), (center_lng + radius)), ylim= c((center_lat - radius), (center_lat + radius)))
      #   
      # )
      
      
      

      pr <- SSP', SSP, '', time, '_', group, '_NonNative_future_prediction
      tr <- threshold(erf_', group, '_predict,"spec_sens")
      

      
      #Quantification with cellstats
      
      tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA <- SSP', SSP, '', time, '_', group, '_NonNative_future_prediction > tr
      
      # SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_cropped <- crop(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      template <- crop(template, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      
      SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      
      output$', group, '_NonNative_Observation_Likelihood <- renderPlot(plot(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP ', SSP, ' ', input$time, ' Prediction\', col = viridis(100)))
      
      
      
      # SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA_cropped <- crop(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      
      SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      
      output$PA <- renderPlot(plot(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP ', SSP, ' ', input$time, ' P/A\'))
        
        
      PA_pixel_sums <- data.frame()


      PA_pixel_sums <-  cbind("SSP', SSP, '', time, '_', group, '_NonNative", cellStats(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                              , cellStats(SSP', SSP, '', time, '_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      colnames(PA_pixel_sums) <- c("Scenario", "Mean Observation Likelihood", "Mean of Predicted Presence")


      output$OL_Quantification <- renderTable(PA_pixel_sums)
      
      
      progress$set(message = \'Done!\',
                   detail = "")
    }
    
    
    ##Give a picture of the actual streetmap next to the output of maps? or not necessary?
    ##Think not... the map (with the box) is already built above--can be referenced to at any time
    
    
   
    
    
    
    
    #For "ALL" selected in time
    if(SSP != "All" & time == \'All\'){
      
      number_of_prediction_graphs_rows <- 2.5
      number_of_prediction_graphs_cols <- 1
      #Train model for predictions
      #Read in and process data
      ', group, '_NonNative_with_envi <- readRDS("D:/Application/data/', group, '_NonNative_with_envi.rds")
      ', group, '_NonNative_background <- readRDS("D:/Application/data/', group, '_NonNative_background.rds")
      
      colnames_', group, '_NonNative_with_envi <- colnames(', group, '_NonNative_with_envi)
      
      colnames_', group, '_NonNative_background <- colnames(', group, '_NonNative_background)
      
      
      
      envi_filename <- c("amt",     "ap",      "isoth",   "mdr",     "mtcm",    "mtcq",    "mtdq",    "mtwarmq", "mtwetq",  "mtwm",    "pcq",     "pdm",     "pdq",    "ps",      "pwarmq",  "pwetq",   "pwm",     "tar",     "ts")
      
      Only_BioClim_Names_and_pa <- c(envi_filename,"pa")
      
      #-----------------------------------------------------
      # progress$set(message = \'Finished loading data!\',
      #              detail = \'Now matching plant observations to selected region...\')
      
      ', group, '_NonNative <- "', group, '_NonNative"
      ', group, '_NonNative_with_envi_filtered <- process_observations_with_envi(radius, center_lat, center_lng, ', group, '_NonNative)
      ', group, '_NonNative_background_filtered <- process_observations_background(radius, center_lat, center_lng, ', group, '_NonNative)
     
      progress$set(message = \'Now training model for prediction...\',
                   detail = "Should be quick!")
      
      #Training Model for Prediction (remember, model is trained based on current data! No need to imput future rasters, not yet)
      ', group, '_envi_train_predict <- return_envi_train(', group, '_NonNative_with_envi_filtered, ', group, '_NonNative_background_filtered)
      ', group, '_rf_regress_predict <- run_a_random_forest_model(', group, '_envi_train_predict)
      
      for (i in 1:nrow(', group, '_envi_train_predict)){
        if(', group, '_envi_train_predict$pa[i] < 1){
          ', group, '_div_pt_predict <- i
          break
        }
      }
      
      
      ', group, '_testpres_predict <- ', group, '_envi_train_predict[1:(', group, '_div_pt_predict-1),]
      ', group, '_testbackgr_predict <- ', group, '_envi_train_predict[', group, '_div_pt_predict:nrow(', group, '_envi_train_predict),]
      
      
      erf_', group, '_predict <- suppressWarnings(evaluate(', group, '_testpres_predict,', group, '_testbackgr_predict, ', group, '_rf_regress_predict))
      
      
      template <- crop(template, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      PA_pixel_sums <- data.frame()
      
      
      
      
      
      
      
      #2140
      progress$set(message = \'Making predictions for 2021-2040 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP', SSP, '2140_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp', SSP, '/SSP', SSP, '2140/.", pattern=\'.tif\',full.names = TRUE)
      SSP', SSP, '2140_envi_layers_stack <- stack(c(SSP', SSP, '2140_envi_map_layers))#,other_data))
      
      
      SSP', SSP, '2140_', group, '_NonNative_future_prediction <- predict(SSP', SSP, '2140_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP', SSP, '2140_pr <- SSP', SSP, '2140_', group, '_NonNative_future_prediction
      SSP', SSP, '2140_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA <- SSP', SSP, '2140_', group, '_NonNative_future_prediction > SSP', SSP, '2140_tr
      

      
      
      SSP', SSP, '2140_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP', SSP, '2140_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP', SSP, '2140_', group, '_NonNative", cellStats(SSP', SSP, '2140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                              , cellStats(SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #4160
      progress$set(message = \'Making predictions for 2041-2060 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP', SSP, '4160_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp', SSP, '/SSP', SSP, '4160/.", pattern=\'.tif\',full.names = TRUE)
      SSP', SSP, '4160_envi_layers_stack <- stack(c(SSP', SSP, '4160_envi_map_layers))#,other_data))
      
      
      SSP', SSP, '4160_', group, '_NonNative_future_prediction <- predict(SSP', SSP, '4160_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP', SSP, '4160_pr <- SSP', SSP, '4160_', group, '_NonNative_future_prediction
      SSP', SSP, '4160_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP', SSP, '4160_', group, '_NonNative_future_prediction_PA <- SSP', SSP, '4160_', group, '_NonNative_future_prediction > SSP', SSP, '4160_tr
      
      
      
      
      SSP', SSP, '4160_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP', SSP, '4160_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP', SSP, '4160_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP', SSP, '4160_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP', SSP, '4160_', group, '_NonNative", cellStats(SSP', SSP, '4160_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP', SSP, '4160_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #6180
      progress$set(message = \'Making predictions for 2061-2080 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP', SSP, '6180_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp', SSP, '/SSP', SSP, '6180/.", pattern=\'.tif\',full.names = TRUE)
      SSP', SSP, '6180_envi_layers_stack <- stack(c(SSP', SSP, '6180_envi_map_layers))#,other_data))
      
      
      SSP', SSP, '6180_', group, '_NonNative_future_prediction <- predict(SSP', SSP, '6180_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP', SSP, '6180_pr <- SSP', SSP, '6180_', group, '_NonNative_future_prediction
      SSP', SSP, '6180_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP', SSP, '6180_', group, '_NonNative_future_prediction_PA <- SSP', SSP, '6180_', group, '_NonNative_future_prediction > SSP', SSP, '6180_tr
      
      
      
      
      SSP', SSP, '6180_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP', SSP, '6180_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP', SSP, '6180_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP', SSP, '6180_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP', SSP, '6180_', group, '_NonNative", cellStats(SSP', SSP, '6180_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP', SSP, '6180_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      #8100
      progress$set(message = \'Making predictions for 2081-2100 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP', SSP, '8100_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp', SSP, '/SSP', SSP, '8100/.", pattern=\'.tif\',full.names = TRUE)
      SSP', SSP, '8100_envi_layers_stack <- stack(c(SSP', SSP, '8100_envi_map_layers))#,other_data))
      
      
      SSP', SSP, '8100_', group, '_NonNative_future_prediction <- predict(SSP', SSP, '8100_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP', SSP, '8100_pr <- SSP', SSP, '8100_', group, '_NonNative_future_prediction
      SSP', SSP, '8100_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP', SSP, '8100_', group, '_NonNative_future_prediction_PA <- SSP', SSP, '8100_', group, '_NonNative_future_prediction > SSP', SSP, '8100_tr
      
      
      
      
      SSP', SSP, '8100_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP', SSP, '8100_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP', SSP, '8100_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP', SSP, '8100_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP', SSP, '8100_', group, '_NonNative", cellStats(SSP', SSP, '8100_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP', SSP, '8100_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      
      
      
      
      
      output$', group, '_NonNative_Observation_Likelihood <- renderPlot({par(mfrow=c(4,1))
        plot(SSP', SSP, '2140_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2021-2040 Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP', SSP, '4160_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2041-2060 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP', SSP, '6180_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2061-2080 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP', SSP, '8100_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2081-2100 Prediction\', width = 110, height = 150,col = viridis(100))
      })
      
      
      
      # SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA_cropped <- crop(SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      
      
      
      output$PA <- renderPlot({
        par(mfrow=c(4,1))
        plot(SSP', SSP, '2140_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2021-2040 P/A\', width = 110, height = 150,)
        plot(SSP', SSP, '4160_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2041-2060 P/A\', width = 110, height = 150,)
        plot(SSP', SSP, '6180_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2061-2080 P/A\', width = 110, height = 150,)
        plot(SSP', SSP, '8100_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP ', SSP, ' 2081-2100 P/A\', width = 110, height = 150,)
        
        })
      
      

      
      output$OL_Quantification <- renderTable(PA_pixel_sums)
      
      
      progress$set(message = \'Done!\',
                   detail = "")
      
      
      
    }
    
    
    
    
    
    
    
    #For "ALL" selected in SSP
    if(SSP == "All" & time != \'All\'){
      
      number_of_prediction_graphs_rows <- 2.5
      number_of_prediction_graphs_cols <- 1
      
      #Train model for predictions
      #Read in and process data
      ', group, '_NonNative_with_envi <- readRDS("D:/Application/data/', group, '_NonNative_with_envi.rds")
      ', group, '_NonNative_background <- readRDS("D:/Application/data/', group, '_NonNative_background.rds")
      
      colnames_', group, '_NonNative_with_envi <- colnames(', group, '_NonNative_with_envi)
      
      colnames_', group, '_NonNative_background <- colnames(', group, '_NonNative_background)
      
      
      
      envi_filename <- c("amt",     "ap",      "isoth",   "mdr",     "mtcm",    "mtcq",    "mtdq",    "mtwarmq", "mtwetq",  "mtwm",    "pcq",     "pdm",     "pdq",    "ps",      "pwarmq",  "pwetq",   "pwm",     "tar",     "ts")
      
      Only_BioClim_Names_and_pa <- c(envi_filename,"pa")
      
      #-----------------------------------------------------
      # progress$set(message = \'Finished loading data!\',
      #              detail = \'Now matching plant observations to selected region...\')
      
      ', group, '_NonNative <- "', group, '_NonNative"
      ', group, '_NonNative_with_envi_filtered <- process_observations_with_envi(radius, center_lat, center_lng, ', group, '_NonNative)
      ', group, '_NonNative_background_filtered <- process_observations_background(radius, center_lat, center_lng, ', group, '_NonNative)
      
      progress$set(message = \'Now training model for prediction...\',
                   detail = "Should be quick!")
      
      #Training Model for Prediction (remember, model is trained based on current data! No need to imput future rasters, not yet)
      ', group, '_envi_train_predict <- return_envi_train(', group, '_NonNative_with_envi_filtered, ', group, '_NonNative_background_filtered)
      ', group, '_rf_regress_predict <- run_a_random_forest_model(', group, '_envi_train_predict)
      
      for (i in 1:nrow(', group, '_envi_train_predict)){
        if(', group, '_envi_train_predict$pa[i] < 1){
          ', group, '_div_pt_predict <- i
          break
        }
      }
      
      
      ', group, '_testpres_predict <- ', group, '_envi_train_predict[1:(', group, '_div_pt_predict-1),]
      ', group, '_testbackgr_predict <- ', group, '_envi_train_predict[', group, '_div_pt_predict:nrow(', group, '_envi_train_predict),]
      
      
      erf_', group, '_predict <- suppressWarnings(evaluate(', group, '_testpres_predict,', group, '_testbackgr_predict, ', group, '_rf_regress_predict))
      
      
      template <- crop(template, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      PA_pixel_sums <- data.frame()
      
      
      
      
      
      
      
      #126
      progress$set(message = \'Making predictions for SSP126 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP126', time, '_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp126/SSP126', time, '/.", pattern=\'.tif\',full.names = TRUE)
      SSP126', time, '_envi_layers_stack <- stack(c(SSP126', time, '_envi_map_layers))#,other_data))
      
      
      SSP126', time, '_', group, '_NonNative_future_prediction <- predict(SSP126', time, '_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP126', time, '_pr <- SSP126', time, '_', group, '_NonNative_future_prediction
      SSP126', time, '_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP126', time, '_', group, '_NonNative_future_prediction_PA <- SSP126', time, '_', group, '_NonNative_future_prediction > SSP126', time, '_tr
      
      
      
      
      SSP126', time, '_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP126', time, '_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP126', time, '_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP126', time, '_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP126', time, '_', group, '_NonNative", cellStats(SSP126', time, '_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP126', time, '_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      

      
      #245
      progress$set(message = \'Making predictions for SSP245 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP245', time, '_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp245/SSP245', time, '/.", pattern=\'.tif\',full.names = TRUE)
      SSP245', time, '_envi_layers_stack <- stack(c(SSP245', time, '_envi_map_layers))#,other_data))
      
      
      SSP245', time, '_', group, '_NonNative_future_prediction <- predict(SSP245', time, '_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP245', time, '_pr <- SSP245', time, '_', group, '_NonNative_future_prediction
      SSP245', time, '_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP245', time, '_', group, '_NonNative_future_prediction_PA <- SSP245', time, '_', group, '_NonNative_future_prediction > SSP245', time, '_tr
      
      
      
      
      SSP245', time, '_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP245', time, '_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP245', time, '_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP245', time, '_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP245', time, '_', group, '_NonNative", cellStats(SSP245', time, '_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP245', time, '_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      #370
      progress$set(message = \'Making predictions for SSP370 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP370', time, '_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp370/SSP370', time, '/.", pattern=\'.tif\',full.names = TRUE)
      SSP370', time, '_envi_layers_stack <- stack(c(SSP370', time, '_envi_map_layers))#,other_data))
      
      
      SSP370', time, '_', group, '_NonNative_future_prediction <- predict(SSP370', time, '_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP370', time, '_pr <- SSP370', time, '_', group, '_NonNative_future_prediction
      SSP370', time, '_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP370', time, '_', group, '_NonNative_future_prediction_PA <- SSP370', time, '_', group, '_NonNative_future_prediction > SSP370', time, '_tr
      
      
      
      
      SSP370', time, '_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP370', time, '_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP370', time, '_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP370', time, '_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP370', time, '_', group, '_NonNative", cellStats(SSP370', time, '_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP370', time, '_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #585
      progress$set(message = \'Making predictions for SSP585 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP585', time, '_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp585/SSP585', time, '/.", pattern=\'.tif\',full.names = TRUE)
      SSP585', time, '_envi_layers_stack <- stack(c(SSP585', time, '_envi_map_layers))#,other_data))
      
      
      SSP585', time, '_', group, '_NonNative_future_prediction <- predict(SSP585', time, '_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP585', time, '_pr <- SSP585', time, '_', group, '_NonNative_future_prediction
      SSP585', time, '_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP585', time, '_', group, '_NonNative_future_prediction_PA <- SSP585', time, '_', group, '_NonNative_future_prediction > SSP585', time, '_tr
      
      
      
      
      SSP585', time, '_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP585', time, '_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP585', time, '_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP585', time, '_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP585', time, '_', group, '_NonNative", cellStats(SSP585', time, '_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP585', time, '_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      
      
      
      
      
      output$', group, '_NonNative_Observation_Likelihood <- renderPlot({par(mfrow=c(4,1))
        plot(SSP126', time, '_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 126 ', input$time, ' Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP245', time, '_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 245 ', input$time, ' Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP370', time, '_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 370 ', input$time, ' Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP585', time, '_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 585 ', input$time, ' Prediction\', width = 110, height = 150, col = viridis(100))
        
        })
      
      
      
      # SSP126', time, '_', group, '_NonNative_future_prediction_PA_cropped <- crop(SSP126', time, '_', group, '_NonNative_future_prediction_PA, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      
      
      
      output$PA <- renderPlot({
        par(mfrow=c(4,1))
        plot(SSP126', time, '_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 126 ', input$time, ' P/A\', width = 110, height = 150,)
        plot(SSP245', time, '_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 245 ', input$time, ' P/A\', width = 110, height = 150,)
        plot(SSP370', time, '_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 370 ', input$time, ' P/A\', width = 110, height = 150,)
        plot(SSP585', time, '_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 585 ', input$time, ' P/A\', width = 110, height = 150,)
        
      })
      
      

      
      output$OL_Quantification <- renderTable(PA_pixel_sums)
      
      
      progress$set(message = \'Done!\',
                   detail = "")
      
      
    }
    
    
    
    
    

    if(SSP == "All" & time == \'All\'){
      
      number_of_prediction_graphs_rows <- 2.5
      number_of_prediction_graphs_cols <- 2.5
      
      #Train model for predictions
      #Read in and process data
      ', group, '_NonNative_with_envi <- readRDS("D:/Application/data/', group, '_NonNative_with_envi.rds")
      ', group, '_NonNative_background <- readRDS("D:/Application/data/', group, '_NonNative_background.rds")
      
      colnames_', group, '_NonNative_with_envi <- colnames(', group, '_NonNative_with_envi)
      
      colnames_', group, '_NonNative_background <- colnames(', group, '_NonNative_background)
      
      
      
      envi_filename <- c("amt",     "ap",      "isoth",   "mdr",     "mtcm",    "mtcq",    "mtdq",    "mtwarmq", "mtwetq",  "mtwm",    "pcq",     "pdm",     "pdq",    "ps",      "pwarmq",  "pwetq",   "pwm",     "tar",     "ts")
      
      Only_BioClim_Names_and_pa <- c(envi_filename,"pa")
      
      #-----------------------------------------------------
      # progress$set(message = \'Finished loading data!\',
      #              detail = \'Now matching plant observations to selected region...\')
      
      ', group, '_NonNative <- "', group, '_NonNative"
      ', group, '_NonNative_with_envi_filtered <- process_observations_with_envi(radius, center_lat, center_lng, ', group, '_NonNative)
      ', group, '_NonNative_background_filtered <- process_observations_background(radius, center_lat, center_lng, ', group, '_NonNative)
      
      progress$set(message = \'Now training model for prediction...\',
                   detail = "Should be quick!")
      
      #Training Model for Prediction (remember, model is trained based on current data! No need to imput future rasters, not yet)
      ', group, '_envi_train_predict <- return_envi_train(', group, '_NonNative_with_envi_filtered, ', group, '_NonNative_background_filtered)
      ', group, '_rf_regress_predict <- run_a_random_forest_model(', group, '_envi_train_predict)
      
      for (i in 1:nrow(', group, '_envi_train_predict)){
        if(', group, '_envi_train_predict$pa[i] < 1){
          ', group, '_div_pt_predict <- i
          break
        }
      }
      
      
      ', group, '_testpres_predict <- ', group, '_envi_train_predict[1:(', group, '_div_pt_predict-1),]
      ', group, '_testbackgr_predict <- ', group, '_envi_train_predict[', group, '_div_pt_predict:nrow(', group, '_envi_train_predict),]
      
      
      erf_', group, '_predict <- suppressWarnings(evaluate(', group, '_testpres_predict,', group, '_testbackgr_predict, ', group, '_rf_regress_predict))
      
      
      template <- crop(template, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      PA_pixel_sums <- data.frame()
      
      
      
      
      #SSP126
      #2140
      progress$set(message = \'Making predictions for SSP126 2021-2040 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP1262140_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp126/SSP1262140/.", pattern=\'.tif\',full.names = TRUE)
      SSP1262140_envi_layers_stack <- stack(c(SSP1262140_envi_map_layers))#,other_data))
      
      
      SSP1262140_', group, '_NonNative_future_prediction <- predict(SSP1262140_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP1262140_pr <- SSP1262140_', group, '_NonNative_future_prediction
      SSP1262140_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP1262140_', group, '_NonNative_future_prediction_PA <- SSP1262140_', group, '_NonNative_future_prediction > SSP1262140_tr
      
      
      
      
      SSP1262140_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP1262140_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP1262140_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP1262140_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP1262140_', group, '_NonNative", cellStats(SSP1262140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP1262140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #4160
      progress$set(message = \'Making predictions for SSP126 2041-2060 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP1264160_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp126/SSP1264160/.", pattern=\'.tif\',full.names = TRUE)
      SSP1264160_envi_layers_stack <- stack(c(SSP1264160_envi_map_layers))#,other_data))
      
      
      SSP1264160_', group, '_NonNative_future_prediction <- predict(SSP1264160_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP1264160_pr <- SSP1264160_', group, '_NonNative_future_prediction
      SSP1264160_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP1264160_', group, '_NonNative_future_prediction_PA <- SSP1264160_', group, '_NonNative_future_prediction > SSP1264160_tr
      
      
      
      
      SSP1264160_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP1264160_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP1264160_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP1264160_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP1264160_', group, '_NonNative", cellStats(SSP1264160_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP1264160_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #6180
      progress$set(message = \'Making predictions for SSP126 2061-2080 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP1266180_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp126/SSP1266180/.", pattern=\'.tif\',full.names = TRUE)
      SSP1266180_envi_layers_stack <- stack(c(SSP1266180_envi_map_layers))#,other_data))
      
      
      SSP1266180_', group, '_NonNative_future_prediction <- predict(SSP1266180_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP1266180_pr <- SSP1266180_', group, '_NonNative_future_prediction
      SSP1266180_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP1266180_', group, '_NonNative_future_prediction_PA <- SSP1266180_', group, '_NonNative_future_prediction > SSP1266180_tr
      
      
      
      
      SSP1266180_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP1266180_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP1266180_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP1266180_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP1266180_', group, '_NonNative", cellStats(SSP1266180_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP1266180_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      #8100
      progress$set(message = \'Making predictions for SSP126 2081-2100 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP1268100_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp126/SSP1268100/.", pattern=\'.tif\',full.names = TRUE)
      SSP1268100_envi_layers_stack <- stack(c(SSP1268100_envi_map_layers))#,other_data))
      
      
      SSP1268100_', group, '_NonNative_future_prediction <- predict(SSP1268100_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP1268100_pr <- SSP1268100_', group, '_NonNative_future_prediction
      SSP1268100_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP1268100_', group, '_NonNative_future_prediction_PA <- SSP1268100_', group, '_NonNative_future_prediction > SSP1268100_tr
      
      
      
      
      SSP1268100_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP1268100_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP1268100_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP1268100_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP1268100_', group, '_NonNative", cellStats(SSP1268100_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP1268100_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      

      
      
      
      
      #SSP245
      #2140
      progress$set(message = \'Making predictions for SSP245 2021-2040 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP2452140_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp245/SSP2452140/.", pattern=\'.tif\',full.names = TRUE)
      SSP2452140_envi_layers_stack <- stack(c(SSP2452140_envi_map_layers))#,other_data))
      
      
      SSP2452140_', group, '_NonNative_future_prediction <- predict(SSP2452140_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP2452140_pr <- SSP2452140_', group, '_NonNative_future_prediction
      SSP2452140_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP2452140_', group, '_NonNative_future_prediction_PA <- SSP2452140_', group, '_NonNative_future_prediction > SSP2452140_tr
      
      
      
      
      SSP2452140_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP2452140_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP2452140_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP2452140_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP2452140_', group, '_NonNative", cellStats(SSP2452140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP2452140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #4160
      progress$set(message = \'Making predictions for SSP245 2041-2060 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP2454160_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp245/SSP2454160/.", pattern=\'.tif\',full.names = TRUE)
      SSP2454160_envi_layers_stack <- stack(c(SSP2454160_envi_map_layers))#,other_data))
      
      
      SSP2454160_', group, '_NonNative_future_prediction <- predict(SSP2454160_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP2454160_pr <- SSP2454160_', group, '_NonNative_future_prediction
      SSP2454160_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP2454160_', group, '_NonNative_future_prediction_PA <- SSP2454160_', group, '_NonNative_future_prediction > SSP2454160_tr
      
      
      
      
      SSP2454160_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP2454160_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP2454160_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP2454160_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP2454160_', group, '_NonNative", cellStats(SSP2454160_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP2454160_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #6180
      progress$set(message = \'Making predictions for SSP245 2061-2080 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP2456180_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp245/SSP2456180/.", pattern=\'.tif\',full.names = TRUE)
      SSP2456180_envi_layers_stack <- stack(c(SSP2456180_envi_map_layers))#,other_data))
      
      
      SSP2456180_', group, '_NonNative_future_prediction <- predict(SSP2456180_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP2456180_pr <- SSP2456180_', group, '_NonNative_future_prediction
      SSP2456180_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP2456180_', group, '_NonNative_future_prediction_PA <- SSP2456180_', group, '_NonNative_future_prediction > SSP2456180_tr
      
      
      
      
      SSP2456180_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP2456180_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP2456180_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP2456180_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP2456180_', group, '_NonNative", cellStats(SSP2456180_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP2456180_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      #8100
      progress$set(message = \'Making predictions for SSP245 2081-2100 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP2458100_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp245/SSP2458100/.", pattern=\'.tif\',full.names = TRUE)
      SSP2458100_envi_layers_stack <- stack(c(SSP2458100_envi_map_layers))#,other_data))
      
      
      SSP2458100_', group, '_NonNative_future_prediction <- predict(SSP2458100_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP2458100_pr <- SSP2458100_', group, '_NonNative_future_prediction
      SSP2458100_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP2458100_', group, '_NonNative_future_prediction_PA <- SSP2458100_', group, '_NonNative_future_prediction > SSP2458100_tr
      
      
      
      
      SSP2458100_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP2458100_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP2458100_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP2458100_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP2458100_', group, '_NonNative", cellStats(SSP2458100_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP2458100_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      
      
      
      #SSP370
      #2140
      progress$set(message = \'Making predictions for SSP370 2021-2040 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP3702140_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp370/SSP3702140/.", pattern=\'.tif\',full.names = TRUE)
      SSP3702140_envi_layers_stack <- stack(c(SSP3702140_envi_map_layers))#,other_data))
      
      
      SSP3702140_', group, '_NonNative_future_prediction <- predict(SSP3702140_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP3702140_pr <- SSP3702140_', group, '_NonNative_future_prediction
      SSP3702140_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP3702140_', group, '_NonNative_future_prediction_PA <- SSP3702140_', group, '_NonNative_future_prediction > SSP3702140_tr
      
      
      
      
      SSP3702140_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP3702140_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP3702140_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP3702140_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP3702140_', group, '_NonNative", cellStats(SSP3702140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP3702140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #4160
      progress$set(message = \'Making predictions for SSP370 2041-2060 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP3704160_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp370/SSP3704160/.", pattern=\'.tif\',full.names = TRUE)
      SSP3704160_envi_layers_stack <- stack(c(SSP3704160_envi_map_layers))#,other_data))
      
      
      SSP3704160_', group, '_NonNative_future_prediction <- predict(SSP3704160_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP3704160_pr <- SSP3704160_', group, '_NonNative_future_prediction
      SSP3704160_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP3704160_', group, '_NonNative_future_prediction_PA <- SSP3704160_', group, '_NonNative_future_prediction > SSP3704160_tr
      
      
      
      
      SSP3704160_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP3704160_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP3704160_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP3704160_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP3704160_', group, '_NonNative", cellStats(SSP3704160_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP3704160_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #6180
      progress$set(message = \'Making predictions for SSP370 2061-2080 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP3706180_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp370/SSP3706180/.", pattern=\'.tif\',full.names = TRUE)
      SSP3706180_envi_layers_stack <- stack(c(SSP3706180_envi_map_layers))#,other_data))
      
      
      SSP3706180_', group, '_NonNative_future_prediction <- predict(SSP3706180_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP3706180_pr <- SSP3706180_', group, '_NonNative_future_prediction
      SSP3706180_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP3706180_', group, '_NonNative_future_prediction_PA <- SSP3706180_', group, '_NonNative_future_prediction > SSP3706180_tr
      
      
      
      
      SSP3706180_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP3706180_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP3706180_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP3706180_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP3706180_', group, '_NonNative", cellStats(SSP3706180_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP3706180_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      #8100
      progress$set(message = \'Making predictions for SSP370 2081-2100 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP3708100_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp370/SSP3708100/.", pattern=\'.tif\',full.names = TRUE)
      SSP3708100_envi_layers_stack <- stack(c(SSP3708100_envi_map_layers))#,other_data))
      
      
      SSP3708100_', group, '_NonNative_future_prediction <- predict(SSP3708100_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP3708100_pr <- SSP3708100_', group, '_NonNative_future_prediction
      SSP3708100_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP3708100_', group, '_NonNative_future_prediction_PA <- SSP3708100_', group, '_NonNative_future_prediction > SSP3708100_tr
      
      
      
      
      SSP3708100_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP3708100_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP3708100_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP3708100_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP3708100_', group, '_NonNative", cellStats(SSP3708100_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP3708100_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      
      #SSP585
      #2140
      progress$set(message = \'Making predictions for SSP585 2021-2040 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP5852140_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp585/SSP5852140/.", pattern=\'.tif\',full.names = TRUE)
      SSP5852140_envi_layers_stack <- stack(c(SSP5852140_envi_map_layers))#,other_data))
      
      
      SSP5852140_', group, '_NonNative_future_prediction <- predict(SSP5852140_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP5852140_pr <- SSP5852140_', group, '_NonNative_future_prediction
      SSP5852140_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP5852140_', group, '_NonNative_future_prediction_PA <- SSP5852140_', group, '_NonNative_future_prediction > SSP5852140_tr
      
      
      
      
      SSP5852140_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP5852140_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP5852140_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP5852140_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP5852140_', group, '_NonNative", cellStats(SSP5852140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP5852140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #4160
      progress$set(message = \'Making predictions for SSP585 2041-2060 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP5854160_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp585/SSP5854160/.", pattern=\'.tif\',full.names = TRUE)
      SSP5854160_envi_layers_stack <- stack(c(SSP5854160_envi_map_layers))#,other_data))
      
      
      SSP5854160_', group, '_NonNative_future_prediction <- predict(SSP5854160_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP5854160_pr <- SSP5854160_', group, '_NonNative_future_prediction
      SSP5854160_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP5854160_', group, '_NonNative_future_prediction_PA <- SSP5854160_', group, '_NonNative_future_prediction > SSP5854160_tr
      
      
      
      
      SSP5854160_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP5854160_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP5854160_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP5854160_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP5854160_', group, '_NonNative", cellStats(SSP5854160_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP5854160_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      #6180
      progress$set(message = \'Making predictions for SSP585 2061-2080 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP5856180_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp585/SSP5856180/.", pattern=\'.tif\',full.names = TRUE)
      SSP5856180_envi_layers_stack <- stack(c(SSP5856180_envi_map_layers))#,other_data))
      
      
      SSP5856180_', group, '_NonNative_future_prediction <- predict(SSP5856180_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP5856180_pr <- SSP5856180_', group, '_NonNative_future_prediction
      SSP5856180_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP5856180_', group, '_NonNative_future_prediction_PA <- SSP5856180_', group, '_NonNative_future_prediction > SSP5856180_tr
      
      
      
      
      SSP5856180_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP5856180_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP5856180_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP5856180_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP5856180_', group, '_NonNative", cellStats(SSP5856180_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP5856180_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      #8100
      progress$set(message = \'Making predictions for SSP585 2081-2100 right now...\',
                   detail = "This entire process may repeat several times!")
      SSP5858100_envi_map_layers <- list.files("D:/CNRM-CM6-1/ssp585/SSP5858100/.", pattern=\'.tif\',full.names = TRUE)
      SSP5858100_envi_layers_stack <- stack(c(SSP5858100_envi_map_layers))#,other_data))
      
      
      SSP5858100_', group, '_NonNative_future_prediction <- predict(SSP5858100_envi_layers_stack,', group, '_rf_regress_predict)
      
      
      SSP5858100_pr <- SSP5858100_', group, '_NonNative_future_prediction
      SSP5858100_tr <- threshold(erf_', group, '_predict,"spec_sens")
      
      SSP5858100_', group, '_NonNative_future_prediction_PA <- SSP5858100_', group, '_NonNative_future_prediction > SSP5858100_tr
      
      
      
      
      SSP5858100_', group, '_NonNative_future_prediction_cropped <- projectRaster(SSP5858100_', group, '_NonNative_future_prediction, template ,method = \'bilinear\')
      SSP5858100_', group, '_NonNative_future_prediction_PA_cropped <- projectRaster(SSP5858100_', group, '_NonNative_future_prediction_PA, template ,method = \'bilinear\')
      
      PA_pixel_sums_this <-  cbind("SSP5858100_', group, '_NonNative", cellStats(SSP5858100_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
                                   , cellStats(SSP5858100_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      
      PA_pixel_sums <- rbind(PA_pixel_sums,PA_pixel_sums_this)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      output$', group, '_NonNative_Observation_Likelihood <- renderPlot({par(mfrow=c(4,4))
        plot(SSP1262140_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 126 2021-2040 Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP1264160_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 126 2041-2060 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP1266180_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 126 2061-2080 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP1268100_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 126 2081-2100 Prediction\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP2452140_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 245 2021-2040 Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP2454160_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 245 2041-2060 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP2456180_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 245 2061-2080 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP2458100_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 245 2081-2100 Prediction\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP3702140_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 370 2021-2040 Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP3704160_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 370 2041-2060 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP3706180_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 370 2061-2080 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP3708100_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 370 2081-2100 Prediction\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP5852140_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 585 2021-2040 Prediction\', width = 110, height = 150, col = viridis(100))
        plot(SSP5854160_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 585 2041-2060 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP5856180_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 585 2061-2080 Prediction\', width = 110, height = 150,col = viridis(100))
        plot(SSP5858100_', group, '_NonNative_future_prediction_cropped, main=\'', group, ' NonNative SSP 585 2081-2100 Prediction\', width = 110, height = 150,col = viridis(100))
        
      })
      
      
      
      # SSP1262140_', group, '_NonNative_future_prediction_PA_cropped <- crop(SSP1262140_', group, '_NonNative_future_prediction_PA, extent((center_lng - radius), (center_lng + radius),(center_lat - radius), (center_lat + radius)))
      
      
      
      output$PA <- renderPlot({
        par(mfrow=c(4,4))
        plot(SSP1262140_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 126 2021-2040 P/A\', width = 110, height = 150, col = viridis(100))
        plot(SSP1264160_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 126 2041-2060 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP1266180_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 126 2061-2080 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP1268100_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 126 2081-2100 P/A\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP2452140_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 245 2021-2040 P/A\', width = 110, height = 150, col = viridis(100))
        plot(SSP2454160_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 245 2041-2060 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP2456180_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 245 2061-2080 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP2458100_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 245 2081-2100 P/A\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP3702140_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 370 2021-2040 P/A\', width = 110, height = 150, col = viridis(100))
        plot(SSP3704160_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 370 2041-2060 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP3706180_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 370 2061-2080 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP3708100_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 370 2081-2100 P/A\', width = 110, height = 150,col = viridis(100))
        
        plot(SSP5852140_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 585 2021-2040 P/A\', width = 110, height = 150, col = viridis(100))
        plot(SSP5854160_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 585 2041-2060 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP5856180_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 585 2061-2080 P/A\', width = 110, height = 150,col = viridis(100))
        plot(SSP5858100_', group, '_NonNative_future_prediction_PA_cropped, main=\'', group, ' NonNative SSP 585 2081-2100 P/A\', width = 110, height = 150,col = viridis(100))
        
        
      })
      
      
      # PA_pixel_sums <- cbind("SSP1262140_', group, '_NonNative", cellStats(SSP1262140_', group, '_NonNative_future_prediction, stat=\'mean\', na.rm=TRUE)
      #                                         , cellStats(SSP1262140_', group, '_NonNative_future_prediction_PA, stat=\'sum\', na.rm=TRUE))
      # 
      
      # PA_pixel_sums <-  cbind("SSP1262140_', group, '_NonNative", cellStats(SSP1262140_', group, '_NonNative_future_prediction_cropped, stat=\'mean\', na.rm=TRUE)
      #                         , cellStats(SSP1262140_', group, '_NonNative_future_prediction_PA_cropped, stat=\'mean\', na.rm=TRUE))
      colnames(PA_pixel_sums) <- c("Scenario", "Mean Observation Likelihood", "Mean of Predicted Presence")
      # PA_pixel_sums <- rbind(PA_pixel_sums, c("SSP1262140_', group, '_NonNative", cellStats(SSP1262140_', group, '_NonNative_future_prediction, stat=\'mean\', na.rm=TRUE)
      #                                         , cellStats(SSP1262140_', group, '_NonNative_future_prediction_PA, stat=\'sum\', na.rm=TRUE)))
      
      output$OL_Quantification <- renderTable(PA_pixel_sums)
      
      
      progress$set(message = \'Done!\',
                   detail = "")
      
      
    }
    
    
    
    
    
    ',sep='')))
    
    
    
    
    
    
    
    
    
    
    
  
    
  })
  
  
  })
  
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)







##################Debugging and finetuning###################
# 1. the partial dependence graphs need a set boundary; or it will too cramped to be legible
# 2. Need to remove Frequency from VarImp--not using ModelSel;
# 3. Neet to have a pop-up explaining what each variable means (on each page...)
# 4. The accuracy are creeply high. is it because of too few models? (but randomForest() is the one for accuracy from the very beginning...)














