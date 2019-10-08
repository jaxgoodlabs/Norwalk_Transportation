#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())
library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(ggplot2)
library(sf)
library(scales)
library(leaflet.extras)

#create list of fixed routes
fixed_route_list<-list.files("fixed routes/")
#make the names of fixed route list human readable
names(fixed_route_list)<-gsub('_',' ',fixed_route_list)
#remove .kml from the end each name
names(fixed_route_list)<-gsub('\\.kml','',names(fixed_route_list))
#add "none" to fixed route list
fixed_route_list[["none"]]<-"none"

#create a list of parking assets
parking_list<-list.files("parking/",pattern="*shx")
#make the names of parking features human readable
names(parking_list)<-gsub('_',' ',parking_list)
#remove file extensions from names
names(parking_list)<-gsub('\\.shx','',names(parking_list))
#add "none" to the list of parking features
parking_list[["none"]]<-"none"
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Norwalk Transit Difficulty"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #radio buttons to choose destination
      radioButtons("destination",
                   "Destination",
                   c("South Norwalk Train Station"="South+Norwalk+Train+Station,+Norwalk,+CT",
                     "Wall Street Theatre / Wall West"="Wall+Street+Theatre,+Norwalk,+CT",
                     "East Norwalk Train Station"="East+Norwalk+Train+Station,+Norwalk,+CT",
                     "Mathew's Park"="Mathews+Park,+Norwalk,+CT")),
      #radio buttons to choose departure time
      radioButtons("departure_time",
                   "Departure Time",
                   c("Saturday, 11am"="1570892400",
                     "Wednesday, 8am"="1570622400"),
                   selected="1570622400"),
      radioButtons("transit_metric",
                   "Measure of Public Transit Difficulty",
                   c("Trip time (min)"="Total Transit Time",
                     "Number of transfers"="Number of Transfers",
                     "Total walking distance (mi.)"="Total Distance to/from transit stops")#,
                   #selected="Median_hh_income"),
      ),
      #check box for adding wheels2u boundery
      checkboxInput("wheels2u","Show Wheels2U service area"),
      #add selector for fixed route lines
      selectInput("fixed_route",
                  "Show a Bus/Shuttle Line",
                  choices=fixed_route_list,
                  selected="none"),
      #add selector for parking features
      selectInput("parking",
                  "Show Parking Feature",
                  choices=parking_list,
                  selected="none")
      
      
    ),
    
    # Show the map
    mainPanel("In the map below, the color of each dot indicates the difficulty of using public transit to get
              from that area to the selected destination. A red X indicates no public transit options are 
available within 1.5 hours of the chosen departure time that require less than 1 total mile of walking to and from stops.",
              leafletOutput("map"),
              "Click on a dot to view route information. Click between dots to reveal demographic information for the area. Estimates obtained using the Google Maps API"
    )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  #turn off scientific notation
  options(scipen=999)

  #load the norwalk demographic stat information
  load("norwalk_stats.rData")
  #load the transit difficulties information
  load("transit_difficulties.rData")
  
  #load wheels2u boundary from kml
  wheels2u_boundary<-readr::read_file("Wheels2U Boundary.kml")
  

  #delete row with no input in norwalk stats
  norwalk_stats<-norwalk_stats%>%filter(!is.na(Median_hh_income))%>%
    #add a column for the data in the popup
    mutate(pop_up=paste(sep="<br/>",
                        paste0("<b>GEOID:</b> ",GEOID),
                        paste0("<b>Pct Racial Minority: </b>",percent(`Pct Non-White`)),
                        paste0("<b>Pct 65 and older: </b>", percent(`Pct Over 65 y.o.`)),
                        paste0("<b>Pct Under 18: </b>", percent(`Pct Under 18 y.o.`)),
                        paste0("<b>Poverty Rate: </b>", percent(`Poverty Rate`)),
                        paste0("<b>Median Household Income: </b>", dollar(`Median_hh_income`))))
  #create two data frames with some template numbers

  #create a reactive object for destination choice
  chosen_destination<-reactive({
    paste0(input$destination)
  })
  #create a reactive object for the departure time choice
  chosen_departure_time<-reactive({
    paste0(input$departure_time)
  })
  #create a reactive object out of dropdown choice
  chosen_stat<-reactive({
    paste0(input$transit_metric)
  })
  #create a reactive object for wheels2u boundary
  wheels2u<-reactive({
    input$wheels2u
  })
  #create a reactive object for bus line choice
  fixed_route<-reactive({
    input$fixed_route
  })
  
  #create a reactive object for parking feature choice
  parking<-reactive({
    input$parking
  })
  #create frame to look up lat/lon for the chosen destination
  destination_layer<-reactive({
    lookup_frame<-data.frame(destinations=c("South+Norwalk+Train+Station,+Norwalk,+CT",
                                            "Wall+Street+Theatre,+Norwalk,+CT",
                                            "East+Norwalk+Train+Station,+Norwalk,+CT",
                                            "Mathews+Park,+Norwalk,+CT"),
                             lat=c(41.0953978,41.1169687,41.104,41.1087948),
                             lon=c(-73.424718,-73.4161961,-73.4067767,-73.4185732))
    lookup_frame[lookup_frame$destinations==chosen_destination(),]
  })
  #create react objects for available and unavailable routes based on radio button selections
  available<-reactive({
    transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["available"]]%>%
      #remove geometry column
      select(-geometry)%>%
      #since walking distance is in meters, make it miles
      mutate(`Total Distance to/from transit stops`=`Total Distance to/from transit stops`/1609.34)%>%
      #add a column for the data in the popup
      mutate(pop_up=paste(sep="<br/>",
                          paste0("<b>Total Transit Time:</b> ",
                                 round(`Total Transit Time`,1)),
                          paste0("<b>Number of Transfers: </b>",`Number of Transfers`),
                          paste0("<b>Total Distance to/from transit stops: </b>", 
                                 round(`Total Distance to/from transit stops`,1)),
                          paste0("<b>Transit Lines Used: </b>", `Transit Lines`)))%>%
      #limit only to stat selected
      dplyr::rename("chosen_stat"=chosen_stat())
  })
  unavailable<-reactive({
    transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["unavailable"]]%>%
      #remove geometry column
      select(-geometry)%>%
      select(-lat_long)%>%
      as.data.frame()
  })
  
  #create color palette for chosen series
  pal <-reactive({
      colorNumeric(palette = "RdYlGn", 
                        domain = available()$chosen_stat, n = nrow(available()),
                        reverse=TRUE)
    })
  

  
  output$map<-renderLeaflet({
    
    #st_transform(crs = "+init=epsg:4326") %>%
    leaflet(
      width = "50%")%>%
      setView(-73.4167485, 41.101619, 12) %>%
      addProviderTiles(provider = "Hydda.Full") 
#        addProviderTiles(provider="CartoDB.Positron")
  })
  
  #create an observer that adds a layer if the wheels2u checkbox is selected
  observe({
    if(wheels2u()){
      leafletProxy({"map"})%>%
        clearGroup("wheels2u")%>%
        addKML(kml=wheels2u_boundary,
                    stroke=FALSE,
                    opacity=.3,
                    color="blue",
                    group="wheels2u")
    }else{
      leafletProxy({"map"})%>%
        clearGroup("wheels2u")
    }
  })
  
  #create an observer that adds the selected fixed route line if one is selected
  observe({
    if(fixed_route()!="none"){
      #load the selected file
      chosen_fixed_route<-readr::read_file(paste0("fixed routes/",fixed_route()))
      leafletProxy({"map"})%>%
        clearGroup("fixed")%>%
        addKML(kml=chosen_fixed_route,
               weight=5,
               smoothFactor=0,
               color="#000000",
               fillOpacity=0.0,group="fixed")
    }else{
      leafletProxy({"map"})%>%
        clearGroup("fixed")
    }
  })

  #add the clear polygon layer to allow revealing demographic stats
  observe({
    leafletProxy("map")%>%
      clearGroup("poly")%>%
      addPolygons(data=norwalk_stats,popup = ~pop_up,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0,
                  #label group for this layer
                  group="poly")
  })
  #add the destination to the map
  observe({
    leafletProxy("map")%>%
      clearGroup("destination")%>%
      addMarkers(data=destination_layer(),
                 group="destination")
  })
  #create parking icon
  parkIcon <- makeIcon(
    iconUrl = "hiclipart.com-id_duloh.png",
    iconWidth = 15, iconHeight = 15#,
    # iconAnchorX = 22, iconAnchorY = 94,
  )
  
  #add parking layer to map based on selection
  observe({
    if(parking()!="none"){
      if(parking()%in%c("On_Street_Parking.shx","Surface_Lots.shx")){
        chosen_parking_feature<-read_sf(paste0("parking/",parking()))%>%
          st_transform(crs = "+init=epsg:4326")%>%
          mutate(pop_up=paste(sep="<br/>",
                              paste0("<b>Days Available:</b> ",OPERDAYS),
                              paste0("<b>Access Type: </b>",`ACCESSTYPE`),
                              paste0("<b>Maximum Duration: </b>",PARKDUR),
                              paste0("<b>Space Type: </b>", `SPACETYPE`)))
      }else{
        chosen_parking_feature<-read_sf(paste0("parking/",parking()))%>%
          st_transform(crs = "+init=epsg:4326")%>%
          mutate(pop_up=Creator)
      }

      leafletProxy("map")%>%
        clearGroup("parking")%>%
        addMarkers(data=chosen_parking_feature, popup=~pop_up,
                   icon=parkIcon,
                   group="parking")
    }else{
      leafletProxy("map")%>%
        clearGroup("parking")
    }

  })
  #create redx icon
  redX <- makeIcon(
    iconUrl = "hiclipart.com-id_xdgeh.png",
    iconWidth = 10, iconHeight = 10#,
    # iconAnchorX = 22, iconAnchorY = 94,
  )
  #add the layer for unavailable routes
  observe({
    leafletProxy("map")%>%
      clearGroup("unavail")%>%
      addMarkers(data=unavailable(),
                 icon=redX,
                 group="unavail")
  })
    #add the layer for available routes
  observe({
    leafletProxy("map")%>%
      clearGroup("avail")%>%
      addCircleMarkers(data=available(),popup =~pop_up,
                       stroke=FALSE,
                       radius=7,
                       fillOpacity=0.6,
                        color=~pal()(chosen_stat),
                       group="avail")
  })
  #add the layer for unavailable routes
  icons <- awesomeIconList(
    times_circle = makeAwesomeIcon(icon = "times-circle", library = "fa", markerColor = "red")
  )


  
    # observe({
  #   leafletProxy("map")%>%
  #     clearGroup("unavail")%>%
  #     addCircleMarkers(data=unavail,
  #                       #icon=icons["times_circle"],
  #                       group="unavail")
  # })
  #add legend to the chart
  observe({
    leafletProxy("map")%>%
      clearControls()%>%
      clearGroup("legend")%>%
      addLegend(data=available(),"bottomright",
                pal = pal(),
                values = ~ chosen_stat,
                title = "",
                opacity = 1,
                group="legend")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)