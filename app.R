# script loading required packages
source(here::here('install.R'))
# Log the loading of libraries
flog.info("All libraries loaded successfully.")

############################################################ DATA and FILTER ########################################################################################################################################################################
if(!file.exists("./data/dwc.rds")){
  githubURL <- ("https://raw.githubusercontent.com/juldebar/MIKAROKA/master/data/dwc.rds")
  download.file(githubURL,"dwc.rds", method="curl")
  data_dwc <- readRDS("dwc.rds")
  # data_dwc <- as_tibble(read.csv("/home/julien/Downloads/0065082-210914110416597.csv",sep="\t"))  %>% 
  #   dplyr::select(gbifID,scientificName,family,eventDate,year,individualCount,recordedBy,decimalLatitude,decimalLongitude,depth)  %>% 
  #   st_as_sf(.,coords=c("decimalLongitude","decimalLatitude"),crs=4326) 
  # saveRDS(data_dwc,"./data/dwc.rds")
}else{
  data_dwc <- readRDS("./data/dwc.rds")
}

# Set default values for filters to be displayed by UI and used by server to filter and process data
# default_wkt <- 'POLYGON ((31.11328 -31.50363, 31.11328 -3.162456, 71.01562 -3.162456, 71.01562 -31.50363, 31.11328 -31.50363))'
switch_taxa <- reactiveVal(TRUE) 

default_wkt <- st_as_text(st_as_sfc(st_bbox(data_dwc)))
default_geom <- st_as_sfc(st_bbox(data_dwc))

wkt <- reactiveVal(default_wkt) 
current_geom <- reactiveVal(default_geom)

default_year <- NULL
target_year <- data_dwc %>% distinct(year) %>% arrange(desc(year))

default_species <- c('Elagatis bipinnulata (Quoy & Gaimard, 1825)','Coryphaena hippurus Linnaeus, 1758','Acanthocybium solandri (Cuvier, 1832)','Uraspis secunda (Poey, 1860)')
# default_species <- NULL
target_species <- data_dwc %>% distinct(scientificName)

# default_family <- c("Coryphaenidae","Carangidae","Scombridae", "Carcharhinidae","Istiophoridae")
default_family <- NULL
target_family <- data_dwc %>% distinct(family)

filters_combinations <- data_dwc %>% st_drop_geometry()  %>% distinct(family, scientificName) %>% arrange(family, scientificName)

################################################################ USER INTERFACE ###################################################################################################################################################

ui <- fluidPage(
  # titlePanel(" viewer: map and plots"),
  navbarPage(title="Data viewer for species occurences complying with Darwin Core data format",
             tabPanel("Species occurences viewer",
                      modalDialog(
                        title = "About this Shiny app",
                        markdown('
                        [<img src="logo_blue-cloud_2026.svg" height="10%">](https://blue-cloud.d4science.org) 
                        <!--- # About this Shiny app --->
                        This is a generic Shiny app which can display any data complying with the Darwin Core format (e.g. GBIF data). This Shiny app has been developped and is hosted in the same **[Virtual Lab](https://blue-cloud.d4science.org/group/globalfisheriesatlas)** provided by 
                        [Blue-Cloud 2026](https://blue-cloud.d4science.org) project which implements best practices for Open Science and FAIR data management:
                        * data: the default dataset used by this app is published on GBIF with following DOI  [<img src="gbif_23m361.svg" height="5%">](https://doi.org/10.15468/23m361).
                        * code: code is open and freely available on [<img src="github-original-wordmark.svg" height="89">](https://github.com/firms-gta/darwin_core_viewer) and main releases have been assigned DOIs on Zenodo .
                        * virtual environment: the R code uses renv package to snapshot the required environment and can be directly executed within the RStudio server hosted by the virtual lab.
                        * team: this work is provided by IRD [<img src="logo_IRD.svg" height="108">](https://www.ird.fr/).
                        You can freely [register as a new member](https://blue-cloud.d4science.org/group/globalfisheriesatlas) of the virtual Lab if you want to learn more.
                                 '),
                        tags$br(),
                        size = "l",
                        easyClose = FALSE,
                        footer=modalButton("OK", icon = NULL)
                      ),
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id = "filters", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = "12%", left = "1.5%", right="auto", width = "auto", height = "auto",
                                        tags$br(),
                                        h1("Select filters to customize the map and the plots"),
                                        tags$br(),
                                        # tags$br(),
                                        pickerInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year$year,
                                          multiple = TRUE,
                                          selected= default_year,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "family",
                                          label = "Family",
                                          choices = target_family$family,
                                          multiple = TRUE,
                                          selected= default_family,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "species",
                                          label = "Scientific Name",
                                          choices = target_species$scientificName,
                                          multiple = TRUE,
                                          selected= default_species,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        textInput(
                                          inputId="polygon",
                                          label="Edit WKT",
                                          value=default_wkt,
                                          width="98%"
                                          ),
                                        actionButton(
                                          inputId = "resetWkt",
                                          label = "Reset WKT",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63B061; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                          ),
                                        actionButton(
                                          inputId = "resetAllFilters",
                                          label = "Reset all filters",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63C5DA; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                          ),
                                        tags$br(),
                                        tags$br(),
                                        tags$br(),
                                        actionButton(
                                          inputId = "submit",
                                          label = "Apply current filters and display data",
                                          # icon = icon("refresh"),
                                          icon("play-circle"), 
                                          style="color: #fff; background-color: #F51D08; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                        ),
                                        tags$br(),
                                        tags$br()
                          ),
                          
                          absolutePanel(id = "plots", class = "panel panel-default",
                                        top = "12%", right = "1.5%", width = "auto", #fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h2("Taxa composition"),
                                        actionButton(
                                          inputId = "switched",
                                          label = "Switch (family or species)",
                                          # icon("move"), 
                                          style="color: #fff; background-color: #008a20; border-color: #2e6da4; font-size: xx-large;font-weight: bold;"
                                        ),
                                        # tags$br(),
                                        tags$br(),
                                        plotlyOutput("pie_map", height ="100%"),
                                        ),
                          absolutePanel(id = "logo", class = "logo", bottom = "2%", left = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='logo_IRD.svg',height='5%'))),
                          absolutePanel(id = "logo", class = "logo", top = "1.5%", right = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://blue-cloud.d4science.org/', tags$img(src='logo_blue-cloud_2026.svg',height='5%')))
                          
                      )
             ),
             tabPanel("Explore current data table",
                      hr(),
                      DT::dataTableOutput("DT_within_WKT")
                      # downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
             ),
             navbarMenu("More",
                         tabPanel("About",
                                 fluidRow(
                                   column(2),
                                   column(6,
                                          markdown('[<img src="logo_blue-cloud_2026.svg" height="10%">](https://blue-cloud.d4science.org)
# Funding
This work has received funding from the European Union’s Horizon Europe research and innovation programme under grant agreement No.  [101094227](https://doi.org/10.3030/101094227) and No. [862409](https://doi.org/10.3030/862409) (Blue-Cloud H2020 project).


# General Disclaimer

This repository contains work in progress. It can be used to explore the content of biodiversity / ecological data using Darwin Core data format Results presented here do not represent the official view of IRD, its staff or consultants. Caution must be taken when interpreting all data presented, and differences between information products published by IRD and other sources using different inclusion criteria and different data cut-off times are to be expected. While steps are taken to ensure accuracy and reliability, all data are subject to continuous verification and change. See here for further background and other important considerations surrounding the source data.


[<img src="logo_IRD.svg" height="108">](https://www.ird.fr/)    
    ')
                                   ),
                                   column(2)
                                   
                                 )
                        )
                        # tabPanel(
                        #   title = "Current WKT polygon",
                        #   textOutput("WKT")
                        # )
             )
  )
)

################################################################ SERVER ###################################################################################################################################################

server <- function(input, output, session) {
  
  
  change <- reactive({
    unlist(strsplit(paste(input$family,collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$family,{
    temp <- filters_combinations %>% filter(family %in% change()[])
    updateSelectInput(session,"species",choices = unique(temp$scientificName))
  }
  )
  
  
  observeEvent(input$resetWkt, {
    wkt(default_wkt)
    current_geom(default_geom)
    updateTextInput(session,"polygon", value = wkt())
  },
  ignoreInit = FALSE)
  

  
  observeEvent(input$resetAllFilters, {
    wkt(default_wkt)
    current_geom(default_geom)
    updateTextInput(session, "polygon", value = default_wkt)
    updatePickerInput(session,"year",choices = target_year$year, selected = NULL )
    updatePickerInput(session,"family",choices = target_family$family, selected = NULL )
    updatePickerInput(session,"species",choices = target_species$scientificName, selected = NULL)
  },
  ignoreInit = TRUE)
  
  observeEvent(input$switched, {
    if(switch_taxa()){switch_taxa(FALSE)}else{switch_taxa(TRUE)}
  })
  
  data <- eventReactive(input$submit, {
    if(is.null(input$species)){filter_species=target_species$scientificName}else{filter_species=input$species}
    if(is.null(input$family)){filter_family=target_family$family}else{filter_family=input$family}
    if(is.null(input$year)){filter_year=target_year$year}else{filter_year=input$year}
    data_dwc %>% 
      filter(year %in% filter_year) %>%
      filter(family %in% filter_family) %>% 
      filter(scientificName %in% filter_species) %>%
      dplyr::filter(st_within(.,st_as_sfc(input$polygon, crs = 4326), sparse = FALSE)) # %>% head(500)

        },ignoreNULL = FALSE)

############################################################# OUTPUTS   ############################################################# 
    

    output$DT_within_WKT <- renderDT({
      data() %>%  dplyr::filter(st_within(.,st_as_sfc(input$polygon, crs = 4326), sparse = FALSE))  %>% st_drop_geometry()
    }) 
    
    # output$WKT <- renderText({
    #   wkt()
    # }) 
    # 
    
  output$mymap <- renderLeaflet({
    
    shiny::validate(
      need(nrow(data())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
    )
    
    
    # df <- data_dwc %>%  filter(st_within(geometry,st_as_sfc(default_wkt, crs = 4326),sparse = FALSE)[, 1]) 
    df <- data()
    # current_selection <- st_sf(st_as_sfc(wkt(), crs = 4326))
    ongoing_geom <- current_geom()
    # all_points <- df %>% st_buffer(2000) %>% st_combine() #%>% st_convex_hull(
    convex_hull <- df %>% st_combine() %>% st_convex_hull()
    all_points <- df %>% st_combine() %>% st_buffer(1)
    
    mymap <-leaflet::leaflet(data=df,options = leafletOptions(minZoom = 1, maxZoom = 40)) %>% 
      # clearPopups()  %>% 
      # clearShapes()   %>% 
      # https://leaflet-extras.github.io/leaflet-providers/preview/ 
      addProviderTiles("Esri.OceanBasemap", group = "bathymetry") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>% 
      addPolygons(data = ongoing_geom,color="red",fillColor = "transparent", group="current_selection") %>%
      addPolygons(data = convex_hull, color="blue",fillColor = "transparent", group="all_points") %>%
      addPolygons(data = all_points, color="blue",fillColor = "transparent", group="all_points") %>%
      clearBounds() %>%
      addMarkers(~as_tibble(st_coordinates(geometry))$X,~as_tibble(st_coordinates(geometry))$Y,
                 popup = ~as.character(scientificName),
                 label = ~as.character(scientificName),
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
                 ) %>% 
        addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("bathymetry","satellite"),
        overlayGroups = c("draw","current_selection","all_points"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%  
      addMiniMap(zoomLevelFixed = 1) %>%
      addScaleBar(
        position = "topright",
        options = scaleBarOptions(
          maxWidth = 10,
          metric = TRUE,
          imperial = FALSE
        )
      )
      # mymap
  })

  observeEvent(input$mymap_draw_stop,{
    # observe({
      #use the draw_stop event to detect when users finished drawing
    # req(input$mymap_draw_new_feature)
    req(input$mymap_draw_stop)
    feature <- input$mymap_draw_new_feature
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds 
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    coord <- st_as_text(st_geometry(geom[1,]))
    wkt(coord)
    updateTextInput(session,"polygon", value = coord)
    new_geom <- st_sf(st_as_sfc(coord, crs = 4326))
    current_geom(new_geom)
    
    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]
    
    
    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)
    
    # mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord) %>%
    # fitBounds(south, east, north, west) %>%
    # # addRectangles(lng1=east,lat1=south,lng2=west,lat2=north,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group = "draw")
    # addPolygons(data = current_selection,color="red",fillColor = "transparent", group="current_selection")
    # # textOutput("wkt")
    
    },ignoreInit = FALSE)
# })

  
  
  output$pie_map <- renderPlotly({
    
    shiny::validate(
      need(nrow(data())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
    )
    
    if(switch_taxa()){
      taxa <- "species"
      pie_data <- data()  %>% st_drop_geometry() %>% dplyr::group_by(scientificName) %>% dplyr::summarise(count = n_distinct(gbifID)) %>% dplyr::arrange(count) # %>% top_n(10)
      fig <- plot_ly(pie_data, labels = ~scientificName, values = ~count, type = 'pie', width = 650, height = 1000,
                     marker = list( line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                     showlegend = TRUE)
    }else{
      taxa <- "family"
      pie_data <- data()  %>% st_drop_geometry() %>% dplyr::group_by(family) %>% dplyr::summarise(count = n_distinct(gbifID)) %>% dplyr::arrange(count) # %>% top_n(10)
      fig <- plot_ly(pie_data, labels = ~family, values = ~count, type = 'pie', width = 650, height = 1200,
                     marker = list( line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                     showlegend = TRUE)
    }
    
    


    fig <- fig %>% layout(title = paste0('Main ',taxa,' composition'),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig


  })

   
}

# Run the application 
shinyApp(ui = ui, server = server)





