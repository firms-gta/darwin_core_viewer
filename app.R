# Source install script
require(here)
source(here::here('install.R'))
# Log the loading of libraries
flog.info("All libraries loaded successfully.")
############################################################ DATA and FILTER ########################################################################################################################################################################
#setwd("./srv/darwin_core_viewer")
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
local_wkt <- 'POLYGON ((31.11328 -31.50363, 31.11328 -3.162456, 71.01562 -3.162456, 71.01562 -31.50363, 31.11328 -31.50363))'
wkt <- reactiveVal(local_wkt) 

default_year <- NULL
target_year <- data_dwc %>% distinct(year) %>% arrange(desc(year))

# default_species <- c('Elagatis bipinnulata (Quoy & Gaimard, 1825)','Coryphaena hippurus Linnaeus, 1758')
default_species <- NULL
target_species <- data_dwc %>% distinct(scientificName)

# default_family <- c("Coryphaenidae","Carangidae","Scombridae", "Carcharhinidae","Istiophoridae")
default_family <- NULL
target_family <- data_dwc %>% distinct(family)

default_depth <- NULL
target_depth <- data_dwc %>% distinct(depth)

filters_combinations <- data_dwc %>% st_drop_geometry()  %>% distinct(family, scientificName) %>% arrange(family, scientificName)

################################################################ USER INTERFACE ###################################################################################################################################################

ui <- fluidPage(
  # titlePanel("Species occurences viewer: map and plots"),
  navbarPage(title="Data viewer for Darwin Core data format",
             tabPanel("Species occurences viewer",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          # tags$head(includeCSS("https://raw.githubusercontent.com/juldebar/MIKAROKA/main/styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = "15%", left = "auto", right="75%", width = "20%", height = "auto",
                                        tags$br(),
                                        h2("Select filters to customize indicators"),
                                        tags$br(),
                                        actionButton(
                                          inputId = "submit",
                                          label = "Display data",
                                          # icon = icon("refresh"),
                                          icon("play-circle"), 
                                          style="color: #fff; background-color: #F51D08; border-color: #2e6da4"
                                        ),
                                        tags$br(),
                                        tags$br(),
                                        selectInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year$year,
                                          multiple = TRUE,
                                          selected= default_year
                                        ),
                                        selectInput(
                                          inputId = "family",
                                          label = "Family",
                                          choices = target_family$family,
                                          multiple = TRUE,
                                          selected= default_family
                                        ),
                                        selectInput(
                                          inputId = "species",
                                          label = "Scientific Name",
                                          choices = target_species$scientificName,
                                          multiple = TRUE,
                                          selected= default_species
                                        ),
                                        textInput(
                                          inputId="polygon",
                                          label="Edit WKT",
                                          value=local_wkt
                                          ),
                                        actionButton(
                                          inputId = "resetWkt",
                                          label = "Reset WKT",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63B061; border-color: #2e6da4"
                                          ),
                                        actionButton(
                                          inputId = "resetAllFilters",
                                          label = "Reset all filters",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63C5DA; border-color: #2e6da4"
                                          ),
                                        # selectInput(
                                        #   inputId = "depth",
                                        #   label = "Depth",
                                        #   choices = target_depth$depth,
                                        #   multiple = TRUE,
                                        #   selected= default_depth
                                        # ),
                                        tags$br(),
                                        tags$br()
                          ),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = "15%", right = "auto", left ="73%", width = "25%", fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        tags$br(),
                                        tags$br(),
                                        plotlyOutput("pie_map", height ="100%"),
                                        ),
                          absolutePanel(id = "logo", class = "card", bottom = "4%", left = "2%", width = "5%", fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://mikaroka.ird.fr/', tags$img(src='http://mikaroka.ird.fr/wp-content/uploads/2020/01/MIKAROKA_logo.png',width = '200')))
                                      # tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='89',width='108')))
                          
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
                                   column(3,
                                          tags$small(
                                            "Funding : MIKAROKA ",
                                            a(href='http://mikaroka.ird.fr/', img(src='http://mikaroka.ird.fr/wp-content/uploads/2020/01/MIKAROKA_logo.png',width='30%'))
                                          )
                                   ),
                                   column(3,
                                          tags$small(
                                            "Source: GBIF data",
                                            img(class="logo_IRD", src=paste0("https://raw.githubusercontent.com/juldebar/MIKAROKA/main/data/gbif_23m361.svg")),
                                            # https://doi.org/10.15468/23m361 https://doi.org/10.15468/dl.5bzzz4
                                          )
                                   ),
                                   column(3,
                                          img(class="logo_IRD",
                                              src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                                          tags$small(
                                            "General Disclaimer:",
                                            "This repository contains work in progress. It can be used to explore the content of biodiversity / ecological data using Darwin Core data format Results presented here do not represent the official view of IRD, its staff or consultants.",
                                            "Caution must be taken when interpreting all data presented, and differences between information products published by IRD and other sources using different inclusion criteria and different data cut-off times are to be expected. While steps are taken to ensure accuracy and reliability, all data are subject to continuous verification and change.  See here for further background and other important considerations surrounding the source data."
                                          )
                                   )
                                   
                                 )
                        ),
                        tabPanel(
                          title = "Current WKT polygon",
                          textOutput("WKT")
                        )
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
    wkt(local_wkt)
  },
  ignoreInit = TRUE)
  
  observe({
    updateTextInput(session, "polygon", value = wkt())
  })
  
  observeEvent(input$resetAllFilters, {
    updateTextInput(session, "polygon", value = wkt())
    updateSelectInput(session,"year",choices = target_year$year, selected = NULL )
    updateSelectInput(session,"family",choices = target_family$family, selected = NULL )
    updateSelectInput(session,"species",choices = target_species$scientificName, selected = NULL )
  },
  ignoreInit = TRUE)
  
  data <- eventReactive(input$submit, {
    if(is.null(input$species)){filter_species=target_species$scientificName}else{filter_species=input$species}
    if(is.null(input$family)){filter_family=target_family$family}else{filter_family=input$family}
    if(is.null(input$year)){filter_year=target_year$year}else{filter_year=input$year}
    # if(is.null(input$depth)){filter_depth=target_depth$depth}else{filter_depth=input$depth}
    data_dwc %>% 
      filter(year %in% filter_year) %>%
      filter(family %in% filter_family) %>% 
      filter(scientificName %in% filter_species) %>%
      # filter(depth %in% filter_depth) %>% 
      dplyr::filter(st_within(.,st_as_sfc(input$polygon, crs = 4326), sparse = FALSE)) # %>% head(500)

        },ignoreNULL = FALSE)

############################################################# OUTPUTS   ############################################################# 
    

    output$DT_within_WKT <- renderDT({
      data() %>%  dplyr::filter(st_within(.,st_as_sfc(input$polygon, crs = 4326), sparse = FALSE))  %>% st_drop_geometry()
    }) 
    
    output$WKT <- renderText({
      wkt()
    }) 
    
    
  output$mymap <- renderLeaflet({
    
    shiny::validate(
      need(nrow(data())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
      
    )
    
    # df <- data_dwc %>%  filter(st_within(geometry,st_as_sfc(local_wkt, crs = 4326),sparse = FALSE)[, 1]) 
    df <- data()  
    
    mymap <-leaflet(data=df,options = leafletOptions(minZoom = 1, maxZoom = 40)) %>% 
      clearPopups()  %>% 
      # https://leaflet-extras.github.io/leaflet-providers/preview/ 
      addProviderTiles("Esri.OceanBasemap", group = "ESRI") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI2") %>% 
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
        baseGroups = c("ESRI", "ESRI2"),
        overlayGroups = c("draw"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )
      # mymap
  })
  
  observe({
    #use the draw_stop event to detect when users finished drawing
    feature <- input$mymap_draw_new_feature
    req(input$mymap_draw_stop)
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds 
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    wkt(st_as_text(st_geometry(geom[1,])))
    coord <- st_as_text(st_geometry(geom[1,]))
    
    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]
    
    
    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)
    
    # mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
    # textOutput("wkt")
    
  })
 
  
  
  output$pie_map <- renderPlotly({
    
    shiny::validate(
      need(nrow(data())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
    )
    
    pie_data <- data()  %>% st_drop_geometry() %>% dplyr::group_by(family) %>% dplyr::summarise(count = n_distinct(gbifID)) %>% dplyr::arrange(count) # %>% top_n(10)
    
    fig <- plot_ly(pie_data, labels = ~family, values = ~count, type = 'pie', width = 350, height = 500,
                   marker = list( line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)

    fig <- fig %>% layout(title = 'Main families composition',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig


  })

   
}

# Run the application 
shinyApp(ui = ui, server = server)





