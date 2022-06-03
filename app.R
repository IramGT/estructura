## LIBRARIES

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(shiny)
library(bslib)
library(shinyjs)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)

# Bases de datos
rutas <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQWRGbbRlcV4z7jSyRk7XppiPZrYi7dZNeiRXlecqb362arymXz8AuR5FXbkITzYnpZceWGJgJGwa0p/pub?gid=1678194350&single=true&output=csv")
promovidos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTl1sgZNKEWnuLCRqy4BxLRV2LUTUh-3B8We6yz4ZL9RWigsi8NMyG1Z4-RZTVDboDoiD9TJCcI7yzl/pub?gid=0&single=true&output=csv")

# Capa shp
shp_mineral <- read_sf(dsn = "seccionHidalgo", layer = "SeccionMineral")
casillas <- read_sf(dsn = "seccionHidalgo", layer = "casillas")

# Alteraciones a la capa
shp_mineral <- shp_mineral %>% 
  select(seccion, MunicipioN)

shp_mineral <- promovidos %>% 
  left_join(shp_mineral, .)

casillas <- casillas %>% 
  left_join(rutas)

opciones <- shp_mineral$MunicipioN

#### UI ####

ui <- (
  # Choices for drop-downs
  navbarPage(h4("INNOVA", style='color:#B3282D'), 
             id="nav",
             tabPanel("REGIONES", 
                      div(class="outer",
                          # Include our custom CSS
                          tags$head(includeCSS("styles.css") ),
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = 60, left = 40, right = "auto", bottom = "auto",
                                        width = 260, height = "auto",
                                        h3(img(src = "JM.jpg", height = 94, width = 186)),
                                        selectInput(inputId = "select_1", label = "", selected = "MINERAL DE LA REFORMA", choices = unique(opciones))
                                        ) ) ),
             tabPanel("CASILLAS", h3(img(src = "JM.jpg", height = 94, width = 186)) , DT::dataTableOutput("ziptable"))
  ) ) 




#### SERVER ####

server <- function(input, output) {
  
     output$map <- renderLeaflet({
      tm <- shp_mineral %>% 
        filter(MunicipioN == input$select_1) %>%
        tm_shape() +
        tm_polygons(alpha = .05, id = "seccion", popup.vars = c("Lista nominal" = "Lista nominal", "Promovidos" = "Cantidad")) +
        tm_basemap("OpenStreetMap") +
        tm_shape(casillas) +
        tm_dots(id = "Localidad", size = .06, clustering = F, col = "RUTA", style = "cat", popup.vars = c("Enlace" = "Enlace", "Cel" = "Celular", "Abogado" = "Abogado", "Tel" = "CelularA")) 
      
      tmap_leaflet(tm)
      
    })


    output$ziptable <- DT::renderDataTable({
      tabla <- rutas %>% 
        dplyr::select(seccion, Localidad, "Ubicación", Google, RUTA)

      DT::datatable(tabla, options = list(pageLength = 50))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
