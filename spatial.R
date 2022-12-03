##Spatial 
library(tmap)
library(sf)
library(shiny)

shape <- st_read(here('data/spatial/data_0.shp'))
tmap_mode('view')

extinct_spatial <- predictors_2 |> 
  filter(extinct == 1) |> 
  left_join(shape, by = c('assessment_id' = 'ASSESSMENT')) |> 
  filter(!st_is_empty(geometry))  |> 
  mutate(extinct = "Yes",
         endemic = as.factor(if_else(endemic ==1, "Yes", "No"))) |> 
  left_join(data, by = 'assessment_id')

extinct_spatial <- extinct_spatial |> 
  dplyr::select(assessment_id, scientific_name = BINOMIAL, endemic = endemic.x, extinct= extinct.x, habitat, threat, use, country_1 = country_1.x, country_2 = country_2.x, country_3 = country_3.x, phylum, class, order, family, genus, geometry) |> 
  st_as_sf()

data(World)
tmap_options(check.and.fix = TRUE)
map <- tm_shape(World) + tm_polygons() + tm_shape(extinct_spatial) + tm_dots( col = 'red',
                                                                      popup.vars = c("habitat", "threat","use", "country_1", "phylum", "class", "order", "family", "genus", "endemic", "scientific_name"), id = 'scientific_name') + tm_view(set.view = 1.2)
                                                                      
map

# ui <- fluidPage(
#   tags$h1("Extinctions in Animalia"),
#   selectInput(inputId = "habitat", label = "Select Habitat:", multiple = TRUE, choices = sort(extinct_spatial$habitat), selected = "Forest"),
#   tmapOutput(outputId = "tmapMap")
# )
# 
# server <- function(input, output) {
#   data <- reactive({
#     airports %>%
#       filter(STATE %in% input$inputState) %>%
#       mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE))
#   })
#   dataTmap <- reactive({
#     sf::st_as_sf(
#       data.frame(
#         airport = data()$AIRPORT,
#         lat = data()$LAT,
#         long = data()$LONG
#       ),
#       coords = c("long", "lat")
#     )
#   })
#   
#   output$leafletMap <- renderLeaflet({
#     leaflet(data = data()) %>%
#       setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
#       addTiles() %>%
#       addMarkers(~LONGITUDE, ~LATITUDE, icon = leafletIcon, popup = ~INFO, label = ~INFO) %>%
#       addProviderTiles(providers$Esri.WorldStreetMap)
#   })
#   
#   output$tmapMap <- renderTmap({
#     tm_shape(us_shp) +
#       tm_view(set.view = c(usaLon, usaLat, usaZoom)) +
#       tm_polygons() +
#       tm_shape(dataTmap()) +
#       tm_symbols(shape = tmapIcon, size = 0.15, border.lwd = NA)
#   })
# }
# 
# 
# shinyApp(ui = ui, server = server)
