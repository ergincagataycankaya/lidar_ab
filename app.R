# # Forget the Previous Deployment Record
# rsconnect::forgetDeployment("C:/Users/ergin/OneDrive/Desktop/lidar")
# # Re-deploy the Application
# rsconnect::deployApp("C:/Users/ergin/OneDrive/Desktop/lidar")
# -----------------------------------------------------------------------------------

library(shiny)
library(leaflet)
library(DT)
library(readxl)
library(dplyr)
library(raster)
library(sf)

# Set working directory and load Excel dataset
# setwd("C:/Users/ergin/OneDrive/Desktop/lidar")
data <- read_excel("www/DATA_SETS_DATABASE.xlsx") %>% mutate(Site = as.factor(Site))

library(rgdal)

# Load and process ecoregions shapefile
ecoregions <- shapefile("www/spatial/ecoregions_ab.shp", 
                        use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = TRUE)
ecoregions$REGION_NAM <- as.factor(ecoregions$REGION_NAM)

# Load boundary shapefile and transform it to match the ecoregions CRS
boundry <- shapefile("www/spatial/AB_Boundry.shp", 
                     use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = TRUE)
boundry <- spTransform(boundry, CRS(proj4string(ecoregions)))


# Limit the number of colors in the palette to fix warning
factpal <- colorFactor(palette = RColorBrewer::brewer.pal(min(length(levels(ecoregions$REGION_NAM)), 11), "Spectral"),
                       domain = ecoregions$REGION_NAM)

# Define UI
ui <- fluidPage(
    titlePanel("Mapping LiDAR Data Collection Across Alberta"),
    fluidRow(
        column(8, align = "center", offset = 1, selectInput(inputId = "Site", label = "SITE NO:", choices = sort(unique(data$Site)))),
        column(8, align = "center", offset = 1, checkboxGroupInput("data_type", "Select Data Types:",
                                                                   choices = c("DLS", "TLS", "MLS", "RGB", "MS"),
                                                                   selected = c("DLS", "TLS", "MLS", "RGB", "MS"), inline = TRUE)),
        tags$head(includeCSS("www/style.css")),
        leafletOutput("map", height = "70vh"),
        actionButton("reset_zoom", "Reset Zoom", style = "margin-top: 15px;")
        
    ),
    # Extend the map height to the end of the window
    
    DTOutput("data_table") # Add table output below the map
)

server <- function(input, output, session) {
    # Show modal dialog for data type selection
    
    # Reactive filtered data based on selected data types and site
    filtered_data <- reactive({
        req(input$Site)
        
        # Filter by Site and Data Type
        selectedPLOT <- data %>%
            filter(Site %in% input$Site & (
                (DLS & "DLS" %in% input$data_type) |
                    (TLS & "TLS" %in% input$data_type) |
                    (MLS & "MLS" %in% input$data_type) |
                    (RGB & "RGB" %in% input$data_type) |
                    (MS & "MS" %in% input$data_type)
            )) %>%
            mutate(
                id = row_number(), # Add a unique ID for each row
                popup = paste(
                    "<b>Project:</b>", Project, "<br>",
                    "<b>Site:</b>", Site, "<br>",
                    "<b>Notes:</b>", Notes, "<br>",
                    "<b>Sensor:</b>", Sensor
                )
            )
        return(selectedPLOT)
    })
    
    # Define color palette for DataType
    pal_PLOT <- colorFactor(palette = RColorBrewer::brewer.pal(5, "Set2"),
                            domain = data$DataType)
    
    # Render the Leaflet map
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
            
            # Add the Ecoregions
            addPolygons(data = ecoregions,
                        stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.1,
                        color = "black", popup = ~as.character(REGION_NAM),
                        fillColor = ~factpal(REGION_NAM),
                        group = "Ecoregions") %>%
            
            # Add the boundry in AB
            addPolygons(data = boundry,
                        stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.1,
                        group = "boundry") %>%
            
            # Add a layers control
            addLayersControl(
                baseGroups = c("Satellite"),
                overlayGroups = c("Ecoregions", "boundry"),
                position = "topleft",
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            
            hideGroup("Ecoregions") %>%
            
            # Add a legend
            addLegend(
                pal = pal_PLOT, values = data$DataType, opacity = 1, position = "topright", title = "Data Type", na.label = "Not Available"
            ) %>%
            
            # Add a minimap
            addMiniMap(
                tiles = c("Esri.WorldImagery")[1],
                toggleDisplay = TRUE) %>%
            addScaleBar(position = 'bottomleft') %>%
            addMeasure(
                position = "bottomleft",
                primaryLengthUnit = "meters",
                primaryAreaUnit = "sqmeters",
                activeColor = "#0bd3d3",
                completedColor = "#f890e7"
            )
    })
    
    # Dynamically add points to the map and add draggable markers with hover tooltips
    observeEvent(input$reset_zoom, {
        leafletProxy("map") %>%
            setView(lng = mean(boundry@bbox[1, ]), lat = mean(boundry@bbox[2, ]), zoom = 6)
    })
    
    observe({
        leafletProxy("map", data = filtered_data()) %>%
            clearMarkers() %>%
            addCircleMarkers(
                lng = ~longitude, lat = ~latitude,
                radius = 4, popup = ~popup,
                color = ~pal_PLOT(DataType),
                stroke = TRUE, weight = 1, fillOpacity = 0.7, # Adjust stroke linewidth to be thinner
                layerId = ~id, # Use ID to link with the data table
                options = markerOptions(draggable = TRUE)
            )
        
        # Fly to the selected plot
        if (nrow(filtered_data()) > 0) {
            leafletProxy("map") %>%
                flyTo(lng = filtered_data()$longitude[1], lat = filtered_data()$latitude[1], zoom = 15)
        }
    })
    
    # Render the data table
    output$data_table <- renderDT({
        datatable(
            filtered_data(),
            options = list(pageLength = 3),
            rownames = FALSE,
            selection = "single" # Allow single row selection
        )
    })
    
    # Respond to marker click and select the corresponding row in the table
    observeEvent(input$map_marker_click, {
        click <- input$map_marker_click
        if (!is.null(click$id)) {
            proxy <- dataTableProxy("data_table")
            selectRows(proxy, which(filtered_data()$id == click$id))
            # Scroll the table to show the selected row
            proxy %>% selectPage(which(filtered_data()$id == click$id) %/% 3 + 1)
        }
    })
    
    # Respond to table row selection and highlight the corresponding marker on the map
    observeEvent(input$data_table_rows_selected, {
        selected_row <- input$data_table_rows_selected
        if (length(selected_row) > 0) {
            selected_data <- filtered_data()[selected_row, ]
            leafletProxy("map") %>%
                clearGroup("selected") %>%
                addCircleMarkers(
                    lng = selected_data$longitude, lat = selected_data$latitude,
                    radius = 10, color = "red", fillOpacity = 1,
                    group = "selected"
                )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
