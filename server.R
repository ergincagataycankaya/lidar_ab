# Server logic for the LiDAR Data Collection Explorer

server <- function(input, output, session) {

  # Reactive: Filter data by selected site
  filtered_data <- reactive({
    req(input$Site)
    data %>%
      filter(Site %in% input$Site) %>%
      mutate(
        id = row_number(),
        popup = paste(
          "<b>Project:</b>", Project, "<br>",
          "<b>Site:</b>", Site, "<br>",
          "<b>Notes:</b>", Notes, "<br>",
          "<b>Sensor:</b>", Sensor
        )
      )
  })

  table_data <- reactive({
    filtered_data() %>%
      select(Year, Month, Day, Project, Site, Crew, DataType, Sensor, Notes, latitude, longitude)
  })

  # Render interactive map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addPolygons(
        data = ecoregions, stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.1,
        color = "black", popup = ~as.character(REGION_NAM),
        fillColor = ~factpal(REGION_NAM), group = "Ecoregions"
      ) %>%
      addPolygons(
        data = boundry, stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.1,
        group = "boundry"
      ) %>%
      addLayersControl(
        baseGroups = c("Satellite"),
        overlayGroups = c("Ecoregions", "boundry"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Ecoregions") %>%
      addLegend(
        pal = pal_PLOT, values = data$DataType, opacity = 1,
        position = "topright", title = "Data Type", na.label = "Not Available"
      ) %>%
      addMiniMap(
        tiles = c("Esri.WorldImagery")[1], toggleDisplay = TRUE
      ) %>%
      addScaleBar(position = 'bottomleft') %>%
      addMeasure(
        position = "bottomleft", primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters", activeColor = "#0bd3d3",
        completedColor = "#f890e7"
      )
  })

  # Map reset zoom button
  observeEvent(input$reset_zoom, {
    leafletProxy("map") %>%
      setView(lng = mean(boundry@bbox[1, ]), lat = mean(boundry@bbox[2, ]), zoom = 6)
  })

  # Update map with selected points and link to table
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 4, popup = ~popup,
        color = ~pal_PLOT(DataType),
        stroke = TRUE, weight = 1, fillOpacity = 0.7,
        layerId = ~id, options = markerOptions(draggable = TRUE)
      )
    if (nrow(filtered_data()) > 0) {
      leafletProxy("map") %>%
        flyTo(lng = filtered_data()$longitude[1], lat = filtered_data()$latitude[1], zoom = 15)
    }
  })

  # Render table with site data
  output$data_table <- renderDT({
    datatable(
      table_data(),
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE,
      selection = "single"
    )
  })

  # Synchronize table row and map marker selection
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      proxy <- dataTableProxy("data_table")
      selectRows(proxy, which(filtered_data()$id == click$id))
      proxy %>% selectPage(which(filtered_data()$id == click$id) %/% 5 + 1)
    }
  })

  observeEvent(input$data_table_rows_selected, {
    selected_row <- input$data_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_data()[selected_row, ]
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addCircleMarkers(
          lng = selected_data$longitude, lat = selected_data$latitude,
          radius = 10, color = "red", fillOpacity = 1, group = "selected"
        )
    }
  })
}
