# User interface for the LiDAR Data Collection Explorer

ui <- fluidPage(
  tags$head(
    # Google Fonts
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
    # Three.js CDN
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    # Lenis CDN
    tags$script(src = "https://cdn.jsdelivr.net/gh/studio-freight/lenis@1.0.19/bundled/lenis.min.js"),
    # Custom CSS and JS
    includeCSS("www/style.css"),
    tags$script(src = "script.js")
  ),
  
  # Three.js WebGL container (fixed background)
  div(id = "webgl-container"),
  
  # Smooth scroll wrapper
  div(id = "smooth-wrapper",
    div(id = "smooth-content",
      # Hero section
      div(class = "hero-section",
        div(class = "app-header", "Mapping LiDAR Data Collection Across Alberta"),
        div(class = "app-subtitle", "Explore sensor deployments across Alberta")
      ),
      
      # Controls panel
      div(class = "glass-panel",
        div(class = "panel-title", "Site Selection"),
        selectInput(
          "Site", "SITE NO:",
          choices = sort(unique(data$Site[!is.na(data$Site)]))
        )
      ),
      
      # Map panel
      div(class = "glass-panel",
        div(class = "panel-title", "Interactive Map"),
        leafletOutput("map", height = "70vh"),
        actionButton("reset_zoom", "Reset Zoom", class = "glass-button")
      ),
      
      # Data table panel
      div(class = "glass-panel",
        div(class = "panel-title", "Site Data"),
        DTOutput("data_table")
      )
    )
  )
)
