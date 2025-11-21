# User interface for the LiDAR Data Collection Explorer

ui <- fluidPage(
  tags$head(
    # Google Fonts
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600&display=swap"
    ),
    # Three.js for 3D effects
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    # Lenis for smooth scrolling
    tags$script(src = "https://cdn.jsdelivr.net/gh/studio-freight/lenis@1.0.29/bundled/lenis.min.js"),
    # Local CSS
    includeCSS("www/style.css"),
    # Local JavaScript
    tags$script(src = "script.js")
  ),
  
  # 3D Background Container
  div(id = "canvas-container"),
  
  # Main scrollable content
  div(class = "main-content",
    # Hero Section
    div(class = "hero-section",
      h1(class = "hero-title", "Alberta LiDAR Data Explorer"),
      p(class = "hero-subtitle", "Explore sensor deployments and LiDAR data collection across Alberta")
    ),
    
    # Dashboard Container
    div(class = "dashboard-container",
      # Site Selection Panel
      div(class = "glass-panel",
        div(class = "panel-header", "Site Selection"),
        selectInput(
          "Site", "SITE NO:",
          choices = sort(unique(data$Site[!is.na(data$Site)]))
        )
      ),
      
      # Map Panel
      div(class = "glass-panel map-panel",
        div(class = "panel-header", "Interactive Map"),
        leafletOutput("map", height = "70vh"),
        actionButton("reset_zoom", "Reset Zoom", class = "reset-button")
      ),
      
      # Data Table Panel
      div(class = "glass-panel",
        div(class = "panel-header", "Site Data"),
        DTOutput("data_table")
      )
    )
  )
)
