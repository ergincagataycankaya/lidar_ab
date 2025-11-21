# User interface for the LiDAR Data Collection Explorer

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600&display=swap", rel = "stylesheet"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/gh/studio-freight/lenis@1.0.29/bundled/lenis.min.js"),
    includeCSS("www/style.css"),
    tags$script(src = "script.js")
  ),
  
  # 3D Background Container
  div(id = "canvas-container"),
  
  # Main Scrollable Content
  div(class = "main-content",
      # Hero Section
      div(class = "hero-section",
          h1(class = "hero-title", "Alberta LiDAR Data Explorer"),
          p(class = "hero-subtitle", "Explore sensor deployments and LiDAR data collection across Alberta")
      ),
      
      # Dashboard Section
      div(class = "dashboard-container",
          # Site Selection Panel
          div(class = "glass-panel",
              selectInput(
                "Site", "SITE NO:",
                choices = sort(unique(data$Site[!is.na(data$Site)]))
              )
          ),
          
          # Map Panel
          div(class = "glass-panel map-panel",
              leafletOutput("map", height = "70vh"),
              actionButton("reset_zoom", "Reset Zoom", class = "reset-btn")
          ),
          
          # Data Table Panel
          div(class = "glass-panel",
              DTOutput("data_table")
          )
      )
  )
)
