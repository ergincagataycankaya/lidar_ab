# User interface for the LiDAR Data Collection Explorer

ui <- fluidPage(
  # Include external libraries and assets
  tags$head(
    # Three.js for 3D graphics
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    
    # Lenis for smooth scrolling
    tags$script(src = "https://cdn.jsdelivr.net/gh/studio-freight/lenis@1.0.19/bundled/lenis.min.js"),
    
    # Google Fonts
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700;800;900&display=swap"
    ),
    
    # Custom CSS and JS
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js"),
    
    # Meta tags for better mobile experience
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$meta(name = "description", content = "Explore LiDAR sensor deployments across Alberta"),
    
    # Additional styles for scroll progress bar
    tags$style(HTML("
      .scroll-progress {
        position: fixed;
        top: 0;
        left: 0;
        height: 3px;
        background: linear-gradient(90deg, #00f3ff, #ff00ff);
        z-index: 9999;
        transition: width 0.3s ease;
      }
    "))
  ),
  
  # Scroll progress indicator
  div(class = "scroll-progress", style = "width: 0%"),
  
  # 3D Canvas background
  div(id = "canvas-container"),
  
  # Grid background effect
  div(class = "grid-bg"),
  
  # Main app container
  div(class = "app-container",
    
    # Hero Section
    div(class = "hero-section parallax-section",
      div(class = "hero-content",
        h1(class = "hero-title", "LiDAR Alberta"),
        p(class = "hero-subtitle", 
          "Mapping Advanced Sensor Deployments Across Alberta's Diverse Landscapes"
        ),
        tags$a(class = "hero-cta", href = "#controls-section", 
               "Explore the Data")
      ),
      div(class = "scroll-indicator")
    ),
    
    # Controls Section
    div(class = "parallax-section dark", id = "controls-section",
      div(class = "controls-section",
        div(class = "glass-panel",
          h2(class = "section-title", "Interactive Explorer"),
          p(class = "section-subtitle", 
            "Select a site to visualize LiDAR data collection locations"
          ),
          
          div(class = "control-group",
            fluidRow(
              column(12, align = "center",
                selectInput(
                  "Site", "SITE NUMBER:",
                  choices = sort(unique(data$Site[!is.na(data$Site)]))
                )
              )
            )
          )
        )
      )
    ),
    
    # Map Section
    div(class = "parallax-section",
      div(class = "controls-section",
        div(class = "glass-panel",
          h2(class = "section-title", "Geographic Distribution"),
          p(class = "section-subtitle", 
            "Explore sensor locations across Alberta's ecoregions"
          ),
          
          div(class = "map-container",
            leafletOutput("map", height = "70vh"),
            div(class = "map-controls",
              actionButton("reset_zoom", "Reset Zoom", class = "btn")
            )
          )
        )
      )
    ),
    
    # Data Table Section
    div(class = "parallax-section dark",
      div(class = "table-section",
        div(class = "glass-panel",
          h2(class = "section-title", "Data Collection Records"),
          p(class = "section-subtitle", 
            "Detailed information about sensor deployments and measurements"
          ),
          
          DTOutput("data_table")
        )
      )
    ),
    
    # Footer Section
    div(class = "parallax-section",
      style = "min-height: 40vh; padding: 3rem 2rem;",
      div(style = "text-align: center; max-width: 800px; margin: 0 auto;",
        h3(class = "gradient-text", 
           style = "font-size: 2rem; margin-bottom: 1rem;",
           "Advanced Geospatial Intelligence"),
        p(style = "color: #a0a0b0; font-size: 1.1rem; line-height: 1.8;",
          "This platform provides comprehensive access to LiDAR sensor deployment data ",
          "across Alberta's diverse ecological regions. Explore spatial patterns, temporal ",
          "trends, and data collection methodologies used in advanced environmental monitoring."
        )
      )
    )
  )
)
