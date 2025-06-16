# User interface for the LiDAR Data Collection Explorer

ui <- fluidPage(
  tags$head(includeCSS("www/style.css")),
  div(class = "app-header", "Mapping LiDAR Data Collection Across Alberta"),
  div(class = "app-subtitle", "Explore sensor deployments across Alberta"),

  div(class = "panel-box",
      fluidRow(
        column(8, align = "center", offset = 2,
               selectInput(
                 "Site", "SITE NO:",
                 choices = sort(unique(data$Site[!is.na(data$Site)]))
               )
        ),
        column(12,
               leafletOutput("map", height = "70vh"),
               actionButton("reset_zoom", "Reset Zoom", style = "margin-top: 15px;")
        )
      )
  ),

  div(class = "panel-box", DTOutput("data_table"))
)
