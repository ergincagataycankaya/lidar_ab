library(httr)
library(sf)
library(leaflet)

# Query the layer as GeoJSON
layer_id <- 0  # Layer ID for Natural Subregions of Alberta
geojson_url <- paste0(
  "https://geospatial.alberta.ca/titan/rest/services/biota/natural_subregions_alberta_2005/MapServer/",
  layer_id,
  "/query?where=1%3D1&outFields=*&f=geojson"
)

# Read the GeoJSON as an sf object
subregions_sf <- st_read(geojson_url)

# Create a color palette matching the attached map
subregion_colors <- c(
  "Alpine" = "#CC99CC",                 # Light Purple
  "Athabasca Plain" = "#FFFFCC",        # Pale Yellow
  "Boreal Subarctic" = "#99CCFF",       # Light Blue
  "Central Mixedwood" = "#66FF66",      # Light Green
  "Central Parkland" = "#FF9933",       # Orange
  "Dry Mixedgrass" = "#FFCC99",         # Light Peach
  "Dry Mixedwood" = "#99CC33",          # Olive Green
  "Foothills Fescue" = "#FFCC66",       # Light Yellow-Orange
  "Foothills Parkland" = "#FF9900",     # Bright Orange
  "Kazan Uplands" = "#CCFFCC",          # Very Light Green
  "Lower Boreal Highlands" = "#009966", # Dark Green
  "Lower Foothills" = "#66CC99",        # Teal
  "Mixedgrass" = "#FFCC33",             # Mustard Yellow
  "Montane" = "#9966CC",                # Purple
  "Northern Fescue" = "#FF9999",        # Light Pink
  "Northern Mixedwood" = "#33CC33",     # Bright Green
  "Peace-Athabasca Delta" = "#6699FF",  # Blue
  "Peace River Parkland" = "#CCCC99",   # Tan
  "Subalpine" = "#9999CC",              # Grayish Purple
  "Upper Boreal Highlands" = "#336633", # Forest Green
  "Upper Foothills" = "#66CC66"         # Mid Green
)

# Map the colors to the subregion names in the data
subregions_sf$color <- subregion_colors[subregions_sf$NSRNAME]

# Create a Leaflet map
leaflet(data = subregions_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~color,                # Use pre-assigned colors
    weight = 1,
    color = "black",                   # Polygon border color
    fillOpacity = 0.6,
    label = ~NSRNAME                   # Show subregion name on hover
  ) %>%
  addLegend(
    colors = subregion_colors,        # Match legend to assigned colors
    labels = names(subregion_colors), # Subregion names
    title = "Natural Subregions of Alberta",
    position = "bottomright"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addMeasure(position = "bottomleft")
