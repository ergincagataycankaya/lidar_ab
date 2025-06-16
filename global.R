# Global settings and data loading for the LiDAR Data Collection Explorer

# ---- Load Required Libraries ----
library(shiny)
library(leaflet)
library(DT)
library(readxl)
library(dplyr)
library(raster)
library(sf)
library(rgdal)
library(RColorBrewer)

# ---- Data Import and Preparation ----

# Load LiDAR site database
data <- read_excel("www/DATA_SETS_DATABASE.xlsx") %>%
  mutate(
    Site = suppressWarnings(as.integer(Site))
  )

# Load ecoregion shapefile and ensure UTF-8 encoding
ecoregions <- shapefile("www/spatial/ecoregions_ab.shp",
                       use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = TRUE)
ecoregions$REGION_NAM <- as.factor(ecoregions$REGION_NAM)

# Load Alberta boundary and match CRS to ecoregions
boundry <- shapefile("www/spatial/AB_Boundry.shp",
                    use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = TRUE)
boundry <- spTransform(boundry, CRS(proj4string(ecoregions)))

# Define color palette for ecoregions (limit to 11 levels for colorblind-friendly palette)
factpal <- colorFactor(
  palette = RColorBrewer::brewer.pal(min(length(levels(ecoregions$REGION_NAM)), 11), "Spectral"),
  domain = ecoregions$REGION_NAM
)

# Color palette for DataType column
pal_PLOT <- colorFactor(
  palette = RColorBrewer::brewer.pal(5, "YlGnBu"),
  domain = data$DataType
)
