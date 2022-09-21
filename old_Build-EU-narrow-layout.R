# BUILD ESPON MAPKIT NARROW

library("mapinsetr")

setwd("/home/nlambert/Documents/ESPON/ESPON-MapKits-2016/Delivery_20170511/prg")

# Sources

source("sources/BuildEmptyTemplate.R")

# Parameters
d <- 20000
prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
xmin <- 1930663
ymin <- 1189404 
#xmax <- 6702156
xmax <- 6627156 - d
ymax <- 5456938


folder <- "../Mapkits/EU-narrow/shp"
name <- "layout"
  
template.spdf <- buildTemplate(xmin, xmax, ymin, ymax, prj)

# Add boxes
boxes.spdf <- readOGR(dsn = "../input/boxes", layer = "boxes_narrow", verbose = TRUE)
boxes.spdf<- raster::shift(boxes.spdf, -75000 - d, 0)

template.spdf <- inset_rbinder(l = list(template.spdf,boxes.spdf))

plot(template.spdf)

# export
folder <- "../Mapkits/EU-narrow/shp/layout"
dir.create(folder)
writeOGR(obj=template.spdf, dsn=folder, layer="template", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)





