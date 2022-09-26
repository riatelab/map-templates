##############################
# EU TEMPLATE FRAMES
#############################
library(sf)

# Main frame ----
xmin <- 2380000
xmax <- 6550000
ymin <- 1350000
ymax <- 5420000

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

frame <- st_sfc(st_polygon(list(cbind(lon, lat))))
frame <- st_as_sf(frame)
st_crs(frame) <- 3035


# Boxes ----
# Global parameters of boxes (2 columns * 4 boxes regularly spaced) ----
box_area <- 280000 # boxes width / height 
x_min <- 5900000 # Xmin for first column of all boxes
y_max <- 4900000 # Ymax (top of boxes)
box_space <- 43000 # Space between boxes

## Guadeloupe ----
xmin <- x_min # Xmin for first column of all boxes
xmax <- xmin + box_area  # Space between 
ymax <- y_max
ymin <- ymax - box_area 
lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)
boxes <- st_sfc(st_polygon(list(cbind(lon, lat))))
boxes <- st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Guadeloupe (FR)"

## Martinique ----
xmin <- xmax + box_space
xmax <- xmin + box_area 
lon <-  c(xmin,xmax,xmax,xmin,xmin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique (FR)"
boxes <- rbind(boxes, xx)

## Réunion ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 3
xx$name <- "Réunion (FR)"
boxes <- rbind(boxes, xx)

## Guyane ----
xmin <- xmax + box_space
xmax <- xmin + box_area
lon <-  c(xmin,xmax,xmax,xmin,xmin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 4
xx$name <- "Guyane (FR)"
boxes <- rbind(boxes, xx)

## Mayotte ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 5
xx$name <- "Mayotte (FR)"
boxes <- rbind(boxes, xx)

## Canaries ----
xmin <- xmax + box_space
xmax <- xmin + box_area
lon <-  c(xmin,xmax,xmax,xmin,xmin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 6
xx$name <- "Canaries (ES)"
boxes <- rbind(boxes, xx)

## Madeire ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 7
xx$name <- "Madeire (PT)"
boxes <- rbind(boxes, xx)

## Açores (main) ----
xmin <- xmax + box_space
xmax <- xmin + box_area
lon <-  c(xmin,xmax,xmax,xmin,xmin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 8
xx$name <- "Açores (PT)"
boxes <- rbind(boxes, xx)

## Açores (second) ----
xmax <- xmin + (.3 * (xmax - xmin))
ymin <- ymax - (.3 * (ymax - ymin))
lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)
xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 9
xx$name <- "Açores (Florès - PT)"
boxes <- rbind(boxes, xx)


# Set input parameters of box (target in WGS84, local EPSG)
boxes$target <- list(c(-61.89, 15.8, -61.15, 16.55), #xmin, ymin, xmax, ymax
                     c(-61.28, 14.35, -60.76, 14.93),
                     c(55.15,-21.45, 55.9,-20.8),
                     c(-55.5, 1.8, -50.8, 6), 
                     c(44.5, -13.5, 45.8, -12.2),
                     c(-18.4, 27.4,- 13.3, 30.3),
                     c(-17.35, 32.55, -16.2, 33.2),
                     c(-28.9, 36.8, -24.8, 39.9),
                     c(-31.4, 39.3, -30.9, 39.8))
                     
boxes$epsg_loc <- c(5490, 5490, 2975, 2972, 4471, 3035, 2191, 3063, 3063)
st_geometry(boxes) <- "geometry"
rm(list=ls()[! ls() %in% c("boxes","frame")])
ls()