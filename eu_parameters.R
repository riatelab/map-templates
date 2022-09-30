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
frame

# Boxes ----
# Global parameters of boxes (2 columns * 4 boxes regularly spaced) ----
box_area <- 280000 # boxes width / height 
x_min <- 5900000 # Xmin for first column of all boxes
y_max <- 5100000 # Ymax (top of boxes)
box_space <- 43000 # Space between boxes


## Canaries
xmin <- x_min
xmax <- xmin + (box_area * 2) + box_space
ymax <- y_max
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
boxes <- st_as_sfc(st_bbox(bb, crs = 3035))
boxes <- st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Canaries (ES)"

## Madeire ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 2
xx$name <- "Madeire (PT)"
boxes <- rbind(boxes, xx)

## Açores (main) ----
xmin <- xmax + box_space
xmax <- xmin + box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 3
xx$name <- "Açores (PT)"
boxes <- rbind(boxes, xx)

## Açores (second) ----
xmax <- xmin + (.3 * (xmax - xmin))
ymin2 <- ymax - (.3 * (ymax - ymin))
bb <- c(xmin = xmin, ymin = ymin2, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 4
xx$name <- "Açores (Florès - PT)"
boxes <- rbind(boxes, xx)


## Guadeloupe ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 5
xx$name <- "Guadeloupe (FR)"
boxes <- rbind(boxes, xx)

## Martinique ----
xmin <- xmax + box_space
xmax <- xmin + box_area 
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 6
xx$name <- "Martinique (FR)"
boxes <- rbind(boxes, xx)

## Réunion ----
xmin <- x_min
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 7
xx$name <- "Réunion (FR)"
boxes <- rbind(boxes, xx)

## Guyane ----
xmin <- xmax + box_space
xmax <- xmin + box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 8
xx$name <- "Guyane (FR)"
boxes <- rbind(boxes, xx)

## Mayotte ----
xmin <- x_min + (box_area / 2) 
xmax <- xmin + box_area
ymax <- ymin - box_space
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 3035))
xx <- st_as_sf(xx)
xx$id <- 9
xx$name <- "Mayotte (FR)"
boxes <- rbind(boxes, xx)

# Set input parameters of box (target in WGS84, local EPSG)
boxes$target <- list(c(-18.4, 27.4,- 13.3, 29.5),
                     c(-17.35, 32.55, -16.2, 33.2),
                     c(-28.9, 36.8, -24.8, 40.2),
                     c(-31.4, 39.3, -30.9, 39.8),
                     c(-62.05, 15.64, -60.99, 16.71), #xmin, ymin, xmax, ymax
                     c(-61.44, 14.19, -60.6, 15.09),
                     c(54.99,-21.61, 56.06,-20.64),
                     c(-55.5, 1.8, -50.8, 6), 
                     c(44.5, -13.5, 45.8, -12.2)
)
                     
boxes$epsg_loc <- c(3035, 2191, 3063, 3063, 5490, 5490, 2975, 2972, 4471)
st_geometry(boxes) <- "geometry"
st_crs(boxes) <- 3035

rm(list=ls()[! ls() %in% c("boxes","frame")])
ls()