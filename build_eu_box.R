
# Guadeloupe
xmin <- 5900000
xmax <- xmin + 280000

ymax <- 4900000
ymin <- ymax - 280000

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

x <- st_sfc(st_polygon(list(cbind(lon, lat))))
x <- st_as_sf(x)
x$id <- 1
x$name <- "Guadeloupe (FR)"


# Martinique
xmin <- xmax + 43000
xmax <- xmin + 280000
lon <-  c(xmin,xmax,xmax,xmin,xmin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique (FR)"
x <- rbind(x, xx)


# Réunion
xmin <- 5900000
xmax <- xmin + 280000
ymax <- ymin - 43000
ymin <- ymax - 280000

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 3
xx$name <- "Réunion (FR)"
x <- rbind(x, xx)

# Guyane
xmin <- xmax + 43000
xmax <- xmin + 280000
lon <-  c(xmin,xmax,xmax,xmin,xmin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 4
xx$name <- "Guyane (FR)"
x <- rbind(x, xx)

# Mayotte
xmin <- 5900000
xmax <- xmin + 280000
ymax <- ymin - 43000
ymin <- ymax - 280000

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 5
xx$name <- "Mayotte (FR)"
x <- rbind(x, xx)

# Canaries
xmin <- xmax + 43000
xmax <- xmin + 280000
lon <-  c(xmin,xmax,xmax,xmin,xmin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 6
xx$name <- "Canaries (ES)"
x <- rbind(x, xx)

# Madeire
xmin <- 5900000
xmax <- xmin + 280000
ymax <- ymin - 43000
ymin <- ymax - 280000

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 7
xx$name <- "Madeire (PT)"
x <- rbind(x, xx)


# Açores (main)
xmin <- xmax + 43000
xmax <- xmin + 280000
lon <-  c(xmin,xmax,xmax,xmin,xmin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 8
xx$name <- "Açores (PT)"
x <- rbind(x, xx)


# Açores (second)
xmax <- xmin + (.3 * (xmax - xmin))
ymin <- ymax - (.3 * (ymax - ymin))

lon <- c(xmin,xmax,xmax,xmin,xmin)
lat <- c(ymin,ymin,ymax,ymax,ymin)

xx <- st_sfc(st_polygon(list(cbind(lon, lat))))
xx <- st_as_sf(xx)
xx$id <- 9
xx$name <- "Açores (Florès - PT)"
x <- rbind(x, xx)

boxes <- x
boxes$target <- list(c(-61.89, 15.8, -61.15, 16.55),
                     c(-61.28, 14.35, -60.76, 14.93),
                     c(54.8,-21.5,56.3,-20.5),
                     c(-66,-6.3,-39,12.5), 
                     c(45, -13.02, 45.28, -12.65),
                     c(-18.4, 27.4,- 13.3, 30.3),
                     c(-17.35, 32.55, -16.2, 33.2),
                     c(-28.088, 36.662, -24.576, 39.22),
                     c(-31.4, 39.3, -30.9, 39.8))
                     
boxes$epsg_loc <- c(5490, 5490, 2975, 2972, 4471, 3035, 2191, 3063, 3063)