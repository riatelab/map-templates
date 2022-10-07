#######################################################
#  Create insets for France
######################################################

library(sf)
library(mapinsetr)
library(mapsf)
source('utils.R')

mask <- st_read("input/fr/voronoi/mask.geojson")
mask <- st_transform(mask, 2154)


# Position of the boxes in the layout
# Get western and Southern point of France Metro
met <- mask[5,]
box_area <- 100000 # boxes width / height 
box_space <- 20000 

## Guadeloupe
xmin <- as.numeric(st_bbox(met)[1])
xmax <- xmin + box_area
ymax <- 6663290
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
boxes <- st_as_sfc(st_bbox(bb, crs = 2154))
boxes <- st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Guadeloupe"

# Martinique
ymax <- ymin - box_space
ymin <- ymax - box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique"
boxes <- rbind(boxes, xx)

# Guyane
xmax <- xmax + 50000
ymax <- ymin - box_space
ymin <- ymax - box_area - 50000
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 3
xx$name <- "Guyane"
boxes <- rbind(boxes, xx)

# Mayotte
xmin <- 761800
xmax <- xmin + box_area
ymin <- as.numeric(st_bbox(met)[2])
ymax <- ymin + box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 4
xx$name <- "Mayotte"
boxes <- rbind(boxes, xx)

# Réunion
xmin <- xmax + box_space
xmax <- xmin + box_area
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 5
xx$name <- "Réunion"
boxes <- rbind(boxes, xx)

# Set input parameters of box (target in WGS84, local EPSG)
boxes$target <- list(c(-62.05, 15.64, -60.99, 16.71), #xmin, ymin, xmax, ymax
                     c(-61.44, 14.19, -60.6, 15.09),
                     c(-55.5, 1.8, -50.8, 6),
                     c(44.8, -13.2, 45.5, -12.5),
                     c(54.99,-21.61, 56.06,-20.64)
                     
)

boxes$epsg_loc <- c(5490, 5490,  2972,  4471, 2975)
st_geometry(boxes) <- "geometry"
st_crs(boxes) <- 2154


# Communes / create insets
input <- st_read("input/fr/voronoi/com_vor.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}
out <- st_transform(out, 4326)
st_write(out, "output/france/com.geojson")

# EPCI/ create insets
input <- st_read("input/fr/voronoi/epci.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

out <- st_transform(out, 4326)
st_write(out, "output/france/epci.geojson")


# Urban areas / create insets
input <- st_read("input/fr/voronoi/aav.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

out <- st_transform(out, 4326)
st_write(out, "output/france/aav.geojson")


# Employment areas
input <- st_read("input/fr/voronoi/zemp.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

out <- st_transform(out, 4326)
st_write(out, "output/france/zemp.geojson")

# Departments
input <- st_read("input/fr/voronoi/dep.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

out <- st_transform(out, 4326)
st_write(out, "output/france/dep.geojson")

# Regions
input <- st_read("input/fr/voronoi/reg.geojson")
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

out <- st_transform(out, 4326)
st_write(out, "output/france/reg.geojson")


# Neighbourhood / work in progress
countries <- st_read("input/fr/voronoi/neighbors.geojson")
countries <- st_transform(countries, 2154)
country_box <- st_sf(st_sfc())
st_crs(country_box) <- st_crs(2154)

mf_map(country_box)
for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  mask_large <- st_transform(mask, 2154)
  mask_large <- st_as_sfc(st_bbox(mask_large + c(-500000,-500000,500000,500000), crs = 2154))
  st_crs(mask_large) <- 2154
  x <- suppressWarnings(st_intersection(countries, mask_large))
  x <- st_cast(x, "MULTIPOLYGON")
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  if(nrow(inset) > 0){
    country_box <- rbind(country_box, inset)
  }
  country_box <- rbind(country_box, inset)
}
country_box <- aggregate(country_box, 
                         by = list(country_box$adm0_a3),
                         FUN = head, 1)

# Extraction borders
borders <- getBorders(countries)
st_crs(borders) <- 2154
bb <- c(xmin = -605000, ymin = 4288000, xmax = 2909000, ymax = 8008000)
mask <- st_as_sfc(st_bbox(bb, crs = 2154))
borders <- st_intersection(borders, mask)

borders_box <- getBorders(country_box)
st_crs(borders_box) <- 2154

country_box <- st_transform(country_box, 4326)
countries <- st_transform(countries, 4326)
borders <- st_transform(borders, 4326)
borders_box <- st_transform(borders_box, 4326)

st_write(country_box, "output/france/country_box.geojson")
st_write(countries, "output/france/countries.geojson")
st_write(borders, "output/france/borders.geojson")
st_write(borders_box, "output/france/borders_box.geojson")


# Facteurs d'agrandissement / réduction
# Guadeloupe 0.84
# Martinique 0.99
# Guyane 0.28
# Mayotte 1.29
# Réunion 0.89


# mf_map(out)
# mf_map(out)
# 
# 
# for (i in 1 : nrow(boxes)){
#   box <- boxes[i,]
#   bb <- as.vector(unlist(box[,"target"]))
#   bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
#   mask <- st_as_sfc(st_bbox(bb, crs = 4326))
#   mask_large <- st_transform(mask, 3035)
#   mask_large <- st_as_sfc(st_bbox(mask_large + c(-500000,-500000,500000,500000), crs = 3035))
#   st_crs(mask_large) <- 3035
#   input <- suppressWarnings(st_intersection(input_countries, mask_large))
#   input <- st_cast(input, "MULTIPOLYGON")
#   mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
#   input <- st_transform(input, box[,"epsg_loc", drop = T][1])
#   inset <- m_r(x = input, mask = mask,  y = box)
#   if(nrow(inset) > 0){
#     country_box <- rbind(country_box, inset)
#   }
# }

