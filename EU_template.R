#########################
# European template
#########################


#devtools::install_github("riatelab/mapinsetr")
library(sf)
library(giscoR)
library(mapsf)
library(mapinsetr)

frame <- st_read("input/eu/frame.geojson")
frame <- st_transform(frame, 3035)

# Main frame ----
main_frame <- function(template, frame, level, up_units = NULL, res){
  if(template == "europe"){
    units <- gisco_get_nuts(year = "2021", epsg = "3035", resolution = res, 
                           nuts_level = level)
    
    neigbours <- gisco_get_countries(year = "2020", epsg = "3035", 
                                     resolution = res)
    
    urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "3035", 
                                         spatialtype = "LB")
    
    st_agr(nuts) = "constant"
    st_agr(countries) = "constant"
    st_agr(frame) = "constant"
    
    nuts <- st_intersection(nuts, frame)
    countries <- st_intersection(countries, frame)
    urban_audit <- st_intersection(urban_audit, frame)
    
    # Get borders
    borders <- st_intersection(st_buffer(countries, 5), st_buffer(countries, 5)) 
    borders <- st_cast(borders,"MULTILINESTRING")
    borders <- borders[borders$CNTR_ID != borders$CNTR_ID.1, ] 
    borders <- borders[,(1:length(countries))]
    
    return(list("units" = nuts, "neighbours" = countries, "borders" = borders,
           "cities" = urban_audit))
  }
}  
  

# Test
europe <- main_frame(template = "europe", frame = frame, level = "2", res = "20")

nuts <- europe$units
countries <- europe$neighbours
borders <- europe$borders
cities <- europe$cities

# Plot
mf_map(frame, col = "lightblue")
mf_map(countries, col = "grey", border = NA, add = TRUE)
mf_map(nuts, col = "peachpuff", border = "grey", add = TRUE)
mf_map(borders, col = "white", lwd = 1, add = TRUE)
mf_map(cities, pch = 21, bg = "red", cex = .6, add = TRUE)









boxes <- st_read("input/eu/boxes.geojson")

boxes <- st_transform(boxes, 3035)
boxes$name <- c("Madeire", "Mayotte", "Martinique", "Guadeloupe", "Canaries",
                "Açores - Flores", "Réunion", "Guyane", "Açores")
boxes$target <- list(c(-17.36,32.3,-16.17,33.1), # OK
                     c(44.95,-13.03,45.357,-12.59), # OK
                     c(-61.28,14.32,-60.76,14.93), 
                     c(-61.89,15.738,-60.86,16.602),
                     c(-20, 25.4,- 10, 30.3),
                     c(-31.397,39.215,-30.898,39.907),
                     c(54.8,-21.5,56.3,-20.5),
                     c(-66,-6.3,-39,12.5),
                     c(-28.088, 36.662, -24.576, 39.22))
boxes$epsg_loc <- c(2191, 4471, 5490, 5490, 3035, 3063, 2975, 2972, 3063)
boxes$res <- c("20", "20")
boxes$k <- c(2, 5, 3.5, 4.2, 0.72, 2, 4.5, 0.85, 2)

epsg_out <- st_crs(frame)

nuts <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "10", nuts_level = "2")
countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "3")


st_agr(nuts) = "constant"
st_agr(countries) = "constant"

tmp <- boxes[3,]
bbox <- st_bbox(tmp)

x <- nuts
prj <- st_crs(bbox)

# Create mask polygon from input parameters
lon <- as.vector(unlist(tmp[,"target"])[c(1,3,3,1,1)])
lat <- as.vector(unlist(tmp[,"target"])[c(2,2,4,4,2)])
mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
st_crs(mask) <- st_crs(nuts)

# names order mngmt
namesorder <- names(x)

# multipolygon mgmt
cp <- class(st_geometry(nuts))[1]=="sfc_MULTIPOLYGON"

# Intersect with mask
x <- suppressWarnings(st_intersection(nuts, st_geometry(mask))) 

# add mask to x
xm <- x[1, ]
st_geometry(xm) <- st_geometry(mask)
x <- rbind(xm,x)

# Transform 
x <- st_transform(x, tmp$epsg_loc)

# resize & move
k <- as.vector(st_set_geometry(tmp[,"k"], NULL))
xy <- c(bbox[1], bbox[2])

cntrd <- st_centroid(st_combine(x))
xg <- (st_geometry(x) - cntrd) * k + cntrd[[1]][] # Resize
st_geometry(x) <- xg + xy - st_bbox(xg)[1:2]


# get rid of mask
x <- x[-1,]

if (cp){x <- st_cast(x, "MULTIPOLYGON")}
st_crs(x) <- prj


# names order mngmt
x <- x[, namesorder]


mf_map(frame)

mf_map(boxes,  add = TRUE)
mf_map(x, add = TRUE)

mf_map(boxes[9,], col = "red", add = TRUE)
mf_map(xg, add = TRUE)
xg
