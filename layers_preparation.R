##############################
# Data / layers preparation
#############################
library(sf)
library(giscoR)
library(mapsf)

source('eu_parameters.R')
source(file = 'functions.R')

# Europe ---- Main frame
europe <- main_frame(template = "europe", frame = frame, level = "2", res = "20")

nuts <- europe$units
countries <- europe$neighbours
borders <- europe$borders
cities <- europe$cities

head(boxes)
# Import layer with territorial units to be included in the boxes / boxes$target prj must fit with proj of input layers
input_nuts <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "2")
input_countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "4326", 
                                     spatialtype = "LB")


nuts <- box_move_and_resize(boxes = boxes, x = input_nuts, x_target = nuts) 
countries_box <- box_move_and_resize(boxes = boxes, x = input_countries,
                                     x_target = countries, x_target_add = FALSE) 
boxes <- box_k(boxes = boxes, x = input_nuts, x_target = nuts)

mf_map(frame, col = "lightblue", border = NA)
mf_map(countries, col = "lightgrey", border = "white", add = TRUE)
mf_map(boxes, col = "lightblue", border = "black", add = TRUE)
mf_map(countries_box, col = "lightgrey", border = "white", add = TRUE)
mf_map(nuts, col = "peachpuff", border = "white", lwd = .25, add = TRUE)


# Extracti borders
borders <- suppressWarnings(st_intersection(st_buffer(countries, 5), 
                                            st_buffer(countries, 5))) 
borders <- st_cast(borders,"MULTILINESTRING")
borders <- borders[borders$CNTR_ID != borders$CNTR_ID.1, ] 
borders <- borders[,(1:length(countries))]


mf_map(input_nuts[input_nuts$NUTS_ID == 'FRY3',])
mf_map(input_countries, add = TRUE, col = NA, border = "red")
head(input_nuts)
