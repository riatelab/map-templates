##############################
# Data / layers preparation
#############################
library(sf)
library(giscoR)
library(mapsf)

source('eu_parameters.R')
source(file = 'functions.R')

# Europe ----
europe <- main_frame(template = "europe", frame = frame, level = "2", res = "20")

nuts <- europe$units
countries <- europe$neighbours
borders <- europe$borders
cities <- europe$cities


# Import layer with territorial units to be included in the boxes / boxes$target prj must fit with proj of input layers
input <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "2")
countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "4326", 
                                     spatialtype = "LB")


toto <- box_move_and_resize(boxes = boxes, x = input, x_target = nuts) 
boxes <- box_k(boxes = boxes, x = input, x_target = nuts)
