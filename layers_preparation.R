##############################
# Data / layers preparation
#############################
library(sf)
library(giscoR)
library(mapsf)

#require(devtools)
#devtools::install_github("riatelab/mapinsetr")
library(mapinsetr)

source('eu_parameters.R')
source(file = 'functions.R')

# Europe ---- Main frame
europe <- main_frame(template = "europe", frame = frame, level = "2", res = "20")

nuts <- europe$units
countries <- europe$neighbours
borders <- europe$borders
cities <- europe$cities

# Import layer with territorial units to be included in the boxes / boxes$target prj must fit with proj of input layers
input_nuts <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "2")
input_countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "4326", 
                                     spatialtype = "LB")



boxes
# Separate 

mf_map(frame, col = "lightblue", border = NA)
mf_map(countries, col = "lightgrey", border = "white", add = TRUE)
mf_map(boxes, col = "lightblue", border = NA, add = TRUE)
mf_map(countries_box, col = "lightgrey", border = NA, add = TRUE)
mf_map(nuts, col = "peachpuff", border = "white", lwd = .25, add = TRUE)
mf_map(cities, col = "red", pch = 21, cex = .8, add = TRUE)

head(cities)
mf_map(boxes, col = NA, border = "black", add = TRUE)


# Extracti borders
borders <- suppressWarnings(st_intersection(st_buffer(countries, 5), 
                                            st_buffer(countries, 5))) 
borders <- st_cast(borders,"MULTILINESTRING")
borders <- borders[borders$CNTR_ID != borders$CNTR_ID.1, ] 
borders <- borders[,(1:length(countries))]


mf_map(input_nuts[input_nuts$NUTS_ID == 'FRY3',])
mf_map(input_countries, add = TRUE, col = NA, border = "red")


st_crs(boxes) <- 3035

i <- 1
box <- boxes[i,]

# Create mask polygon from input parameters

# NUTS
for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input_nuts <- st_transform(input_nuts, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = input_nuts, mask = mask,  y = box, verbose = TRUE)
  nuts <- rbind(nuts, inset)
}

# URBAN AUDIT
input_countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
country_box <- st_sf(st_sfc())
st_crs(country_box) <- st_crs(frame)
boxes
i <- 5

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input_countries <- st_transform(input_countries, box[,"epsg_loc", drop = T][1])
  input_countries <- st_make_valid(input_countries)
  head(mask)
  inset <- m_r(x = input_countries, mask = mask,  y = box, verbose = TRUE)
  if(nrow(inset) > 0){
  country_box <- rbind(country_box, inset)
  }
}

input_countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
input_countries <- st_transform(input_countries, 4622)
input_countries <- input_countries[input_countries$ISO3_CODE != "RUS",]
input_countries <- st_make_valid(input_countries)
input_countries <- st_is_valid(input_countries)

input_countries$cnt2 = stringr::str_count(input_countries$geometry, ",")
input_countries <- input_countries[input_countries$cnt2 > 1,]

mf_map(input_countries)

st_is_valid(input_countries)
head(input_countries)

st_is_valid(input_countries)

input_countries <- input_countries %>% st_cast()
mf_map(nuts)
mf_map(input_countries)
head(input_countries)
