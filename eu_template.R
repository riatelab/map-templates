##############################
# Data / layers preparation
#############################
library(sf)
library(giscoR)
library(mapsf)
#devtools::install_github("riatelab/mapinsetr")
library(mapinsetr)

source('eu_parameters.R')
head(frame)
# Import last version of cities
cities <- st_read("input/eu/URAU_LB_2021_4326_CITIES.geojson")
fua <- st_read("input/eu/URAU_LB_2021_4326_FUA.geojson")
urban_audit <- rbind(cities, fua)
urban_audit <- st_transform(urban_audit, 3035)

# Main frame ----
main_frame <- function(template, frame, level, up_units = NULL, res){
  if(template == "europe"){
    units <- gisco_get_nuts(year = "2021", epsg = "3035", resolution = res, 
                            nuts_level = level)
    
    neighbours <- gisco_get_countries(year = "2020", epsg = "3035", 
                                      resolution = res)
    
    
    nuts <- suppressWarnings(st_intersection(units, st_geometry(frame)))
    countries <- suppressWarnings(st_intersection(neighbours, st_geometry(frame)))
    urban_audit <- suppressWarnings(st_intersection(urban_audit, st_geometry(frame)))
    
    return(list("units" = nuts, "neighbours" = countries, "cities" = urban_audit))
  }
}  


# Europe ----
## Main frame ----
europe <- main_frame(template = "europe", frame = frame, level = "2", res = "20")

nuts2 <- europe$units
countries <- europe$neighbours
cities <- europe$cities
europe <- main_frame(template = "europe", frame = frame, level = "3", res = "20")
nuts3 <- europe$units

## Create mask polygon from input parameters ----
# NUTS 3
input_nuts <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "3")
for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input <- st_transform(input_nuts, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = input, mask = mask,  y = box, verbose = TRUE)
  nuts3 <- rbind(nuts3, inset)
}

# NUTS 2
input_nuts <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "2")
for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input <- st_transform(input_nuts, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = input, mask = mask,  y = box, verbose = TRUE)
  nuts2 <- rbind(nuts2, inset)
}

# NEIGHBOURING TERRITORIES IN THE BOW
input_countries <- gisco_get_countries(year = "2020", epsg = "3035", resolution = "20")
country_box <- st_sf(st_sfc())
st_crs(country_box) <- st_crs(frame)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask_large <- st_transform(mask, 3035)
  mask_large <- st_as_sfc(st_bbox(mask_large + c(-500000,-500000,500000,500000), crs = 3035))
  st_crs(mask_large) <- 3035
  input <- suppressWarnings(st_intersection(input_countries, mask_large))
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input <- st_transform(input, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = input, mask = mask,  y = box, verbose = TRUE)
  if(nrow(inset) > 0){
  country_box <- rbind(country_box, inset)
  }
}

# URBAN AUDIT
urban_audit <- st_transform(urban_audit, 4326)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
  lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
  mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
  st_crs(mask) <- 4326
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  input <- st_transform(urban_audit, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = input, mask = mask,  y = box, verbose = TRUE)
  cities <- rbind(cities, inset)
}

# Extraction borders
borders <- suppressWarnings(st_intersection(st_buffer(countries, 5), 
                                            st_buffer(countries, 5))) 
borders <- st_cast(borders,"MULTILINESTRING")
borders <- borders[borders$CNTR_ID != borders$CNTR_ID.1, ] 
borders <- borders[,(1:length(countries))]
borders_box <- suppressWarnings(st_intersection(st_buffer(country_box, 5), 
                                            st_buffer(country_box, 5))) 
borders_box <- st_cast(borders_box,"MULTILINESTRING")
borders_box <- borders_box[borders_box$CNTR_ID != borders_box$CNTR_ID.1, ] 
borders_box <- borders_box[,(1:length(country_box))]

# Feed regional layers with Eurostat data (POP * AREA) ----
library(eurostat)
library(reshape2)
df <- get_eurostat("reg_area3", time_format = "num") # Telecharger la table ESTAT
levels(as.factor(df$landuse))
df <- df[df$landuse == "TOTAL",]
df <- dcast(df, geo ~ time, value.var = "values") # Redimensionnement de la table au format geo * time
colnames(df) <- paste0("AREA_", colnames(df))
df[,(length(df))] <- ifelse(is.na(df[,(length(df))]), df[,(length(df)-1)], df[,(length(df))])
df[,(length(df))] <- ifelse(is.na(df[,(length(df))]), df[,(length(df)-2)], df[,(length(df))])

nuts3 <- merge(nuts3, df[,c("AREA_geo", "AREA_2021")], by.x = "NUTS_ID", by.y = "AREA_geo", all.x = TRUE)
nuts2 <- merge(nuts2, df[,c("AREA_geo", "AREA_2021")], by.x = "NUTS_ID", by.y = "AREA_geo", all.x = TRUE)


df <- get_eurostat("demo_r_pjanaggr3", time_format = "num") 
df <- df[df$sex == "T",]
df <- df[df$age == "TOTAL",]
df <- dcast(df, geo ~ time, value.var = "values") 
colnames(df) <- paste0("POP_", colnames(df))
df[,(length(df))] <- ifelse(is.na(df[,(length(df))]), df[,(length(df)-1)], df[,(length(df))])
df[,(length(df))] <- ifelse(is.na(df[,(length(df))]), df[,(length(df)-2)], df[,(length(df))])

nuts3 <- merge(nuts3, df[,c("POP_geo", "POP_2021")], by.x = "NUTS_ID", by.y = "POP_geo", all.x = TRUE)
nuts2 <- merge(nuts2, df[,c("POP_geo", "POP_2021")], by.x = "NUTS_ID", by.y = "POP_geo", all.x = TRUE)
head(nuts3)
varsel <- c("NUTS_ID", "NAME_LATN", "URBN_TYPE", "MOUNT_TYPE", "COAST_TYPE", "POP_2021", "AREA_2021") 
nuts3 <- nuts3[,varsel]
nuts2 <- nuts2[,varsel]

nuts3$DENS_2021 <- nuts3$POP_2021 / nuts3$AREA_2021
nuts2$DENS_2021 <- nuts2$POP_2021 / nuts2$AREA_2021

# Feed city layers with Eurostat data (POP * AREA) ----
## Clean city layer
cities$AREA <- cities$AREA_SQM / 1000000
cities <- cities[,c("URAU_CODE", "URAU_NAME", "CITY_CPTL", "URAU_CATG", "AREA")]

fua <- cities[cities$URAU_CATG == "F",]
cities <- cities[cities$URAU_CATG == "C",]
fua$URAU_CATG <- NULL
cities$URAU_CATG <- NULL

## Add population to cities layer
df <- get_eurostat("urb_cpop1", time_format = "num") 
df <- df[df$indic_ur == "DE1001V",]
df <- dcast(df, cities ~ time, value.var = "values") 
tmp <- df
df$POP <- df[cbind(1:nrow(df), max.col(!is.na(df), ties.method = 'last'))]
tmp$POP_YEAR <- substr(colnames(tmp[max.col(!is.na(tmp), 'last')]), 1, 4)
df <- merge(df, tmp[,c("cities", "POP_YEAR")], by = "cities", all.x = TRUE)
df$POP <- as.numeric(df$POP)

cities <- merge(cities, df[,c("cities", "POP", "POP_YEAR")], by.x = "URAU_CODE", by.y = "cities", all.x = TRUE)
cities$DENS <- cities$POP / cities$AREA

## Add population to FUA layer
df <- get_eurostat("urb_lpop1", time_format = "num") 
df <- df[df$indic_ur == "DE1001V",]
df <- dcast(df, cities ~ time, value.var = "values") 
tmp <- df
df$POP <- df[cbind(1:nrow(df), max.col(!is.na(df), ties.method = 'last'))]
tmp$POP_YEAR <- substr(colnames(tmp[max.col(!is.na(tmp), 'last')]), 1, 4)
df <- merge(df, tmp[,c("cities", "POP_YEAR")], by = "cities", all.x = TRUE)
df$POP <- as.numeric(df$POP)
fua <- merge(fua, df[,c("cities", "POP", "POP_YEAR")], by.x = "URAU_CODE", by.y = "cities", all.x = TRUE)
fua$DENS <- fua$POP / fua$AREA

boxes$target <- NULL

# Export layers
st_write(frame, "output/europe/frame.shp", options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(countries, "output/europe/countries.geojson", options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(boxes, "output/europe/boxes.geojson", delete_layer = TRUE)
st_write(country_box, "output/europe/country_boxes.geojson", options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(borders, "output/europe/borders.geojson", delete_layer = TRUE)
st_write(borders_box, "output/europe/borders_boxes.geojson", options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(nuts2, "output/europe/nuts2.geojson", delete_layer = TRUE)
st_write(nuts3, "output/europe/nuts3 .geojson", options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(cities, "output/europe/cities.geojson", delete_layer = TRUE)
st_write(fua, "output/europe/fua .geojson", options = "ENCODING=UTF-8", delete_layer = TRUE)


# Check layers
mf_map(frame, col = "lightblue", border = NA)
mf_map(countries, col = "lightgrey", border = NA, add = TRUE)
mf_map(boxes, col = "lightblue", border = NA, add = TRUE)
mf_map(country_box, col = "lightgrey", border = NA, add = TRUE)
mf_map(nuts3, col = "peachpuff", border = "white", lwd = .25, add = TRUE)
mf_map(borders, col = "white", lwd = .5, add = TRUE)
mf_map(borders_box, col = "white", lwd = .5, add = TRUE)
mf_map(cities, col = "red", pch = 21, cex = .8, add = TRUE)
mf_map(boxes, col = NA, add = TRUE)