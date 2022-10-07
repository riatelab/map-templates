#######################################################
#  Preparation Voronoi layers and aggregation France
######################################################

# Obj: From Municipalities centroids and generalized small scale Natural Earth layer,
# Create Voronoi Layer including all territories (buffer area for points falling out of Natural Eearth layer),

library(sf)
library(mapsf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rmapshaper)
library(readxl)
#remotes::install_github("ropensci/rnaturalearthhires")


# 1 - Preparing Municipalities centroids ----

#  Extracting centroids from municipal layer 2022 (too large file for github)
# com <- st_read("input/fr/COMMUNE.shp") # INPUT DATA (not saved) COMMUNE 2022 from IGN
# com <- st_transform(com, 2154)
# com$SUPERFICIE <- as.numeric(st_area(com) / 1000000)
# com <- st_centroid(com)
#st_write(com, "input/fr/COMMUNE.shp", delete_layer =  TRUE)
com <- st_read("input/fr/COMMUNE.shp")

# Arrondissements for Paris, Lyon, Marseille
arr <- st_read("input/fr/ARRONDISSEMENT_MUNICIPAL.shp")
arr <- st_transform(arr, 2154)
arr$SUPERFICIE <- as.numeric(st_area(arr) / 1000000)
arr <- merge(arr, com[,c("INSEE_COM", "SIREN_EPCI"), drop = TRUE], by = "INSEE_COM", all.x = TRUE)
arr$INSEE_COM <- NULL
colnames(arr)[4] <- "INSEE_COM"
arr$INSEE_DEP <- substr(arr$INSEE_COM, 1, 2)
arr <- st_centroid(arr)
st_geometry(arr[arr$INSEE_COM == "75112", ]) <-  st_sfc(st_point(c(655101.1, 6860266))) # Moving Point to municipal town-hall to have a single territory when aggregating in Paris

var <- c("INSEE_COM", "NOM", "POPULATION", "SUPERFICIE", "INSEE_DEP", "SIREN_EPCI")
com <- rbind(com[,var], arr[,var])

# Delete Paris, Lyon and Marseille from municipal layer
com <- com[!com$INSEE_COM %in% c("75056", "13055", "69123"),]
com <- com[order(com$INSEE_COM),]


# 2 - Country layer : Natural Earth, small scale, simplified  ----
countries <- ne_countries(scale = 10, returnclass = "sf")
countries <- ms_simplify(countries, keep = .4)
countries <- countries[,c("adm0_a3", "name")]

# Extract French territories for mask-box purpose
fr <- countries[countries$adm0_a3 == "FRA",]
fr <- st_cast(fr, "POLYGON")
fr$id <- seq(from = 1, to = nrow(fr), by =1)
fr_df <- st_set_geometry(fr, NULL)

fr_df[1,c(1:2)] <- c("GUY", "Guyane")
fr_df[2,c(1:2)] <-  c("MET", "France Métropolitaine")
fr_df[3,c(1:2)] <- c("MAR", "Martinique")
fr_df[4,c(1:2)] <- c("GUA", "Guadeloupe")
fr_df[5,c(1:2)] <- c("GUA", "Guadeloupe")
fr_df[6,c(1:2)] <- c("GUA", "Guadeloupe")
fr_df[7,c(1:2)] <- c("GUA", "Guadeloupe")
fr_df[8,c(1:2)] <- c("REU", "La Réunion")
fr_df[9,c(1:2)] <- c("MAY", "Mayotte")
fr_df[10,c(1:2)] <- c("MAY", "Mayotte")
fr_df[11,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[12,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[13,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[14,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[15,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[16,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[17,c(1:2)] <- c("MET", "France Métropolitaine")
fr_df[18,c(1:2)] <- c("MET", "France Métropolitaine")

fr <- merge(fr[,"id"], fr_df, by = "id")
fr <- aggregate(fr[,"adm0_a3"], by = list(fr$adm0_a3), FUN = head, 1)
fr <- st_transform(fr, 4326)
fr <- st_cast(fr, "MULTIPOLYGON")

st_write(fr, "input/fr/voronoi/mask.geojson")


# 3 - Create Voronoi ----
# When the point falls out of French boundaries, create a buffer around the point
# and join it to the original layer
voronoi <- function(x, var, proj, cent, buffer){
  x <- st_transform(x, proj)
  c <- st_transform(cent, proj)
  st_agr(x) = "constant"
  st_agr(c) = "constant"
  out <- st_difference(c, x, sparse = FALSE)
  st_agr(out) <- "contant"
  if(nrow(out) > 0){
    out <- st_buffer(out, buffer)
    out <- st_union(x, out)
    out <- aggregate(out, by = list(out[[var]]), FUN = head, 1)
  }
  else{
    out <- x
  }
  box <- st_as_sfc(st_bbox(x, crs = proj))
  v <- st_voronoi(st_union(c), box)
  v <- st_as_sf(st_intersection(st_cast(v), out))
  v <- st_join(st_sf(v), y = c, join = st_intersects)
  v <- st_cast(v, "MULTIPOLYGON")
  return(v)
}

# Voronoi creation for outermost territories and France
gua <- voronoi(x =  fr[fr$adm0_a3 == "GUA",], cent = com[com$INSEE_DEP == "971",], 
               var = "adm0_a3", proj = 2154, buffer = 1000)
mar <- voronoi(x =  fr[fr$adm0_a3 == "MAR",], cent = com[com$INSEE_DEP == "972",], 
               var = "adm0_a3", proj = 2154, buffer = 1000)
guy <- voronoi(x =  fr[fr$adm0_a3 == "GUY",], cent = com[com$INSEE_DEP == "973",], 
               var = "adm0_a3", proj = 2154, buffer = 1000)
reu <- voronoi(x =  fr[fr$adm0_a3 == "REU",], cent = com[com$INSEE_DEP == "974",], 
               var = "adm0_a3", proj = 2154, buffer = 1000)
may <- voronoi(x =  fr[fr$adm0_a3 == "MAY",], cent = com[com$INSEE_DEP == "976",], 
               var = "adm0_a3", proj = 2154, buffer = 1000)
met <- voronoi(x =  fr[fr$adm0_a3 == "MET",], 
               cent = com[!com$INSEE_DEP %in% c("971", "972", "973", "974", "976"),], 
               var = "adm0_a3",  proj = 2154, buffer = 1000)
com <- rbind(met, gua, mar, guy, reu, may) # Attach all layers
com <- st_transform(com, 4326)
com$INSEE_DEP <- NULL
com$SIREN_EPCI <- NULL
st_write(com, dsn = "input/fr/voronoi/com_vor.geojson")


# 4 - Rebuild national boundaries with the union of previous layers, and build neighbouring countries layer 
fra <- st_union(com, by_feature = FALSE)
inter <- countries[countries$adm0_a3 != 'FRA',]
inter <- st_difference(inter, fra) %>% st_collection_extract("POLYGON")
inter <- st_cast(inter, "MULTIPOLYGON")

fra <- st_as_sf(fra)
fra$adm0_a3 <- "FRA"
fra$name <- "France"
st_geometry(fra) <- "geometry"
countries <- rbind(inter, fra)
st_write(countries, dsn = "input/fr/voronoi/countries.geojson")

# Select countries to appear in the map template
sel <- c("BEL", "LUX", "DEU", "NLD", "CHE", "ITA", "ESP", "GBR", "JEY", "GGY", "IRL",
         "AUT", "CZE", "PRT", "SVN", "DMA", "LCA", "GUY", "SUR", "BRA", "FRA")

sel <- countries[countries$adm0_a3 %in% sel,]
st_write(sel, dsn = "input/fr/neighbors.geojson")


# 5 - Aggregate EPCI, ZEMP, UU, REG, EPCI, DEP ----
zon <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                            skip = 5, sheet = "COM")) 
zon2 <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                            skip = 5, sheet = "ARM")) 

sel <- c("CODGEO", "DEP", "REG", "EPCI", "ZE2020", "AAV2020", "TAAV2017")
zon <- rbind(zon[,sel], zon2[,sel])

zon_name <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                                 skip = 5, sheet = "Zones_supra_communales")) 


com <- merge(com, zon, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)


# Départements
dep <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$DEP),
                 FUN = sum)
colnames(dep)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "DEP",]
dep <- merge(dep, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
dep <- st_cast(dep, "MULTIPOLYGON")
st_write(dep, dsn = "input/fr/voronoi/dep.geojson")

# Régions
reg <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$REG),
                 FUN = sum)
colnames(reg)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "REG",]
reg <- merge(reg, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
reg <- st_cast(reg, "MULTIPOLYGON")
st_write(reg, dsn = "input/fr/voronoi/reg.geojson")


# EPCI
epci <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$EPCI),
                 FUN = sum)
colnames(epci)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "EPCI",]
epci <- merge(epci, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
epci <- st_cast(epci, "MULTIPOLYGON")
st_write(epci, dsn = "input/fr/voronoi/epci.geojson")

# ZEMP
zemp <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                  by = list(com$ZE2020),
                  FUN = sum)
colnames(zemp)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "ZE2020",]
zemp <- merge(zemp, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
              all.x = TRUE)
zemp <- st_cast(zemp, "MULTIPOLYGON")
st_write(zemp, dsn = "input/fr/voronoi/zemp.geojson")

# AAV
com_urb <- com[com$AAV2020 != "000",]
aav <- aggregate(com_urb[,c("POPULATION", "SUPERFICIE")],
                by = list(com_urb$AAV2020),
                FUN = sum)
colnames(aav)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "AAV2020",]
aav <- merge(aav, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
              all.x = TRUE)
aav <- st_cast(aav, "MULTIPOLYGON")

zon <- aggregate(com_urb[,c("AAV2020", "TAAV2017")],
                  by = list(com_urb$AAV2020),
                  FUN = head, 1)
zon <- st_set_geometry(zon, NULL)
aav <- merge(aav, zon[,c("AAV2020", "TAAV2017")], by.x = "CODGEO", by.y = "AAV2020",
            all.x = TRUE)
st_write(aav, dsn = "input/fr/voronoi/aav.geojson")