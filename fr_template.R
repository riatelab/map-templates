###############
# 
###############

library(sf)
library(mapsf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rmapshaper)
remotes::install_github("ropensci/rnaturalearthhires")


# 1 - Source 1 : Municipalités (centroides) ----

#  Extraction centroides et labels départements
# com <- st_read("input/fr/COMMUNE.shp") # INPUT DATA (not saved) COMMUNE 2022 from IGN
# com <- st_transform(com, 2154)
# com$SUPERFICIE <- as.numeric(st_area(com) / 1000000)
# com <- st_centroid(com)
#st_write(com, "input/fr/COMMUNE.shp", delete_layer =  TRUE)
com <- st_read("input/fr/COMMUNE.shp")

# Arrondissements pour Paris, Lyon, Marseille
arr <- st_read("input/fr/ARRONDISSEMENT_MUNICIPAL.shp")
arr <- st_transform(arr, 2154)
arr$SUPERFICIE <- as.numeric(st_area(arr) / 1000000)
arr <- merge(arr, com[,c("INSEE_COM", "SIREN_EPCI"), drop = TRUE], by = "INSEE_COM", all.x = TRUE)
arr$INSEE_COM <- NULL
colnames(arr)[4] <- "INSEE_COM"
arr$INSEE_DEP <- substr(arr$INSEE_COM, 1, 2)
arr <- st_centroid(arr)
st_geometry(arr[arr$INSEE_COM == "75112", ]) <-  st_sfc(st_point(c(655101.1, 6860266)))

var <- c("INSEE_COM", "NOM", "POPULATION", "SUPERFICIE", "INSEE_DEP", "SIREN_EPCI")
com <- rbind(com[,var], arr[,var])

# Supression communes Paris, Lyon, Marseille
com <- com[!com$INSEE_COM %in% c("75056", "13055", "69123"),]
com <- com[order(com$INSEE_COM),]


# 2 - Masque de destination : Natural Earth ----
countries <- ne_countries(scale = 10, returnclass = "sf")
countries <- ms_simplify(countries, keep = .4)
countries <- countries[,c("adm0_a3", "name")]



# Extraction des territoires français pour calcul de Masque Voronoi
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


# Create Voronoi ----

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


com <- rbind(met, gua, mar, guy, reu, may)
com <- st_transform(com, 4326)

com$INSEE_DEP <- NULL
com$SIREN_EPCI <- NULL
st_write(com, dsn = "output/france/com.geojson")


# Rebuild boundaries with new contours
fra <- st_union(com, by_feature = FALSE)
inter <- countries[countries$adm0_a3 != 'FRA',]
inter <- st_difference(inter, fra) %>% st_collection_extract("POLYGON")
inter <- st_cast(inter, "MULTIPOLYGON")

fra <- st_as_sf(fra)
fra$adm0_a3 <- "FR"
fra$name <- "France"
st_geometry(fra) <- "geometry"
countries <- rbind(inter, fra)


