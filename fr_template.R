###############
# 
###############

library(sf)
library(mapsf)

# Importer pays voisins + simplifier 
countries <- st_read("input/fr/ne_10m_admin_0_countries.shp")


# Chefs lieux
com <- st_read("input/fr/COMMUNE.shp")
com <- st_set_geometry(com, NULL)
chefL <- st_read('input/fr/CHFLIEU_COMMUNE.shp')

chefL <- merge(chefL, com[,c("ID", "INSEE_COM", "NOM", "POPULATION", "SIREN_EPCI")], 
               by.x = "ID_COM", by.y = "ID")


arr <- st_read("input/fr/ARRONDISSEMENT_MUNICIPAL.shp")


head(chefL)
head(com)

75056, 13055, 69123