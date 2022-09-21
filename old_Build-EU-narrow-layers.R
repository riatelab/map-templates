# ****************
# MAPKITS (2016)
# ***************
#library(devtools)
#install_github("riatelab/mapinsetr")
library("rgdal")
library("mapinsetr")
library("cartography")
library("rgeos")
library("maptools")
library("foreign")


#setwd("/home/nlambert/Documents/ESPON/ESPON-MapKits-2016/Delivery_20170511")
setwd("C:/Users/Ronan/Desktop/Delivery_20170511")


source("prg/sources/EsponInsets.R")


prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"


# remote neighborhood
inset.spdf <-   readOGR(dsn = "input/remote", layer = "remote_territories", verbose = TRUE)
inset_neighbours.spdf <-   inset.spdf[is.na(inset.spdf@data$level),]


# template
boxes.spdf <- readOGR(dsn= "Mapkits/EU-narrow/shp/layout",layer = "template", verbose=TRUE)

# Countries [OK]

countries.spdf <- readOGR(dsn = "input/countries", layer = "world", verbose = TRUE)
countries.spdf  <- spTransform(x =  countries.spdf, CRSobj = prj)
sr <- gIntersection(countries.spdf, boxes.spdf[boxes.spdf@data$type=="mainframe",], byid=TRUE)
ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
row.names(sr) <- ids
data <-as.data.frame(countries.spdf@data[row.names(countries.spdf@data) %in% ids,])
row.names(data) <- ids
countries.spdf <- sp::SpatialPolygonsDataFrame(Sr = sr, data = data, match.ID = TRUE)
colnames(countries.spdf@data) <- "id"
countries.spdf <- countries.spdf[countries.spdf@data$id !="GL",]
countries.spdf <- countries.spdf[countries.spdf@data$id !="LY",]
countries.spdf <- countries.spdf[countries.spdf@data$id !="EG",]
countries.spdf <- countries.spdf[countries.spdf@data$id !="PS",]

plot(boxes.spdf)
plot(countries.spdf,add=T)
writeOGR(obj=countries.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="countries", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)




# Borders

nationalborders.spdf <- getBorders(countries.spdf)

sr <- as(nationalborders.spdf , 'SpatialLines')
sr2 <- gBuffer(as(boxes.spdf[boxes.spdf@data$type=="boxes",] , 'SpatialPolygons'),width=1)

sr <- gDifference(sr, sr2, byid=FALSE, id=NULL, drop_lower_td=FALSE,unaryUnion_if_byid_false=TRUE, checkValidity=FALSE)
row.names(sr) <- "1"
data <-as.data.frame(c("id","name"))
data <- as.data.frame("id")
data[1]<-"borders"
nationalborders.spdf <- sp::SpatialLinesDataFrame(sr, data = data, match.ID = TRUE)
plot(nationalborders.spdf)
nationalborders.spdf@data
writeOGR(obj=nationalborders.spdf , dsn="Mapkits/EU-narrow/shp/layout", layer="national-borders", driver="ESRI Shapefile",encoding="UTF-8",overwrite_layer=TRUE,verbose=F)


# capital cities [OK]

# cities.spdf <- readOGR(dsn = "input/world", layer = "capital", verbose = TRUE,encoding = " ISO-8859-1")
# cities.spdf <- cities.spdf[!is.na(cities.spdf@data$narrow),]
# cities.spdf@data
# cities.spdf  <- spTransform(x =  cities.spdf, CRSobj = proj4string(boxes.spdf))
# sr <- gIntersection(cities.spdf, boxes.spdf[boxes.spdf$type=="mainframe",], byid=TRUE)
# ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
# row.names(sr) <- ids
# data <-as.data.frame(cities.spdf@data[row.names(cities.spdf@data) %in% ids,])
# row.names(data) <- ids
# cities.spdf <- sp::SpatialPointsDataFrame(coords = sr@coords, data = data, match.ID = TRUE)
# proj4string(cities.spdf) <-  proj4string(boxes.spdf)
# citiesneighborhood.spdf <- cities.spdf[cities.spdf@data$narrow==0,]
# citiesneighborhood.spdf <-citiesneighborhood.spdf[!citiesneighborhood.spdf@data$NAME %in% c("Kiev","Moscow"),]
# cities.spdf <- cities.spdf[cities.spdf@data$narrow==1,]
# writeOGR(obj=cities.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="capital-cities", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
# writeOGR(obj=citiesneighborhood.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="capital-cities-neighborhood", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
# write.dbf(data.frame(citiesneighborhood.spdf@data), paste("Mapkits/EU-narrow/shp/layout","capital-cities-neighborhood",sep="/"), factor2char = TRUE, max_nchar = 254)


# Martinique [OK]
d <- 95000


martinique.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-61.28,14.32,-60.76,14.93),
  mask_bbox = NULL,
  x_target = 6230000-d, y_target = 4050000,
  prj = "+init=epsg:2973",
  k=5.5
)

# Reunion [OK]

reunion.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(54.8,-21.5,56.3,-20.5),
  mask_bbox = NULL,
  x_target = 6220000-d, y_target = 3573846,
  prj = "+init=epsg:2975",
  k=4.5
)

# Guadeloupe [OK]

guadeloupe.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-61.89,15.738,-60.86,16.602),
  mask_bbox = NULL,
  x_target = 6210000-d, y_target = 4510000,
  prj = "+init=epsg:2970",
  k=4.2
)

guadeloupe2.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-63.2,18.006,-62.9,18.1388),
  mask_bbox = NULL,
  x_target = 6187383-d, y_target = 4832390,
  prj = "+init=epsg:2970",
  k=4.2
)

# Mayotte [OK]

mayotte.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(44.97,-13.03,45.357,-12.59),
  mask_bbox = NULL,
  x_target = 5850000-d, y_target = 3580000,
  prj = "+init=epsg:2975",
  k=7
)

# Guyane [OK]

guyane.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-66,-6.3,-39,12.5),
  mask_bbox = c(773402.2-100000,235589.5-100000,1099478.2+100000,636708.6+100000),
  x_target = 5696031-d, y_target = 3967331,
  prj = "+init=epsg:3313",
  k= 0.85
  #k = 0.827
 )

# Guyane (patch: box intersection)
proj4string(boxes.spdf) <- "" 
proj4string(boxes.spdf) <-  proj4string(guyane.spdf)
sr <- gIntersection(guyane.spdf, boxes.spdf[row.names(boxes.spdf)=="12",], byid=TRUE)
ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
row.names(sr) <- ids
data <-as.data.frame(guyane.spdf@data[row.names(guyane.spdf@data) %in% ids,])
row.names(data) <- ids
guyane.spdf <- sp::SpatialPolygonsDataFrame(Sr = sr, data = data, match.ID = TRUE)

# Canarias [OK]

canarias.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-20, 25.4,- 10, 30.3),
  mask_bbox =  c(1512839,700000,2110908,1260211),
  x_target = 5702820-d, y_target = 4473480.29426741,
  prj = "+init=epsg:3035",
  k = 0.72
)

# Liechtenstein [OK]

liechtenstein.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(9.35, 47,9.75, 47.30),
  mask_bbox = NULL,
  x_target = 6150000-d, y_target = 4975000,
  prj = "+init=epsg:3035",
  k = 14.4
)


# liechtenstein (patch: box intersection)
proj4string(boxes.spdf) <- "" 
proj4string(boxes.spdf) <-  proj4string(liechtenstein.spdf)
sr <- gIntersection(liechtenstein.spdf, boxes.spdf[boxes.spdf$id=="19",], byid=TRUE)
ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
row.names(sr) <- ids
data <-as.data.frame(liechtenstein.spdf@data[row.names(liechtenstein.spdf@data) %in% ids,])
row.names(data) <- ids
liechtenstein.spdf <- sp::SpatialPolygonsDataFrame(Sr = sr, data = data, match.ID = TRUE)


# Madeira [OK]

madeira.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-17.6,31.9,-15.5,33.7),
  mask_bbox = NULL,
  x_target = 6256200-d, y_target = 3110754,
  prj = "+init=epsg:2191",
  k=3
)


# Malta [OK]

malta.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(14,35,15,37),
  mask_bbox = NULL,
  x_target = 5770000-d, y_target = 5090000,
  prj = "+init=epsg:3035",
  k=8
)

# Acores [OK]

acores1.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-28.902,38.243,-26.99,39.22),
  mask_bbox = c(327322,4234220,516127,4350699),
  x_target = 5742000-d, y_target = 2960000,
  prj = "+init=epsg:3063",
  k=2
)

acores2.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-31.397,39.215,-30.898,39.907),
  mask_bbox = c(122539,4414510,165956,4356586),
  x_target = 5732487-d, y_target = 3290000,
  prj = "+init=epsg:3063",
  k=2
)

acores3.spdf <- ESPON_inset(
  spdf = inset.spdf,
  geo_bbox <- c(-28.088,36.662,-24.576,38.105),
  mask_bbox = c(581725,4061816,692737,4216139),
  x_target = 5920000-d, y_target = 3150000,
  prj = "+init=epsg:3063",
  k=2
)


# Bind

ESPON_insets <- inset_rbinder(l = list(martinique.spdf, guyane.spdf,
                                       reunion.spdf, mayotte.spdf, guadeloupe.spdf, guadeloupe2.spdf,
                                       madeira.spdf, canarias.spdf,acores1.spdf, acores2.spdf, acores3.spdf,
                                       malta.spdf, liechtenstein.spdf))



# remote neigbourhood

tmp <- ESPON_insets

tmp@data$version[is.na(tmp@data$version)] <- 2013
tmp@data$level[is.na(tmp@data$level)] <- 0
tmp <- subset(tmp, level==0)
tmp <- subset(tmp, version==2013)
tmp$id <- row.names(tmp)
remoteborders.spdf <- getBorders(tmp,spdfid = "id")
writeOGR(obj=remoteborders.spdf , dsn="Mapkits/EU-narrow/shp/layout", layer="remote-borders", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)

remote1 <- ESPON_insets[!is.na(ESPON_insets@data$level),] # A deplacer **********

remote2 <- ESPON_insets[is.na(ESPON_insets@data$level),c("id","name")]
writeOGR(obj=remote2, dsn="Mapkits/EU-narrow/shp/layout", layer="remote-non-espon", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)


# coastlines

tmp <- inset_rbinder(l = list(martinique.spdf, guyane.spdf,
                                       reunion.spdf, mayotte.spdf, guadeloupe.spdf, guadeloupe2.spdf,
                                       madeira.spdf, canarias.spdf,acores1.spdf, acores2.spdf, acores3.spdf,
                                       malta.spdf))

coastlineboxes <- rgeos::gBuffer(tmp,byid=FALSE)
sr <- as(coastlineboxes , 'SpatialLines')
sr2 <- gBuffer(as(boxes.spdf[boxes.spdf@data$type=="boxes",] , 'SpatialLines'),width=1000)
proj4string(sr) <- ""
proj4string(sr2) <- ""
proj4string(sr) <- prj
proj4string(sr2) <- prj
sr <- gDifference(sr, sr2, byid=FALSE, id=NULL, drop_lower_td=FALSE,unaryUnion_if_byid_false=TRUE, checkValidity=FALSE)
row.names(sr) <- "1"
data <-as.data.frame(c("id","name"))
data <- as.data.frame("id")
data[1]<-"coastline"
coastlineboxes <- sp::SpatialLinesDataFrame(sr, data = data, match.ID = TRUE)

coastlinemain <- rgeos::gBuffer(countries.spdf,byid=FALSE,width=7)
sr <- as(coastlinemain , 'SpatialLines')
sr2 <- gBuffer(as(boxes.spdf[boxes.spdf@data$type=="mainframe",] , 'SpatialLines'),width=6000)
proj4string(sr) <- ""
proj4string(sr2) <- ""
proj4string(sr) <- prj
proj4string(sr2) <- prj
sr <- gDifference(sr, sr2, byid=FALSE, id=NULL, drop_lower_td=FALSE,unaryUnion_if_byid_false=TRUE, checkValidity=FALSE)
sr3 <- gBuffer(boxes.spdf[boxes.spdf@data$type=="boxes",],width=1)
proj4string(sr3) <- ""
proj4string(sr3) <- prj
sr <- gDifference(sr, sr3, byid=FALSE, id=NULL, drop_lower_td=FALSE,unaryUnion_if_byid_false=TRUE, checkValidity=FALSE)
row.names(sr) <- "1"
data <-as.data.frame(c("id","name"))
data <- as.data.frame("id")
data[1]<-"coastline"
coastlinemain <- sp::SpatialLinesDataFrame(sr, data = data, match.ID = TRUE)

costlines.spdf <- inset_rbinder(l = list(coastlinemain,coastlineboxes))
writeOGR(obj=costlines.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="coastlines", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)

plot(costlines.spdf)

# Nuts

dir.create("Mapkits/EU-narrow/shp/nuts")

BuildLayers <- function(level,version){
shp <- paste("nuts",level,"-geo-",version,sep="")
nuts.spdf <-   readOGR(dsn = paste("input/nuts",version,sep=""), layer = shp, verbose = TRUE)
nuts.spdf <- nuts.spdf[nuts.spdf$CC == 0,]
nuts.spdf <- spTransform(x =  nuts.spdf, CRSobj = prj)
mask.spdf <- boxes.spdf[boxes.spdf@data$type=="mainframe",]
proj4string(mask.spdf) <- ""
proj4string(mask.spdf) <- prj
sr <- gIntersection(nuts.spdf, mask.spdf, byid=TRUE)
ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
row.names(sr) <- as.vector(as.matrix(data.frame(do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[1]))
data <- as.data.frame(nuts.spdf@data[row.names(nuts.spdf) %in% ids,])
nuts.spdf <- nuts.spdf[row.names(nuts.spdf) %in% ids,]
nuts.spdf <- sp::SpatialPolygonsDataFrame(Sr = sr, data = nuts.spdf@data, match.ID = TRUE)
nuts.spdf@data <- nuts.spdf@data[,c("id","name")]
insets <- remote1[remote1@data$version == version,]
insets <- insets[insets@data$level == level,]
insets@data
insets@data <- insets@data[c("id","name")]
proj4string(insets) <- ""
proj4string(insets) <- prj
nuts.spdf <- inset_rbinder(l = list(nuts.spdf, insets))
nuts.spdf@data$id <- as.factor(as.character(nuts.spdf@data$id))
data <- nuts.spdf@data
sr <- rgeos::gUnaryUnion(nuts.spdf, id = nuts.spdf@data$id)
data2 <- as.data.frame(as.character(row.names(sr)))
colnames(data2) <- c("id")
row.names(data2) <- data2$id
nuts.spdf <-sp::SpatialPolygonsDataFrame(Sr = sr, data =data2,match.ID=TRUE)
nuts.spdf@data <- data.frame(nuts.spdf@data , data[match(nuts.spdf@data[,"id"], data[,"id"]),])
nuts.spdf@data <- nuts.spdf@data[,c("id","name")]


# BORDERS
borders.spdf <- getBorders(nuts.spdf)

# CENTROIDES
centroides <- as.data.frame(sp::coordinates(nuts.spdf))
centroides.spdf <-sp::SpatialPointsDataFrame(centroides, data =data2,match.ID=TRUE)
centroides.spdf@data <- data.frame(centroides.spdf@data , data[match(centroides.spdf@data[,"id"], data[,"id"]),])
centroides.spdf@data <- centroides.spdf@data[,c(1,3)]
proj4string(centroides.spdf) <- proj4string(nuts.spdf) 

# EXPORT
name_polygons <- paste("nuts-version",version,"-level",level,sep="")
name_borders <- paste("nuts-version",version,"-level",level,"-borders",sep="")
name_centroides  <- paste("nuts-version",version,"-level",level,"-centroides",sep="")

writeOGR(obj=nuts.spdf, dsn="Mapkits/EU-narrow/shp/nuts", layer=name_polygons, driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
writeOGR(obj=borders.spdf , dsn="Mapkits/EU-narrow/shp/nuts", layer=name_borders, driver="ESRI Shapefile",encoding="UTF-8",overwrite_layer=TRUE,verbose=F)
writeOGR(obj=centroides.spdf, dsn="Mapkits/EU-narrow/shp/nuts", layer=name_centroides, driver="ESRI Shapefile",encoding="UTF-8",overwrite_layer=TRUE,verbose=F)

write.dbf(nuts.spdf@data, paste("Mapkits/EU-narrow/shp/nuts",name_polygons,sep="/"), factor2char = TRUE, max_nchar = 254)
write.dbf(borders.spdf@data, paste("Mapkits/EU-narrow/shp/nuts",name_borders,sep="/"), factor2char = TRUE, max_nchar = 254)
write.dbf(centroides.spdf@data, paste("Mapkits/EU-narrow/shp/nuts",name_centroides,sep="/"), factor2char = TRUE, max_nchar = 254)

}

# ECRITURE
BuildLayers(level = 0,version = 2013)
BuildLayers(level = 1,version = 2013)
BuildLayers(level = 2,version = 2013)
BuildLayers(level = 3,version = 2013)
BuildLayers(level = 0,version = 2010)
BuildLayers(level = 1,version = 2010)
BuildLayers(level = 2,version = 2010)
BuildLayers(level = 3,version = 2010)
BuildLayers(level = 0,version = 2006)
BuildLayers(level = 1,version = 2006)
BuildLayers(level = 2,version = 2006)
BuildLayers(level = 3,version = 2006)

# UMZ
 # umz.spdf <-   readOGR(dsn = "input/cities", layer = "UMZ_10k", verbose = TRUE)
 # umz.spdf  <- spTransform(x =  umz.spdf, CRSobj = prj)
 # writeOGR(obj=umz.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="UMZ", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)

# FUA !! WARNING REMOTE TERRITORIES
 # fua.spdf <-   readOGR(dsn = "input/cities", layer = "FUA_LUZ", verbose = TRUE)
 # fua.spdf  <- spTransform(x =  fua.spdf, CRSobj = prj)
 # writeOGR(obj=fua.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="FUA (LUZ) - tmp", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)

# Cyprus
cy.spdf <-   readOGR(dsn = "input/layouts", layer = "cyprus_non_espon_space", verbose = TRUE)
writeOGR(obj=cy.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="cyprus_non_espon_space", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)


# Scales & box labels

scaleVal <- 500000
template.spdf <- boxes.spdf
bb <- template.spdf[template.spdf@data$type=="mainframe",]@bbox
xmax <- bb[3]
ymin <- bb[2]
w <- bb[3] - bb[1]
h <- bb[4] - bb[2]
xpos <- xmax - scaleVal - w/43.83347
ypos <- ymin + h/80
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+scaleVal," ",ypos,")",sep=""))
scale.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="main", value=paste((scaleVal/1000),"km",sep=" ")))
proj4string(scale.spdf) <- prj
writeOGR(obj=scale.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="scale-main", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
write.dbf(scale.spdf@data, paste("Mapkits/EU-narrow/shp/layout","scale-main",sep="/"), factor2char = TRUE, max_nchar = 254)

idbox = "15"
scaleVal= 10000
k=8
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Malta", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- s.spdf
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
boxlabels.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Malta"))


idbox = "19"
scaleVal= 5000
k=14.4
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Liechtenstein", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Liechtenstein"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "14"
scaleVal= 100000
k=0.72
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Canarias (ES)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Canarias (ES)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "10"
scaleVal= 10000
k=4.2
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Guadeloupe (FR)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Guadeloupe (FR)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "13"
scaleVal= 100000
k=0.85
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Guyane (FR)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Guyane (FR)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "22"
scaleVal= 10000
k=5.5
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Martinique (FR)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Martinique (FR)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "12"
scaleVal= 10000
k=7
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Mayotte (FR)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Mayotte (FR)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "21"
scaleVal= 10000
k=4.5
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Réunion (FR)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Réunion (FR)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "11"
scaleVal= 50000
k=2
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Açores (PT)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Açores (PT)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))

idbox = "7"
scaleVal= 20000
k=3
xpos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[3]-30000 - scaleVal*k
ypos = boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]+30000
scale.sp <- rgeos::readWKT(paste("LINESTRING(",xpos," ",ypos,",",xpos+(scaleVal*k)," ",ypos,")",sep=""))
s.spdf <- SpatialLinesDataFrame(scale.sp, data.frame(id="Madeira (PT)", value=paste((scaleVal/1000),"km",sep=" ")))
scales.spdf <- inset_rbinder(l = list(scales.spdf, s.spdf))
boxlabels.sp <- rgeos::readWKT(paste("POINT(",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[1])," ",round(boxes.spdf[boxes.spdf@data$id==idbox,]@bbox[2]),")",sep=""))
b.spdf <- SpatialPointsDataFrame(boxlabels.sp, data.frame(id="Madeira (PT)"))
boxlabels.spdf <- inset_rbinder(l = list(boxlabels.spdf, b.spdf))
boxlabels.spdf <- raster::shift(boxlabels.spdf, 15000, 15000)

proj4string(scales.spdf) <- prj
writeOGR(obj=scales.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="scales-remote", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
write.dbf(scales.spdf@data, paste("Mapkits/EU-narrow/shp/layout","scales-remote",sep="/"), factor2char = TRUE, max_nchar = 254)

proj4string(boxlabels.spdf) <- prj
writeOGR(obj=boxlabels.spdf, dsn="Mapkits/EU-narrow/shp/layout", layer="boxlabels", driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=F)
write.dbf(boxlabels.spdf@data, paste("Mapkits/EU-narrow/shp/layout","boxlabels",sep="/"), factor2char = TRUE, max_nchar = 254)


# plot(boxes.spdf)
# plot(scale.spdf,add=T,col="red")
# scales.spdf@data

