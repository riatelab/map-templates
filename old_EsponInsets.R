ESPON_inset <- function(spdf, geo_bbox,mask_bbox,x_target,y_target,prj,k){
  
  # select a geographical area
  tmp.spdf <-spdf
  geo.sp <- rgeos::readWKT(paste("POLYGON((",geo_bbox[1]," ",geo_bbox[2],",",geo_bbox[1]," ",geo_bbox[4],",",geo_bbox[3]," ",geo_bbox[4],",",geo_bbox[3]," ",geo_bbox[2],",",geo_bbox[1]," ",geo_bbox[2],"))",sep=""))
  proj4string(geo.sp) <- proj4string(spdf) 
  sr <- gIntersection(tmp.spdf, geo.sp, byid=TRUE)
  
  ids <- (do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[,1]
  row.names(sr) <- as.vector(as.matrix(data.frame(do.call('rbind', (strsplit(as.character(row.names(sr))," "))))[1]))
  
  data <- as.data.frame(tmp.spdf@data[row.names(tmp.spdf) %in% ids,])
  tmp.spdf <- tmp.spdf[row.names(tmp.spdf) %in% ids,]
  
  tmp.spdf <- sp::SpatialPolygonsDataFrame(Sr = sr, data = tmp.spdf@data, match.ID = TRUE)
  
  # select a bbox
  tmp.spdf <- spTransform(x =  tmp.spdf, CRSobj = prj)
  proj4string(tmp.spdf) <- ""
  proj4string(tmp.spdf) <- "+init=epsg:3035"
  bb <-  readWKT(writeWKT(gEnvelope(tmp.spdf)))@bbox
  
  if (is.null(mask_bbox)){ mask_bbox <- tmp.spdf@bbox } 
  x1 = mask_bbox[1]; y1 =mask_bbox[2] ; x2 = mask_bbox[3] ; y2 = mask_bbox[4]
  mask <- rgeos::readWKT(paste("POLYGON((",x1," ",y1,",",x1," ",y2,",",x2," ",y2,",",x2," ",y1,",",x1," ",y1,"))",sep=""))
  proj4string(mask) <- "+init=epsg:3035"
  output.spdf <- move_and_resize( spdf = tmp.spdf, mask = mask, xy = c(x_target,y_target), k = k, prj = "+init=epsg:3035")
  output.spdf@data[,1:length(output.spdf@data)-1,]
  return(output.spdf)  
}