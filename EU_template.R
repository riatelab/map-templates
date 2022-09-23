#########################
# European template
#########################

library(sf)
library(giscoR)
library(mapsf)


# Main frame ----
main_frame <- function(template, frame, level, up_units = NULL, res){
  if(template == "europe"){
    units <- gisco_get_nuts(year = "2021", epsg = "3035", resolution = res, 
                           nuts_level = level)
    
    neighbours <- gisco_get_countries(year = "2020", epsg = "3035", 
                                     resolution = res)
    
    urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "3035", 
                                         spatialtype = "LB")
    
    nuts <- suppressWarnings(st_intersection(units, st_geometry(frame)))
    countries <- suppressWarnings(st_intersection(neighbours, st_geometry(frame)))
    urban_audit <- suppressWarnings(st_intersection(urban_audit, st_geometry(frame)))
    
    # Get borders
    borders <- suppressWarnings(st_intersection(st_buffer(countries, 5), 
                                                st_buffer(countries, 5))) 
    borders <- st_cast(borders,"MULTILINESTRING")
    borders <- borders[borders$CNTR_ID != borders$CNTR_ID.1, ] 
    borders <- borders[,(1:length(countries))]
    
    return(list("units" = nuts, "neighbours" = countries, "borders" = borders,
           "cities" = urban_audit))
  }
}  


box_move_and_resize <- function(boxes, x, x_target){
  
  for (i in 1:nrow(boxes)){
    box <- boxes[i,]

    # Create mask polygon from input parameters
    lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
    lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
    mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
    st_crs(mask) <- st_crs(x)
    
    # if pts 
    if (class(st_geometry(x))[1]=="sfc_POINT"){
      x <- ptbox_move_and_resize(x = x, mask = mask, xy = xy, prj = prj, k = k)
      return(x)
    }
    
    # names order mngmt
    namesorder <- names(x)
    
    # multipolygon mgmt
    cp <- class(st_geometry(x))[1]=="sfc_MULTIPOLYGON"
    
    # Intersect with mask
    sel <- suppressWarnings(st_intersection(x, st_geometry(mask))) 

    # add mask to x
    xm <- sel[1, ]
    st_geometry(xm) <- st_geometry(mask)
    sel <- rbind(xm, sel)

    # Transform 
    sel <- st_transform(sel, box[,"epsg_loc", drop = T])
    
    # Move to the center of the box
    xy <- suppressWarnings(st_centroid(box))
    xy <- st_bbox(xy)[1:2]
    
    cntrd <- st_centroid(st_combine(sel))
    xg <- (st_geometry(sel) - cntrd) + cntrd[[1]][] 
    st_geometry(sel) <- xg + xy - (st_bbox(st_centroid(xg))[1:2])
    
    # Resize
    bbox_ref <- st_bbox(box) # Bbox in the template
    bbox_input <- st_bbox(sel[1,]) # Bbox of the selection
    
    x_dif <- (bbox_ref[3] - bbox_ref[1]) / (bbox_input[3] - bbox_input[1]) # Diff in X  
    y_dif <- (bbox_ref[4] - bbox_ref[2]) / (bbox_input[4] - bbox_input[2]) # diff in Y   
    k_min <- min(x_dif, y_dif) # Target resize ratio
    
    cntrd <- st_centroid(st_combine(sel))
    xg <- (st_geometry(sel) - cntrd) * k_min + cntrd[[1]][] 
    st_geometry(sel) <- xg + xy - (st_bbox(st_centroid(xg))[1:2])
    
    # get rid of mask
    sel <- sel[-1,]
    
    if (cp){sel <- st_cast(sel, "MULTIPOLYGON")}
    st_crs(sel) <- st_crs(x_target)
    
    # names order mngmt / return nothing if no territories included in the box
    if(nrow(sel) > 0){
      sel <- sel[, namesorder]
      x_target <- rbind(x_target, sel)
    }
  }
return(x_target)
}


ptbox_move_and_resize <- function(boxes, x, x_target){
  # names order mngmt
  namesorder <- names(x)
  
  # intersect mask and x
  x <- suppressWarnings(
    st_collection_extract(
      st_intersection(x, st_geometry(mask)),
      type = c("POINT")
    )
  )
  
  # add mask to x
  xm <- x[1, ]
  st_geometry(xm) <- st_geometry(mask)
  x <- rbind(xm,x)
  
  # Move to the center of the box
  xy <- suppressWarnings(st_centroid(tmp))
  xy <- st_bbox(xy)[1:2]
  
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) + cntrd[[1]][] 
  st_geometry(x) <- xg + xy - (st_bbox(st_centroid(xg))[1:2])
  
  # Resize
  bbox_ref <- st_bbox(box) # Bbox in the template
  bbox_input <- st_bbox(x[1,]) # Bbox of the selection
  
  x_dif <- (bbox_ref[3] - bbox_ref[1]) / (bbox_input[3] - bbox_input[1]) # Diff in X  
  y_dif <- (bbox_ref[4] - bbox_ref[2]) / (bbox_input[4] - bbox_input[2]) # diff in Y   
  k_min <- min(x_dif, y_dif) # Target resize ratio
  
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) * k_min + cntrd[[1]][] 
  st_geometry(x) <- xg + xy - (st_bbox(st_centroid(xg))[1:2])
  
  
  # get rid of mask
  x <- x[-1,]
  st_crs(x) <- prj
  
  # names order mngmt
  x <- x[, namesorder]
  
  if(nrow(x > 0)){
    x <- x[, namesorder]
    
    x_target <- rbind(x_target, x)}

}

i <- 1
box_k <- function(boxes, x, x_target){
  for (i in 1:nrow(boxes)){
    box <- boxes[i,]
    
    # Create mask polygon from input parameters
    lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
    lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
    mask <- st_sfc(st_polygon(list(cbind(lon, lat))))
    st_crs(mask) <- st_crs(x)
    
    # multipolygon mgmt
    cp <- class(st_geometry(x))[1]=="sfc_MULTIPOLYGON"
    
    # Intersect with mask
    sel <- suppressWarnings(st_intersection(x, st_geometry(mask))) 
    
    # add mask to x
    xm <- sel[1, ]
    st_geometry(xm) <- st_geometry(mask)
    sel <- rbind(xm, sel)
    
    # Transform 
    sel <- st_transform(sel, box[,"epsg_loc", drop = T])
    
    # Move to the center of the box
    xy <- suppressWarnings(st_centroid(box))
    xy <- st_bbox(xy)[1:2]
    
    cntrd <- st_centroid(st_combine(sel))
    xg <- (st_geometry(sel) - cntrd) + cntrd[[1]][] 
    st_geometry(sel) <- xg + xy - (st_bbox(st_centroid(xg))[1:2])
    
    # Resize
    bbox_ref <- st_bbox(box) # Bbox in the template
    bbox_input <- st_bbox(sel[1,]) # Bbox of the selection
    
    x_dif <- (bbox_ref[3] - bbox_ref[1]) / (bbox_input[3] - bbox_input[1]) # Diff in X  
    y_dif <- (bbox_ref[4] - bbox_ref[2]) / (bbox_input[4] - bbox_input[2]) # diff in Y   
    k_min <- min(x_dif, y_dif) # Target resize ratio
    boxes[i, "k"] <- k_min
  }
  return(boxes)
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



# Import target boxes
boxes <- st_read("input/eu/boxes.geojson")

boxes <- st_transform(boxes, 3035)
boxes$name <- c("Madeire", "Mayotte", "Martinique", "Guadeloupe", "Canaries",
                "Açores - Flores", "Réunion", "Guyane", "Açores")
boxes$target <- list(c(-17.35, 32.55, -16.2, 33.2), # OK
                     c(45, -13.02, 45.28, -12.65), # OK
                     c(-61.28, 14.35, -60.76, 14.93), # OK
                     c(-61.89, 15.8, -61.15, 16.55), # OK
                     c(-18.4, 27.4,- 13.3, 30.3), # OK
                     c(-31.4, 39.3, -30.9, 39.8), # OK
                     c(54.8,-21.5,56.3,-20.5),
                     c(-66,-6.3,-39,12.5),
                     c(-28.088, 36.662, -24.576, 39.22))
boxes$epsg_loc <- c(2191, 4471, 5490, 5490, 3035, 3063, 2975, 2972, 3063)


# Import layer with territorial units to be included in the boxes / boxes$target prj must fit with proj of input layers
input <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "20", nuts_level = "2")
countries <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "20")
urban_audit <- gisco_get_urban_audit(year = "2020", epsg = "4326", 
                                     spatialtype = "LB")
head(bbox)

i <- 7
box <- boxes[i,]
lon <- as.vector(unlist(box[,"target"])[c(1,3,3,1,1)])
lat <- as.vector(unlist(box[,"target"])[c(2,2,4,4,2)])
mask <- st_sfc(st_polygon(list(cbind(lon, lat))))

mf_map(toto)
mf_map(input, add = TRUE)

head(input)
toto <- box_move_and_resize(boxes = boxes, x = input, x_target = nuts) 
boxes <- box_k(boxes = boxes, x = input, x_target = nuts)

head(toto)
mf_map(toto)
boxes$geometry


i <- 6
st_coordinates(boxes[i,])
xmin <- st_coordinates(boxes[i,])[3,1]
xmax <- st_coordinates(boxes[i,])[1,1]
ymin <- st_coordinates(boxes[i,])[2,2]
ymax <- st_coordinates(boxes[i,])[1,2]

i <- 9
st_coordinates(boxes[i,])
xmin2 <- st_coordinates(boxes[i,])[1,1]
xmax2 <- st_coordinates(boxes[i,])[4,1]


(xmax - xmin) / (xmax2 - xmin2) 

mf_map(nuts)
mf_map(boxes[9,], add = T, col = "red")

xmax - xmin
ymax - ymin

i <- 3
ymax2 <- st_coordinates(boxes[i,])[1,2]
ymin - ymax2

locator()

st_coordinates(boxes[3,])

mart_x <- c(6043093, 5818604)
mart_y <- c(4720499, 4497906)
diff <- mart_x[1] - mart_x[2]
diff2 <- mart_y[1] - mart_y[2]

mf_map(toto)


#boxes$k <- c(2, 5, 3.5, 4.2, 0.72, 2, 4.5, 0.85, 2)

epsg_out <- st_crs(frame)

mf_map(x_target)

tmp <- boxes[1,]
bbox <- st_bbox(tmp)

x <- nuts
x_target <- europe$units