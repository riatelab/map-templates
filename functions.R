#########################
# Layers fonction
#########################

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
    
    return(list("units" = nuts, "neighbours" = countries, "cities" = urban_audit))
  }
}  

# Box resize ----
## Main function ----
box_move_and_resize <- function(boxes, x, x_target, x_target_add = TRUE){
  
  out <- st_sf(st_sfc())
  st_crs(out) <- st_crs(x_target)
  
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
      out <- rbind(out, sel)
    }
  }
  
  if(x_target_add == TRUE){
    out <- rbind(x_target, out)
  }
return(out)
}


## Move and resize point layers ----
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

# Get resize value in box layer ----
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