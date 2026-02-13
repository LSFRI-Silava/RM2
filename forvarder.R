library(sf)
library(plyr)
library(purrr)
library(data.table)
library(tidyr)
library(raster)
library(sp)
library(DBI)
library(RPostgres)
library(tidyverse)
library(writexl)
library(akima)

forvardera_rises <- function(workdir, resultsdir, rut_depth_threshold,risu_bufers) {

# Connecting to the database ----

  con <- dbConnect(RPostgres::Postgres(),
                   dbname = "Your data Base",
                   host = "Data base IP",
                   port = Port,
                   user = "Username",
                   password = "Password",
                   sslmode = "require")

  linija <- st_read("...../RM2_R_kods_2025_v0/LVM_TK/Pievesanas_cels.shp") 
  landfill <- st_read(paste0(workdir,resultsdir)) 
   bbox<-st_bbox(landfill)
  
  plot(st_geometry(landfill))
  sql_query <- paste0("SELECT t.*, st_x(t.geom),st_y(t.geom)
FROM public.gnss_data t WHERE t.geom && ST_SetSRID(ST_MakeBox2D(
    	  ST_SetSRID(ST_Point(",bbox$xmin,",",bbox$ymin,"), 3059),
        ST_SetSRID(ST_Point(",bbox$xmax,", ",bbox$ymax,"), 3059)),3059)");

  data <- dbGetQuery(con, sql_query)
  data <- data %>% filter(fix == 2)
  class(data)

  data_sf <- st_as_sf(data, coords = c("st_x", "st_y"), crs = 3059)
 
# Add LVM parcels to the selection 
## Select points located in the landfill 
  points_inside_landfill <- st_filter(data_sf,landfill) 
  data_sf <- points_inside_landfill 
  data_sf

## Create a buffer and calculate the area
  buffer_1 <- st_buffer(data_sf, dist = risu_bufers)
  buffer_union <- st_union(buffer_1)
  buffer_union_area <- st_area(buffer_union)
  buffer_union_area_ha <- as.numeric(buffer_union_area) / 1e4

  print(buffer_union_area)

  new_coords = st_crs(3059)
  data_3059 <- st_transform(data_sf, new_coords)
  data_3059
  
  LKS <- data_3059 %>%
    mutate(lon = unlist(map(data_3059$geometry,1)),
           lat = unlist(map(data_3059$geometry,2)))
  LKS
  class(LKS)
  
# Make grid 
  area_fishnet_grid = st_make_grid(LKS, c(2, 2), what = "polygons", square = TRUE)
  #plot(area_fishnet_grid)
  
## To sf and add grid ID 
  fishnet_grid_sf = st_sf(area_fishnet_grid) %>% 
  mutate(grid_id = 1:length(lengths(area_fishnet_grid)))
  class(fishnet_grid_sf)
  
## Filter maximum elevation within grid cells 
  Test_points_grid_id <- st_join(LKS, left = FALSE, fishnet_grid_sf["grid_id"]) # join points
  Points_no_geometry <- Test_points_grid_id
  class(Points_no_geometry)
  st_geometry(Points_no_geometry) <- NULL
  class(Points_no_geometry)
  
# Atsijā Vacc neprecīzos punktus 
  Filter_Vacc <- filter(Points_no_geometry, vacc<0.05)
  Filter_fix <- filter(Filter_Vacc, fix==2)
  class(Filter_fix)
  
  Filter_top_points <- Filter_fix %>%
    group_by(grid_id) %>%
    slice(which.max(alt))
  class(Filter_top_points)
  
  Filter_top_points <- Filter_fix %>%
    group_by(grid_id) %>%
    filter(alt == max(alt))
 

# Create elevation raster 
  test_temp <- Filter_top_points
  test_temp$x <- test_temp$lon
  test_temp$y <- test_temp$lat
  test_temp<-na.omit(test_temp)
  coordinates(test_temp) = ~x + y
  plot(test_temp)
  class(test_temp)

  interp_points <- with(test_temp, interp(test_temp$lon, test_temp$lat, test_temp$alt, extrap = TRUE, nx = 1000, ny = 1000, 
                                          duplicate = "mean"))

## Convert the interpolation result to a raster 
  raster_data <- raster(interp_points)
  class(raster_data)
  plot(raster_data, main = "Altitude Interpolation")

  dev.off()

# Calculate distance between points 
  LKS_df <- LKS %>% st_drop_geometry()
  LKS_df
  tab_2 <- LKS_df %>% dplyr::select(id, lon, lat) %>% mutate(NR = case_when(id>=0 ~ id+1))
  LKS_apvienots <- full_join(LKS_df, tab_2, by = c("id" = "NR"))
  LKS_apvienots$dist <- sqrt(abs(LKS_apvienots$lon.x - LKS_apvienots$lon.y)^2+abs(LKS_apvienots$lat.x - LKS_apvienots$lat.y)^2)
  
## Remove stationary points 
  LKS_dist_filter <- filter(LKS_apvienots,dist > 0.05)

## Set new ID 
  LKS_dist_filter$ID_2 <- seq.int(nrow(LKS_dist_filter))
  
## Calculate bearing 
  LKS_dist_filter$bearring <- (atan(( LKS_dist_filter$lon.y - LKS_dist_filter$lon.x )/
                                      ( LKS_dist_filter$lat.y - LKS_dist_filter$lat.x )))*180/3.14159 +(180*((( LKS_dist_filter$lat.y - LKS_dist_filter$lat.x )<0)+
                                                                                                     ((( LKS_dist_filter$lon.y - LKS_dist_filter$lon.x )<0 & ( LKS_dist_filter$lat.y - LKS_dist_filter$lat.x )>0)*2)))
## Calculate turning point 
  tab_3 <- LKS_dist_filter %>% dplyr::select(ID_2, bearring) %>% mutate(NR = case_when(ID_2>=0 ~ ID_2+1))
  LKS_apvienots_2 <- full_join(LKS_dist_filter, tab_3, by = c("ID_2" = "NR"))
  LKS_apvienots_2$turn <- if_else((abs( LKS_apvienots_2$bearring.x - LKS_apvienots_2$bearring.y ))>120
                                  & (abs( LKS_apvienots_2$bearring.x - LKS_apvienots_2$bearring.y ))<240, 1, 0)
  
## Added number of spins 
  total_turns <- sum(LKS_apvienots_2$turn, na.rm = TRUE)
  
## Remove lines with empty cells 
  LKS_apvienots_2 <- LKS_apvienots_2[complete.cases(LKS_apvienots_2$turn),]
  
## Grouping by passes 
  LKS_apvienots_2$group<-cumsum(LKS_apvienots_2$turn==1)
  
## Calculate distance from any point to closest point from higher pass group
  LKS_apvienots_2$ID <- LKS_apvienots_2$group
  LKS_apvienots_2$X <- LKS_apvienots_2$lon.x
  LKS_apvienots_2$Y <- LKS_apvienots_2$lat.x
  LKS_apvienots_2$uid <- LKS_apvienots_2$ID_2.y
  
  setDT(LKS_apvienots_2)
  class(LKS_apvienots_2)
  
  dist <- function(x,y,d) {
    d[, nn_dist:=sqrt((X-x)^2 + (Y-y)^2)][order(nn_dist)][1]
  }
  LKS_apvienots_2[, c("nnX", "nnY", "nn", "nn_dist"):=dist(X,Y, LKS_apvienots_2[ID>.BY$ID,.(X,Y,uid)]),by=.(ID,uid)]
  
## Filter by noise
  LKS_apvienots_2$noise <- abs(LKS_apvienots_2$L-LKS_apvienots_2$R)
  LKS_noise_filter <- filter(LKS_apvienots_2, noise < 20)
  LKS_noise_filter
  class(LKS_noise_filter)
  
## Extract raster values
  New_data <- dplyr::select(LKS_apvienots_2, c('ID_2','lon.x','lat.x', 'alt', 'nn_dist'))
  class(New_data)
  
  coords <- cbind(New_data$lon.x, New_data$lat.x)   # x1: lon; x2: lat
  spdf <- SpatialPointsDataFrame(coords, New_data)
  class(spdf)
  head(spdf)
  
  values <- raster::extract(raster_data$layer,spdf)
  spdf$elev <- values
  head(spdf)
  class(spdf)
  
# Filter by rut depth 
  spdf$rut_depth <- (spdf$elev - spdf$alt)
  DF <- as.data.frame(spdf)
  LKS_rut_depth_filter <- filter(DF, rut_depth > rut_depth_threshold) ### Insert rut depth here

## Filter by last group
  LKS_group_filter <- filter(LKS_rut_depth_filter, (nn_dist > 0.5 | is.na(nn_dist)))
 
## Set new ID
  LKS_group_filter$ID_3 <- seq.int(nrow(LKS_group_filter))
  
## Calculate distance between rut points
  tab_4 <- LKS_group_filter %>% dplyr::select(ID_3, lon.x, lat.x) %>% mutate(NR = case_when(ID_3>=0 ~ ID_3+1))
  LKS_apvienots_3 <- full_join(LKS_group_filter, tab_4, by = c("ID_3" = "NR"))
  LKS_apvienots_3$dist_ruts <- sqrt(abs(LKS_apvienots_3$lon.x.x - LKS_apvienots_3$lon.x.y)^2+abs(LKS_apvienots_3$lat.x.x - LKS_apvienots_3$lat.x.y)^2)
  LKS_apvienots_3 <- LKS_apvienots_3[complete.cases(LKS_apvienots_3$lat.x.y),]
  
## Rut groups
  
  LKS_apvienots_3$rut_group <- if_else(LKS_apvienots_3$dist_ruts>2,1,0)
  LKS_apvienots_3$rut_group_ID <- cumsum(LKS_apvienots_3$rut_group==1)
  LKS_apvienots_3 <- LKS_apvienots_3 %>% drop_na(lon.x.x)

## Create lines of ruts
  points_to_line <- function(data, lon, lat, id_field = NULL, sort_field = NULL) {
    
    # Convert to SpatialPointsDataFrame
    coordinates(data) <- c(lon, lat)
    
    if (!is.null(sort_field)) {
      if (!is.null(id_field)) {
        data <- data[order(data[[id_field]], data[[sort_field]]), ]
      } else {
        data <- data[order(data[[sort_field]]), ]
      }
    }
    
    if (is.null(id_field)) {
      
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      
      return(lines)
      
    } else if (!is.null(id_field)) {  
      
      # Split into a list by ID field
      paths <- sp::split(data, data[[id_field]])
      
      sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line0")))
      
      for (p in 2:length(paths)) {
        id <- paste0("line", as.character(p))
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
        sp_lines <- spRbind(sp_lines, l)
      }
      
      return(sp_lines)
    }
  }
  
## Filter lone points
  tab_5 <- LKS_apvienots_3 %>% dplyr::select(ID_3, rut_group) %>% mutate(NR = case_when(ID_3>=0 ~ ID_3-1))
  LKS_apvienots_4 <- full_join(LKS_apvienots_3, tab_5, by = c("ID_3" = "NR"))
  LKS_apvienots_4$lone_rut <- if_else(LKS_apvienots_4$rut_group.x+LKS_apvienots_4$rut_group.y==2,1,0)
  LKS_apvienots_lone_rut_filter <- filter(LKS_apvienots_4, lone_rut<1)
  LKS_apvienots_lone_rut_filter <- LKS_apvienots_lone_rut_filter %>% drop_na(nn_dist)
  rut_length <- ifelse(LKS_apvienots_lone_rut_filter$ID_3.y > 5, 
                       {
                         list(v_lines <- points_to_line(data = LKS_apvienots_lone_rut_filter,
                                                        lon = "lon.x.x",
                                                        lat = "lat.x.x",
                                                        id_field = "rut_group_ID",
                                                        sort_field = "ID_3.y"))
                         
                         
                         #Convert SpatialLines to sf object
                         sf_lines <- sf::st_as_sf(v_lines)
                         
                         v_lines
                         
                         class(v_lines)
                         plot(v_lines)
                         sum(st_length(sf_lines))
                       },
                       {0})
  dev.off()
  

## Calculate distances
 Travel_distance<- sum(LKS_apvienots_2$dist)
 print(paste("Total distance travelled -", round(Travel_distance, digits = 0), "m"))
 print(paste("Total length of ruts -", round(max(rut_length)), "m"))
 platiba <- landfill$VMDAREA
  
## Teksta faila sagatavosana 
 sink(file = paste(workdir, resultsdir, sep = "", "/output.txt"))
 cat(paste("Nogabalā veiktais attālums -", round(Travel_distance, digits = 0), "m"), "\n")
cat(paste("Rises garums nogabalā -", round(max(rut_length)), "m"), "\n")
 cat(paste("Nogaba platība -", platiba, "ha"), "\n")
cat(paste("Rises garums uz hektāra -", round((round(max(rut_length))/platiba),digits = 0), "m"), "\n")
cat(paste("TK aizņemtā platība -", round(buffer_union_area_ha, digits = 1), "ha", "\n"))
sink()
 
 write_xlsx(LKS_apvienots_2, 
            paste0(workdir, resultsdir, "/celi.xlsx"), 
            col_names = TRUE, 
            format_headers = TRUE)
  

  tryCatch({
    v_lines <- points_to_line(data = LKS_apvienots_lone_rut_filter,
                              lon = "lon.x.x",
                              lat = "lat.x.x",
                              id_field = "rut_group_ID",
                              sort_field = "ID_3.y")
    
    class_v_lines <- class(v_lines)
    plot(v_lines)

    IDS <- data.frame()
    for (i in (1:length(v_lines))) {
      id <- data.frame(v_lines@lines[[i]]@ID)
      IDS <- rbind(IDS, id)
    }
    IDS
    class(IDS)
    
    colnames(IDS)[1] <- "linkid"
    
    ruts_full <- join(IDS, LKS_apvienots_3)
    
    rut_df <- SpatialLinesDataFrame(v_lines,
                                    data.frame(ruts_full, row.names = ruts_full[,1]))
    
    class(rut_df)

    plot(rut_df)
    dev.off()

    sf_rut_df <- st_as_sf(rut_df, coords = c("x", "y"), crs = 3059)
    
    # Define the output directory and layer name
    output_directory <- paste(workdir, resultsdir, sep = "")
    output_path <- file.path(output_directory, "rises_all.shp")
    
# Write the sf object to a shapefile
    st_write(sf_rut_df, output_path)
    
  }, error = function(e) {
    # Handle the error (e.g., print a message, set a default value for v_lines, etc.)
    cat("An error occurred:", conditionMessage(e), "\n")
    v_lines <- 0  # Set v_lines to 0 in case of an error
  })
  


 png(file = paste(workdir, resultsdir, "/rises_plot.png", sep = ""), width = 1920, height = 1920)
  #Plot the first layer
 plot(st_geometry(landfill))
 #Plot the second layer on top of the first one
 plot(rut_df, add = TRUE, col = "red", pch = 20, lwd = 7) 
 dev.off()

# Calculation of root outside the felling area 


  linija <- st_transform(linija, st_crs(landfill))
  linijas_in_poligons <- linija[st_intersects(linija, landfill, sparse = FALSE), ]
  plot(st_geometry(linijas_in_poligons))
  
  buffer_10m <- st_buffer(linijas_in_poligons, dist = 10)
  buffer_intersect_landfill <- st_intersection(buffer_10m, landfill)
  
  buffer_10m_cleaned <- st_difference(buffer_10m, st_union(buffer_intersect_landfill))
  
  plot(st_geometry(buffer_10m_cleaned))

  bboxx<-st_bbox(buffer_10m_cleaned)
  sql_query_new <- paste0("SELECT t.*, st_x(t.geom),st_y(t.geom)
FROM public.gnss_data t WHERE t.geom && ST_SetSRID(ST_MakeBox2D(
    	  ST_SetSRID(ST_Point(",bboxx$xmin,",",bboxx$ymin,"), 3059),
        ST_SetSRID(ST_Point(",bboxx$xmax,", ",bboxx$ymax,"), 3059)),3059)");
  
  data_new <- dbGetQuery(con, sql_query_new)
  data_new <- data_new %>% filter(fix == 2)
  class(data_new)
  
  data_sf_new <- st_as_sf(data_new, coords = c("st_x", "st_y"), crs = 3059)
  
  points_inside_new <- st_filter(data_sf_new,buffer_10m_cleaned) 

  data_sf_new <- points_inside_new
  data_sf_new
  new_coords_new = st_crs(3059)
  data_3059_new <- st_transform(data_sf_new, new_coords_new)
  data_3059_new
  
  LKS_new <- data_3059_new %>%
    mutate(lon = unlist(map(data_3059_new$geometry,1)),
           lat = unlist(map(data_3059_new$geometry,2)))
  LKS_new
  class(LKS_new)
  
  # Make grid 
  area_fishnet_grid_new = st_make_grid(LKS_new, c(2, 2), what = "polygons", square = TRUE)

  fishnet_grid_sf_new = st_sf(area_fishnet_grid_new) %>%
    mutate(grid_id = 1:length(lengths(area_fishnet_grid_new)))
  class(fishnet_grid_sf_new)
  
  # Filter maximum elevation within grid cells
  Test_points_grid_id_new <- st_join(LKS_new, left = FALSE, fishnet_grid_sf_new["grid_id"]) # join points
  Points_no_geometry_new <- Test_points_grid_id_new
  class(Points_no_geometry_new)
  st_geometry(Points_no_geometry_new) <- NULL
  class(Points_no_geometry_new)
  
  Filter_Vacc_new <- filter(Points_no_geometry_new, vacc<0.05)
  Filter_fix_new <- filter(Filter_Vacc_new, fix==2)
  class(Filter_fix_new)
  
  Filter_top_points_new <- Filter_fix_new %>%
    group_by(grid_id) %>%
    slice(which.max(alt))
  class(Filter_top_points_new)
  
  Filter_top_points_new <- Filter_fix_new %>%
    group_by(grid_id) %>%
    filter(alt == max(alt))
  
  # Create elevation raster
  test_temp_new <- Filter_top_points_new
  test_temp_new$x <- test_temp_new$lon
  test_temp_new$y <- test_temp_new$lat
  test_temp_new<-na.omit(test_temp_new)
  coordinates(test_temp_new) = ~x + y
  class(test_temp_new)
  
  interp_points_new <- with(test_temp_new, interp(test_temp_new$lon, test_temp_new$lat, test_temp_new$alt, extrap = TRUE, nx = 1000, ny = 1000, duplicate = "mean"))
  
  # Convert the interpolation result to a raster
  raster_data_new <- raster(interp_points_new)
  class(raster_data_new)
  # Plot the raster
  plot(raster_data_new, main = "Altitude Interpolation")
  dev.off()
  
  ### Calculate distance between points
  LKS_df_new <- LKS_new %>% st_drop_geometry()
  LKS_df_new
  tab_2_new <- LKS_df_new %>% dplyr::select(id, lon, lat) %>% mutate(NR = case_when(id>=0 ~ id+1))
  LKS_apvienots_new <- full_join(LKS_df_new, tab_2_new, by = c("id" = "NR"))
  LKS_apvienots_new$dist <- sqrt(abs(LKS_apvienots_new$lon.x - LKS_apvienots_new$lon.y)^2+abs(LKS_apvienots_new$lat.x - LKS_apvienots_new$lat.y)^2)
  
  
  ### Remove stationary points
  LKS_dist_filter_new <- filter(LKS_apvienots_new,dist > 0.05)
  
  ### Set new ID
  LKS_dist_filter_new$ID_2 <- seq.int(nrow(LKS_dist_filter_new))
  
  ### Calculate bearing
  LKS_dist_filter_new$bearring <- (atan(( LKS_dist_filter_new$lon.y - LKS_dist_filter_new$lon.x )/
                                      ( LKS_dist_filter_new$lat.y - LKS_dist_filter_new$lat.x )))*180/3.14159 +(180*((( LKS_dist_filter_new$lat.y - LKS_dist_filter_new$lat.x )<0)+
                                                                                                               ((( LKS_dist_filter_new$lon.y - LKS_dist_filter_new$lon.x )<0 & ( LKS_dist_filter_new$lat.y - LKS_dist_filter_new$lat.x )>0)*2)))
  
  ### Calculate turning point
  tab_3_new <- LKS_dist_filter_new %>% dplyr::select(ID_2, bearring) %>% mutate(NR = case_when(ID_2>=0 ~ ID_2+1))
  LKS_apvienots_2_new <- full_join(LKS_dist_filter_new, tab_3_new, by = c("ID_2" = "NR"))
  LKS_apvienots_2_new$turn <- if_else((abs( LKS_apvienots_2_new$bearring.x - LKS_apvienots_2_new$bearring.y ))>120
                                  & (abs( LKS_apvienots_2_new$bearring.x - LKS_apvienots_2_new$bearring.y ))<240, 1, 0)
  
  ### Remove lines with empty cells
  LKS_apvienots_2_new <- LKS_apvienots_2_new[complete.cases(LKS_apvienots_2_new$turn),]
  
  ### Grouping by passes
  LKS_apvienots_2_new$group<-cumsum(LKS_apvienots_2_new$turn==1)
  
  ### Calculate distance from any point to closest point from higher pass group
  LKS_apvienots_2_new$ID <- LKS_apvienots_2_new$group
  LKS_apvienots_2_new$X <- LKS_apvienots_2_new$lon.x
  LKS_apvienots_2_new$Y <- LKS_apvienots_2_new$lat.x
  LKS_apvienots_2_new$uid <- LKS_apvienots_2_new$ID_2.y
  
  setDT(LKS_apvienots_2_new)
  class(LKS_apvienots_2_new)
  
  dist_new <- function(x,y,d) {
    d[, nn_dist:=sqrt((X-x)^2 + (Y-y)^2)][order(nn_dist)][1]
  }
  LKS_apvienots_2_new[, c("nnX", "nnY", "nn", "nn_dist"):=dist_new(X,Y, LKS_apvienots_2_new[ID>.BY$ID,.(X,Y,uid)]),by=.(ID,uid)]
  
  ### Filter by noise
  LKS_apvienots_2_new$noise <- abs(LKS_apvienots_2_new$L-LKS_apvienots_2_new$R)
  LKS_noise_filter_new <- filter(LKS_apvienots_2_new, noise < 20)
  LKS_noise_filter_new
  class(LKS_noise_filter_new)
  
  ### Extract raster values
  New_data_new <- dplyr::select(LKS_apvienots_2_new, c('ID_2','lon.x','lat.x', 'alt', 'nn_dist'))
  class(New_data_new)
  
  coords_new <- cbind(New_data_new$lon.x, New_data_new$lat.x)   # x1: lon; x2: lat
  spdf_new <- SpatialPointsDataFrame(coords_new, New_data_new)
  class(spdf_new)
  head(spdf_new)
  
  values_new <- raster::extract(raster_data_new$layer,spdf_new)
  spdf_new$elev <- values_new
  head(spdf_new)
  class(spdf_new)
  
  ### Filter by rut depth
  spdf_new$rut_depth <- (spdf_new$elev - spdf_new$alt)
  DF_new <- as.data.frame(spdf_new)

  LKS_rut_depth_filter_new <- filter(DF_new, rut_depth > rut_depth_threshold) 
  
  ### Filter by last group
  LKS_group_filter_new <- filter(LKS_rut_depth_filter_new, (nn_dist > 0.5 | is.na(nn_dist)))
  
  ### Set new ID
  LKS_group_filter_new$ID_3 <- seq.int(nrow(LKS_group_filter_new))
  
  ### Calculate distance between rut points
  tab_4_new <- LKS_group_filter_new %>% dplyr::select(ID_3, lon.x, lat.x) %>% mutate(NR = case_when(ID_3>=0 ~ ID_3+1))
  LKS_apvienots_3_new <- full_join(LKS_group_filter_new, tab_4_new, by = c("ID_3" = "NR"))
  LKS_apvienots_3_new$dist_ruts <- sqrt(abs(LKS_apvienots_3_new$lon.x.x - LKS_apvienots_3_new$lon.x.y)^2+abs(LKS_apvienots_3_new$lat.x.x - LKS_apvienots_3_new$lat.x.y)^2)
  LKS_apvienots_3_new <- LKS_apvienots_3_new[complete.cases(LKS_apvienots_3_new$lat.x.y),]
  
  ### Rut groups
  
  LKS_apvienots_3_new$rut_group <- if_else(LKS_apvienots_3_new$dist_ruts>2,1,0)
  LKS_apvienots_3_new$rut_group_ID <- cumsum(LKS_apvienots_3_new$rut_group==1)
  LKS_apvienots_3_new <- LKS_apvienots_3_new %>% drop_na(lon.x.x)
  
  ### Create lines of ruts
  points_to_line_new <- function(data_new, lon, lat, id_field = NULL, sort_field = NULL) {
    
    # Convert to SpatialPointsDataFrame
    coordinates(data_new) <- c(lon, lat)
    
    if (!is.null(sort_field)) {
      if (!is.null(id_field)) {
        data_new <- data_new[order(data_new[[id_field]], data_new[[sort_field]]), ]
      } else {
        data_new <- data_new[order(data_new[[sort_field]]), ]
      }
    }
    
    if (is.null(id_field)) {
      
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      
      return(lines)
      
    } else if (!is.null(id_field)) {  
      
      # Split into a list by ID field
      paths <- sp::split(data_new, data_new[[id_field]])
      
      sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line0")))
      
      for (p in 2:length(paths)) {
        id <- paste0("line", as.character(p))
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
        sp_lines <- spRbind(sp_lines, l)
      }
      
      return(sp_lines)
    }
  }
  
  ### Filter lone points
  tab_5_new <- LKS_apvienots_3_new %>% dplyr::select(ID_3, rut_group) %>% mutate(NR = case_when(ID_3>=0 ~ ID_3-1))
  LKS_apvienots_4_new <- full_join(LKS_apvienots_3_new, tab_5_new, by = c("ID_3" = "NR"))
  LKS_apvienots_4_new$lone_rut <- if_else(LKS_apvienots_4_new$rut_group.x+LKS_apvienots_4_new$rut_group.y==2,1,0)
  LKS_apvienots_lone_rut_filter_new <- filter(LKS_apvienots_4_new, lone_rut<1)
  LKS_apvienots_lone_rut_filter_new <- LKS_apvienots_lone_rut_filter_new %>% drop_na(nn_dist)
  rut_length_new <- ifelse(LKS_apvienots_lone_rut_filter_new$ID_3.y > 5, 
                       {
                         list(v_lines_new <- points_to_line_new(data = LKS_apvienots_lone_rut_filter_new,
                                                        lon = "lon.x.x",
                                                        lat = "lat.x.x",
                                                        id_field = "rut_group_ID",
                                                        sort_field = "ID_3.y"))
                         
                         
                         #Convert SpatialLines to sf object
                         sf_lines_new <- sf::st_as_sf(v_lines_new)
                         
                         v_lines_new
                         
                         class(v_lines_new)
                         plot(v_lines_new)
                         sum(st_length(sf_lines_new))
                       },
                       {0})
  dev.off()
  
### Calculate distances
  Travel_distance_new<- sum(LKS_apvienots_2_new$dist)
  print(paste("Total length of ruts tk -", round(max(rut_length_new)), "m"))


  sink(file = paste(workdir, resultsdir, sep = "", "/output_tk.txt"))
  cat(paste("Rises garums TK -", round(max(rut_length_new)), "m"), "\n")
  sink()
  
  # Define the file path for the CSV output
  output_file <- paste(workdir, resultsdir, "/output_tk.csv", sep = "")
    # Create the data frame with the required information
  output_data <- data.frame(
    Description = "Rises garums TK",
    Value = round(max(rut_length_new)),
    Unit = "m"
  )
  # Write the data frame to a CSV file
  write.csv(output_data, file = output_file, row.names = FALSE)
  
  
  tryCatch({
    v_lines_new <- points_to_line_new(data = LKS_apvienots_lone_rut_filter_new,
                              lon = "lon.x.x",
                              lat = "lat.x.x",
                              id_field = "rut_group_ID",
                              sort_field = "ID_3.y")
    
    class_v_lines_new <- class(v_lines_new)
    
    plot(v_lines_new)
    
    # Continue with the rest of your code if there is no error
    IDS_new <- data.frame()
    for (i in (1:length(v_lines_new))) {
      id_new <- data.frame(v_lines_new@lines[[i]]@ID)
      IDS_new <- rbind(IDS_new, id_new)
    }
    IDS_new
    class(IDS_new)
    
    colnames(IDS_new)[1] <- "linkid"
    
    ruts_full_new <- join(IDS_new, LKS_apvienots_3_new)
    
    rut_df_tk_new <- SpatialLinesDataFrame(v_lines_new,
                                    data.frame(ruts_full_new, row.names = ruts_full_new[,1]))
    
    class(rut_df_tk_new)
    
    plot(rut_df_tk_new)
    dev.off()
    
    sf_rut_df_new <- st_as_sf(rut_df_tk_new, coords = c("x", "y"), crs = 3059)
    
    # Define the output directory and layer name
    output_directory_new <- paste(workdir, resultsdir, sep = "")
    output_path_new <- file.path(output_directory_new, "rises_tk_all.shp")
    
    # Write the sf object to a shapefile
    st_write(sf_rut_df_new, output_path_new)
    
  }, error = function(e) {
    # Handle the error (e.g., print a message, set a default value for v_lines, etc.)
    cat("An error occurred:", conditionMessage(e), "\n")
    v_lines_new <- 0  # Set v_lines to 0 in case of an error
  })
  

  png(file = paste(workdir, resultsdir, "/rises_plot_tk.png", sep = ""), width = 1920, height = 1920)
  # Plot the first layer
  plot(st_geometry(buffer_10m_cleaned))
  plot(rut_df_tk_new, add = TRUE, col = "blue", pch = 20, lwd = 7) 
  dev.off()
}

