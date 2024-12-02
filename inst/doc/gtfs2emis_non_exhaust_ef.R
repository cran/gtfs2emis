## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## -----------------------------------------------------------------------------
library(gtfs2emis)
emi_europe_emep_wear(dist = units::set_units(1,"km"),
                              speed =  units::set_units(30,"km/h"),
                              pollutant = c("PM10","TSP","PM2.5"),
                              veh_type = "Ubus Std 15 - 18 t",
                              fleet_composition = 1,
                              load = 0.5,
                              process = c("tyre"),
                              as_list = TRUE)

## ----message = FALSE----------------------------------------------------------
emi_europe_emep_wear(dist = units::set_units(1,"km"),
                              speed =  units::set_units(30,"km/h"),
                              pollutant = c("PM10","TSP","PM2.5"),
                              veh_type = "Ubus Std 15 - 18 t",
                              fleet_composition = 1,
                              load = 0.5,
                              process = c("brake"),
                              as_list = TRUE)

## ----message = FALSE----------------------------------------------------------
emi_europe_emep_wear(dist = units::set_units(1,"km"),
                              speed =  units::set_units(30,"km/h"),
                              pollutant = c("PM10","TSP","PM2.5"),
                              veh_type = "Ubus Std 15 - 18 t",
                              fleet_composition = 1,
                              load = 0.5,
                              process = c("road"),
                              as_list = TRUE)

## ----message = FALSE,fig.height=3, fig.width=8--------------------------------
library(units)
library(ggplot2)

emis_list <- emi_europe_emep_wear(dist = units::set_units(rep(1,100),"km"),
                     speed =  units::set_units(1:100,"km/h"),
                     pollutant = c("PM10","TSP","PM2.5"),
                     veh_type = c("Ubus Std 15 - 18 t"),
                     fleet_composition = c(1),
                     load = 0.5,
                     process = c("brake","tyre","road"),
                     as_list = TRUE)
ef_dt <- gtfs2emis::emis_to_dt(emis_list,emi_vars = "emi"
                               ,segment_vars = "speed")
ggplot(ef_dt)+
  geom_line(aes(x = as.numeric(speed),y = as.numeric(emi),color = pollutant))+
  facet_wrap(facets = vars(process))+
  labs(x = "Speed (km/h)",y = "Emissions (g)")+
  theme_minimal()

## ----message = FALSE----------------------------------------------------------
library(gtfstools)
library(sf)

# read GTFS
gtfs_file <- system.file("extdata/bra_cur_gtfs.zip", package = "gtfs2emis")
gtfs <- gtfstools::read_gtfs(gtfs_file) 

# keep a single trip_id to speed up this example
gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4451136")

# run transport model
tp_model <- transport_model(gtfs_data = gtfs_small,
                            spatial_resolution = 100,
                            parallel = FALSE)

# Fleet data, using Brazilian emission model and fleet
fleet_data_ef_emep <- data.frame(veh_type = "Ubus Std 15 - 18 t",
                                 fleet_composition = 1,
                                 euro = "V",   # for hot-exhaust emissions 
                                 fuel = "D",   # for hot-exhaust emissions 
                                 tech = "SCR") # for hot-exhaust emissions 
# Emission model (hot-exhaust)
emi_list_he <- emission_model(
  tp_model = tp_model,
  ef_model = "ef_europe_emep",
  fleet_data = fleet_data_ef_emep,
  pollutant = "PM10"
)

# Emission model (non-exhaust)
emi_list_ne <- emi_europe_emep_wear(
  dist = tp_model$dist,
  speed = tp_model$speed,
  pollutant = "PM10",
  veh_type = c("Ubus Std 15 - 18 t"),
  fleet_composition = c(1),
  load = 0.5,
  process = c("brake","tyre","road"),
  as_list = TRUE)

emi_list_ne$tp_model <- tp_model

## ----message = FALSE----------------------------------------------------------
# create spatial grid
grid <- sf::st_make_grid(
  x = sf::st_make_valid(tp_model)
  , cellsize = 0.25 / 200
  , crs= 4326
  , what = "polygons"
  , square = FALSE
)

# grid (hot-exhaust)
emi_grid_he <- emis_grid( emi_list_he,grid,time_resolution = 'day'
                          ,aggregate = TRUE)
setDT(emi_grid_he)
pol_names <- setdiff(names(emi_grid_he),"geometry")
emi_grid_he_dt <- melt(emi_grid_he,measure.vars = pol_names,id.vars = "geometry")
emi_grid_he_dt <- sf::st_as_sf(emi_grid_he_dt)

# grid (non-exhaust)
emi_grid_ne <- emis_grid( emi_list_ne,grid,time_resolution = 'day'
                       ,aggregate = TRUE)
setDT(emi_grid_ne)
pol_names <- setdiff(names(emi_grid_ne),"geometry")
emi_grid_ne_dt <- melt(emi_grid_ne,measure.vars = pol_names,id.vars = "geometry")
emi_grid_ne_dt <- sf::st_as_sf(emi_grid_ne_dt)

# bind grid
emi_grid_dt <- data.table::rbindlist(l = list(emi_grid_he_dt,emi_grid_ne_dt))
emi_grid_sf  <- sf::st_as_sf(emi_grid_dt)

## ----message = FALSE,fig.height=3, fig.width=6--------------------------------
# plot
library(ggplot2)

ggplot(emi_grid_sf) +
  geom_sf(aes(fill= as.numeric(value)), color=NA) +
  geom_sf(data = tp_model$geometry,color = "black")+
  scale_fill_continuous(type = "viridis")+
  labs(fill = "PM10 (g)")+
  facet_wrap(facets = vars(variable),nrow = 1)+
  theme_void()

## ----message = FALSE,fig.height=3, fig.width=6--------------------------------
# Emissions by time
emi_time_he <- emis_summary(emi_list_he,by = "time")
emi_time_ne <- emis_summary(emi_list_ne,by = "time")

emi_time <- data.table::rbindlist(l = list(emi_time_he,emi_time_ne))

ggplot(emi_time)+
  geom_col(aes(x = process,y = as.numeric(emi),fill = as.numeric(emi)))+
  scale_fill_continuous(type = "viridis")+
  labs(fill = "PM10 level",y = "Emissions (g)")+
  theme_minimal()

