## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  library(devtools)
#  # From CRAN
#  install.packages("gtfs2emis")
#  
#  # Dev. version with latest features
#  # install.packages("remotes")
#  remotes::install_github("ipeaGIT/gtfs2emis")

## -----------------------------------------------------------------------------
data_path <- system.file("extdata", package = "gtfs2emis")
list.files(data_path)

## ----message = FALSE----------------------------------------------------------
library(gtfs2emis)
library(gtfstools)
library(progressr)
library(data.table)
library(ggplot2)
library(units)
library(sf)

## ----message = FALSE----------------------------------------------------------
# path to GTFS.zip file
gtfs_file <- system.file("extdata/irl_dub_gtfs.zip", package = "gtfs2emis")

# read GTFS
gtfs <- gtfstools::read_gtfs(gtfs_file)

# Keep Monday services GTFS
gtfs <- gtfstools::filter_by_weekday(gtfs, 
                                     weekday = c('saturday', 'sunday'), 
                                     keep = FALSE)

## ----message = FALSE----------------------------------------------------------
# generate transport model
progressr::with_progress( 
  
  tp_model <- transport_model(gtfs_data = gtfs,
                              min_speed = 2,
                              max_speed = 80,
                              spatial_resolution = 100,
                              parallel = TRUE) 
  )

head(tp_model)

## ----message = FALSE, fig.width=5, fig.height=5-------------------------------
ggplot(data = tp_model) +
  geom_sf(aes(color= as.numeric(speed))) +
  scale_color_continuous(type = "viridis")+
  labs(color = "Speed (km/h)")+
  theme_void()


## ----message = FALSE----------------------------------------------------------
fleet_file <- system.file("extdata/irl_dub_fleet.txt", package = "gtfs2emis")
fleet_df <- read.csv(fleet_file)
head(fleet_df)


## ----message = FALSE----------------------------------------------------------
emi_list <- emission_model(tp_model = tp_model,
                           ef_model = "ef_europe_emep",
                           fleet_data = fleet_df,
                           pollutant = c("CO2","PM10"),
                           reference_year = 2020
                           )

names(emi_list)

## ----eval = TRUE--------------------------------------------------------------
emi_dt <- emis_to_dt(emi_list = emi_list,
                     veh_vars = c("veh_type", "euro", "fuel"),
                     pol_vars = "pollutant"
                     )

head(emi_dt) 


## ----message = FALSE----------------------------------------------------------
emi_by_pol <- emis_summary(emi_list = emi_list,
                                 by = "pollutant") 
emi_by_pol


## ----fig.height=5, fig.width=8------------------------------------------------
emi_by_veh <- emis_summary(emi_list = emi_list,
                          by = "vehicle") 
head(emi_by_veh)

## ----message = FALSE, fig.width=5, fig.height=6-------------------------------
emi_by_veh <- emis_summary(emi_list = emi_list,
                          by = "vehicle",
                          veh_vars = c("veh_type","euro")) 
head(emi_by_veh)

# plot
ggplot(data = emi_by_veh) +
  geom_col(aes(x = euro, y = as.numeric(emi/1000), fill = pollutant), 
           color=NA, show.legend = FALSE) +
  labs(y="Total emissions (Kg)", x="Euro standard") +
  facet_wrap(~pollutant, scales = "free", nrow = 2) +
  theme_minimal()


## ----fig.width=5, fig.height=6------------------------------------------------
emi_by_time <- emis_summary(emi_list = emi_list,
                                by = "time") 
head(emi_by_time) 

# plot
ggplot(data = emi_by_time) +
  geom_col(aes(x = factor(timestamp_hour), y = as.numeric(emi/1000), fill = pollutant),
           color=NA, show.legend = FALSE) +
  labs(y="Total emissions (Kg)", x="Hour of the day") +
  facet_wrap(~pollutant, scales = "free", nrow = 2) + 
  theme_minimal()


## ----message = FALSE, fig.width=5, fig.height=5-------------------------------
# create spatial grid
mygrid <- sf::st_make_grid(
  x = sf::st_make_valid(emi_list$tp_model)
  , cellsize = 0.25 / 200
  , crs= 4326
  , what = "polygons"
  , square = FALSE)

 ggplot() + 
   geom_sf(data=mygrid) +
   theme_void()

## ----message=FALSE, fig.width=5, fig.height=5---------------------------------
mygrid_emi <- emis_grid(emi_list, mygrid,time_resolution = "day"
                        ,quiet = FALSE)

ggplot() +
  geom_sf(data = mygrid_emi, aes(fill= as.numeric(CO2_Euro_III)), color=NA) +
  geom_sf(data = emi_list$tp_model$geometry,color = "black")+
scale_fill_continuous(type = "viridis")+
  labs(fill = "CO2 (g)")+
  theme_void()


