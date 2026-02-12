## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----message = FALSE, echo = FALSE--------------------------------------------
#library(devtools)
#devtools::load_all()

## ----eval = FALSE,message = FALSE---------------------------------------------
# #library(gtfs2emis)

## ----message = FALSE----------------------------------------------------------
library(units)
library(gtfs2emis)
library(ggplot2)


ef_europe <- ef_europe_emep(speed = units::set_units(10:100,"km/h")
                            ,veh_type = c("Ubus Midi <=15 t"
                                          ,"Ubus Std 15 - 18 t"
                                          ,"Ubus Artic >18 t")
                            ,euro = c("III", "IV", "V")
                            ,pollutant = c("PM10", "NOx")
                            ,fuel = c("D", "D", "D")
                            ,tech = c("-", "SCR", "SCR")
                            ,as_list = TRUE)
names(ef_europe)

## -----------------------------------------------------------------------------
ef_europe_dt <- emis_to_dt(emi_list = ef_europe
                           ,emi_vars = "EF"
                           ,veh_vars = c("veh_type","euro","fuel","tech")
                           ,pol_vars = "pollutant"
                           ,segment_vars = c("slope","load","speed"))
head(ef_europe_dt)

## ----fig.height=4, fig.width=7------------------------------------------------
ef_europe_dt$name_fleet <- paste(ef_europe_dt$veh_type, "/ Euro"
                                 , ef_europe_dt$euro)

# plot
ggplot(ef_europe_dt) + 
  geom_line(aes(x = speed,y = EF,color = name_fleet))+
  labs(color = "Category / EURO")+
  facet_wrap(~pollutant,scales = "free")+
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
ef_europe_co2 <- ef_europe_emep(speed = units::set_units(10:100,"km/h")
                                ,veh_type = "Ubus Std 15 - 18 t"
                                ,euro = "VI",pollutant = "CO2"
                                ,tech = "DPF+SCR"
                                ,as_list = TRUE)

## -----------------------------------------------------------------------------
fleet_filepath <- system.file("extdata/bra_cur_fleet.txt", package = "gtfs2emis")
cur_fleet <- read.table(fleet_filepath,header = TRUE, sep = ",", nrows = 1)
cur_fleet

## -----------------------------------------------------------------------------
cur_local_ef <- ef_brazil_cetesb(pollutant = "CO2"
                                 ,veh_type = cur_fleet$type_name_br
                                 ,model_year = cur_fleet$year)
head(cur_local_ef)

# convert Local EF to data.frame
cur_local_ef_dt <- emis_to_dt(emi_list = cur_local_ef
                             ,emi_vars = "EF")

## -----------------------------------------------------------------------------
# Euro EF
cur_euro_ef <- ef_europe_emep(speed = units::set_units(10:100,"km/h")
                              ,veh_type = cur_fleet$veh_type
                              ,euro = cur_fleet$euro
                              ,pollutant = "CO2"
                              ,tech = "-"
)

# convert to data.frame
cur_euro_ef_dt <- emis_to_dt(emi_list = cur_euro_ef
                             ,emi_vars = "EF"
                             ,veh_vars = c("veh_type","euro","fuel","tech")
                             ,segment_vars = "speed")
cur_euro_ef_dt$source <- "Euro EF"

## -----------------------------------------------------------------------------
cur_scaled_ef <- ef_scaled_euro(ef_local = cur_local_ef$EF
                                ,speed = units::set_units(10:100,"km/h")
                                ,veh_type = cur_fleet$veh_type
                                ,euro = cur_fleet$euro
                                ,pollutant = "CO2"
                                ,tech = "-"
                                )
# convert to data.frame
cur_scaled_ef_dt <- emis_to_dt(emi_list = cur_scaled_ef
                               ,emi_vars = "EF"
                               ,veh_vars = c("veh_type","euro","fuel","tech")
                               ,segment_vars = "speed")
cur_scaled_ef_dt$source <- "Scaled EF"

## ----fig.width=6, fig.height=5------------------------------------------------
# rbind data
cur_ef <- rbind(cur_euro_ef_dt, cur_scaled_ef_dt)
cur_ef$source <- factor(cur_ef$source
                        ,levels = c("Scaled EF", "Euro EF"))

# plot
ggplot() + 
  # add scaled and euro EF
  geom_line(data = cur_ef
            ,aes(x = speed,y = EF
                 ,group = source,color = source))+
  # add local EF
  geom_hline(aes(yintercept = cur_local_ef_dt$EF)
            ,colour = "black",linetype="dashed") + 
  geom_point(aes(x = units::set_units(19,'km/h')
                 ,y = cur_local_ef$EF)) + 
  # add local EF text
  geom_text(aes(x = units::set_units(19,'km/h')
                , y = cur_local_ef_dt$EF)
            ,label = sprintf('Local EF = %s g/km at 19 km/h',round(cur_local_ef_dt$EF,1))
            ,hjust = 0,nudge_y = 100,nudge_x = 1
            ,size = 3,fontface = 1)+
  # configs plots
  scale_color_manual(values=c("red","blue"))+
  coord_cartesian(ylim = c(0,max(cur_scaled_ef_dt$EF)))+
  labs(color = NULL)

