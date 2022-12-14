#' @title 
#' Convert emission estimates from list to data.table format
#' 
#' @description 
#' Read emission estimates generated by the \code{\link{emission_model}} or from
#' emission factor functions (e.g. \code{\link{ef_brazil_cetesb}}) and convert 
#' them into a `data.table` format.
#' 
#' @param emi_list list. A list of emission estimates
#' @param emi_vars character. data.frame names of 'emi_list' object attributed 
#'        to emissions or  emission factors. Default is 'emi'.
#' @param veh_vars character. data.frame names of 'emi_list' object attributed 
#'        to vehicle characteristics. Default is 'veh_type'.
#' @param pol_vars character. data.frame names of 'emi_list' object attributed 
#'        to pollutants. Default is 'pollutant'.
#' @param segment_vars character. data.frame names of 'emi_list' object 
#'        attributed to the road segments. Default is NULL.
#' 
#' @return data.table.
#' 
#' @family emission analysis

#' @examples
#' \donttest{
#' library(gtfs2emis)
#' library(gtfstools)
#' 
#' # read GTFS
#' gtfs_file <- system.file("extdata/bra_cur_gtfs.zip", package = "gtfs2emis")
#' gtfs <- gtfstools::read_gtfs(gtfs_file) 
#' 
#' # keep a single trip_id to speed up this example
#' gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4451136")
#'   
#' # run transport model
#' tp_model <- transport_model(gtfs_data = gtfs_small,
#'                             min_speed = 2,
#'                             max_speed = 80,
#'                             new_speed = 20,
#'                             spatial_resolution = 100,
#'                             parallel = FALSE)
#' 
#' # Example using Brazilian emission model and fleet
#' fleet_data_ef_cetesb <- data.frame(veh_type = "BUS_URBAN_D",
#'                                    model_year = 2010:2019,
#'                                    fuel = "D",
#'                                    fleet_composition = rep(0.1,10)
#'                                    )
#'                                    
#' emi_list <- emission_model(
#'                 tp_model = tp_model,
#'                 ef_model = "ef_brazil_cetesb",
#'                 fleet_data = fleet_data_ef_cetesb,
#'                 pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                 )
#' 
#' # convert emission list to data.table
#' dt <- emis_to_dt(emi_list)
#' }
#' @export
emis_to_dt <- function(emi_list, emi_vars = "emi", veh_vars = "veh_type"
                       , pol_vars = "pollutant", segment_vars = NULL){
  
  
  
  # checkings -----
  checkmate::assert_list(emi_list, null.ok = FALSE)
  # emi_vars
  checkmate::assert_vector(emi_vars,any.missing = FALSE,min.len = 1,null.ok = FALSE)
  checkmate::assert_character(emi_vars,any.missing = FALSE,min.len = 1)
  for(i in emi_vars) checkmate::assert_choice(i,names(emi_list),null.ok = FALSE)
  # veh_vars
  checkmate::assert_vector(veh_vars,any.missing = FALSE,min.len = 1,null.ok = FALSE)
  checkmate::assert_character(veh_vars,any.missing = FALSE,min.len = 1)
  for(i in veh_vars) checkmate::assert_choice(i,names(emi_list),null.ok = FALSE)
  # pol_vars
  checkmate::assert_vector(pol_vars,any.missing = FALSE,min.len = 1,null.ok = FALSE)
  checkmate::assert_character(pol_vars,any.missing = FALSE,min.len = 1)
  for(i in pol_vars) checkmate::assert_choice(i,names(emi_list),null.ok = FALSE)
  # segment_vars
  checkmate::assert_vector(segment_vars,any.missing = FALSE,min.len = 1,null.ok = TRUE)
  checkmate::assert_character(segment_vars,any.missing = FALSE,min.len = 1,null.ok = TRUE)
  for(i in segment_vars) checkmate::assert_choice(i,names(emi_list),null.ok = FALSE)
  # all variables
  all_vars = c(veh_vars, pol_vars,emi_vars)
  
  # check units 
  myunits <- sapply(seq_along(emi_list[[emi_vars]]),function(i){
    units::deparse_unit(emi_list[[emi_vars]][[i]])
  })
  
  if(length(unique(myunits)) == 1){
    myunits <- myunits[1]
  }else{
    stop("Invalid units: units are not the same for all emissions columns")
  }
  
  #
  # merge------
  #
  
  dt <- lapply(1:length(emi_list[[pol_vars]]),function(i){ # i = 3
    
    # add vars
    tmp_dt <- lapply(all_vars,function(j){ # j = 3;i = 3
      emi_list[[j]][[i]]
    })
    
    if(!is.null(segment_vars)){
      tmp_dt <- do.call(c, list(tmp_dt, emi_list[segment_vars]))
      tmp_dt <- do.call(cbind,tmp_dt)
      tmp_dt <- data.table::as.data.table(tmp_dt)
      names(tmp_dt) <- c(all_vars,segment_vars)
    }else{
      tmp_dt <- do.call(cbind,tmp_dt)
      tmp_dt <- data.table::as.data.table(tmp_dt)
      names(tmp_dt) <- all_vars
    }
    
    
    # convert columns
    
    tmp_dt[,segment_id := 1:nrow(tmp_dt)]
    if(!is.null(emi_vars))      tmp_dt[,(emi_vars) := lapply(.SD, as.numeric), .SDcols = emi_vars]
    if(!is.null(veh_vars))      tmp_dt[,(veh_vars) := lapply(.SD, as.factor), .SDcols = veh_vars]
    if(!is.null(segment_vars))  tmp_dt[,(segment_vars) := lapply(.SD, as.numeric), .SDcols = segment_vars]
    if(!is.null(pol_vars))      tmp_dt[,(pol_vars) := lapply(.SD, as.character), .SDcols = pol_vars]
    return(tmp_dt)
  }) 
  dt <- data.table::rbindlist(dt)
  
  # add units back ----
  
  units(dt[[emi_vars]]) <- myunits
  
  # return
  
  return(dt)
}