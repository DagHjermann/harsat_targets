
#
# Folders: 
# - input_data = full contaminant data  
# - harsat_data = sample contaminant data, station data (with "NO" in filename), info file


library(targets)
# library(harsat)

source("R/functions.R")
source("R/functions_utility.R")


if (FALSE){
  # 
  # Do only once
  # Using 'get_info_object', see R/functions  
  #
  info <- get_info_object(
    compartment = "biota",
    purpose = "OSPAR",
    contaminants = "raw_data_sample.csv",   # NB, replace biota data filename above, as appropriate 
    stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
    data_dir = file.path("harsat_data"),
    data_format = "external",
    info_dir = file.path("information", "OSPAR_2022")
  )
  str(info, 1)
  saveRDS(info, "harsat_data/info.rds")
  
}

# debugonce(read_stations2)
# test <- read_stations2(infile = "harsat_data/ICES_DOME_STATIONS_20230829_NO.csv", info)
# test <- read_contaminants2(infile = "harsat_data/raw_data_sample.csv", 



# inspect pipeline

tar_manifest()
tar_visnetwork()
tar_make()

x <- tar_read(biota_data)
str(x, 1)

object <- tar_read(biota_timeseries)
str(object, 1)
rm(object)


split_timeseries_object <- function(object){
  # split time series in list
  timeSeries_list <- split(object$timeSeries, object$timeSeries$determinand)
  determs <- names(timeSeries_list)
  # split data in list
  data_list <- lapply(determs, function(determ) { subset(object$data, determinand %in% determ) })
  # define result as a list with one element per determinand
  result <- vector(mode = "list", length = length(determs))
  names(result) <- determs
  # define each list item
  for (i in seq_along(result)){
    result[[i]] <- list(
      call = object$call,
      call.data = object$call.data,
      info = object$info,
      data = data_list[[i]],
      timeSeries = timeSeries_list[[i]]
    )
  }
  result
}

x <- tar_read(biota_timeseries)
biota_timeseries_list <- split_timeseries_object(x)
str(biota_timeseries_list, 1)
str(biota_timeseries_list, 2)



# time series
timeseries_list <- split(x$timeSeries, x$timeSeries$determinand)
determs <- names(timeseries_list)

# data
data_list <- lapply(determs, function(determ) { subset(x$data, determinand %in% determ) })
str(data_list, 1)
# Check
lapply(data_list, function(df) { table(df$determinand)} )

#
# info
#
str(x$info, 1)
str(x$info$determinand, 1)
info_determinand_list <- lapply(determs, 
                                function(determ) x$info$determinand[rownames(x$info$determinand) %in% determ,])
str(info_determinand_list, 1)
str(x$info$thresholds, 1)
info_thresholds_list <- lapply(determs, 
                                function(determ) subset(x$info$thresholds, determinand %in% determ))
str(info_thresholds_list, 1)


