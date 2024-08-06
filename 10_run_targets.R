
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

# test results using 'ggplot_assessment'  
x <- tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]])
ggplot_assessment(x)
ggplot_assessment(x, plot_points = "all")
ggplot_assessment(x, plot_points = "annual", logscale = FALSE)
ggplot_assessment(x, plot_points = "all", logscale = FALSE, ylim = c(0,17))

# test doing assessment of one part of data using intermediate results
obj_ts_list <- tar_read(biota_timeseries_list)
names(obj_ts_list)
obj_ts <- obj_ts_list[["CD"]]
# library(harsat)
source("R/functions.R")
obj_ass <- run_assessment_tar(
  obj_ts, # biota_timeseries
  # subset = sel_series,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10))
)
  



#
# info file
#

# test 'split_info_object'  
# split "info" object in a way so we reduce the amount of data in each object (which is copied N times)
# But see bottom of _targets.R for better solution (using a common info file)  


x <- tar_read(biota_timeseries_PFOS)
str(x$info, 1)
str(split_info_object(x$info, c("CD", "PFOS")), 1)
str(split_info_object(x$info, c("CD", "PFOS"))[[1]], 1)
