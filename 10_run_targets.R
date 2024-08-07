
#
# Folders: 
# - input_data = full contaminant data  
# - harsat_data = sample contaminant data, station data (with "NO" in filename), info file

# updating the 'harsat' package:
# - In RStudio lower right window, choose "Files" and go to seksjon 212/HARSAT  
# - Choose "More" > "Open terminal here"   
# - In RStudio lower left window, choose "Terminal" and write "git pull origin develop"
# - restart R
# - install the new package in the R console, using devtools::install("../HARSAT")  
#   (might also need to update dependency packages)

library(targets)

# library(harsat)            # only needed for testing, not for checking/running pipeline
# source("R/functions.R")    # (same)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Create info file ----
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

if (FALSE){
  # 
  # Do only once
  # Using 'get_info_object', see R/functions  
  #
  source("R/functions_utility.R")
  
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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Check/run pipeline ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# inspect pipeline

tar_manifest()
tar_visnetwork()
tar_make()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Test results of pipeline using using 'ggplot_assessment' ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

x <- tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]]
library(ggplot2)
source("R/functions_utility.R")
ggplot_assessment(x)
ggplot_assessment(x, plot_points = "all")
ggplot_assessment(x, plot_points = "annual", logscale = FALSE)
ggplot_assessment(x, plot_points = "all", logscale = FALSE)
ggplot_assessment(x, plot_points = "all", logscale = FALSE, ylim = c(0,17))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# add 'targets_group' column and test 'read_data_tar' ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

if (FALSE){
  # do only once!
  # back up of contaminants file
  file.copy("harsat_data/raw_data_sample.csv", "harsat_data/raw_data_sample_backup.csv")
}

if (FALSE){
  # do only once!
  # add 'targets_group' - will be used to split the targets pipeline into groups
  f1 <- read.csv("harsat_data/raw_data_sample.csv")
  f2 <- f1 %>% 
    mutate(targets_group = determinand)
  write.csv(f2, "harsat_data/raw_data_sample.csv")
}

# test: run the stuff below
file_info <- tar_read(file_info)
file_stations <- tar_read(file_stations)
file_contaminants <- tar_read(file_contaminants)
# tar_target(file_info, "raw_data_sample.csv", format = "file"),
biota_data <- read_data_tar(
    compartment = "biota",
    purpose = "OSPAR",
    contaminants = file,   
    data_dir = file.path("harsat_data"),
    data_format = "external",
    info_dir = file.path("information", "OSPAR_2022"),
    filename_info = file_info,
    filename_stations = file_stations,
    filename_contaminants = file_contaminants
  )

str(biota_data$data)
# by adding the new column, the data file got 2 new columns:
# 'x' (just being row number) and 'targets_group'



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# test 'split_info_object' ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# split "info" object in a way so we reduce the amount of data in each object (which is copied N times)
# But see bottom of _targets.R for better solution (using a common info file)  


x <- tar_read(biota_timeseries_PFOS)
str(x$info, 1)
source("R/functions_utility.R")
str(split_info_object(x$info, c("CD", "PFOS")), 1)
str(split_info_object(x$info, c("CD", "PFOS"))[[1]], 1)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# test 'run_assessment_tar'  ----
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# will be used for the common info file:
obj_ts_all <- tar_read(biota_timeseries_all)
str(obj_ts_all, 1)
# data (time series object, without 'info' part, split for each parameter)
obj_ts_list <- tar_read(biota_timeseries_list)
names(obj_ts_list)
# time series object for one parameter
obj_ts <- obj_ts_list[["CD"]]
# library(harsat)
# test assessment function
source("R/functions.R")
obj_ass <- run_assessment_tar(
  obj_ts, # biota_timeseries
  info = obj_ts_all$info,
  # subset = sel_series,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10))
)




