
library(targets)
library(tarchetypes)
library(tibble)
source("R/functions.R")
tar_option_set(packages = c("harsat"))

# Todo / improvements, see bottom:


list(
  tar_target(file_info, "harsat_data/info.rds", format = "file"),
  tar_target(file_stations, "harsat_data/ICES_DOME_STATIONS_20230829_NO.csv", format = "file"),
  tar_target(file_contaminants, "harsat_data/raw_data_sample.csv", format = "file"),
  # tar_target(file_info, "raw_data_sample.csv", format = "file"),
  tar_target(
    biota_data,
    read_data_tar(
      compartment = "biota",
      purpose = "OSPAR",
      contaminants = file,   
      data_dir = file.path("harsat_data"),
      data_format = "external",
      info_dir = file.path("information", "OSPAR_2022"),
      filename_info = file_info,
      filename_stations = file_stations,
      filename_contaminants = file_contaminants
      )),
  tar_target(biota_data_tidy1, tidy_data_tar(biota_data)),
  tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy1)),
  tar_target(
    biota_timeseries_all, 
    create_timeseries_tar(
      biota_data_tidy2,
      determinands = ctsm_get_determinands(biota_data_tidy2$info),
      determinands.control = NULL,
      oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
      return_early = FALSE,
      print_code_warnings = FALSE,
      get_basis = get_basis_most_common,
      normalise = FALSE,
      normalise.control = list()
    )),
  tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all)),
  tar_target(info, biota_timeseries_all[["info"]]),
  tar_map(
    list(determinand = c("CD", "PFOS")),
    tar_target(biota_timeseries, biota_timeseries_list[[determinand]]),
    tar_target(
      biota_assessment,
      run_assessment_tar(
        biota_timeseries,
        info = info,
        # subset = sel_series,
        AC = NULL,
        get_AC_fn = NULL,
        recent_trend = 20,
        parallel = FALSE, 
        extra_data = NULL,
        control = list(power = list(target_power = 80, target_trend = 10)) 
      )),
    tar_target(
      biota_assess_data, get_assessment_data(biota_assessment))
    )
)

# Improvement done:
# Originally, 'biota_timeseries_list' and all 'biota_timeseries' contained each one copy of
#   'info', including all data  
# I changed this to instead use one common 'info' object that is used by 'run_assessment'  
# This was done by fetching 'info' from the 'biota_timeseries_all', object, and then rewrite 'run_assessment', 
#   so 'info' is an argument (an input), and then add a line setting 'ctsm_ob$info' to be the input 'info'  
# 
# Ie, we replaced:
#
# tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all)),
# tar_map(
#   list(determinand = c("CD", "PFOS")),
#   tar_target(biota_timeseries, biota_timeseries_list[[determinand]]),
#   tar_target(
#     biota_assessment,
#     run_assessment(
#       biota_timeseries,
#
# with
#
# tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all)),
# tar_target(info, biota_timeseries_all[["info"]]),
# tar_map(
#   list(determinand = c("CD", "PFOS")),
#   tar_target(biota_timeseries, biota_timeseries_list[[determinand]]),
#   tar_target(
#     biota_assessment,           
#     run_assessment_tar(         <===== modified version of 'biota_assessment' 
#       biota_timeseries,
#       info = info,                     <===== NEW
#
