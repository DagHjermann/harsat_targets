
library(targets)
library(tarchetypes)
library(tibble)
source("R/functions.R")
tar_option_set(packages = c("harsat"))

# Todo / improvements, see bottom:

# Branching approach 1: 
# Static branching, using 'targets_group' column in 'raw_data_sample.csv' 
#   which then is preserved until branching actually is done, needing special functions 
#   'create_timeseries_tar', 'output_timeseries_tar', 'tidy_data_tar' and 'tidy_contaminants_tar'
# This won't work anymore due to changes in 'split_info_object'  

# list(
#   tar_target(file_info, "harsat_data/info.rds", format = "file"),
#   tar_target(file_stations, "harsat_data/ICES_DOME_STATIONS_20230829_NO.csv", format = "file"),
#   tar_target(file_contaminants, "harsat_data/raw_data_sample.csv", format = "file"),
#   # tar_target(file_info, "raw_data_sample.csv", format = "file"),
#   tar_target(
#     biota_data,
#     read_data_tar(
#       compartment = "biota",
#       purpose = "OSPAR",
#       contaminants = file,   
#       data_dir = file.path("harsat_data"),
#       data_format = "external",
#       info_dir = file.path("information", "OSPAR_2022"),
#       filename_info = file_info,
#       filename_stations = file_stations,
#       filename_contaminants = file_contaminants
#       )),
#   tar_target(biota_data_tidy1, tidy_data_tar(biota_data)),
#   tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy1)),
#   tar_target(
#     biota_timeseries_all, 
#     create_timeseries_tar(
#       biota_data_tidy2,
#       determinands = ctsm_get_determinands(biota_data_tidy2$info),
#       determinands.control = NULL,
#       oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
#       return_early = FALSE,
#       print_code_warnings = FALSE,
#       get_basis = get_basis_most_common,
#       normalise = FALSE,
#       normalise.control = list()
#     )),
#   tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all)),
#   tar_target(info, biota_timeseries_all[["info"]]),
#   tar_map(
#     list(determinand = c("CD", "PFOS")),
#     tar_target(biota_timeseries, biota_timeseries_list[[determinand]]),
#     tar_target(
#       biota_assessment,
#       run_assessment_tar(
#         biota_timeseries,
#         info = info,
#         # subset = sel_series,
#         AC = NULL,
#         get_AC_fn = NULL,
#         recent_trend = 20,
#         parallel = FALSE, 
#         extra_data = NULL,
#         control = list(power = list(target_power = 80, target_trend = 10)) 
#       )),
#     tar_target(
#       biota_assess_data, get_assessment_data(biota_assessment))
#     )
# )

# Branching approach 2: 
# Static branching, using a separate file 'branching_groups.csv' at branching 
#   - can then use the original harsat functions 'create_timeseries', 'output_timeseries', 
#   'tidy_data' and 'tidy_contaminants'
# Initial version: just Branching approach 1, copy-pasted

list(
  tar_target(file_info, "harsat_data/info.rds", format = "file"),
  tar_target(file_stations, "harsat_data/ICES_DOME_STATIONS_20230829_NO.csv", format = "file"),
  tar_target(file_contaminants, "harsat_data/raw_data_sample.csv", format = "file"),
  tar_target(file_branching, "harsat_data/branching_groups.csv", format = "file"),
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
  tar_target(biota_data_tidy, tidy_data_tar(biota_data)),
  tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy)),
  tar_target(
    biota_timeseries_all, 
    create_timeseries(
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
  tar_target(branching_groups, read.csv(file_branching)),
  tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all, branching_groups)),
  tar_target(info, biota_timeseries_all[["info"]]),
  tar_map(
    list(branch = c("CD_allstations", "PFOS_allstations")),
    tar_target(biota_timeseries, biota_timeseries_list[[branch]]),
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
#
# New improvement (branching approach 2):
# Branching approach 1 assumed that the branching pattern is given as an extra column in the data, 'targets_group'. 
# In order for the pipeline to preserve this column, we needed to make new versions of the following 'harsat' functions:
#   create_timeseries (-> create_timeseries_tar)
#   output_timeseries (-> output_timeseries_tar - internal function used by create_timeseries)
#   tidy_data (-> tidy_data_tar)
#   tidy_contaminants (-> tidy_contaminants_tar - internal function used by tidy_data)
# This worked OK. BUT resulted in a potentially quite heavy maintenance burden: each time one of these functions are changed, we need to 
#   make new versions of the "_tar" versions of them manually (change the code).
# Therefore, we change dthe entire pipeline (branching approach 2:
#   - the user mus make a "grouping file" with the column "branch"
#   - this is not used until "split_timeseries_object"
#   - therefore, we can use the original harsat versions of the functions mentioned above (create_timeseries etc.)
