
library(targets)
library(tarchetypes)
library(tibble)
source("R/functions.R")
tar_option_set(packages = c("harsat"))

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
  tar_target(biota_data_tidy1, tidy_data(biota_data)),
  tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy1)),
  tar_target(
    biota_timeseries, 
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
  tar_target(
    biota_timeseries_list, split_timeseries_object(biota_timeseries)),
  tar_map(
    list(determinand = "CD", "PFOS"),
    tar_target(biota_timeseries, biota_timeseries_list[[determinand]]), 
    tar_target(
      biota_assessment,
      run_assessment(
        biota_timeseries,
        # subset = sel_series,
        AC = NULL,
        get_AC_fn = NULL,
        recent_trend = 20,
        parallel = FALSE, 
        extra_data = NULL,
        control = list(power = list(target_power = 80, target_trend = 10))
      )
    )
  )
)

