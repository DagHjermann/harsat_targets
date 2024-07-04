
#
# Folders: 
# - input_data = full contaminant data  
# - harsat_data = sample contaminant data, station data (with "NO" in filename), info file


library(targets)
library(harsat)

source("R/functions.R")


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
