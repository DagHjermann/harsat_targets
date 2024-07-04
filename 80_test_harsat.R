
#
# based on '05_test_milkys_data.R' in project 'test_harsat'  
#

# Packages -------------------------------------------

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "harsat")

library(harsat)
library(dplyr)
library(readr)
source("R/functions.R")

#
# Prepare Norwegian data (from Milkys) ---------------------------------------------------------------------------
#

#
# . Read concentrations (dat_orig_1) ----------
#
dat_orig_1 <- readRDS("input_data/105_data_with_uncertainty_2024-07-02.rds")
names(dat_orig_1)

# check stations  
dat_orig_1 %>% filter(MYEAR == 2023) %>% xtabs(~STATION_CODE, .)

#
# . Read stations ----------
# ICES station dictionary

#
# . Lookup data for ICES station names (STATN / station_name) -------
# for joining onto concentration data  
#

# The stuff commented out below is not needed, as we have an RDS file  
# source("../Milkys2_pc//844_ICES_submission_check_functions.R")
# data_ices <- read_ices_file(fn)
# data_ices <- add_field_codes(data_ices)

# Data submitted to ICES  
# - used to make 'lookup_statn', plus to check station names etc

# run only once:
# fn <- "../Milkys2_pc/Files_to_ICES/2022/Rdata/842_NIVA2022CF_08.rds"
# fn2 <- "input_data/842_NIVA2022CF_08.rds"
# file.copy(fn, fn2)

data_ices <- readRDS(fn)
length(data_ices)
str(data_ices, 1)
data_91 <- data_ices[["91"]]
# table(data_91$STNNO)
# table(data_91$STATN)

# readLines("data/example_OSPAR/stations.txt", n = 2)
dat_stations <- readr::read_tsv("input_data/stations.txt", guess_max = 15000)

# Are all ICES station names from table 91 (which also contains NIVA codes) also found in the station dictionary.?
statn_1 <- unique(data_91$STATN) 
statn_2 <- unique(dat_stations$station_name)
check <- statn_1 %in% statn_2

if (mean(check) < 1){
  stop("Some stations in data_91 are not found in the ICES station dictionary")
} else {
  message("All stations in data_91 are found in the ICES station dictionary")
}

lookup_statn <- bind_rows(
  data_91 %>% select(STNNO, STATN),
  data.frame(STNNO = "19N", STATN = "19N Breøyane")
)

#
# . Create dat_orig_2 -----------------
#
# Add column STATN to data by left join    
dat_orig_2 <- dat_orig_1 %>%
  left_join(
    lookup_statn,
    by = c("STATION_CODE" = "STNNO"), relationship = "many-to-one")

# 28% of the stations were not found, we show below that this is stations ended in 2021 or
# or before
mean(is.na(dat_orig_2$STATN)) 

# Summarize for each station the last year, 
check <- dat_orig_2 %>%
  group_by(STATION_CODE, STATN) %>%
  summarize(
    n_year = length(unique(MYEAR)),
    last_year = max(MYEAR), .groups = "drop") %>% 
  arrange(desc(last_year))

check2 <- check %>% 
  filter(last_year >= 2015 & is.na(STATN)) %>%
  group_by(last_year) %>%
  summarize(STATION_CODEs = paste(STATION_CODE, collapse = ", "))
check2

# Of the stations in use in 2022, only the industry station I969 and I969 are not found  
# Should probably also add some stations ended the later years: 36A (Tjøme), 33F (flatfish Sande) ++

#
# . Create dat_orig_3 -----------------
#
# We drop stations lacking STATN, plus some parameters    
dat_orig_3 <- dat_orig_2 %>% 
  filter(!is.na(STATN)) %>%
  # also drop the following determinands, as we at the moment lack have sex, n_individual  
  filter(!PARAM %in% c("EROD", "VDSI"))  

#
# . Lookup data for ICES station code (station_code) -------
# - For joining onto concentration data  
# - Taken from ICES station dictionary  
#

lookup_ices_station_code_1 <- dat_stations %>% 
  select(station_name, station_code, station_activefromdate, station_activeuntildate)

# 12 stations have several rows in the station dictionary
#   because they have moved one or several times
check <- lookup_ices_station_code_1 %>%
  filter(station_name %in% dat_orig_3$STATN) %>%
  add_count(station_name) %>%
  filter(n > 1)
check$station_name %>% unique %>% length

# Thus, we select those rows which has no value for 
# 'station_activeuntildate', i.e. that are still active
lookup_ices_station_code_2 <- lookup_ices_station_code_1 %>%
  filter(is.na(station_activeuntildate))  

#
# . Lookup data  -------
#
# .. 'sample' ----
#   could/should use SAMPLE_ID from Nivabasen, now we just make unique numbers
#   based on STATION_CODE, MYEAR, LATIN_NAME, and SAMPLE_NO2
lookup_sampleid <- dat_orig_3 %>%
  distinct(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2) %>%
  arrange(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2)
lookup_sampleid$sample <- seq_len(nrow(lookup_sampleid))

# .. 'matrix' ----
# based on code in '842_ICES_submission_2022data.Rmd' 
lookup_matrix <- tibble(
  TISSUE_NAME = c("Muskel", "Blod", "Galle", "Liver - microsome", "Lever", "Whole soft body"),
  matrix = c("MU", "BL", "BI", "LIMIC", "LI", "SB")
)

# table(dat_orig_4$TISSUE_NAME)
# table(data_ices[["10"]]$MATRX)

# .. 'unit' ----
# based on code in '842_ICES_submission_2022data.Rmd' 
lookup_unit <- data.frame(
  UNIT = c("MG_P_KG", "NG_P_G", "UG_P_KG", 
           "%", "PERCENT",
           "ng/min/mg protein", "pmol/min/mg protein", 
           "Index", "idx"),
  unit = c("mg/kg", "ng/kg", "ug/kg", 
           "%", "%",
           "ng/min/mg protein", "pmol/min/mg protein", 
           "idx", "idx"), 
  stringsAsFactors = FALSE)


#
# . Create dat_orig_4 -----------------
#

# Add 3 columns by left join: 
# - station_code (for ICES, a number)
# - 'sample' (sample ID)
# - 'matrix'     
# Also filter away (for now) some "difficult" units
dat_orig_4 <- dat_orig_3 %>% 
  left_join(
    lookup_ices_station_code_2, 
    by = c("STATN" = "station_name"), relationship = "many-to-one") %>%
  left_join(
    lookup_sampleid, 
    by = c("STATION_CODE", "MYEAR", "LATIN_NAME", "SAMPLE_NO2"), relationship = "many-to-one") %>%
  left_join(
    lookup_matrix, 
    by = c("TISSUE_NAME"), relationship = "many-to-one") %>%
  left_join(
    lookup_unit, 
    by = c("UNIT"), relationship = "many-to-one") %>%
  # remove rows where we couldn't find the corresponding maxtrix or unit  
  filter(
    !is.na(matrix) & !is.na(unit))

dat_orig_3 %>% filter(MYEAR == 2023) %>% xtabs(~STATION_CODE, .)
dat_orig_4 %>% filter(MYEAR == 2023) %>% xtabs(~STATION_CODE, .)

cat("----------------------------------------------\nOriginal units:\n----------------------------------------------\n")
table(dat_orig_3$UNIT)
cat("----------------------------------------------\nDeleted data with the following units:\n----------------------------------------------\n")
setdiff(unique(dat_orig_3$UNIT), unique(dat_orig_4$UNIT))


#
# . Create dat_formatted -----------------
#

# Need to do later:
# 1. fix sex (for EROD stations at least)
# 2. check units
# 3. add relativeøly recently used stations  

dat_formatted <- dat_orig_4 %>% 
  mutate(
    country = "Norway",
    sample_latitude = NA, 
    sample_longitude = NA,
    date = case_when(
      # !is.na(SAMPLE_DATE) ~ lubridate::ymd(SAMPLE_DATE), # lubridate::floor_date?
      !is.na(SAMPLE_DATE) ~ as.character(SAMPLE_DATE), 
      TRUE ~ paste0(MYEAR, "-10-15")),
    sex = "",
    n_individual = 1,
    subseries = NA,
    determinand = toupper(PARAM),
    basis = "W",
    censoring = case_when(
      FLAG1 %in% "<" ~ "Q",
      TRUE ~ ""),
    limit_detection = NA, 
    limit_quantification = case_when(
      FLAG1 %in% "<" ~ VALUE_WW,
      TRUE ~ NA),
    method_pretreatment = "",
    method_analysis = "",
    method_extraction = "") %>%
  rename(
    station_name = STATN,
    year = MYEAR,
    species = LATIN_NAME,
    value = VALUE_WW,
    uncertainty = UNCRT,
    unit_uncertainty = METCU) %>%
  select(
    country, station_code, station_name, sample_latitude, sample_longitude,
    year, date, species, sex, n_individual, subseries, sample, 
    determinand, matrix, basis, unit, value, censoring,
    limit_detection, limit_quantification, uncertainty, unit_uncertainty,
    method_pretreatment, method_analysis, method_extraction) 

#
# . Write data to files ----- 
#

#
# Write data to csv file  
#
readr::write_csv(
  dat_formatted, "harsat_data/raw_data.csv")

#
# Write a subset of data as test file  
#
# check station names
dat_formatted %>% filter(year >= 2023) %>% xtabs(~station_name, .)

dat_formatted_sample <- dat_formatted %>%
  filter(
    station_name %in% c("98B1 Bjørnerøya (east)", "30A Gressholmen"), 
    determinand %in% c("CD", "PFOS"))
readr::write_csv(
  dat_formatted_sample, "harsat_data/raw_data_sample.csv")

# Series of the test sample:
dat_formatted_sample %>%
  distinct(station_name, determinand, year) %>%
  count(station_name, determinand)
  
# Write stations to csv file  
readr::write_csv(
  subset(dat_stations, station_country == "Norway") %>% rename(country = station_country),
  "harsat_data/ICES_DOME_STATIONS_20230829_NO.csv")

#
# Run harsat processing-------------------------------------------------------------
#

#
# . read_data ------------------------------------------------------------------------
#

# debugonce(read_data)
biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "raw_data_sample.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("harsat_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

# debugonce(harsat:::read_stations)
biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "raw_data_sample.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("harsat_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)


#
# . tidy_data  ------------------------------------------------------
#

biota_data <- tidy_data(biota_data)

#
# . create_timeseries --------------------------------------------
#

oddities.dir <- file.path("oddities", "milkys")
if (!dir.exists(oddities.dir)) {
  dir.create(oddities.dir, recursive = TRUE)
} 

# debugonce(create_timeseries)


# biota_data$data should not include country and station_name  
# - will lead to an error in left_join on station_code
# IMPROVEMENT: deleting these variable this should happen automatically, with a warning 

if ("country" %in% names(biota_data$data))
  biota_data$data$country <- NULL
if ("station_name" %in% names(biota_data$data))
  biota_data$data$station_name <- NULL

biota_timeseries <- create_timeseries(
  biota_data,
  determinands = ctsm_get_determinands(biota_data$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

# IMPROVEMENT: Could be added at the end of create_time series
cat("Number of time series created:", nrow(biota_timeseries$timeSeries), "\n")

dir("oddities/biota")
check <- read.csv("oddities/biota/method_analysis_queries.csv")
head(check, 3)
table(check$determinand)

check <- read.csv("oddities/biota/species_group_queries.csv")
nrow(check)
head(check, 3)
table(check$species_group)

check <- read.csv("oddities/biota/value_queries.csv")
nrow(check)
head(check, 3)
table(addNA(check$value))
check %>% filter(value == 120)  # drywt% over 100  

#
# . run_assessment ------------------------------------------------------
#

# str(biota_timeseries, 1)
# nrow(biota_timeseries$timeSeries)
# View( biota_timeseries$timeSeries)

#
# We pick just a few, using the 'subset' option in 'biota_assessment'  
# IMPROVEMENT (documentation):
# The 'subset' option takes a boolean vector (TRUE/FALSE) the same length as 'biota_timeseries$timeSeries'  
#

# not using sel_series - we just run for the whole 'biota_assessment' object  

biota_assessment <- run_assessment(
  biota_timeseries,
  # subset = sel_series,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10)) 
)

# the original time series:
biota_assessment$timeSeries

# check the assessment object of the first time series
i <- 1
str(biota_assessment$assessment[[i]], 1)


#
# . check_assessment (checks convergence) ---------------------------------------------
#

check_assessment(biota_assessment, save_result = FALSE)
# All assessment models have converged

#
# . write_summary_table (creates one csv file) ----------------------------------------
#
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

# fn <- "external_amap_output.csv"
fn <- "summary_table.csv"
write_summary_table(
  biota_assessment,
  output_file = fn,   # NB, file will be overwritten so change name as appropriate to retain results
  output_dir = summary.dir,
  export = TRUE,
  determinandGroups = NULL,
  symbology = NULL,
  collapse_AC = NULL, 
  extra_output = "power"
)



#
# . plot_assessment (creates jpg files) ----------------------------------------------------------
#

plot.dir <- file.path("plots")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 

plot_assessment(
  biota_assessment,
  # subset = sel_series2,     # not needed
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)
length(dir(plot.dir))  # 77 - one too much!!


#
# . get_assessment_data (for ggplot) ----------------------------------------------------------------
#
# Using function 'get_assessment_data' in 'functions.R'  
#

plotdat <- get_assessment_data(
  biota_assessment)

str(plotdat, 1)

i <- 1
str(plotdat[[i]], 1)
str(plotdat[[i]]$assessment, 1)
str(plotdat[[i]]$assessment$contrasts, 1)
str(plotdat[[i]]$info, 1)

#
# . plot using ggplot -------------------------------------------------------------------------------
#

library(ggplot2)

i <- 3
plotdat[[i]]$assessment$fullData
ggplot(plotdat[[i]]$assessment$fullData, aes(year)) +
  geom_ribbon(
    data = plotdat[[i]]$assessment$pred, 
    aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
    fill = "lightblue") + 
  geom_path(
    data = plotdat[[i]]$assessment$pred, 
    aes(y = exp(fit))) + 
  geom_point(
    aes(y = concentration, color = censoring),
    color = "darkred") +
  scale_y_log10() +
  labs(title = plotdat[[i]]$output_id)

str(plotdat[[i]]$info, 1)


#
# . write HTML reports ----------------------------------------------------------------------------
#


report.dir <- file.path("reports")
if (!dir.exists(report.dir)) {
  dir.create(report.dir, recursive = TRUE)
} 

report_assessment(
  biota_assessment,
  # subset = sel_series2,  # not needed
  output_dir = report.dir
)




