

get_assessment_data <- function(
    assessment_obj, 
    subset = NULL) {
  
  # silence non-standard evaluation warnings
  seriesID <- NULL
  
  # graphics_functions.R
  
  info <- assessment_obj$info
  timeSeries <- assessment_obj$timeSeries 
  
  
  # set up time series information:
  # - merge with station information
  # - add in additional useful variables 
  # - subset if necessary
  
  timeSeries <- tibble::rownames_to_column(timeSeries, "series")
  
  timeSeries <- dplyr::left_join(
    timeSeries, 
    assessment_obj$stations, 
    by = "station_code"
  )
  
  timeSeries$group <- ctsm_get_info(
    info$determinand, 
    timeSeries$determinand, 
    "group", 
    info$compartment,
    sep = "_"
  )
  
  timeSeries$distribution <- ctsm_get_info(
    info$determinand, 
    timeSeries$determinand, 
    "distribution"
  )
  
  if (info$compartment == "water") {
    timeSeries$matrix <- "WT"
  }
  
  timeSeries <- harsat:::apply_subset(timeSeries, subset, parent.frame())
  
  series_id <- row.names(timeSeries)
  
  
  # plot each timeSeries
  
  result <- lapply(series_id, function(id) {
    
    data <- dplyr::filter(assessment_obj$data, seriesID == id)
    
    assessment <- assessment_obj$assessment[[id]]
    
    
    # get relevant series info
    
    series <- timeSeries[id, ]
    
    
    # get file name from id, and add country and station name 
    # for easier identification
    
    output_id <- sub(
      series$station_code,
      paste(series$station_code, series$country, series$station_name), 
      id,
      fixed=TRUE
    )
    
    
    # get rid of any slashes that might have crept in 
    
    output_id <- gsub(" / ", " ", output_id, fixed = TRUE)
    output_id <- gsub("/", " ", output_id, fixed = TRUE)
    
    output_id <- gsub(" \ ", " ", output_id, fixed = TRUE)
    output_id <- gsub("\\", " ", output_id, fixed = TRUE)
    
    
    # plot assessment with index
    
    list(data=data, assessment=assessment, series=series, info=info, output_id=output_id)
    
  })
  
  names(result) <- series_id
  result 
  
}  

#
# read_data() adapted for used with targets 
#

# Introduced 'filename_info' to read pre-made 'info' object (made using get_info_object)
# Removed 'stations', replaced by filename_stations
# Also:
# - added 'harsat:::' in front of 'control_default', 'control_modify', and 'read_info'

read_data_tar <- function (compartment = c("biota", "sediment", "water"), 
                        purpose = c("OSPAR", "HELCOM", "AMAP", "custom"), 
                        contaminants,   # file name
                        data_dir = ".", 
                        data_format = c("ICES", "external"), 
                        info_files = list(), 
                        info_dir = ".", 
                        filename_info,
                        filename_stations,
                        filename_contaminants,
                        extraction = NULL, 
                        max_year = NULL, 
                        oddity_dir = "oddities", 
                        control = list()) 
{
  compartment <- match.arg(compartment)
  purpose <- match.arg(purpose)
  data_format <- match.arg(data_format)
  if (!is.null(max_year)) {
    if (length(max_year) != 1) {
      stop("max_year must be a single integer valued year")
    }
    if (!is.integer(max_year)) {
      if (!isTRUE(all.equal(max_year, as.integer(max_year)))) {
        stop("max_year must be an integer valued year")
      }
      max_year <- as.integer(max_year)
    }
  }
  if (!is.null(extraction)) {
    if (length(extraction) > 1 | !is.character(extraction)) {
      stop("'extraction' must be a single character string of the form ", 
           "\"yyyy-mm-dd\": e.g. \"", lubridate::today(), 
           "\"", call. = FALSE)
    }
  }
  if (!is.null(extraction)) {
    extraction <- suppressWarnings(lubridate::ymd(extraction))
    if (is.na(extraction)) {
      stop("extraction not recognised: it must be a valid date of the form ", 
           "\"yyyy-mm-dd\": e.g. \"", lubridate::today(), 
           "\"", call. = FALSE)
    }
  }
  info <- list(compartment = compartment, purpose = purpose, 
               extraction = extraction, data_format = data_format, 
               max_year = max_year, oddity_dir = oddity_dir)
  cntrl <- harsat:::control_default(purpose, compartment)
  cntrl <- harsat:::control_modify(cntrl, control)
  if (any(names(cntrl) %in% names(info))) {
    id <- names(cntrl)
    id <- id[id %in% names(info)]
    warning("\n conflict between function arguments and control elements ", 
            "- results may be unexpected:\n ", paste(id, collapse = ", "), 
            "\n", call. = FALSE, immediate. = TRUE)
  }
  info <- append(info, cntrl)
  # info <- read_info(info, info_dir, info_files)
  info <- readRDS(filename_info)
  stations <- read_stations_tar(filename_stations, info)
  data <- read_contaminants_tar(filename_contaminants, info)
  if (data_format == "ICES") {
    data <- harsat:::add_stations(data, stations, info)
    stations <- harsat:::finalise_stations(stations, info)
    data <- harsat:::finalise_data(data, info)
  }
  if (is.null(info$max_year)) {
    info$max_year <- max(data$year)
    cat("\nArgument max_year taken to be the maximum year in the data:", 
        info$max_year, "\n")
  }
  info$recent_years <- seq(info$max_year - info$reporting_window + 
                             1, info$max_year)
  out <- list(call = match.call(), info = info, data = data, 
              stations = stations)
  # list(info=info, stations=stations, data = data)
  out
}

# read_data_tar <- function (filename_info) {
#   info <- readRDS(filename_info)
#   info
# }


# the first line "infile <- file.path(data_dir, file)" commented out
# - added "harsat:::" in front of report_file_digest, safe_read_file

read_stations_tar <- function (infile, info) {
  #  infile <- file.path(data_dir, file)
  cat("Reading station dictionary from:\n '", infile, "'\n", 
      sep = "")
  if (info$data_format == "ICES") {
    var_id <- c(station_code = "character", station_country = "character", 
                helcom_subbasin = "character", helcom_l3 = "character", 
                helcom_l4 = "character", ices_ecoregion = "character", 
                ospar_region = "character", ospar_subregion = "character", 
                is_amap_area = "logical", is_helcom_area = "logical", 
                is_ospar_area = "logical", station_name = "character", 
                station_longname = "character", station_replacedby = "character", 
                station_asmtmimeparent = "character", station_activefromdate = "character", 
                station_activeuntildate = "character", station_programgovernance = "character", 
                station_governance = "character", station_purpm = "character", 
                station_latitude = "numeric", station_latituderange = "numeric", 
                station_longitude = "numeric", station_longituderange = "numeric", 
                station_geometry = "character", station_datatype = "character", 
                station_wltyp = "character", station_mstat = "character", 
                station_notes = "character", station_deprecated = "logical")
    harsat:::report_file_digest(infile)
    stations <- harsat:::safe_read_file(infile, sep = "\t", quote = "", 
                               na.strings = c("", "NULL"), strip.white = TRUE, colClasses = var_id)
  }
  if (info$data_format == "external") {
    var_id <- c(country = "character", station_name = "character", 
                station_code = "character", station_longname = "character", 
                station_latitude = "numeric", station_longitude = "numeric", 
                station_type = "character", waterbody_type = "character")
    if (!is.null(info$region$id)) {
      extra <- rep("character", length(info$region$id))
      names(extra) <- info$region$id
      var_id <- c(var_id, extra)
    }
    required <- c("country", "station_name", "station_code", 
                  "station_latitude", "station_longitude", info$region$id)
    harsat:::report_file_digest(infile)
    stations <- harsat:::safe_read_file(infile, strip.white = TRUE, nrows = 1)
    ok <- required %in% names(stations)
    if (!all(ok)) {
      id <- required[!ok]
      id <- sort(id)
      stop("The following variables are not in the stations file. ", 
           "Please update the stations file to continue. ", 
           "Note that the variable names are case sensitive.\n", 
           "Variables: ", paste(id, collapse = ", "))
    }
    ok <- names(var_id) %in% names(stations)
    stations <- harsat:::safe_read_file(infile, 
                                        na.strings = c("", "NULL"), 
                                        strip.white = TRUE, 
                                        colClasses = var_id[ok])
    id <- c("station_longname", "station_type", "waterbody_type")
    for (i in id) {
      if (is.null(stations[[i]])) 
        stations[[i]] <- NA_character_
    }
    if (!all(names(var_id) %in% names(stations))) {
      stop("coding error - seek help from HARSAT team")
    }
  }
  stations
}


read_contaminants_tar <- function (infile, info){
  .data <- NULL
  # infile <- file.path(data_dir, file)
  cat("\nReading contaminant and effects data from:\n '", infile, 
      "'\n", sep = "")
  if (info$data_format == "ICES") {
    var_id <- c(country = "character", mprog = "character", 
                helcom_subbasin = "character", helcom_l3 = "character", 
                helcom_l4 = "character", ices_ecoregion = "character", 
                ospar_region = "character", ospar_subregion = "character", 
                is_amap_monitoring = "logical", is_helcom_monitoring = "logical", 
                is_medpol_monitoring = "logical", is_ospar_monitoring = "logical", 
                is_amap_area = "logical", is_helcom_area = "logical", 
                is_ospar_area = "logical", rlabo = "character", slabo = "character", 
                alabo = "character", statn = "character", myear = "integer", 
                date = "Date", latitude = "numeric", longitude = "numeric", 
                dephu = "numeric", dephl = "numeric", purpm = "character", 
                finfl = "character", param = "character", pargroup = "character", 
                matrx = "character", basis = "character", value = "numeric", 
                munit = "character", detli = "numeric", lmqnt = "numeric", 
                uncrt = "numeric", metcu = "character", qflag = "character", 
                vflag = "character", metoa = "character", metcx = "character", 
                metpt = "character", metst = "character", metps = "character", 
                metfp = "character", smtyp = "character", smpno = "character", 
                subno = "character", dcflgs = "character", tblanalysisid = "integer", 
                tblparamid = "integer", tblsampleid = "character", 
                tblspotid = "integer", tbluploadid = "integer")
    if (info$compartment == "biota") {
      extra <- c(rlist = "character", speci = "character", 
                 speci_name = "character", aphiaid = "integer", 
                 worms_name = "character", aphiaid_accepted = "integer", 
                 worms_accepted_name = "character", sexco = "character", 
                 stage = "character", noinp = "integer", bulkid = "character", 
                 tblbioid = "character", accessionid = "integer")
      var_id <- c(var_id, extra)
    }
    harsat:::report_file_digest(infile)
    data <- harsat:::safe_read_file(infile, sep = "\t", na.strings = c("", 
                                                              "NULL"), strip.white = TRUE, colClasses = var_id)
    if (info$compartment == "biota" && info$use_stage) {
      data$subseries <- data$stage
    }
    else {
      data$subseries <- NA_character_
    }
    data <- dplyr::mutate(data, param = toupper(.data$param), 
                          munit = tolower(.data$munit))
    return(data)
  }
  if (info$data_format == "external") {
    var_id <- c(country = "character", station_code = "character", 
                station_name = "character", sample_latitude = "numeric", 
                sample_longitude = "numeric", year = "integer", date = "Date", 
                depth = "numeric", subseries = "character", sample = "character", 
                determinand = "character", matrix = "character", 
                basis = "character", unit = "character", value = "numeric", 
                censoring = "character", limit_detection = "numeric", 
                limit_quantification = "numeric", uncertainty = "numeric", 
                unit_uncertainty = "character", method_pretreatment = "character", 
                method_analysis = "character", method_extraction = "character")
    if (info$compartment == "biota") {
      var_id = c(var_id, species = "character", sex = "character", 
                 n_individual = "integer")
    }
    required <- c("country", "station_code", "station_name", 
                  "year", "sample", "determinand", "matrix", "unit", 
                  "value")
    if (info$compartment %in% c("biota", "sediment")) {
      required <- c(required, "basis")
    }
    if (info$compartment %in% c("biota")) {
      required <- c(required, "species")
    }
  }
  if (info$data_format == "external") {
    harsat:::report_file_digest(infile)
    data <- harsat:::safe_read_file(infile, strip.white = TRUE, nrows = 1)
    ok <- required %in% names(data)
    if (!all(ok)) {
      id <- required[!ok]
      id <- sort(id)
      stop("The following variables are not in the data file. ", 
           "Please update the data file to continue. ", 
           "Note that the variable names are case sensitive.\n", 
           "Variables: ", paste(id, collapse = ", "))
    }
    ok <- names(var_id) %in% names(data)
    data <- harsat:::safe_read_file(infile, na.strings = c("", "NULL"), 
                           strip.white = TRUE, colClasses = var_id[ok])
  }
  if (info$data_format == "external") {
    id <- c("sample_latitude", "sample_longitude", "depth", 
            "limit_detection", "limit_quantification", "uncertainty")
    for (i in id) {
      if (is.null(data[[i]])) 
        data[[i]] <- NA_real_
    }
    id <- c("subseries", "basis", "basis", "censoring", "unit_uncertainty", 
            "method_pretreatment", "method_analysis", "method_extraction")
    for (i in id) {
      if (is.null(data[[i]])) 
        data[[i]] <- NA_character_
    }
    if (is.null(data$date)) 
      data$date <- as.Date(NA)
    if (info$compartment == "biota") {
      if (is.null(data$sex)) 
        data$sex <- NA_character_
      if (is.null(data$n_individual)) 
        data$n_individual <- NA_integer_
    }
    if (!all(names(var_id) %in% names(data))) {
      stop("coding error - seek help from HARSAT team")
    }
  }
  if (info$data_format == "external") {
    uncertainty_present <- which(complete.cases(data$uncertainty))
    uncertainty_present_valid_units <- data$unit_uncertainty[uncertainty_present] %in% 
      c("%", "U2", "SD")
    if (!all(uncertainty_present_valid_units)) {
      stop("Missing or invalid uncertainty units for specified uncertainty values. ", 
           "Please check that all uncertainty values have a valid unit: %, U2, or SD")
    }
  }
  if (info$data_format == "external") {
    names(data) <- tolower(names(data))
  }
  data <- dplyr::mutate(data, determinand = toupper(.data$determinand), 
                        unit = tolower(.data$unit))
  data
}

# tidy_data_tar - 'targets' version of 'tidy_data' in the harsat package
# copy of the original function, except that 'tidy_contaminants' is replaced by 'tidy_contaminants_tar'  

tidy_data_tar <- function (ctsm_obj) {
  info <- ctsm_obj$info
  data <- ctsm_obj$data
  stations <- ctsm_obj$stations
  harsat:::initialise_oddities(info$oddity_dir, info$compartment)
  if (info$data_format %in% "ICES") {
    ok <- data$retain
    if (!all(ok)) {
      cat("\nDropping", sum(!ok), "records from data flagged for deletion. Possible reasons are:\n", 
          "- vflag = suspect\n", switch(info$compartment, 
                                        biota = "- species missing\n", sediment = paste("- upper depth > 0m\n", 
                                                                                        "- unusual matrix\n"), water = paste("- upper depth >5.5m\n", 
                                                                                                                             "- filtration missing\n", "- matrix = 'SPM'\n")))
      data <- data[ok, ]
    }
  }
  if (info$data_format %in% c("ICES", "external")) {
    ok <- !is.na(data$station_code)
    if (!all(ok)) {
      cat("\nDropping", sum(!ok), "records from data that have no valid station code\n")
      data <- data[ok, ]
    }
    ok <- stations$station_code %in% data$station_code
    if (!all(ok)) {
      cat("\nDropping", sum(!ok), "stations that are not associated with any data\n")
      stations <- stations[ok, ]
    }
  }
  stations <- harsat:::tidy_stations(stations, info)
  data <- tidy_contaminants_tar(data, info)
  ctsm_obj$stations <- stations
  ctsm_obj$data <- data
  ctsm_obj
}

# tidy_contaminants_tar - 'targets' version of 'tidy_data' in the harsat package
# copy of the original function, except that "targets_group" is added in output

tidy_contaminants_tar <- function (data, info) {
  cat("\nCleaning contaminant and biological effects data\n")
  if (any(data$year > info$max_year)) {
    message_txt <- paste0("years greater than ", info$max_year, 
                          " will be excluded; to change this use", "\n   the max_year argument of ctsm_read_data")
    not_ok <- data$year > info$max_year
    data <- ctsm_check(data, not_ok, action = "delete", 
                       message = message_txt, file_name = "data_too_recent", 
                       info = info)
  }
  if (!any(data$year %in% info$recent_years)) {
    stop("no data in the period ", min(info$recent_years), 
         " to ", max(info$recent_years), " inclusive; nothing to assess!\n", 
         "Consider changing the max_year argument or the reporting_window control\n", 
         "option in read_data.", call. = FALSE)
  }
  if (info$data_format == "external") {
    data$replicate <- seq(from = 1, to = nrow(data), by = 1)
  }
  data$sample <- as.character(data$sample)
  var_id <- c("station_code", "sample_latitude", "sample_longitude", 
              "year", "date", "time", "depth", "species", "sex", "n_individual", 
              "subseries", "sample", "replicate", "determinand", "pargroup", 
              "matrix", "basis", "filtration", "method_analysis", 
              "method_extraction", "method_pretreatment", "unit", 
              "value", "censoring", "limit_detection", "limit_quantification", 
              "uncertainty", "unit_uncertainty", "alabo", "qalink", "targets_group")
  data <- data[intersect(var_id, names(data))]
  data
}



tidy_data2 <- function(data){
  if ("country" %in% names(data$data))
    data$data$country <- NULL
  if ("station_name" %in% names(data$data))
    data$data$station_name <- NULL
  data
}


# create_timeseries_tar
# copy of create_timeseries from harsat package

create_timeseries_tar <- function (ctsm.obj, determinands = ctsm_get_determinands(ctsm.obj$info), 
                                   determinands.control = NULL, oddity_path = "oddities", return_early = FALSE, 
                                   print_code_warnings = FALSE, get_basis = get_basis_default, 
                                   normalise = FALSE, normalise.control = list()) 
{
  .data <- .month <- .not_ok <- group <- value <- NULL
  determinand <- uncertainty <- uncertainty_sd <- uncertainty_rel <- species_group <- NULL
  determinands <- force(determinands)
  if (!is.character(determinands)) {
    stop("determinands must be a character string")
  }
  if (length(determinands) == 0L) {
    stop("No determinands have been been selected for assessment:\n", 
         "  please update the determinand reference table or supply the\n", 
         "  determinands directly via the determinands argument.")
  }
  if (length(normalise) != 1L) {
    stop("normalise should be a length 1 logical or a function")
  }
  if (!(is.logical(normalise) | is.function(normalise))) {
    stop("normalise should be a length 1 logical or a function")
  }
  out <- list(call = match.call(), call.data = ctsm.obj$call, 
              info = ctsm.obj$info)
  info <- ctsm.obj$info
  station_dictionary <- ctsm.obj$stations
  data <- ctsm.obj$data
  rm(ctsm.obj)
  is.recent <- function(year) year %in% info$recent_years
  oddity_path <- initialise_oddities(info$oddity_dir, info$compartment)
  ctsm_check_stations(station_dictionary)
  wk <- ctsm_check_determinands(info, data, determinands, determinands.control)
  data <- wk$data
  determinands <- wk$determinands
  determinands.control <- wk$control
  cat("\nCleaning data\n")
  id <- c("station_code", "date", "filtration", "species", 
          "sample", "matrix", "determinand")
  id <- intersect(id, names(data))
  ord <- do.call("order", data[id])
  data <- data[ord, ]
  var_id <- c("country", "station_code", "station_name")
  var_id <- intersect(var_id, names(station_dictionary))
  data <- dplyr::left_join(data, station_dictionary[var_id], 
                           by = "station_code")
  data <- dplyr::relocate(data, dplyr::all_of(var_id))
  ok <- data$station_code %in% station_dictionary$station_code
  data <- ctsm_check(data, !ok, action = "delete", message = "Stations in data not in station dictionary", 
                     file_name = "unidentified_stations", info = info)
  if (info$compartment == "biota") {
    data$species <- ifelse(data$species %in% row.names(info$species), 
                           info$species[data$species, "reference_species"], 
                           data$species)
  }
  cat("   Dropping stations with no data between", min(info$recent_years), 
      "and", max(info$recent_years), "\n")
  id.names <- intersect(c("station_code", "species"), names(data))
  id <- do.call("paste", data[id.names])
  ok <- id %in% id[is.recent(data$year)]
  data <- droplevels(data[ok, ])
  if (info$compartment == "biota") {
    ok <- ctsm_get_info(info$species, data$species, "assess")
    if (!all(ok)) {
      id <- data$species[!ok]
      id <- unique(id)
      id <- sort(id)
      cat("   Dropping following species - see species reference table:\n", 
          paste(id, collapse = ", "), "\n")
    }
    data <- data[ok, ]
  }
  if (info$region$all) {
    data <- dplyr::left_join(data, station_dictionary[c("station_code", 
                                                        info$region$id)], by = "station_code")
    if (any(is.na(data[info$region.id]))) {
      cat("   Dropping stations with missing region information in station dictionary\n")
      data <- tidyr::drop_na(data, dplyr::all_of(info$region$id))
    }
    data <- data[setdiff(names(data), info$region$id)]
  }
  data$group <- ctsm_get_info(info$determinand, data$determinand, 
                              "group", info$compartment, sep = "_")
  if (info$compartment == "biota") {
    data$species_group <- ctsm_get_info(info$species, data$species, 
                                        "species_group")
  }
  if (!"pargroup" %in% names(data)) {
    data$pargroup <- ctsm_get_info(info$determinand, data$determinand, 
                                   "pargroup")
  }
  data$distribution <- ctsm_get_info(info$determinand, data$determinand, 
                                     "distribution", na_action = "output_ok")
  ok <- with(data, sample %in% sample[group != "Auxiliary"])
  if (any(!ok)) {
    cat("   Dropping samples with only auxiliary variables\n")
    data <- data[ok, ]
  }
  wk <- c("species_group", "sex", "no_individual", "matrix", 
          "basis", "unit", "method_analysis", "value")
  for (var_id in wk) {
    data <- ctsm_check_variable(data, var_id, info)
  }
  if (info$compartment == "sediment") {
    data$digestion <- ctsm_get_digestion(data, info)
  }
  data <- ctsm_check(data, paste(sample, determinand, matrix), 
                     action = "delete.dups", message = "Replicate measurements, only first retained", 
                     file_name = "replicate_measurements", info = info)
  data <- check_censoring(data, info, print_code_warnings)
  data <- check_uncertainty(data, info, type = "reported")
  data <- dplyr::mutate(data, uncertainty = dplyr::case_when(.data$unit_uncertainty %in% 
                                                               "U2" ~ .data$uncertainty/2, .data$unit_uncertainty %in% 
                                                               "%" ~ .data$value * .data$uncertainty/100, .default = .data$uncertainty), 
                        unit_uncertainty = "SD")
  for (i in names(determinands.control)) {
    wk <- determinands.control[[i]]
    linkFunction <- switch(wk$action, replace = determinand.link.replace, 
                           sum = determinand.link.sum, bespoke = get(paste("determinand.link", 
                                                                           i, sep = "."), mode = "function"))
    args = list(data = data, info = info, keep = i, drop = wk$det)
    if ("weights" %in% names(wk)) {
      args = c(args, list(weights = wk$weights))
    }
    data <- do.call(linkFunction, args)
  }
  id <- c(determinands, ctsm_get_auxiliary(determinands, info))
  data <- dplyr::filter(data, .data$determinand %in% id)
  data <- check_subseries(data, info)
  rownames(data) <- NULL
  cat("\nCreating time series data\n")
  data$new.unit <- ctsm_get_info(info$determinand, data$determinand, 
                                 "unit", info$compartment, sep = "_")
  data$concentration <- data$value
  id <- c("concentration", "uncertainty", "limit_detection", 
          "limit_quantification")
  data[id] <- lapply(data[id], convert_units, from = data$unit, 
                     to = data$new.unit)
  data <- ctsm_merge_auxiliary(data, info)
  if (info$compartment == "biota") {
    data <- ctsm.imposex.check.femalepop(data, info)
  }
  data <- ctsm_convert_to_target_basis(data, info, get_basis)
  if (return_early) {
    out <- c(out, output_timeseries(data, station_dictionary, 
                                    info, extra = "alabo"))
    return(out)
  }
  data$uncertainty <- ctsm_estimate_uncertainty(data, "concentration", 
                                                info)
  if (info$compartment == "sediment") {
    for (norm_id in c("AL", "LI", "CORG", "LOIGN")) {
      if (norm_id %in% names(data)) {
        norm_uncrt <- paste0(norm_id, ".uncertainty")
        data[[norm_uncrt]] <- ctsm_estimate_uncertainty(data, 
                                                        norm_id, info)
      }
    }
  }
  data <- ctsm_check(data, distribution %in% c("normal", "lognormal") & 
                       !is.na(concentration) & is.na(uncertainty), action = "delete", 
                     message = "Missing uncertainties which cannot be imputed", 
                     file_name = "missing_uncertainties", info = info)
  if (info$compartment == "biota" && !is.null(info$bivalve_spawning_season)) {
    data <- dplyr::mutate(data, .month = months(as.Date(.data$date)), 
                          .not_ok = species_group %in% c("Bivalve", "Gastropod") & 
                            (is.na(.month) | .month %in% info$bivalve_spawning_season) & 
                            !group %in% c("Effects", "Imposex", "Metabolites"))
    if (any(data$.not_ok)) {
      cat("   Dropping bivalve and gastropod contaminant data collected during the\n", 
          "   spawning season, which is taken to be the following months:\n   ", 
          paste(info$bivalve_spawning_season, collapse = ", "), 
          "\n")
      data <- dplyr::filter(data, !.not_ok)
    }
    data[c(".month", ".not_ok")] <- NULL
  }
  if (is.logical(normalise) && normalise) {
    data <- switch(info$compartment, biota = stop("there is no default normalisation function for biota"), 
                   sediment = stop("there is no default normalisation function for sediment"), 
                   water = stop("there is no default normalisation function for water"))
  }
  else if (is.function(normalise)) {
    data <- normalise(data, station_dictionary, info, normalise.control)
  }
  data <- check_uncertainty(data, info, type = "calculated")
  notok <- data$distribution %in% c("normal", "lognormal") & 
    !is.na(data$concentration) & is.na(data$uncertainty)
  if (any(notok)) {
    stop("uncertainties missing where they should be present: \n", 
         "contact HARSAT development team")
  }
  cat("   Dropping groups of compounds / stations with no data between", 
      min(info$recent_years), "and", max(info$recent_years), 
      "\n")
  id_names <- intersect(c("station_code", "filtration", "species", 
                          "group"), names(data))
  id <- do.call("paste", data[id_names])
  ok <- id %in% id[is.recent(data$year) & !is.na(data$concentration)]
  data <- data[ok, ]
  out <- c(out, output_timeseries_tar(data, station_dictionary, 
                                  info))
  out
}

# output_timeseries_tar
# copy of output_timeseries from harsat package

output_timeseries_tar <- function (data, station_dictionary, info, extra = NULL) {
  .data <- .group <- seriesID <- NULL
  id = c("station_code", "species", "filtration", "year", 
         "sex", "sample", "group", "determinand")
  data <- dplyr::arrange(data, dplyr::across(dplyr::any_of(id)))
  id <- c("station_code", "sample_latitude", "sample_longitude", 
          "species", "sex", "depth", "year", "date", "time", "sample", 
          "matrix", "filtration", "subseries", "group", "determinand", 
          "basis", "unit", "value", "method_analysis", "n_individual", 
          "concOriginal", "censoringOriginal", "uncrtOriginal", 
          "concentration", "new.basis", "new.unit", "censoring", 
          "limit_detection", "limit_quantification", "uncertainty", "targets_group")
  if (!is.null(extra)) {
    id <- c(id, extra)
  }
  auxiliary <- ctsm_get_auxiliary(data$determinand, info)
  auxiliary_id <- paste0(rep(auxiliary, each = 5), c("", ".censoring", 
                                                     ".limit_detection", ".limit_quantification", ".uncertainty"))
  id <- c(id, auxiliary_id)
  data <- dplyr::select(data, dplyr::any_of(id))
  row.names(data) <- NULL
  data <- droplevels(data)
  id <- c("station_code", "determinand")
  if (info$compartment %in% c("biota")) {
    id <- c(id, "species", "matrix", "subseries", "sex", 
            "method_analysis")
  }
  if (info$compartment %in% "sediment") {
    id <- c(id, "matrix", "subseries")
  }
  if (info$compartment %in% "water") {
    id <- c(id, "filtration", "subseries")
  }
  timeSeries <- data[id]
  if (info$compartment == "biota") {
    timeSeries <- dplyr::mutate(timeSeries, sex = dplyr::if_else(.data$determinand %in% 
                                                                   "EROD", .data$sex, NA_character_), .group = ctsm_get_info(info$determinand, 
                                                                                                                             .data$determinand, "group", "biota", sep = "_"), 
                                method_analysis = dplyr::if_else(.group %in% "Metabolites", 
                                                                 .data$method_analysis, NA_character_), .group = NULL, 
    )
  }
  timeSeries <- tidyr::unite(timeSeries, "seriesID", dplyr::all_of(names(timeSeries)), 
                             sep = " ", remove = FALSE, na.rm = TRUE)
  data <- cbind(seriesID = timeSeries$seriesID, data)
  timeSeries$basis <- data$new.basis
  timeSeries$unit <- data$new.unit
  data$new.basis <- data$new.unit <- NULL
  timeSeries <- droplevels(dplyr::distinct(timeSeries))
  timeSeries <- tibble::column_to_rownames(timeSeries, "seriesID")
  data <- ctsm_check(data, paste(seriesID, sample), action = "delete.dups", 
                     message = "Measurements still replicated, only first retained", 
                     file_name = "replicate_measurements_extra", info = info)
  is_recent_data <- (data$year %in% info$recent_years) & !is.na(data$concentration)
  id <- data$seriesID[is_recent_data]
  id <- unique(id)
  data <- data[data$seriesID %in% id, ]
  data <- droplevels(data)
  ok <- row.names(timeSeries) %in% id
  timeSeries <- timeSeries[ok, ]
  timeSeries <- droplevels(timeSeries)
  ok <- station_dictionary$station_code %in% timeSeries$station_code
  station_dictionary <- station_dictionary[ok, ]
  row.names(station_dictionary) <- NULL
  list(data = data, stations = station_dictionary, timeSeries = timeSeries)
}


# Function for splitting a timeseries object into a list 
# with one object per determinand
# - 'info' is removed from the timeseries object, instead the assessment function will read from a common 'info' file 

split_timeseries_object <- function(object){
  # split time series into a list
  timeSeries_list <- split(object$timeSeries, object$timeSeries$determinand)
  determs <- names(timeSeries_list)
  # split data into a list
  data_list <- lapply(determs, function(determ) { subset(object$data, determinand %in% determ) })
  # define result as a list with one element per determinand
  result <- vector(mode = "list", length = length(determs))
  names(result) <- determs
  # define each list item
  for (i in seq_along(result)){
    result[[i]] <- list(
      call = object$call,
      call.data = object$call.data,
      # info = object$info,
      data = data_list[[i]],
      # data = object$data,
      stations = object$stations,
      timeSeries = timeSeries_list[[i]]
    )
  }
  result
}

if (FALSE){
  # for test
  x <- tar_read(biota_timeseries)
  biota_timeseries_list <- split_timeseries_object(x)
  str(biota_timeseries_list, 1)
  # Compare the two next - they should be of identical structure:
  str(biota_timeseries_list[[1]], 1)
  str(x, 1)
  # Test that 'run_assessment' can use each list object
  i <- 1
  biota_assessment_test <- harsat::run_assessment(
    biota_timeseries_list[[i]],
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 20,
    parallel = FALSE, 
    extra_data = NULL,
    control = list(power = list(target_power = 80, target_trend = 10)) 
  )
}





#
# run_assessment_tar
# - a copy of run_assessment() from harsat  
# - except
#     - uses a common 'info' file ('info' is given as an argument)  
#     - triple colons added in order to access no-exported functions run_control_default, run_control_modify, assessment_engine

run_assessment_tar <- function (ctsm_ob, info, subset = NULL, AC = NULL, get_AC_fn = NULL, 
          recent_trend = 20L, parallel = FALSE, extra_data = NULL, 
          control = list(), ...) 
{
  ctsm_ob$call <- match.call()
  ctsm_ob$info <- info
  ctsm_ob$info$recent.trend <- recent_trend
  ctsm_ob$info$AC <- AC
  ctsm_ob$info$get_AC_fn <- get_AC_fn
  if (!is.null(AC) && is.null(ctsm_ob$info$get_AC_fn)) {
    ctsm_ob$info$get_AC_fn <- get_AC[[ctsm_ob$info$compartment]]
  }
  if (any(ctsm_ob$data$group %in% "Imposex")) {
    if (is.null(extra_data)) {
      stop("`extra_data` must be supplied for imposex assessments")
    }
    ok <- c("VDS_estimates", "VDS_confidence_limits") %in% 
      names(extra_data)
    if (!all(ok)) {
      stop("argument extra_data must be a list with components ", 
           "VDS_estimates and VDS_confidence_limits")
    }
  }
  ctsm_ob$info$extra_data <- extra_data
  cntrl <- harsat:::run_control_default()
  cntrl <- harsat:::run_control_modify(cntrl, control)
  if (any(names(cntrl) %in% names(ctsm_ob$info))) {
    id <- names(cntrl)
    id <- id[id %in% names(ctsm_ob$info)]
    warning("\n conflict between components of ctsm_ob$info and control parameters ", 
            "- results may be unexpected:\n ", paste(id, collapse = ", "), 
            "\n", call. = FALSE, immediate. = TRUE)
  }
  ctsm_ob$info <- append(ctsm_ob$info, cntrl)
  ctsm_ob$assessment <- vector(mode = "list", length = nrow(ctsm_ob$timeSeries))
  names(ctsm_ob$assessment) <- row.names(ctsm_ob$timeSeries)
  ctsm_ob$call.data <- NULL
  series_id <- row.names(ctsm_ob$timeSeries)
  if (!is.null(substitute(subset))) {
    timeSeries <- tibble::rownames_to_column(ctsm_ob$timeSeries, 
                                             "series")
    ok <- eval(substitute(subset), timeSeries, parent.frame())
    series_id <- timeSeries[ok, "series"]
  }
  out <- harsat:::assessment_engine(ctsm_ob, series_id, parallel = parallel, 
                           ...)
  ctsm_ob$assessment[names(out)] <- out
  ctsm_ob
}



