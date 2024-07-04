#
# Utility functions, used only outside targets
#
# (so the will not be shown in the vis_network graphs)
#

#
# Original version of 'read_data', here only for reference and copy/paste (and renamed 'read_data_orig' so it's not actually used)
#

read_data_orig <- function (compartment = c("biota", "sediment", "water"), purpose = c("OSPAR", 
                                                                                       "HELCOM", "AMAP", "custom"), contaminants, stations, data_dir = ".", 
                            data_format = c("ICES", "external"), info_files = list(), 
                            info_dir = ".", extraction = NULL, max_year = NULL, oddity_dir = "oddities", 
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
  cntrl <- control_default(purpose, compartment)
  cntrl <- control_modify(cntrl, control)
  if (any(names(cntrl) %in% names(info))) {
    id <- names(cntrl)
    id <- id[id %in% names(info)]
    warning("\n conflict between function arguments and control elements ", 
            "- results may be unexpected:\n ", paste(id, collapse = ", "), 
            "\n", call. = FALSE, immediate. = TRUE)
  }
  info <- append(info, cntrl)
  info <- read_info(info, info_dir, info_files)
  stations <- read_stations(stations, data_dir, info)
  data <- read_contaminants(contaminants, data_dir, info)
  if (data_format == "ICES") {
    data <- add_stations(data, stations, info)
    stations <- finalise_stations(stations, info)
    data <- finalise_data(data, info)
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
  out
}

#
# Not used in targets, only used once to create rds file to be read by targets
#
# identical to 'read_data'
# but stopping after
#   info <- read_info(info, info_dir, info_files)
# to return the info object
# Also:
# - added 'harsat:::' in front of 'control_default', 'control_modify', and 'read_info'

get_info_object <- function (compartment = c("biota", "sediment", "water"), purpose = c("OSPAR", 
                                                                                        "HELCOM", "AMAP", "custom"), contaminants, stations, data_dir = ".", 
                             data_format = c("ICES", "external"), info_files = list(), 
                             info_dir = ".", extraction = NULL, max_year = NULL, oddity_dir = "oddities", 
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
  info <- harsat:::read_info(info, info_dir, info_files)
  info
}
