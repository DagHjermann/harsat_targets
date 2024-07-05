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

# debugonce(ggplot_assessment)
ggplot_assessment <- function(assessment_data,
                              plot_points = "annual",
                              logscale = TRUE,
                              pointcolor = "darkred",
                              pointshapes = c(19, 6),
                              trendcolor_line = "darkblue",
                              trendcolor_fill = "lightblue",
                              trendwidth = 0.8,
                              ylim = NULL){
  if (!is.na(plot_points)){
    if (plot_points == "all"){
      pointdata <- assessment_data$assessment$fullData
      pointdata$y = pointdata$concentration
      pointdata$LOQ = ifelse(pointdata$censoring %in% "Q", "Under LOQ", "Over LOQ")
    } else if (plot_points == "annual"){
      pointdata <- assessment_data$assessment$annualIndex
      pointdata$y = exp(pointdata$index)
      pointdata$LOQ = ifelse(pointdata$censoring %in% "Q", "Under LOQ", "Over LOQ")
    } else {
      warning(
        "plot_points = 'all': plot all concentrations\n",
        "plot_points = 'annual': plot annual index",
        "plot_points = NA: plot trend only"
      )
    }
  } else {
    pointdata <- NULL
  }
  if (!is.null(pointdata)){
    gg <- ggplot(pointdata, aes(year)) +
      geom_ribbon(
        data = assessment_data$assessment$pred, 
        aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
        fill = trendcolor_fill) + 
      geom_path(
        data = assessment_data$assessment$pred, 
        aes(y = exp(fit)),
        color = trendcolor_line,
        size = rel(trendwidth)) + 
      geom_point(
        aes(y = y, shape = LOQ),
        color = pointcolor) +
      scale_shape_manual(
        values = c("Over LOQ" = pointshapes[1], "Under LOQ" = pointshapes[2]))
  } else {
    gg <- ggplot(assessment_data$assessment$pred, aes(year)) +
      geom_ribbon(
        aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
        fill = trendcolor_fill) + 
      geom_path(
        aes(y = exp(fit)),
        color = trendcolor_line,
        size = rel(trendwidth))
  }
  gg <- gg  +
    labs(
      title = assessment_data$output_id,
      y = "Concentration / index") 
  if (!is.null(ylim)){
    if (!(is.numeric(ylim) & length(ylim) == 2)){
      stop("ylim must be a vector of two numbers, e.g., 'c(0,10)'")
    }
  }
  if (logscale){
    gg <- gg + scale_y_log10(limits = ylim)
  } else {
    gg <- gg + scale_y_continuous(limits = ylim)
  }
  gg
}

if (FALSE){
  # testing
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]])
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    plot_points = "all")
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]], 
    plot_points = "annual", logscale = FALSE)
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]], 
    plot_points = "all", logscale = FALSE, ylim = c(0,17))
}



