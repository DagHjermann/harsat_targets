
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

# Show network, but not status (up to date, outdated etc.)
tar_glimpse()

# Show status (up to date, outdated etc.)
tar_visnetwork()

# Alternative
txt <- tar_mermaid()
writeLines(txt, "clipboard")  # copies mermaid code
# the past into e.g. https://mermaid.live/ 
# Show outdated targets
tar_outdated()   

tar_validate()

# run the pipeline  
# targets that need to be updated, will be updated (this may take time!)
# the rest of the targets will be skipped
tar_make()

# summarize results of updating 
tar_progress()             # note: doesn't show which targets that are outdated
tar_progress_summary()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Test results of pipeline using using 'ggplot_assessment' ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

str(tar_read(biota_assess_data_CD), 1)

#
# test some plots
#
str(tar_read(biota_assess_data_CD), 1)
str(tar_read(biota_assess_data_PFOS), 1)
x <- tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]]
library(ggplot2)
source("R/functions_utility.R")
ggplot_assessment(x)
ggplot_assessment(x, plot_points = "all")
ggplot_assessment(x, plot_points = "annual", logscale = FALSE)
ggplot_assessment(x, plot_points = "all", logscale = FALSE)
ggplot_assessment(x, plot_points = "all", logscale = FALSE, ylim = c(0,17))

#
# plot all, version 1 
#
plots <- list()
i <- 1
assessdata_list <- tar_read(biota_assess_data_CD)
for (assessdata in assessdata_list){
  x2 <- ggplot_assessment(assessdata)
  plots[[i]] <- x2
  i <- i + 1
}
assessdata_list <- tar_read(biota_assess_data_PFOS)
for (assessdata in assessdata_list){
  x2 <- ggplot_assessment(assessdata)
  plots[[i]] <- x2
  i <- i + 1
}
# str(plots, 1)
# Plot all created plots in a grid
cowplot::plot_grid(plotlist = plots)


#
# plot all, version 2 
# - makes list, but still needs to hard-code each branch result in the code
#
assessdata_list <- c(
  tar_read(biota_assess_data_CD), 
  tar_read(biota_assess_data_PFOS)
)
str(assessdata_list, 1)
# Create all plots and plot them in a grid
plots <- lapply(assessdata_list, ggplot_assessment)
cowplot::plot_grid(plotlist = plots)

#
# plot all, version 3 
# - makes list, no need for hard-coding each branch result
#

# Get list of objects
df_targets_all <- tar_progress()
target_names <- grep("biota_assess_data", df_targets_all$name, value = TRUE)
# Load all biota assessment data, as separate objects
tar_load_raw(target_names)
# Combine objects to a list (a list of lists, actually)
object_list_unflattened <- lapply(target_names, get)
# Remove separate objects
rm(list = target_names)
# ls()
# str(object_list_unflattened, 1)
# Flatten the list of lists, to just a list  
object_list <- unlist(object_list_unflattened, recursive = FALSE)
str(object_list, 1)
# Create all plots and plot them in a grid
plots <- lapply(object_list, ggplot_assessment)
cowplot::plot_grid(plotlist = plots)


#
# plot all, version 4 
# - as above, but combined to a function
#
source("R/functions_utility.R")
library(ggplot2)
assessment_data <- combine_assessment_data()
plots <- lapply(assessment_data, ggplot_assessment)
cowplot::plot_grid(plotlist = plots)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Branching approach 1: add 'targets_group' column and test 'read_data_tar' ----  
#
# This won't work anymore due to changes in 'split_info_object'  
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

# library(harsat)
# debugonce(tidy_data)
biota_data_tidy1 <- tidy_data_tar(biota_data)
biota_data_tidy2 <- tidy_data2(biota_data_tidy1)
str(biota_data_tidy2$data)

# debugonce(create_timeseries)
biota_timeseries_all <- create_timeseries_tar(
  biota_data_tidy2,
  determinands = ctsm_get_determinands(biota_data_tidy2$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

str(biota_timeseries_all, 1)
str(biota_timeseries_all$data, 1)
biota_timeseries_list <- split_timeseries_object(biota_timeseries_all)

# time series object for one parameter
obj_ts <- biota_timeseries_list[["CD"]]
# library(harsat)

# test assessment function
source("R/functions.R")
obj_ass <- run_assessment_tar(
  biota_timeseries_list[["CD"]], # biota_timeseries
  info = biota_timeseries_all$info,
  # subset = sel_series,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10))
)

# get assessment data
data_ass <- get_assessment_data(obj_ass)

# check assessment data
str(data_ass, 1)
library(ggplot2)
source("R/functions_utility.R")
ggplot_assessment(data_ass[[1]])


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# test 'split_info_object' ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# split "info" object in a way so we reduce the amount of data in each object (which is copied N times)
# We end up not using this approach, see bottom of _targets.R for better solution (using a common info file)  

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



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Branching approach 2: create 'branching_groups' data ----  
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

biota_data <- tar_read(biota_data)

if (FALSE){

  # create 'branching_groups' data
  # 'branch' column will define the branching
  
  
  branching_groups <- biota_data$data |>
    # note: not including sex
    dplyr::count(determinand, station_code, species, matrix, subseries, basis) %>%
    # define 'branching_groups' for this test sample
    mutate(
      subseries = addNA(subseries),
      branch = paste0(determinand, "_allstations")
    )
  # write to file:
  readr::write_csv(branching_groups, "harsat_data/branching_groups.csv")
}


# inspect pipeline
tar_manifest()
tar_glimpse()
tar_visnetwork()

tar_make()

# For development of split_timeseries_object:
#   tar_make(biota_timeseries_all)
#   tar_make(branching_groups)
#   x <- tar_read(biota_timeseries_all)
#   x2 <- tar_read(branching_groups)
#   test <- split_timeseries_object(object = x, df_branching = x2)

if (FALSE){
  x1 <- tar_read(file_info)
  x2 <- tar_read(file_stations)
  x3 <- tar_read(file_contaminants)
  test <- read_data_tar(
    compartment = "biota",
    purpose = "OSPAR",
    contaminants = file,   
    data_dir = file.path("harsat_data"),
    data_format = "external",
    info_dir = file.path("information", "OSPAR_2022"),
    filename_info = x1,
    filename_stations = x2,
    filename_contaminants = x3
  )
  
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# Solving errors - example ----
#
# How to solve an error when running tar_make()
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# 1. The error. After updating harsat, some targets were outdated
# and we ran tar_make:

tar_make()

# Output:
#
# ✔ skipped target file_contaminants
# ✔ skipped target file_info
# ✔ skipped target file_stations
# ✔ skipped target file_branching
# ✔ skipped target biota_data
# ✔ skipped target branching_groups
# ✔ skipped target biota_data_tidy
# ✔ skipped target biota_data_tidy2
# ✔ skipped target biota_timeseries_all
# ✔ skipped target info
# ▶ dispatched target biota_timeseries_list
# ✖ errored target biota_timeseries_list
# ✖ errored pipeline [0.78 seconds]
# Error:
#   ! Error running targets::tar_make()
# Error messages: targets::tar_meta(fields = error, complete_only = TRUE)
# Debugging guide: https://books.ropensci.org/targets/debugging.html
# How to ask for help: https://books.ropensci.org/targets/help.html
# Last error message:
#   undefined columns selected
# Last error traceback:
#   split_timeseries_object(biota_timeseries_all, branching_groups)
# object$data[!sel_missing]
# `[.data.frame`(object$data, !sel_missing)
# stop("undefined columns selected")
# .handleSimpleError(function (condition)  {     state$error <- build_mess...
# h(simpleError(msg, call))

# 2. Find the key parts in error message. These two parts tell us where in the 
# pipeline the error happened:
#
# last thing dispatched: 'dispatched target biota_timeseries_list'
# Last error traceback:
#   split_timeseries_object(biota_timeseries_all, branching_groups)
# So there was an error when running 'split_timeseries_object' 
#
# Also, we see that 'split_timeseries_object' returne the following error message:
# Last error message:
#   undefined columns selected

# 3. Re-create the error using "ordinary R code", i.e. outside targets. 
# In _targets.R, we see that the line with the error is:
#   tar_target(biota_timeseries_list, split_timeseries_object(biota_timeseries_all, branching_groups)),
# In usual R code, this is equivalent to
#   biota_timeseries_list <- split_timeseries_object(biota_timeseries_all, branching_groups)
# In order to run this line, we need the objects 'biota_timeseries_all' and 'branching_groups'
# We get them like this:
biota_timeseries_all <- tar_read(biota_timeseries_all)
branching_groups <- tar_read(branching_groups)
# We also need to load necessary packages and scripts (see top of the _targets.R code)
library(harsat)
source("R/functions.R")
# Now we can test this:
biota_timeseries_list <- split_timeseries_object(biota_timeseries_all, branching_groups)
# Error in `[.data.frame`(object$data, !sel_missing) : 
#   undefined columns selected
# Thus, this recreated the error that we got when running tar_make(). We are then ready to 
# actually find out what's wrong.

# 4. Debugging (i.e. find out what's wrong)  
# I usually use this command:
debugonce(split_timeseries_object)
# which puts 'split_timeseries_object' into debugging mode once. Debugging mode means that
# when you run the function, you will  step through the function line by line. 
# (You can also use 'debug(split_timeseries_object)' - then you have to use 
# 'undebug(split_timeseries_object)' after debugging is finished.)
biota_timeseries_list <- split_timeseries_object(biota_timeseries_all, branching_groups)
# While debugging, you can
#   - in the console, check the different objects in the function, e.g. 'str(object$data)' to see the 
#   columns and the number of variables and rows
#   - see the values of variables in the "Environment" pane of RStudio (top right),
#   and click on data frames to view them in the top left pane
#   - copy lines form the functions to the console and change them before you run them, to see 
#   if you get different result
#   - you will run through loops line by line N times - if the loop is tedious and uninteresting, 
#   there is a button to skip through them (i.e. run the loop, but not line by line)
