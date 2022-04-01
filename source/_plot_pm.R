# Plot PM Data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# these plots include:
#   - state, regional, and metro trends
#   - county-level plots in a variety of metro and non-metro regions

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# update submodule ####
system("git submodule update --remote")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# UPDATE date value
date <- Sys.Date()-1

# define first date for plotting
test_date_breaks <- "20 days"
total_test_date_breaks <- "20 days"
hosp_breaks <- "2 months"
x_angle <- 25

# create caption
caption_text_tests <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project and the State of Missouri"
caption_text_tests_census <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project, the State of Missouri, and and the U.S. Census Bureau"
caption_text_census_map <- "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau"

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# qa promts ####

## confirm St. Louis Pandemic Task Force data
hospital_update <- usethis::ui_yeah("Have you manually updated the Pandemic Task Force data from the latest slides?")

if (hospital_update == FALSE){
  if (weekdays(date) == "Tuesday"){
    hospital_update <- usethis::ui_yeah("Did you manually update the Pandemic Task Force data yesterday?")
    hosp_date <- date-1 
  }
} else if (hospital_update == TRUE){
  hosp_date <- date
}

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies

## packages
### tidyverse
library(dplyr)          # data wrangling
library(forcats)        # factor tools
library(ggplot2)        # mapping and plotting
library(readr)          # csv file tools
library(stringr)        # string tools
library(tidyr)          # data wrangling

### spatial packages
library(sf)             # mapping tools

### other packages
library(ggrepel)        # map labeling
library(RColorBrewer)   # color palettes
library(waffle)         # waffle plot

## functions
source("source/functions/get_coords.R")        # convert sf geometry to x,y columns
source("source/functions/map_breaks.R")        # creating map beaks
source("source/functions/round_any.R")         # rounding
source("source/functions/save_plots.R")        # save plots and maps
source("source/functions/sequoia_theme.R")     # theme for plots and maps

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# update plots
## overview plots
# source("source/workflow/02_state_test_plots.R")
# source("source/workflow/03_state_individual_plots.R")

# regional plots
source("source/workflow/18_stl_zip_plots_v3.R")
source("source/workflow/19_stl_individual_plots.R")

if (hospital_update == TRUE){
  source("source/workflow/20_stl_hospital_plots.R")  
}

source("source/workflow/22_mo_deaths.R")
source("source/workflow/23_vaccines.R")
source("source/workflow/24_district_vaccines.R")
source("source/workflow/25_vaccination_progress.R")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(map_bins, map_breaks, round_any, save_plots, sequoia_theme, x_angle)
rm(get_coords, caption_text_tests, caption_text_tests_census, test_date_breaks, 
   total_test_date_breaks)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

commit <- function(){
  
  auto_update <- usethis::ui_yeah("Do you want to automatically update the remote GitHub repo?")
  
  system("git add -A")
  system(paste0("git commit -a -m 'build pm plots for ", as.character(date), "'"))
  
  if (auto_update == TRUE){
    system("git push origin master")
  }
  
}
