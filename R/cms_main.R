options(scipen = 999)

# Packages ----------------------------------------------------------------
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(rvest)))
suppressMessages(suppressWarnings(library(stringi)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(janitor)))
suppressMessages(suppressWarnings(library(zoo)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(library(gt)))
suppressMessages(suppressWarnings(library(gtExtras)))

# Utilities ---------------------------------------------------------------

source('Function/borders.R')
source('Function/gt_theme_excel_customized.R')
source('Function/combine_to_html.R')
source('R/color.R')


# Call Scrapping ----------------------------------------------------------

source('R/btrc_scrapping_v02.R')


# The End -----------------------------------------------------------------


