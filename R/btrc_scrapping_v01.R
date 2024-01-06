

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(gt)
library(gtExtras)
library(janitor)
library(magrittr)
library(stringi)


# Scrap Link --------------------------------------------------------------

btrc_link <-
  'http://www.btrc.gov.bd/site/page/0ae188ae-146e-465c-8ed8-d76b7947b5dd/-'

btrc_link_eng <- 
  'http://www.btrc.gov.bd/site/page/0ae188ae-146e-465c-8ed8-d76b7947b5dd/-'

# Bengali to English Month ------------------------------------------------

bengali_months <- c(
  "জানুয়ারি" = "January",
  "ফেব্রুয়ারি" = "February",
  "মার্চ" = "March",
  "এপ্রিল" = "April",
  "মে" = "May",
  "জুন" = "June",
  "জুলাই" = "July",
  "আগস্ট" = "August",
  "সেপ্টেম্বর" = "September",
  "অক্টোবর" = "October",
  "নভেম্বর" = "November",
  "ডিসেম্বর" = "December"
)

bengali_months <- tibble(
  bengali = c(
    "জানুয়ারি",
    "ফেব্রুয়ারি",
    "মার্চ",
    "এপ্রিল",
    "মে",
    "জুন",
    "জুলাই",
    "আগস্ট",
    "সেপ্টেম্বর",
    "অক্টোবর",
    "নভেম্বর",
    "ডিসেম্বর"
  ),
  english = c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )
)

# Scrap Data --------------------------------------------------------------

rawHtml <- btrc_link %>%
  read_html() 

updated <- rawHtml %>% 
  html_node(".updateText") %>%
  html_text() %>% 
  gsub("সর্ব-শেষ হাল-নাগাদ: ", "", text) %>% 
  stringi::stri_trans_general('Bengali-Latin')


rawSub <- rawHtml %>% 
  html_table(header = TRUE, trim = TRUE) %>%
  extract2(1) %>%
  set_names(c('month', 'gp', 'rb', 'bl', 'tt', 'industry')) %>%
  separate(month, into = c('month', 'year'), sep = ' ') %>%
  mutate(across(
    year:industry,
    ~ stringi::stri_trans_general(.x, 'Bengali-Latin')
  ),
  across(everything(), trimws)) %>%
  left_join(bengali_months, by = c('month' = 'bengali')) %>% 
  mutate(month = english) %>% 
  select(-english)
