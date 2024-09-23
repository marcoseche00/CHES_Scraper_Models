## CHES Scraper Model
## Party Vote Check 

## Marcos Echevarria-Eirea
## 09/2024

rm(list=ls())

# Load libraries ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(conflicted)
library(stringr)
library(dplyr)
library(xml2)
library(httr)

# Objective --------------------------------------------------------------------

#example: Spain
url <- "https://en.wikipedia.org/wiki/2023_Spanish_general_election"

tables <- read_html(url) %>%
  html_elements('table') %>%
  lapply(function(table) {
    caption <- table %>%
      html_element('caption') %>%
      html_text(trim = TRUE)
    table_data <- table %>%
      html_table(fill = TRUE)
    list(caption = caption, data = table_data)
  })

captions <- sapply(tables, function(x) x$caption) #extract captions
print(captions) #check with table we want. 15 in this case

results <- data.frame(tables[[15]]) #we save that as the result

#transformation


# URL scraper ------------------------------------------------------------------

#countries we want for CHES
national_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", 
                        "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                        "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                        "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                        "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                        "Spain", "Sweden", "Norway", "Switzerland", "Iceland", 
                        "Turkey", "United Kingdom")

european_countries <- national_countries[1:27]  #EU countries

#search URL function (using RSelenium)


# Table scraper ----------------------------------------------------------------

