## CHES Scraper Model
## Scholar Email Check

## Marcos Echevarria-Eirea
## 07/2024

rm(list=ls())

library(RSelenium)
library(wdman)
library(binman)
library(netstat)
library(tidyverse)
library(conflicted)
library(dplyr)
library(rvest)
library(httr)
library(xlsx)
library(stringi)
library(readxl)
library(rvest)
library(stringr)
library(R.utils)

# Debug RSelenium (if needed)

binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)

#Step 1: set up Chrome search --------------------------------------------------

# Download Java Script + set up
# If unable to set up, check out this useful tutorial: https://www.youtube.com/watch?v=GnpJujF9dBw
# https://www.youtube.com/watch?v=BnY4PZyL9cg

# Check Chrome version

chromeCommand <- chrome(retcommand = T, verbose = F, check = F) # It might be outdated
# Enable hidden items in Mac: https://www.wikihow.com/Show-Hidden-Files-and-Folders-on-a-Mac 
# Shift + Command + . should work

binman::list_versions("chromedriver")

# If it doesn't run when updating chromedriver, make sure to only leave chromedriver file in chromedriver folder

# Set incognito window

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "128.0.6613.138", # This should be your Chrome version
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = list(
                               chromeOptions = list(
                                 args = list("--incognito")
                               )
                             ))

#Create a client object

remDr <- rs_driver_object$client

#Step 2: create functions ------------------------------------------------------

# Delay

random_delay <- function(min_seconds = 2, max_seconds = 5) {
  delay <- runif(1, min = min_seconds, max = max_seconds)
  Sys.sleep(delay)
}

# Email scraper function

scrape_emails <- function(page_source) {
  email_pattern <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
  emails <- str_extract_all(page_source, email_pattern)
  emails <- unlist(emails)
  if (length(emails) > 0) {
    return(emails[1])  # Assuming the first email found is the correct one
  } else {
    return(NA)
  }
}

# Scraper 1 (Google Scholar)

google_scholar_scraper <- function(row, remDr) {
  first_name <- row[["First Name"]]
  last_name <- row[["Last Name"]]
  original_email <- row[["Email"]]
  
  # Navigate to Google Scholar
  tryCatch({
    withTimeout({
      remDr$navigate('https://scholar.google.com/')
    }, timeout = 20, onTimeout = "error") # In case of timeout
  }, error = function(e) {
    print(paste("Timeout error navigating to Google Scholar:", e$message))
    return(NULL)
  })
  
  # Find search bar and perform search
  search_bar <- remDr$findElement(using = 'id', 'gs_hdr_tsi')
  search_query <- paste(first_name, last_name)
  search_bar$sendKeysToElement(list(search_query, key = "enter"))
  
  # Wait for results to load
  random_delay()
  
  # Identify scholars that appear
  scholar_names <- remDr$findElements(using = 'css selector', '.gs_rt2 a')
  
  if (length(scholar_names) > 0) {
    # Click on the first search result
    tryCatch({
      withTimeout({
        first_result_element <- scholar_names[[1]]
        profile_link <- first_result_element$getElementAttribute('href')[[1]]  # Get the profile link
        
        first_result_element$clickElement()  # Click on the profile link
      }, timeout = 20, onTimeout = "error")
      
      # Wait for the profile page to load
      random_delay()
      
      # Get affiliation
      affiliation_text <- NA
      affiliation_element <- remDr$findElement(using = 'class name', 'gsc_prf_il')
      if (!is.null(affiliation_element)) {
        affiliation_text <- affiliation_element$getElementText()
      }
      
      # Get fields
      fields_elements <- remDr$findElements(using = 'css selector', '#gsc_prf_int a.gsc_prf_inta')
      fields <- sapply(fields_elements, function(x) x$getElementText()[[1]]) %>% unlist() %>% paste(collapse = ", ")
      
      # Click on year and get last publication year
      last_publication_year <- NA
      active <- NA
      try({
        withTimeout({
          # Click on Year
          year_sort_element <- remDr$findElement(using = 'css selector', '#gsc_a_ha a.gsc_a_a')
          year_sort_element$clickElement()
          
          # Get the last publication year
          last_publication_year_element <- remDr$findElement(using = 'css selector', '.gsc_a_h.gsc_a_hc.gs_ibl')
          last_publication_year <- last_publication_year_element$getElementText()[[1]]
          
          active <- ifelse(as.integer(last_publication_year) > 2022, 1, 0)
        }, timeout = 20, onTimeout = "error")
      }, silent = TRUE)
      
      # Initialize variable for new_email
      new_email <- NA
      
      # Get personal webpage link (if available) and get link
      webpage_link <- NA
      try({
        withTimeout({
          webpage_element <- remDr$findElement(using = 'css selector', 'a.gsc_prf_ila[rel="nofollow"]')
          webpage_link <- webpage_element$getElementAttribute('href')[[1]]
          
          if (!is.na(webpage_link)) {
            webpage_element$clickElement()
            
            # Wait for the page to load
            random_delay()
            
            # Extract the page source
            page_source <- remDr$getPageSource()[[1]]
            
            # Scrape emails from the page source
            new_email <- scrape_emails(page_source)
          }
        }, timeout = 20, onTimeout = "error")
      }, silent = TRUE)
      
      # Results
      result <- list(
        `Google Scholar Profile` = profile_link,
        Affiliation = affiliation_text,
        Fields = fields,
        `Last Published` = last_publication_year,
        Active = active,
        `Webpage Link` = webpage_link,
        `New Email` = new_email
      )
      
      return(result)
      
    }, error = function(e) {
      print(paste("Error clicking on search result or extracting profile information:", e$message))
      return(NULL)
    })
    
  } else {
    print("No scholar profiles found.")
    return(NULL)
  }
}

# Scraper 2 (Google)

chrome_search <- function(row, remDr) {
  # Ensure row is a data frame row
  if (!inherits(row, "data.frame")) {
    stop("row should be a data frame row.")
  }
  
  # Convert columns to character
  first_name <- as.character(row[["First Name"]])
  last_name <- as.character(row[["Last Name"]])
  
  # Initialize variables
  max_links <- 5  #limit to 5 links
  
  # Check if 'New Email' is not NA
  if (!is.na(row[["New Email"]])) {
    return(list(new_email = row[["New Email"]], email_webpage = NA))
  }
  
  # Clean Affiliation column to match desired format
  affiliation <- as.character(row[["Affiliation"]])
  if (is.na(affiliation) || affiliation == "NA" || affiliation == "") {
    affiliation <- "political science"
  }
  
  # Construct the search query with specific format
  search_terms <- paste(first_name, last_name, affiliation, sep = " ")
  
  # Encode the search query for the URL
  search_url <- paste0(
    "https://www.google.com/search?q=",
    URLencode(search_terms, reserved = TRUE)
  )
  
  # Navigate to Google search page with timeout
  tryCatch({
    withTimeout({
      remDr$navigate(search_url)
    }, timeout = 20, onTimeout = "error")
  }, error = function(e) {
    print(paste("Timeout error navigating to Google search page:", e$message))
    return(list(new_email = NA, email_webpage = NA))
  })
  
  # Handle cookie consent pop-up (if any)
  try({
    random_delay()
    cookie_reject_button <- remDr$findElement(using = "css selector", ".QS5gu.sy4vM")
    cookie_reject_button$clickElement()
  }, silent = TRUE)
  
  # Get the result links on the first page
  result_links <- remDr$findElements(using = "css selector", "div.yuRUbf a")
  result_urls <- sapply(result_links[1:min(length(result_links), max_links)], function(link) {
    tryCatch({
      link$getElementAttribute("href")[[1]]
    }, error = function(e) {
      NA
    })
  })
  
  # Filter out NA values
  result_urls <- result_urls[!is.na(result_urls)]
  
  # Check each link found
  for (url in result_urls) {
    print(paste("Navigating to URL:", url)) # Debug print
    
    # Navigate to each result URL with timeout
    tryCatch({
      withTimeout({
        remDr$navigate(url)
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      print(paste("Timeout error navigating to result URL:", e$message))
      next
    })
    
    random_delay()
    
    # Get the page source and search for emails
    page_source <- remDr$getPageSource()[[1]]
    
    # Scrape emails from the page source
    extracted_email <- scrape_emails(page_source)
    
    if (!is.na(extracted_email)) {
      return(list(new_email = extracted_email, email_webpage = url))
    }
    
    print("Email not found, moving to next URL.") # Debug print
    
    # Navigate back to the search results with timeout
    tryCatch({
      withTimeout({
        remDr$goBack()
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      print(paste("Timeout error navigating back to search results:", e$message))
      next
    })
    
    random_delay()
  }
  
  # If no email found after iterating, return NA
  return(list(new_email = NA, email_webpage = NA))
}

#Step 3: set up data -----------------------------------------------------------

#load data ------------

#your data

# Add variables

dat$`Google Scholar Profile` <- NA
dat$Affiliation <- NA
dat$Fields <- NA
dat$`Last Published` <- NA
dat$Active <- NA
dat$`Webpage Link` <- NA
dat$`New Email` <- NA

# Remove duplicates

dup_names <- duplicated(dat[, c("First Name", "Last Name")]) | 
  duplicated(dat[, c("First Name", "Last Name")], fromLast = TRUE)
first_occurrence <- !duplicated(dat[, c("First Name", "Last Name")])
dat <- dat[first_occurrence | !dup_names, ]

# Remove accents

dat <- dat %>% 
  mutate(`First Name` = stri_trans_general(`First Name`, "Latin-ASCII")) %>% 
  mutate(`Last Name` = stri_trans_general(`Last Name`, "Latin-ASCII"))

#Step 4: perform search --------------------------------------------------------

# First search (Google Scholar + linked website)

for (i in 1:nrow(dat)) {
  random_delay()
  result <- google_scholar_scraper(dat[i, ], remDr)
  
  if (!is.null(result)) {
    dat$`Google Scholar Profile`[i] <- result$`Google Scholar Profile`
    dat$Affiliation[i] <- result$Affiliation
    dat$Fields[i] <- result$Fields
    dat$`Last Published`[i] <- result$`Last Published`
    dat$Active[i] <- result$Active
    dat$`Webpage Link`[i] <- result$`Webpage Link`
    dat$`New Email`[i] <- result$`New Email`
  } else {
  }
} 
  # Might have to deal with CAPTCHA

# Second search (Google search)

for (i in 1:nrow(dat)) {
  if (is.na(dat[i, "New Email"])) {
    # Ensure row is a data frame row
    if (!inherits(dat[i, ], "data.frame")) {
      stop("row should be a data frame row.")
    }
    
    # Error handling
    tryCatch({
      search_result <- chrome_search(dat[i, ], remDr)
      dat[i, "New Email"] <- search_result$new_email
      dat[i, "Email Webpage"] <- search_result$email_webpage
    }, error = function(e) {
      print(paste("Error in chrome_search for row", i, ":", e))
      dat[i, "New Email"] <- NA
      dat[i, "Email Webpage"] <- NA
    })
  }
} 

#Step 5: save data -------------------------------------------------------------

folder_path <- "path"
write.xlsx(dat, file = file.path(folder_path, "name_of_file.xlsx"))

#Step 6: cleanup ---------------------------------------------------------------

remDr$close()
rs_driver_object$server$stop()
