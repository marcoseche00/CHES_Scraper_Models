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

#debug RSelenium (if needed)
binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)

#Step 1: set up Chrome search -------------------------------------------------------------------------------------------------------------

#set incognito window
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "126.0.6478.62",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = list(
                               chromeOptions = list(
                                 args = list("--incognito")
                               )
                             ))

#create a client object
remDr <- rs_driver_object$client

#Step 2: create functions ----------------------------------------------------------------------------------------------------------------

#delay
random_delay <- function(min_seconds = 2, max_seconds = 5) {
  delay <- runif(1, min = min_seconds, max = max_seconds)
  Sys.sleep(delay)
}

#email scraper function
scrape_emails <- function(page_source) {
  email_pattern <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
  emails <- str_extract_all(page_source, email_pattern)
  emails <- unlist(emails)
  if (length(emails) > 0) {
    return(emails[1])  #assuming the first email found is the correct one
  } else {
    return(NA)
  }
}

#model1
google_scholar_scraper <- function(row, remDr) {
  first_name <- row[["First Name"]]
  last_name <- row[["Last Name"]]
  original_email <- row[["Email"]]
  
  #navigate to Google Scholar
  tryCatch({
    withTimeout({
      remDr$navigate('https://scholar.google.com/')
    }, timeout = 20, onTimeout = "error") #in case of timeout
  }, error = function(e) {
    print(paste("Timeout error navigating to Google Scholar:", e$message))
    return(NULL)
  })
  
  #find search bar and perform search
  search_bar <- remDr$findElement(using = 'id', 'gs_hdr_tsi')
  search_query <- paste(first_name, last_name)
  search_bar$sendKeysToElement(list(search_query, key = "enter"))
  
  #wait for results to load
  random_delay()
  
  #identify scholars that appear
  scholar_names <- remDr$findElements(using = 'css selector', '.gs_rt2 a')
  
  if (length(scholar_names) > 0) {
    #click on the first search result
    tryCatch({
      withTimeout({
        first_result_element <- scholar_names[[1]]
        profile_link <- first_result_element$getElementAttribute('href')[[1]]  #get the profile link
        
        first_result_element$clickElement()  #click on the profile link
      }, timeout = 20, onTimeout = "error")
      
      #wait for the profile page to load
      random_delay()
      
      #get affiliation
      affiliation_text <- NA
      affiliation_element <- remDr$findElement(using = 'class name', 'gsc_prf_il')
      if (!is.null(affiliation_element)) {
        affiliation_text <- affiliation_element$getElementText()
      }
      
      #get fields
      fields_elements <- remDr$findElements(using = 'css selector', '#gsc_prf_int a.gsc_prf_inta')
      fields <- sapply(fields_elements, function(x) x$getElementText()[[1]]) %>% unlist() %>% paste(collapse = ", ")
      
      #click on year and get last publication year
      last_publication_year <- NA
      active <- NA
      try({
        withTimeout({
          #click on Year
          year_sort_element <- remDr$findElement(using = 'css selector', '#gsc_a_ha a.gsc_a_a')
          year_sort_element$clickElement()
          
          #get the last publication year
          last_publication_year_element <- remDr$findElement(using = 'css selector', '.gsc_a_h.gsc_a_hc.gs_ibl')
          last_publication_year <- last_publication_year_element$getElementText()[[1]]
          
          active <- ifelse(as.integer(last_publication_year) > 2022, 1, 0)
        }, timeout = 20, onTimeout = "error")
      }, silent = TRUE)
      
      #initialize variable for new_email
      new_email <- NA
      
      #get personal webpage link (if available) and get link
      webpage_link <- NA
      try({
        withTimeout({
          webpage_element <- remDr$findElement(using = 'css selector', 'a.gsc_prf_ila[rel="nofollow"]')
          webpage_link <- webpage_element$getElementAttribute('href')[[1]]
          
          if (!is.na(webpage_link)) {
            webpage_element$clickElement()
            
            #wait for the page to load
            random_delay()
            
            #extract the page source
            page_source <- remDr$getPageSource()[[1]]
            
            #scrape emails from the page source
            new_email <- scrape_emails(page_source)
          }
        }, timeout = 20, onTimeout = "error")
      }, silent = TRUE)
      
      #results
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

#model2
chrome_search <- function(row, remDr) {
  #ensure row is a data frame row
  if (!inherits(row, "data.frame")) {
    stop("row should be a data frame row.")
  }
  
  #convert columns to character
  first_name <- as.character(row[["First Name"]])
  last_name <- as.character(row[["Last Name"]])
  
  #initialize variables
  max_links <- 5  #limit to 5 links
  
  #check if 'New Email' is not NA
  if (!is.na(row[["New Email"]])) {
    return(list(new_email = row[["New Email"]], email_webpage = NA))
  }
  
  #clean Affiliation column to match desired format
  affiliation <- as.character(row[["Affiliation"]])
  if (is.na(affiliation) || affiliation == "NA" || affiliation == "") {
    affiliation <- "political science"
  }
  
  #construct the search query with specific format
  search_terms <- paste(first_name, last_name, affiliation, sep = " ")
  
  #encode the search query for the URL
  search_url <- paste0(
    "https://www.google.com/search?q=",
    URLencode(search_terms, reserved = TRUE)
  )
  
  #navigate to Google search page with timeout
  tryCatch({
    withTimeout({
      remDr$navigate(search_url)
    }, timeout = 20, onTimeout = "error")
  }, error = function(e) {
    print(paste("Timeout error navigating to Google search page:", e$message))
    return(list(new_email = NA, email_webpage = NA))
  })
  
  #handle cookie consent pop-up (if any)
  try({
    random_delay()
    cookie_reject_button <- remDr$findElement(using = "css selector", ".QS5gu.sy4vM")
    cookie_reject_button$clickElement()
  }, silent = TRUE)
  
  #get the result links on the first page
  result_links <- remDr$findElements(using = "css selector", "div.yuRUbf a")
  result_urls <- sapply(result_links[1:min(length(result_links), max_links)], function(link) {
    tryCatch({
      link$getElementAttribute("href")[[1]]
    }, error = function(e) {
      NA
    })
  })
  
  #filter out NA values
  result_urls <- result_urls[!is.na(result_urls)]
  
  #check each link found
  for (url in result_urls) {
    print(paste("Navigating to URL:", url)) #debug print
    
    #navigate to each result URL with timeout
    tryCatch({
      withTimeout({
        remDr$navigate(url)
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      print(paste("Timeout error navigating to result URL:", e$message))
      next
    })
    
    random_delay()
    
    #get the page source and search for emails
    page_source <- remDr$getPageSource()[[1]]
    
    #scrape emails from the page source
    extracted_email <- scrape_emails(page_source)
    
    if (!is.na(extracted_email)) {
      return(list(new_email = extracted_email, email_webpage = url))
    }
    
    print("Email not found, moving to next URL.") #debug print
    
    #navigate back to the search results with timeout
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
  
  #if no email found after iterating, return NA
  return(list(new_email = NA, email_webpage = NA))
}

#model3
last_publication_email_scraper <- function(row, remDr) {
  first_name <- row[["First Name"]]
  last_name <- row[["Last Name"]]
  
  #create name variable that matches format of interest
  name <- paste(substr(first_name, 1, 1), last_name, sep = " ")
  
  #skip if 'New Email' is not NA
  if (!is.na(row[["New Email"]])) {
    return(list(new_email = row[["New Email"]], email_webpage = NA))
  }
  
  #retrieve the Google Scholar Profile link
  scholar_profile_link <- row[["Google Scholar Profile"]]
  
  if (is.na(scholar_profile_link) || scholar_profile_link == "") {
    return(list(new_email = NA, email_webpage = NA))
  }
  
  #navigate to the Google Scholar profile with timeout
  tryCatch({
    withTimeout({
      remDr$navigate(scholar_profile_link)
    }, timeout = 20, onTimeout = "error")
  }, error = function(e) {
    print(paste("Timeout error navigating to Google Scholar profile:", e$message))
    return(list(new_email = NA, email_webpage = NA))
  })
  
  #wait for the profile page to load
  random_delay()
  
  #sort by year
  year_sort_element <- remDr$findElement(using = 'css selector', value = '#gsc_a_ha a.gsc_a_a')
  year_sort_element$clickElement()
  
  #wait for results to sort by year
  random_delay()
  
  #look for publications where scholar is first author
  html <- remDr$getPageSource()[[1]] 
  page <- read_html(html)
  
  #extract titles and authors
  authors_list <- page %>%
    html_nodes("div.gs_gray") %>%
    html_text()
  
  titles <- page %>%
    html_nodes("a.gsc_a_at") %>%
    html_text()
  
  #iterate through each publication's authors
  for (i in seq_along(authors_list)) {
    authors <- str_split(authors_list[i], ",")[[1]]
    
    #check if the first author matches 'name'
    if (str_trim(authors[1]) == name) {
      #click on the corresponding link
      title_link <- page %>%
        html_nodes("a.gsc_a_at") %>%
        html_attr("href") %>%
        .[i]
      
      #navigate to publication page with timeout
      tryCatch({
        withTimeout({
          full_link <- paste0("https://scholar.google.com", title_link)
          remDr$navigate(full_link)
        }, timeout = 20, onTimeout = "error")
      }, error = function(e) {
        print(paste("Timeout error navigating to publication page:", e$message))
        next  #move to the next iteration of the for loop
      })
      
      #click again to access full details if available
      second_link <- remDr$findElement(using = 'class name', value = 'gsc_oci_title_link')
      if (!is.null(second_link)) {
        second_link$clickElement()
        
        #get the updated page source
        page_source <- remDr$getPageSource()[[1]]
        
        #scrape emails from the page source
        extracted_email <- scrape_emails(page_source)
        
        if (!is.na(extracted_email)) {
          return(list(new_email = extracted_email, email_webpage = full_link))
        }
      }
    }
  }
  
  #if no relevant publications found or no email extracted, move on to the next row
  return(list(new_email = NA, email_webpage = NA))
} #needs to be improved

#Step 3: set up data ---------------------------------------------------------------------------------------------------------------------

#convert csv to excel (export to)
#remove first row
#remove accents in excel
  #formula: 
#=SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(A2, "à", "a"), "á", "a"), "â", "a"), "ã", "a"), "ä", "a"), "å", "a"), "æ", "ae"), "ç", "c"), "è", "e"), "é", "e"), "ì", "i"), "í", "i"), "î", "i"), "ï", "i")
  #substitute A2 with B2 for surnames
  #delete D and E columns after pasting new names and surmames in A and B

#load data ------------

#your data

#add variables
dat$`Google Scholar Profile` <- NA
dat$Affiliation <- NA
dat$Fields <- NA
dat$`Last Published` <- NA
dat$Active <- NA
dat$`Webpage Link` <- NA
dat$`New Email` <- NA

#remove duplicates
dup_names <- duplicated(dat[, c("First Name", "Last Name")]) | 
  duplicated(dat[, c("First Name", "Last Name")], fromLast = TRUE)
first_occurrence <- !duplicated(dat[, c("First Name", "Last Name")])
dat <- dat[first_occurrence | !dup_names, ]

#Step 4: perform search -------------------------------------------------------------------------------------------------------------------

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
} #first search (Google Scholar + linked website)
  #might have to deal with CAPTCHA
for (i in 1:nrow(dat)) {
  if (is.na(dat[i, "New Email"])) {
    #ensure row is a data frame row
    if (!inherits(dat[i, ], "data.frame")) {
      stop("row should be a data frame row.")
    }
    
    #error handling
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
} #second search (Google search)
for (i in 1:nrow(dat)) {
  if (is.na(dat[i, "New Email"])) {
    search_result <- last_publication_email_scraper(dat[i, ], remDr)
    dat[i, "New Email"] <- search_result$new_email
    dat[i, "Email Webpage"] <- search_result$email_webpage
  }
} #final search (article search) #needs to be improved

print(dat)

#Step 5: save data -----------------------------------------------------------------------------------------------------------------------

#folder_path <- "path"
#write.xlsx(dat, file = file.path(folder_path, "name_of_file.xlsx"))

#Step 6: cleanup --------------------------------------------------------------------------------------------------------------------------

remDr$close()
rs_driver_object$server$stop()
