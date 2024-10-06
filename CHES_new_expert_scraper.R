## CHES Scraper Model
## New Experts

## Marcos Echevarria-Eirea
## 09/2024
## Based on Claudiu-Cristian Papasteri's scraper (https://claudiu.psychlab.eu/)

rm(list=ls())

# Load libraries ---------------------------------------------------------------

library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readxl)
library(stringi) # To remove accents

# Scraper ----------------------------------------------------------------------

# Function -----

scrape_gs <- function(term, pages, crawl_delay, useragent_string) {
  
  gs_url_base <- url # Left as url to be able to alter
  
  result_list <- list()
  i <- 1
  
  for (n_page in (pages - 1) * 10) {  # Google Scholar page indexing starts with 0, 10 articles per page
    
    gs_url <- paste0(gs_url_base, "?start=", n_page, "&q=", noquote(gsub("\\s+", "+", trimws(term))))
    t0 <- Sys.time()
    
    # Use httr::user_agent() function to create the useragent header
    session <- rvest::session(gs_url, httr::user_agent(useragent_string))  # Pass user agent to the session
    
    t1 <- Sys.time()
    response_delay <- as.numeric(t1 - t0)  # Calculate response time
    wbpage <- rvest::read_html(session)
    
    # Avoid HTTP error 429 due to too many requests - use crawl delay & back off
    Sys.sleep(crawl_delay + 3 * response_delay + runif(n = 1, min = 0.5, max = 1))
    
    if ((i %% 10) == 0) {  # Sleep every 10 iterations
      message("taking a break")
      Sys.sleep(10 + 10 * response_delay + runif(n = 1, min = 0, max = 1))
    }
    
    i <- i + 1
    
    # Scrape data
    titles <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rt"))
    authors_years <- rvest::html_text(rvest::html_elements(wbpage, ".gs_a"))
    part_abstracts <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rs"))
    bottom_row_nodes <- rvest::html_elements(wbpage, ".gs_fl")
    bottom_row_nodes <- bottom_row_nodes[!grepl("gs_ggs gs_fl", as.character(bottom_row_nodes), fixed = TRUE)]  # Exclude download links
    bottom_row <- rvest::html_text(bottom_row_nodes)
    
    # Process data
    authors <- gsub("^(.*?)\\W+-\\W+.*", "\\1", authors_years, perl = TRUE)
    years <- gsub("^.*(\\d{4}).*", "\\1", authors_years, perl = TRUE)
    citations <- strsplit(gsub("(?!^)(?=[[:upper:]])", " ", bottom_row, perl = TRUE), "  ")  # Split on capital letter to get citations link
    citations <- lapply(citations, "[", 3)
    n_citations <- suppressWarnings(as.numeric(sub("\\D*(\\d+).*", "\\1", citations)))
    
    # Store in list
    result_list <- append(
      result_list, 
      list(
        list(
          page = n_page / 10 + 1,
          term = term,
          title = titles, 
          authors = authors, 
          year = years,
          n_citations = n_citations,
          abstract = part_abstracts
        )
      )
    )
  }
  
  # Return as data frame
  result_df <- lapply(result_list, as.data.frame)
  result_df <- as.data.frame(do.call(rbind, result_df))
  return(result_df)
}

# Define user agent -----

useragent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36"

# Define url -----

url <- "https://scholar.google.com/scholar?as_ylo=2020" # Here we are interested on 2020 onward

# Collect -----

new_experts <- scrape_gs(
  term = 'intext: ("European party" OR "European parties")',  # Adjust search term accordingly; the example here is party/parties Europe/European
  pages = 1:20,                               # Specify pages to scrape (here 20 pages); should be more than enough
  crawl_delay = 1.2,                          # Delay between requests to avoid being blocked
  useragent_string                            # Pass the useragent string
) 

  # If blocked, use a VPN to change IP address

# Data cleaning ----------------------------------------------------------------

# Create a data set with all authors (Initial + Surname) present + delete repeated authors -----

new_experts <- new_experts %>% 
  distinct(authors, .keep_all = TRUE) %>%  # Select columns + unique values (deleting repeated authors)
  separate_rows(authors, sep = ", ") %>% # Separate authors with commas
  separate(authors, into = c("Initial", "Last Name"), sep = " ", extra = "merge") %>% # Separate initials from last_names and get rid of spaces between the two
  mutate(`Last Name` = stri_trans_general(`Last Name`, "Latin-ASCII")) %>% # Remove accents
  arrange(`Last Name`) # Order alphabetically

# Load existing expert list

existing_experts <- "path/to/your/file.xlsx"
existing_experts <- read_excel(belgium, sheet = 1)

existing_experts <- existing_experts %>%
  select(`First Name`, `Last Name`) %>% # Select variables of interest
  mutate(`Last Name` = stri_trans_general(`Last Name`, "Latin-ASCII")) %>% 
  mutate(Initial = substr(`First Name`, 1, 1)) # Create an initial column for comparison

# Compare and delete

new_experts <- new_experts %>%
  anti_join(existing_experts, by = c("Last Name" = "Last Name", "Initial" = "Initial"))

# To generate new expert lists: ------------------------------------------------

# Manually go over abstracts and select OR run email_scraper on new names (or both)

# Save new experts data and it's done!


