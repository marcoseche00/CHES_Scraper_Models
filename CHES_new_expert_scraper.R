## CHES Scraper Model
## New Experts

## Marcos Echevarria-Eirea
## 09/2024
## Using SerpAPI for Google Scholar (https://serpapi.com/google-scholar-api)

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

# Define your API key -----

api_key <- "YOUR_SERPAPI_KEY"  # Replace with your actual API key

# Define search parameters -----

search_query <- 'European parties' # Example search; any results containing the keywords of interest

# Make API request -----

url <- "https://serpapi.com/search"

params <- list(
    api_key = api_key,
    engine = "google_scholar",
    q = search_query,
    as_ylo = 2020,  # Start year
    as_yhi = 2024,  # End year
    start = 0,  # For pagination: 0 for first page, 10 for second, etc.
    num = 20  # Number of results to fetch per page (max 20)
)

# Unfortunately, this call only collects 20 results at a time.
# To check more results, start the call at later starts
  
response <- GET(url, query = params)

# Parse response -----

results <- fromJSON(content(response, as = "text"), flatten = TRUE)

# Extract the specified components from all_papers

titles <- results[["organic_results"]][["title"]]
links <- results[["organic_results"]][["link"]]
snippets <- results[["organic_results"]][["snippet"]]
summary <- results[["organic_results"]][["publication_info.summary"]]
n_citations <- results[["organic_results"]][["inline_links.cited_by.total"]]

# Combine all extracted data into a data frame

results <- data.frame(
  title = titles,
  link = links,
  snippet = snippets,
  summary = summary,
  n_citations = n_citations
)

# Data cleaning ----------------------------------------------------------------

# Create a data set with all authors (Initial + Surname) present + delete repeated authors -----

new_experts <- results %>% 
  mutate(authors = gsub(" -.*", "", summary)) %>% # Get authors out of summary
  separate_rows(authors, sep = ", ") %>% # Separate authors with commas
  distinct(authors, .keep_all = TRUE) %>%  # Select columns + unique values (deleting repeated authors)
  separate(authors, into = c("Initial", "Last Name"), sep = " ", extra = "merge") %>% # Separate initials from last_names and get rid of spaces between the two
  mutate(`Last Name` = stri_trans_general(`Last Name`, "Latin-ASCII")) %>% # Remove accents
  arrange(`Last Name`) %>% # Order alphabetically
  mutate(year = stringr::str_extract(summary, "\\d{4}")) # Create a year variable

# Load existing expert list

existing_experts <- "path/to/your/file.xlsx"
existing_experts <- read_excel(existing_experts, sheet = 1)

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


