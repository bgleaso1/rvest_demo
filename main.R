library(rvest)
library(stringr)
library(tidyverse)

######################## GET ALL MOVIES ON THE HOMEPAGE ########################

# Get links --------------------------------------------------------------------
base_link <- "https://www.imdb.com"
homepage <- read_html(base_link)

partial_links <-
  homepage %>%
  html_nodes(".title a") %>%
  html_attr("href")

full_links <- paste0(base_link, partial_links)

# Function to return a dataframe of movie data ---------------------------------
get_movie_data <- function(link) {
  
  doc <- read_html(link)
  
  movie_name <-
    doc %>%
    html_nodes("div.title_wrapper h1") %>%
    html_text() %>%
    str_trim() %>%
    str_remove_all("[:space:]\\([:digit:]{4}\\)")
  
  movie_year <-
    doc %>%
    html_nodes("div.title_wrapper h1") %>%
    html_text() %>%
    str_extract("(?<=[:space:]\\()[:digit:]{4}(?=\\))")
  
  actors <- 
    doc %>%
    html_nodes(".primary_photo+ td a") %>%
    html_text() %>%
    str_remove("\\\n") %>%
    str_trim()
  
  character <-
    doc %>%
    html_nodes("#titleCast .character") %>%
    html_text(trim = T) %>%
    str_replace("[:space:]{2,}", "") %>%
    str_replace("/(?=[:alpha:])", "/ ")
  
  out <- tibble(
    link = link,
    title = movie_name,
    year = as.integer(movie_year),
    actor = actors,
    character_played = character
  )
  
  return(out)
  
}

# Loop through all links to get their corresponding movie data -----------------
movies <- tibble(
    title = character(0),
    year = integer(0),
    actor = character(0),
    character_played = character(0)
)

for(link in full_links) {
  
  movies <-
    movies %>%
    bind_rows(get_movie_data(link))
  
}

