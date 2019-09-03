library(rvest)
library(stringr)
library(tidyverse)

# Get HTML document
doc <- read_html("https://www.imdb.com/title/tt4154796/")

# 
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
  title = movie_name,
  year = as.integer(movie_year),
  actor = actors,
  character_played = character
)

