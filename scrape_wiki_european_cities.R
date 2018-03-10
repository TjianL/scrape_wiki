library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)

html.population <- read_html('https://en.wikipedia.org/wiki/List_of_European_cities_by_population_within_city_limits')
html.population

df.euro_cities <- html.population %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()

df.euro_cities %>% head()
df.euro_cities %>% names()

df.euro_cities <- df.euro_cities %>% select(-Date,-Image, -Location, -`Ref.`, -`2011 Eurostat\npopulation[3]`)

#========================================================================
# CLEAN UP VARIABLE: population
# - when the data are scraped, there are some extraneous characters
#   in the "population" variable.
#   ... you can see leading numbers and some other items
# - We will use stringr functions to extract the actual population data
#   (and remove the stuff we don't want)
# - We are executing this transformation inside dplyr::mutate() to 
#   modify the variable inside the dataframe
#========================================================================

df.euro_cities %>% names()
colnames(df.euro_cities) <- c('rank','city','country','population')
df.euro_cities %>% head()
df.euro_cities <- df.euro_cities %>% mutate(population = str_extract(population,"♠.*$") %>% str_replace("♠","") %>% parse_number())

#==========================================================================
# REMOVE "notes" FROM CITY NAMES
# - two cities had extra characters for footnotes
#   ... we will remove these using stringr::str_replace and dplyr::mutate()
#==========================================================================
df.euro_cities <- df.euro_cities %>% mutate(city = str_replace(city, "\\[.\\]",""))
df.euro_cities %>% head()

#=========================
# REMOVE CITIES UNDER 1 MM
#=========================

df.euro_cities <- df.euro_cities %>% filter(population > 1000000)

#=================
# COERCE TO TIBBLE
#=================

df.euro_cities <- df.euro_cities %>% as_tibble()
data.geo <- geocode(df.euro_cities$city)

?geocode
