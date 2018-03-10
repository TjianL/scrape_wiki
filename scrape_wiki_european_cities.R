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

#========================================================
# GEOCODE
# - here, we're just getting longitude and latitude data 
#   using ggmap::geocode()
#========================================================

data.geo <- data.frame(city=df.euro_cities$city,geocode(df.euro_cities$city),stringsAsFactors = FALSE)
data.geo_retry <- data.geo %>% filter(data.geo$lon %>% is.na())
data.geo_retry <- data.frame(city=data.geo_retry$city,geocode(data.geo_retry$city),stringsAsFactors = FALSE)

data.geo[data.geo$lon %>% is.na(),] <- data.geo_retry
data.geo %>% head()

df.euro_cities <- merge(df.euro_cities, data.geo, by='city', all = TRUE)

#==============
# GET WORLD MAP
#==============

map.europe <- map_data("world")

#=================================
# PLOT BASIC MAP
# - this map is "just the basics"
#=================================

ggplot() +
  geom_polygon(data = map.europe, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.euro_cities, aes(x = lon, y = lat, size = population), color = "red", alpha = .3) +
  coord_cartesian(xlim = c(-9,45), ylim = c(32,70))

#====================================================
# PLOT 'POLISHED' MAP
# - this version is formatted and cleaned up a little
#   just to make it look more aesthetically pleasing
#====================================================

#-------------
# CREATE THEME
#-------------
theme.maptheeme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 32)) +
  theme(plot.subtitle = element_text(size = 16)) +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 10)) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(panel.grid = element_blank())


#------
# PLOT
#------
#fill = "#AAAAAA",colour = "#818181", size = .15)
ggplot() +
  geom_polygon(data = map.europe, aes(x = long, y = lat, group = group), fill = "#DEDEDE",colour = "#818181", size = .15) +
  geom_point(data = df.euro_cities, aes(x = lon, y = lat, size = population), color = "red", alpha = .3) +
  geom_point(data = df.euro_cities, aes(x = lon, y = lat, size = population), color = "red", shape = 1) +
  coord_cartesian(xlim = c(-9,45), ylim = c(32,70)) +
  labs(title = "European Cities with Large Populations", subtitle = "Cities with over 1MM population, within city limits") +
  scale_size_continuous(range = c(.7,15), breaks = c(1100000, 4000000, 8000000, 12000000), name = "Population", labels = scales::comma_format()) +
  theme.maptheeme
