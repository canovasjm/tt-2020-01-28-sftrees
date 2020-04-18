# 01 required libraries ---------------------------------------------------
library(tidyverse)
library(ggridges)
library(here)


# 02 get data -------------------------------------------------------------
# get the data from the internet
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# save the data locally
# write_csv(sf_trees, path = here("sf_trees.csv"))

# read the data locally
sf_trees <- readr::read_csv(file = here("sf_trees.csv"))


# 03 data cleaning --------------------------------------------------------
# remove NA in date
sf_trees <- sf_trees %>% 
  drop_na("date")

# most common species
top_species <- sf_trees %>% 
  count(species, 
        sort = TRUE, 
        name = "popular_trees") %>%
  head(11)

top_species

# remove the first since there is not species' name
top_species <- top_species[-1, ]


# top 10 species count
top_species %>% 
  select(popular_trees) %>% 
  sum()

# names of 10 most popular species
top_species_name <- top_species[ , 1] %>% 
  unlist() %>% 
  as.character()

# data frame to work with
df <- sf_trees %>%  
  filter(species %in% top_species_name)
head(df)[ , 1:5]

# get only common names
sub(".*:: ", "", top_species_name)

# keep only common name for species
df$species <- sub(".*:: ", "", df$species)
head(df)[ , 1:5]

# 04 plots ----------------------------------------------------------------
# make plot
p1 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(scale = 1, rel_min_height = 0.009)
p1

# make plot
p2 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(scale = 1, rel_min_height = 0.1)
p2


# make plot
p3 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(rel_min_height = 0.009)
p3


# jittered_points = TRUE



# make plot
p4 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_point()
p4



# 05 testing code ---------------------------------------------------------
# testing plotly
p5 <- ggplot(df, aes(date)) + 
   geom_histogram(bins = 15)
p5
ggplotly(p5)

