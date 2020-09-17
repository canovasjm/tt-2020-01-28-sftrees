# 01 required libraries ---------------------------------------------------
library(plotly)
library(tidyverse)
library(ggridges)
library(leaflet)

# 02 get data -------------------------------------------------------------
# get the data from the internet
# sf_trees <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# save the data locally
# write_csv(sf_trees, path = "data/sf_trees.csv")

# read the data locally
sf_trees <- read_csv(file = "data/sf_trees.csv")


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

# print top species
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

# print df head
head(df)[ , 1:5]

# get only common names
sub(".*:: ", "", top_species_name)

# keep only common name for species
df$species <- sub(".*:: ", "", df$species)

# print df head
head(df)[ , 1:5]

# 04 plots ----------------------------------------------------------------
# plot 1: nice
p1 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(scale = 1, rel_min_height = 0.009) +
  labs(
    title = "Ridgeline plots",
    subtitle = "10 most popular trees in San Francisco",
    caption = "Source: data.sfgov.org",
    x = "Date",
    y = "Common name"
  )

# print plot
p1

# save plot
ggsave(filename = "ridgelines-p1.png", path = "plots")



# plot 2: change rel_min_height
p2 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(scale = 1, rel_min_height = 0.1) +
  labs(
    title = "Ridgeline plots",
    subtitle = "10 most popular trees in San Francisco",
    caption = "Source: data.sfgov.org",
    x = "Date",
    y = "Common name"
  )

# print plot
p2



# plot 3: some overlapping, no scale = 1
p3 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_density_ridges(rel_min_height = 0.009) +
  labs(
    title = "Ridgeline plots",
    subtitle = "10 most popular trees in San Francisco",
    caption = "Source: data.sfgov.org",
    x = "Date",
    y = "Common name"
  )

# print plot
p3



# plot 4: only points with some noise
p4 <- ggplot(df, aes(x = date, y = fct_rev(species))) + 
  geom_point() +
  geom_jitter() +
  labs(
    title = "Points",
    subtitle = "10 most popular trees in San Francisco",
    caption = "Source: data.sfgov.org",
    x = "Date",
    y = "Common name"
  )

# print plot
p4





# 05 testing plotly -------------------------------------------------------
# histogram with plotly
p5 <- ggplot(df, aes(date)) + 
  geom_histogram(bins = 15)

# print histogram
p5

# create ggplotly
ggplotly(p5)



# 06 map ------------------------------------------------------------------
# data from a San Francisco map
sf_map <- map_data(map = "county", region = "california")
sf_map <- sf_map %>% 
  filter(subregion == "san francisco")


# first attemp to plot a map
ggplot(sf_map, aes(long, lat)) +
  geom_polygon(fill = "white", colour = "black") +
  geom_point(
    data = df, aes(y = latitude, x = longitude, color = species),
    alpha = 1/50, 
    size = 1
  )


# second attemp to plot a map
ggplot(data = df) +
  geom_point(
    aes(y = latitude, x = longitude, color = species),
    alpha = 1/50, 
    size = 1) +
  coord_quickmap()



# 07 testing leafleat -----------------------------------------------------
# use of a pop up
df[1:1000, ] %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(
    radius = 2, 
    color = "darkgreen",
    popup = ~ paste0(address, " - ", species)
  )


# use of a pop up with html enhancing
df[1:1000, ] %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(
    radius = 2, 
    color = "red",
    popup = ~ paste0("<b>", species, "</b>", 
                     "<br/>", 
                     address)
  )


# use of a label
df[1:1000, ] %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(
    radius = 2, 
    color = "red",
    label = ~ paste0(species, " (", address, ")")
  )


# colorFactor() and addLegend()
df$species <- as.factor(df$species)
pal <- colorFactor(palette = "RdYlBu", levels = unique(df$species))

# use of a legend and color factor variable 
df[1:1000, ] %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(
    radius = 2,
                   color = ~ pal(species),
                   label = ~ paste0(species, " - (", address, ")")
    ) %>% 
  addLegend(
    pal = pal, 
    values = unique(df$species), 
    opacity = 0.75, 
    title = "Specie", 
    position = "topleft"
    )
