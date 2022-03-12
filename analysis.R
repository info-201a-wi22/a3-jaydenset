library(readr)
library(dplyr)
library(tidyverse)
incarceration <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")



# Splitting Data by race
white_data <- incarceration %>%
  select(
    year, fips, state, county_name, white_pop_15to64, white_jail_pop,
    white_prison_pop, white_jail_pop_rate, white_prison_pop_rate
  ) %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))

black_data <- incarceration %>%
  select(year, fips, state, county_name, contains("black")) %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))

# DPLYR Exploration -------------------------------------------------------


# Creating a new column that puts county name and state together
# Example (COUNTRY NAME, STATE)
incarceration <- incarceration %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))


# According to the most recent date finding the countries
# What counties has the highest black jail population?
location_highest_blk_jail_pop <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(location)
#There was a big list of locations rather than one so most likely a few 
#counties have the same black population. 

# According to the most recent date
# What county has the highest black population

county_highest_blk_pop <- incarceration %>%
  filter(year == max(year)) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(location)
#New York County was the highest black population according to the data 

# What county has the highest ratio of black people jailed compared to all black
# people in their county?
highest_ratio_blk_jailed <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(ratio = black_jail_pop / black_pop_15to64) %>%
  filter(ratio == max(ratio, na.rm = TRUE)) %>%
  pull(location)

# In the most recent year, finding the county that had the highest difference 
#between black people jailed and white people in jailed. 

county_highest_blk_white_diff <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(blk_white_diff = black_jail_pop - white_jail_pop) %>%
  filter(blk_white_diff == max(blk_white_diff, na.rm = TRUE)) %>%
  pull(location)

#Cook County, IL has the highest black to white difference in the jail


# Find the top 5 locations with highest black jail population rate
# This will list the 5 locations in arranged order with the highest 
# black jail population rate 
top_5_locations <- black_data %>%
  filter(year == max(year)) %>%
  top_n(5, wt = black_jail_pop_rate) %>%
  arrange(-black_jail_pop_rate) %>%
  pull(location)

#The 5 locations were Dagget County, UT, King County, TX, Caldwell County, MO
#St.Clair County, Mo, and Throckmorton County, TX. 






# Trends Over Time Chart --------------------------------------------------

#Filtering the data. The first variable is filtering the average black jail 
#population by year. While the second variable is filtering by a county and 
#showing all the jail populations of different ethnic groups by year in New York 
#county
jail_pop_data <- incarceration %>%
  select(year, county_name, black_jail_pop) %>%
  group_by(year) %>%
  summarise(mean = mean(black_jail_pop, na.rm = TRUE))

jail_pop_data_2 <- incarceration %>%
  filter(county_name == "New York County") %>%
  select(
    year, county_name, black_jail_pop, aapi_jail_pop, white_jail_pop,
    other_race_jail_pop, native_jail_pop, latinx_jail_pop
  )



# Creating a Chart that puts the different ethinic jail populations 
# together so that It can be translated to the graph showing 
# different ethnicities as different graph lines. 

jail_long <-
  pivot_longer(
    data = jail_pop_data_2,
    cols = c(
      "black_jail_pop", "aapi_jail_pop", "white_jail_pop",
      "other_race_jail_pop", "native_jail_pop", "latinx_jail_pop"
    ),
    names_to = "race",
    values_to = "value"
  )



# Creating Graph using gg plot, and customizing the graph to be easily readable by
# the user
race_growth_yearly <- ggplot(jail_long) +
  geom_point(aes(x = year, y = value, group = race, color = race)) +
  labs(
    title = "Jail Population By Race in New York County",
    x = "year",
    y = "jail population"
  )

#In New York County the black population has always been the top population over 
#over the years provided by the data




# Variable Comparison Chart -----------------------------------------------


# Filter data to show only New York County and any data after the year 2000

new_york_county_blk_data <- black_data %>%
  filter(county_name == "New York County") %>%
  filter(year > "2000")

#The actual creation of the variable comparison chart, showing 
#black population on x axis and black jail population on y axis. 
# this graph will allow us to see if there is a correlation between a rise in
# black Jail population in NY county and the rise in black jail population in 
#NY Couny
compare_black_jail_pop <- ggplot(new_york_county_blk_data) +
  geom_point(aes(x = black_pop_15to64, y = black_jail_pop)) +
  labs(
    title = "2000-2018 NY County Jail Population Comparison",
    x = "Black Population",
    y = "Black Jail Population"
  )

#The data shocked me as, there were moments on the graph that 
# The population was steady but the growth of the black jail population 
#was rapidly growing! 


# Creating Map ------------------------------------------------------------

# Setting Data Up to filter to most recent year, in order to avoid a weird 
#looking graph
black_jail_pop_filter <- black_data %>%
  filter(year == max(year))

# Use map data function to join black_data "counties" dataset with the map data
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")


# Merge maps data and black_jail_pop_filter
map_data <- county_shapes %>%
  left_join(black_jail_pop_filter, by = "fips") %>%
  filter(county_name != "Unknown")





# Set Up Blank Theme so that there aren't any weird looking lines on graph
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Create Map a general map of black jail pop rate in all of America:
black_jail_pop_rate_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "gray", size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop_rate)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme




#I want to create a more specific looking map in order to 
# Read the black jail population rate better and be able to tell details
# The general map was too vague 
# Create a map focusing on the state of California as it has a long history 
# of an unjust police system:
cali_map_data <- county_shapes %>%
  left_join(black_jail_pop_filter, by = "fips") %>%
  filter(state == "CA", county_name != "Unknown")

black_jail_pop_rate_map_cali <- ggplot(cali_map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "gray", size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop_rate)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme +
  ggtitle("Black Jail Population Rate in California")


#Map found that there is a growing black jail population rate in North 
#California which shocked me. 