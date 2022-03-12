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
# Example (COUNTY NAME, STATE)
incarceration <- incarceration %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))


# What counties has the highest black jail population?
location_highest_black <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(location)
#There was a big list of locations rather than one so most likely a few 
#counties have the same black population. 


# What county has the highest black population
county_highest_black <- incarceration %>%
  filter(year == max(year)) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(location)
#New York County was the highest black population according to the data 


# Who has the highest ratio of black people jailed compared to black pop
highest_ratio_black <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(ratio = black_jail_pop / black_pop_15to64) %>%
  filter(ratio == max(ratio, na.rm = TRUE)) %>%
  pull(location)


# County with the highest black vs white differential jailed 
county_highest_black_white_ratio <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(black_white_ratio = black_jail_pop - white_jail_pop) %>%
  filter(black_white_ratio == max(black_white_diff, na.rm = TRUE)) %>%
  pull(location)

#Cook County, IL has the highest black to white difference in jail


# Find the top 5 locations with highest black jail population rate
top_5_locations <- black_data %>%
  filter(year == max(year)) %>%
  top_n(5, wt = black_jail_pop_rate) %>%
  arrange(-black_jail_pop_rate) %>%
  pull(location)

#The 5 locations were Dagget County, UT, King County, TX, Caldwell County, MO
#St.Clair County, Mo, and Throckmorton County, TX. 






# Trends Over Time Chart --------------------------------------------------

#The first variable is filtering the average black jail population by year. 
#The second variable is filtering by county, and showing all the jail populations 
#of different ethnic groups by year in Pierce County 
jail_pop <- incarceration %>%
  select(year, county_name, black_jail_pop) %>%
  group_by(year) %>%
  summarise(mean = mean(black_jail_pop, na.rm = TRUE))

jail_pop_2 <- incarceration %>%
  filter(county_name == "Pierce County") %>%
  select(
    year, county_name, black_jail_pop, aapi_jail_pop, white_jail_pop,
    other_race_jail_pop, native_jail_pop, latinx_jail_pop
  )



# Creating a Chart that puts the different ethinic jail populations together
jail_long <-
  pivot_longer(
    data = jail_pop_2,
    cols = c(
      "black_jail_pop", "aapi_jail_pop", "white_jail_pop",
      "other_race_jail_pop", "native_jail_pop", "latinx_jail_pop"
    ),
    names_to = "race",
    values_to = "value"
  )



# Creating Graph using gg plot
race_growth <- ggplot(jail_long) +
  geom_point(aes(x = year, y = value, group = race, color = race)) +
  labs(
    title = "Jail Population By Race in Pierce County",
    x = "year",
    y = "jail population"
  )


# Variable Comparison Chart -----------------------------------------------


# Filter data to show only Pierce County and data after the year 2000

pierce_county_black <- black_data %>%
  filter(county_name == "Pierce County") %>%
  filter(year > "2000")

# Creating the variable comparison chart, showing 
#black population on x axis and black jail population on y axis. 
compare_black_jail <- ggplot(pierce_county_black) +
  geom_point(aes(x = black_pop_15to64, y = black_jail_pop)) +
  labs(
    title = "2000-2018 Pierce County Jail Population Comparison",
    x = "Black Population",
    y = "Black Jail Population"
  )

#The data shocked me as, there were moments on the graph that 
# The population was steady but the growth of the black jail population 
#was rapidly growing! 


# Creating Map ------------------------------------------------------------

# Setting Data Up to filter to most recent year
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




# Set Up Blank Theme 
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


# Create a map focusing on the state of California
cali_map <- county_shapes %>%
  left_join(black_jail_pop_filter, by = "fips") %>%
  filter(state == "CA", county_name != "Unknown")

black_jail_pop_cali <- ggplot(cali_map) +
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