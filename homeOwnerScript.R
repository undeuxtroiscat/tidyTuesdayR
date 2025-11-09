# load libs
library(tidytuesdayR)
library(tidyverse)

# load homeowner.csv dataset
tt <- tt_load("2021-02-09")
data <- tt$home_owner
# preview
glimpse(data)

# Group year into decades, group by decade and race, arrange by decade and percent ownership
data_decade <- data |> 
  mutate(decade = floor(year/10) * 10) |> 
  group_by(decade, race) |> 
  summarise(
    avg_home_owner_pct = mean(home_owner_pct, na.rm = TRUE), 
    .groups = "drop"
  ) |> 
  arrange(decade, avg_home_owner_pct) 

data_decade # view transformed dataset

# plot pct ownership trend over time
data_p <- data_decade |> 
  ggplot(aes(x = decade, y = avg_home_owner_pct, color = race, group = race)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Homeownership percentage for families by race/ethnicity over time",
    x = NULL,
    y = "Home ownership (%)",
    color = "Race/ethncity"
  ) +
  theme_bw()
data_p # view plot

ggsave("homeownership_by_race.png", plot = data_p)
