# Ryan Woodbury
# Tidy Tuesday
# February 23, 2021

library(tidyverse)
library(lubridate)
theme_set(theme_light())

## Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed
earn <- tuesdata$earn

## quick summary
### employed data
skimr::skim(employed)
glimpse(employed)


### earn data
skimr::skim(earn)
glimpse(earn)


## Explore some variables

### employed

employed_filter <- employed %>% 
  filter(race_gender != "TOTAL")


### earn

earn %>% count(sex) # this looks OK, but some cleaning of the "Both Sexes"
earn %>% count(race) # again, OK, but can filter out "All Races"
earn %>% count(ethnic_origin) # `All Origins` or `Hispanic or Latino`
earn %>% count(age) # Quite messy
hist(earn$n_persons)
hist(earn$median_weekly_earn)

View(earn_filter <- earn %>% 
  filter(sex == "Both Sexes",
         race %in% c("White", "Black or African American"),
         age == "25 to 54 years") %>% 
  mutate(race = recode(race, "Black or African American" = "Black")))


earn_filter_long <- earn_filter %>% 
  pivot_wider(id_cols = c(year, quarter),
              names_from = race,
              values_from = median_weekly_earn) %>% 
  mutate(earn_ratio = Black/White,
         year_quarter = year + quarter/4 - .125)

## From Hunter: mutate(year_quarter = year+quarter/4-.125)

ggplot(earn_filter_long, aes(x = year_quarter, y = earn_ratio)) +
  geom_line() +
  scale_y_continuous(name = "Earnings Ratio", limits = c(0,1), labels = function(y) scales::percent(y)) +
  labs(title = "Ratio of earnings of Black and White employees",
       caption = "By: Ryan Woodbury | Tidy Tuesday R4DS community | Data from BLS.gov")


earn_NoAll <- earn %>% 
  filter(sex != "Both Sexes", 
         race != "All Races",
         str_detect(age, "and over"),
         ethnic_origin == "All Origins") %>% 
  mutate(race = recode(race, "Black or African American" = "Black"),
         age = str_extract(age, "\\d+"),
         date = lubridate::yq(paste(year, quarter,sep="-")))

ggplot(earn_NoAll, aes(date, n_persons, color = race, shape = age)) +
  geom_point() +
  geom_path(aes(linetype = factor(sex)))


ggplot(filter(earn_NoAll, race == "All Races" & age == "16"), aes(date, n_persons, color = age, shape = sex)) +
  geom_point() +
  geom_path()
