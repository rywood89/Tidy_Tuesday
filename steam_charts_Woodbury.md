---
title: "Steam Chart (Tidy Tuesday - March 16, 2021)"
author: "Ryan Woodbury"
date: "3/16/2021"
output: 
  html_document:
    code_folding: hide
    keep_md: true
    latex_engine: xelatex
---




```r
library(tidyverse) # data manipulation
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(tidytuesdayR) # Tidy Tuesday data
#library(tidymodels) # Modeling!
#library(stacks) # Ensemble methods
#library(gt) # html tables
library(lubridate) # Date time functions
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
#library(plotly) # interactive plots
library(ggiraph) # interactive plots
library(ggfx) # lightsaber effect?
library(ggtext)

#library(ggthemes) # doesn't work with plotly

#library(ggimage)
#library(extrafont)
#library(scales)

theme_set(theme_classic())
```


```r
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   gamename = col_character(),
##   year = col_double(),
##   month = col_character(),
##   avg = col_double(),
##   gain = col_double(),
##   peak = col_double(),
##   avg_peak_perc = col_character()
## )
```


```r
glimpse(games)
```

Quick exploration of Steam Charts data.

The data is long-form, aggregated by (month and year). For every (`month`, `year`) every game (`gamename`) has some player number stats: 
- `avg`: average number of players playing game X at the same time during the month
- `gain`: gain (or loss) difference in average compared to previous month (NA = first month)
- `peak`: highest number of players playing game X at the same time during (`month`, `year`)
- `avg_peak_perc`: share of average in the maximum value (`avg`/`peak`) in %

The games are ONLY Steam games, not Epic games or some other non-Steam games. Indeed, Steam is only played via computers (typically PCs). Many games can be played across several platforms, but these data are only Steam-specific. For example, Rocket League is cross-platform compatible, but only users running it through Steam are a part of this aggregated data set.


```r
games %>% 
  filter(gamename == "Dota 2") %>% 
  count()

table(games$year)
```

The data looks pretty clean, just need to re-format a few variables into numeric/date. I do think the character months will convert to dates using {lubridate} though.


```r
skimr::skim(games)
```


```r
games %>% distinct(gamename) %>% View()

## Let's look at some Star Wars games... just because I like Star Wars.
```


## Star Wars game

For old times sake, let's just explore the Star Wars games. I might eventually add in some comparison to the larger data set, but I'm just going to focus on Star Wars games.


```r
sw_games <- games %>% 
  filter(str_detect(str_to_lower(gamename), "star wars"))

#glimpse(sw_games)
```

This is a much smaller data set, but still fun to explore.

There is something going on within `gamename`, some unicode values. There is probably a function to remove unicode. "\\u0099" is "trademark" 


```r
sw_games_clean <- sw_games %>% 
  mutate(month_year = ym(paste(year, month, sep = ", ")),
         avg_peak_perc = str_remove(avg_peak_perc, "%"),
         avg_peak_perc = as.numeric(avg_peak_perc),
         gamename = str_remove_all(gamename, "\\u0099"),
         tooltip = paste0("Game: ", gamename, "<br>Average player count: ", avg, "<br>Date: ", month(month_year, label = T), ", ", year(month_year)))
```

Let's take a look at month averages of the games across time


```r
sw_plot <- ggplot(sw_games_clean) +
  ggfx::with_outer_glow(
  geom_path_interactive(aes(month_year, 
                            avg, 
                            group = gamename, 
                            color = gamename, tooltip = gamename), size = 1.25, show.legend = F, alpha = 0.5), 
  colour = "lightblue", expand = 8, sigma = 5) +
  geom_point_interactive(aes(month_year, 
                            avg, 
                            group = gamename, 
                            color = gamename,
                            tooltip = tooltip, data_id = tooltip), show.legend = F) +
  labs(title = "Star Wars games",
       subtitle = "Monthly average player count",
       x = "Date",
       y = "Number of players") +
  ggdark::dark_theme_classic() +
  geom_point(data = tibble(x_year = ym(paste(sample(2012:2021, replace = T, size = 25), sample(1:12, replace = T, size = 25), sep = ", ")),
                           y_point = sample(5000:17500, replace = F, size = 25)), aes(x_year, y_point), size = .5, color = "yellow", shape = 8) +
  scale_color_brewer_interactive(palette = "Spectral") +
  theme(
    text = element_text(colour = "#ffe81f", family = "Impact")
  )
```

```
## Inverted geom defaults of fill and color/colour.
## To change them back, use invert_geom_defaults().
```

```r
#if( interactive() ) print(ggiraph(ggobj = sw_plot))

# plotly::ggplotly(sw_plot, tooltip = "text") %>% 
#   plotly::layout(showlegend = FALSE)

#print(ggiraph(ggobj = sw_plot))

ggiraph(ggobj = sw_plot)
```

```{=html}
<div id="htmlwidget-40920f136dfe92cadd35" style="width:672px;height:480px;" class="girafe html-widget"></div>
```
