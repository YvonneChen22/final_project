Data_merge
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data2015_df = read_csv("./data/2015.csv")|>
  janitor::clean_names()|>
  mutate(year=2015)|>
  select(- standard_error, -dystopia_residual)|>
  select(year, country, region, rank, score, everything() )
```

    ## Rows: 158 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): Country, Region
    ## dbl (10): Rank, Score, Standard Error, GDP per Capita, social support, Healt...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data2016_df = read_csv("./data/2016.csv")|>
  janitor::clean_names()|>
  mutate(year=2016)|>
  select(- lower_confidence_interval, - upper_confidence_interval, -dystopia_residual)|>
  select(year, country, region, rank, score, everything() )
```

    ## Rows: 157 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): Country, Region
    ## dbl (11): Rank, Score, Lower Confidence Interval, Upper Confidence Interval,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
region_df = data2015_df|>
  select(country, region)

data2017_df = read_csv("./data/2017.csv")|>
  janitor::clean_names()|>
  mutate(year=2017)|>
  select(- whisker_high, - whisker_low, -dystopia_residual)|>
  select(-generosity, everything(), generosity)
```

    ## Rows: 155 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): Country
    ## dbl (11): Rank, Score, Whisker.high, Whisker.low, GDP per Capita, Social sup...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data2017_df = left_join(data2017_df, region_df)|>
  select(year, country, region, everything())|>
  mutate(
    
  )
```

    ## Joining with `by = join_by(country)`

``` r
data2018_df = read_csv("./data/2018.csv")|>
  janitor::clean_names()|>
  mutate(
    year=2018)|>
  select(-generosity, everything(), generosity)
```

    ## Rows: 156 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Country, Perception of corruption
    ## dbl (7): rank, Score, GDP per capita, Social support, Health life expectancy...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data2018_df = left_join(data2018_df, region_df)|>
  select(year, country, region, everything())|>
  mutate(
    perception_of_corruption = as.numeric(perception_of_corruption)
  )
```

    ## Joining with `by = join_by(country)`

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `perception_of_corruption =
    ##   as.numeric(perception_of_corruption)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
data2019_df = read_csv("./data/2019.csv")|>
  janitor::clean_names()|>
  mutate(year=2019)|>
  select(-generosity, everything(), generosity)
```

    ## Rows: 156 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Country
    ## dbl (8): rank, Score, GDP per capita, Social support, Health life expectancy...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data2019_df = left_join(data2019_df, region_df)|>
  select(year, country, region, everything())
```

    ## Joining with `by = join_by(country)`

``` r
hap_df = bind_rows(data2015_df, data2016_df, data2017_df, data2018_df, data2019_df)|>
  mutate(
    region = if_else(country == "Taiwan Province of China", "Eastern Asia", region),
     region = if_else(country == "Hong Kong S.A.R., China", "Eastern Asia", region),
    region = if_else(country == "Belize", "Latin America and Caribbean", region),
    region = if_else(country == "Somalia", "Sub-Saharan Africa", region),
    region = if_else(country == "Namibia", "Sub-Saharan Africa", region),
    region = if_else(country == "South Sudan", "Sub-Saharan Africa", region),
    region = if_else(country == "Trinidad & Tobago", "Latin America and Caribbean", region),
    region = if_else(country == "Northern Cyprus", "Western Europe", region),
    region = if_else(country == "North Macedonia", "Central and Eastern Europe", region),
    region = if_else(country == "Gambia", "Sub-Saharan Africa", region),
    country = if_else(country == "Hong Kong S.A.R., China", "Hong Kong", country),
    country = if_else(country == "Northern Cyprus", "North Cyprus", country),
    country = if_else(country == "Somaliland region", "Somalia", country),
    country = if_else(country == "Taiwan Province of China", "Taiwan", country),
    country = if_else(country == "Trinidad & Tobago", "Trinidad and Tobago", country)
    )

write_csv(hap_df, "./data/combined_happiness.csv")
```
