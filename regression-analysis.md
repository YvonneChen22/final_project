Regression Analysis
================
Tingyi Li
2023-12-03

\#Part 1

\#Multiple Linear Regression for Happiness Score Prediction

``` r
happiness_score = read_csv("./Data/combined_happiness.csv")
```

    ## Rows: 782 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): country, region
    ## dbl (9): year, rank, score, gdp_per_capita, social_support, health_life_expe...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
predicted_model = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = happiness_score)

predicted_model|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 2.1750674 | 0.0799098 | 27.219021 | 0.0000000 |
| gdp_per_capita               | 1.1359972 | 0.0841455 | 13.500396 | 0.0000000 |
| social_support               | 0.6458962 | 0.0809226 |  7.981656 | 0.0000000 |
| health_life_expectancy       | 1.0127106 | 0.1320119 |  7.671359 | 0.0000000 |
| freedom_to_make_life_choices | 1.4812709 | 0.1634584 |  9.062068 | 0.0000000 |
| perception_of_corruption     | 0.8582286 | 0.2234402 |  3.840976 | 0.0001326 |
| generosity                   | 0.5924416 | 0.1757022 |  3.371851 | 0.0007837 |

\#Part 2

\#Multiple linear Regression for Happiness Score Prediction in 2015

``` r
data_2015 = happiness_score|>
  filter(year == 2015)

model2015 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2015)

model2015|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 1.8601849 | 0.1904824 | 9.7656541 | 0.0000000 |
| gdp_per_capita               | 0.8606572 | 0.2203113 | 3.9065501 | 0.0001409 |
| social_support               | 1.4088916 | 0.2226751 | 6.3271187 | 0.0000000 |
| health_life_expectancy       | 0.9753090 | 0.3162930 | 3.0835619 | 0.0024325 |
| freedom_to_make_life_choices | 1.3334329 | 0.3850157 | 3.4633211 | 0.0006944 |
| perception_of_corruption     | 0.7845381 | 0.4365328 | 1.7972032 | 0.0743018 |
| generosity                   | 0.3889329 | 0.3910029 | 0.9947057 | 0.3214707 |

\#Multiple linear Regression for Happiness Score Prediction in 2016

``` r
data_2016 = happiness_score|>
  filter(year == 2016)

model2016 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2016)

model2016|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error |  statistic |   p.value |
|:-----------------------------|----------:|----------:|-----------:|----------:|
| (Intercept)                  | 2.1902936 | 0.1583237 | 13.8342722 | 0.0000000 |
| gdp_per_capita               | 0.7214128 | 0.2171212 |  3.3226274 | 0.0011204 |
| social_support               | 1.2297543 | 0.2297496 |  5.3525847 | 0.0000003 |
| health_life_expectancy       | 1.4364028 | 0.3489120 |  4.1168054 | 0.0000632 |
| freedom_to_make_life_choices | 1.5139349 | 0.3879659 |  3.9022369 | 0.0001435 |
| perception_of_corruption     | 0.9189270 | 0.4647615 |  1.9772014 | 0.0498518 |
| generosity                   | 0.1594941 | 0.3621060 |  0.4404626 | 0.6602362 |

\#Multiple linear Regression for Happiness Score Prediction in 2017

``` r
data_2017 = happiness_score|>
  filter(year == 2017)

model2017 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2017)

model2017|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 1.7430289 | 0.1873581 |  9.303195 | 0.0000000 |
| gdp_per_capita               | 0.7844334 | 0.2045131 |  3.835615 | 0.0001849 |
| social_support               | 1.1177711 | 0.2020608 |  5.531856 | 0.0000001 |
| health_life_expectancy       | 1.2888803 | 0.3215255 |  4.008640 | 0.0000965 |
| freedom_to_make_life_choices | 1.4757152 | 0.3425093 |  4.308541 | 0.0000298 |
| perception_of_corruption     | 0.8266072 | 0.4843307 |  1.706700 | 0.0899751 |
| generosity                   | 0.3807181 | 0.3293271 |  1.156049 | 0.2495240 |

\#Multiple linear Regression for Happiness Score Prediction in 2018

``` r
data_2018 = happiness_score|>
  filter(year == 2018)

model2018 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2018)

model2018|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 1.8234561 | 0.1977108 |  9.222845 | 0.0000000 |
| gdp_per_capita               | 0.9017459 | 0.2423630 |  3.720642 | 0.0002816 |
| social_support               | 1.1150219 | 0.2117195 |  5.266505 | 0.0000005 |
| health_life_expectancy       | 0.9671218 | 0.3425058 |  2.823666 | 0.0054021 |
| freedom_to_make_life_choices | 1.3984419 | 0.3185447 |  4.390096 | 0.0000214 |
| perception_of_corruption     | 0.7277888 | 0.5277904 |  1.378935 | 0.1699951 |
| generosity                   | 0.5235668 | 0.4717544 |  1.109829 | 0.2688726 |

\#Multiple linear Regression for Happiness Score Prediction in 2019

``` r
data_2019 = happiness_score|>
  filter(year == 2019)

model2019 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2018)

model2019|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 1.8234561 | 0.1977108 |  9.222845 | 0.0000000 |
| gdp_per_capita               | 0.9017459 | 0.2423630 |  3.720642 | 0.0002816 |
| social_support               | 1.1150219 | 0.2117195 |  5.266505 | 0.0000005 |
| health_life_expectancy       | 0.9671218 | 0.3425058 |  2.823666 | 0.0054021 |
| freedom_to_make_life_choices | 1.3984419 | 0.3185447 |  4.390096 | 0.0000214 |
| perception_of_corruption     | 0.7277888 | 0.5277904 |  1.378935 | 0.1699951 |
| generosity                   | 0.5235668 | 0.4717544 |  1.109829 | 0.2688726 |

\#Part 3
