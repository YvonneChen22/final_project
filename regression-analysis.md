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

The variable “freedom to make life choices” exhibits the largest
coefficient (1.4812709) with a highly significant p-value(\<0.0000001),
indicating it exerts the most substantial effect on happiness scores.
Following this, “GDP per capita”, “Health life expectancy”, and “Social
support” are also significant contributors to happiness in order of
their estimates. “Perception of corruption” and “Generosity” also
significantly impact happiness but to a lesser extent, based on their
estimates and p-values.

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

In 2015, the variable “social support” exhibited the largest coefficient
(1.4088916) with the lowest p-value(\<0.0000001) among all variables.
This indicated that it performed the most substantial effect on
happiness score in 2015. Following “social support”, “freedom to make
life choices”, “health life expectancy” and “gdp per capita” also
appeared to have relatively high estimates with significant p-values ,
indicating that there were sufficient evidences to show their essential
effects on happiness score. However, “perception of corruption” and
“generosity” appeared to have p-values larger than 0.05 indicating that
they didn’t have significant impact on happiness score.

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

In 2016, the variable “freedom to make life choices” exhibited the
largest coefficient (1.5139349) with a significant p-value(0.0001435),
while the variable “health life expectancy” and “social support”
exhibited the second and the third largest coefficient and more
significant p-values(\<0.0001) in comparison to variable “freedom to
make life choices”. This indicated that they all exhibited a very
outstanding impact on happiness score in 2016. Following them,
“perception of corruption” and “gdp per capita” also appeared to have
relatively high estimates with p-values smaller than 0.05, indicating
that there were sufficient evidences to show their effects on happiness
score. However, variable “generosity” appeared to have p-values
0.6602362, which is larger than 0.05, indicating that it didn’t exhibit
significant impact to 2016’s happiness score.

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

In 2017, the variable “freedom to make life choices” and “health life
expectancy” exhibited the largest 2 coefficient (1.4757152) and
(1.2888803) with significant p-values (0.0000298) and (0.0000965)
respectively, while the variable “social support” exhibited the third
largest coefficient and more significant p-values(\<0.000001) in
comparison to variable “freedom to make life choices” and “health life
expectancy”. This indicated that they all exhibited a very outstanding
impacts on happiness score in 2017. Following them, “gdp per capita”
also appeared to have relatively high estimates (0.7844334) with
p-values (0.0001849), indicating that there were sufficient evidences to
show a significant impact on happiness score. However, variable
“perception of corruption” and “generosity” appeared to have p-values
larger than 0.05, indicating that they didn’t exhibit significant impact
to 2017’s happiness score.

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

In 2018, the variable “freedom to make life choices” and “social
support” exhibited the largest 2 coefficient (1.3984419) and (1.1150219)
with significant p-values (0.0000214) and (0.0000005) respectively,
indicating that they play very substantial job on impacting happiness
score. Following them, the variables “health life expectancy” and “gdp
per capita” exhibited relatively large coefficients with significant
p-values(\<0.01), indicating that they also exhibited very outstanding
impacts on happiness score in 2018. However, variable “perception of
corruption” and “generosity” appeared to have p-values larger than 0.05,
indicating that they didn’t exhibit significant impact to 2018’s
happiness score.

\#Multiple linear Regression for Happiness Score Prediction in 2019

``` r
data_2019 = happiness_score|>
  filter(year == 2019)

model2019 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = data_2019)

model2019|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 1.7952202 | 0.2110734 | 8.5051942 | 0.0000000 |
| gdp_per_capita               | 0.7753716 | 0.2182254 | 3.5530776 | 0.0005103 |
| social_support               | 1.1241916 | 0.2369001 | 4.7454252 | 0.0000048 |
| health_life_expectancy       | 1.0781427 | 0.3345385 | 3.2227764 | 0.0015596 |
| freedom_to_make_life_choices | 1.4548324 | 0.3753378 | 3.8760610 | 0.0001587 |
| perception_of_corruption     | 0.9722802 | 0.5423607 | 1.7926818 | 0.0750526 |
| generosity                   | 0.4897834 | 0.4977455 | 0.9840036 | 0.3267089 |

In 2019, the variable “freedom to make life choices” and “social
support” exhibited the largest 2 coefficient (1.4548324) and (1.1241916)
with significant p-values (0.0001587) and (0.0000048) respectively,
indicating that they play very substantial job on impacting happiness
score. Following them, the variables “health life expectancy” and “gdp
per capita” exhibited relatively large coefficients with significant
p-values(\<0.01), indicating that they also exhibited very outstanding
impacts on happiness score in 2019. However, variable “perception of
corruption” and “generosity” appeared to have p-values larger than 0.05,
indicating that they didn’t exhibit significant impact to 2019’s
happiness score.

According to the linear regressions performed separately for each year
from 2015 to 2019, the main factor that impacted the happiness score
changed over time. In 2015, “social support” had the most substantial
impact on the happiness score, and its impact kept being significant
until 2019. In the following 4 years, “freedom to make life choices”
appeared to make the greatest impact on the happiness score. “health
life expectancy” also impacted significantly on the happiness score
since it exhibited the second or the third largest coefficient in the
linear regression model from 2016 to 2019.

\#Part 3

\#Multiple linear Regression for Happiness Score Prediction in Central
and Eastern Europe

``` r
central_east = happiness_score|>
  filter(region == "Central and Eastern Europe")

model1 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = central_east)

model1|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  3.1506152 | 0.3761585 |  8.3757648 | 0.0000000 |
| gdp_per_capita               |  1.0993611 | 0.2033891 |  5.4052129 | 0.0000003 |
| social_support               |  0.5447398 | 0.1437145 |  3.7904299 | 0.0002240 |
| health_life_expectancy       | -0.1987347 | 0.4097214 | -0.4850485 | 0.6284110 |
| freedom_to_make_life_choices |  1.7050905 | 0.3442244 |  4.9534273 | 0.0000021 |
| perception_of_corruption     | -0.5976465 | 0.5501586 | -1.0863167 | 0.2792328 |
| generosity                   |  0.6492740 | 0.5101488 |  1.2727149 | 0.2052593 |

From 2015 to 2019, in Central and Eastern European countries, variables
“freedom to make life choices” exhibited the largest
coefficient(1.7050905) with significant p-value(0.0000021) indicating
that it impacted substantially on happiness score. Meanwhile, variables
“gdp per capita” and “social support” exhibited relatively high
coefficients (1.0993611) and (0.5447398) respectively with significant
p-value (\<0.001) showing their essential impact on happiness score.
Variables “health life expectancy”, “perception of corruption” and
“generosity” exhibited smaller and even negative coefficients; however,
these variables did not exhibit significant p-values(\>0.05) indicating
that it was not significant enough to tell that they had impact on
happiness score.

\#Multiple linear Regression for Happiness Score Prediction in Eastern
Asia

``` r
east_asia = happiness_score|>
  filter(region == "Eastern Asia")

model2 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = east_asia)

model2|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  0.6698077 | 0.9885689 |  0.6775529 | 0.5048144 |
| gdp_per_capita               |  2.6321054 | 0.3987773 |  6.6004398 | 0.0000010 |
| social_support               |  0.1680339 | 0.2050669 |  0.8194102 | 0.4209627 |
| health_life_expectancy       |  1.5122195 | 0.7333569 |  2.0620513 | 0.0506769 |
| freedom_to_make_life_choices |  1.8238357 | 0.9140536 |  1.9953269 | 0.0579853 |
| perception_of_corruption     | -6.1001405 | 1.5633140 | -3.9020571 | 0.0007172 |
| generosity                   |  0.8764310 | 0.8352973 |  1.0492444 | 0.3049665 |

From 2015 to 2019, in Eastern Asian countries, variable “perception of
corruption” exhibited extremely high coefficient (-6.1001405) with a
significant p-value(0.0007172) indicating that “perception of
corruption” had extremely substantial negative impact on happiness
score. Variable “gdp per capita” exhibited the second highest
coefficient (2.6321054) with a significant p-value (0.0000010)
indicating that it had essential impact on happiness score. However,
other variables exhibited p-values larger than 0.05 showing that it was
not significant enough to tell that they had impacts on happiness score.

\#Multiple linear Regression for Happiness Score Prediction in Latin
America and Caribbean

``` r
latin_caribbean = happiness_score|>
  filter(region == "Latin America and Caribbean")

model3 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = latin_caribbean)

model3|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  2.9870953 | 0.3603714 |  8.2889347 | 0.0000000 |
| gdp_per_capita               |  1.4869935 | 0.2138900 |  6.9521417 | 0.0000000 |
| social_support               | -0.6315397 | 0.2278693 | -2.7714998 | 0.0066120 |
| health_life_expectancy       |  1.7455082 | 0.3819569 |  4.5699082 | 0.0000135 |
| freedom_to_make_life_choices |  2.4568168 | 0.3534921 |  6.9501327 | 0.0000000 |
| perception_of_corruption     |  1.3409006 | 0.9170861 |  1.4621317 | 0.1467204 |
| generosity                   | -0.3165373 | 0.5087516 | -0.6221843 | 0.5351824 |

From 2015 to 2019, in Latin American and Caribbean countries, variable
“freedom to make life choices” had the most significant impact on
happiiness score since it exhibited the highest coefficient(2.4568168)
with extremely significant p-value (\<0.000001). Following this,
variables “health life expectancy” and “gdp per capita” exhibited
relatively high coefficients (1.7455082) and (1.4869935) respectively
with significant p-values indicating their essential impact on happiness
score. Variable “social support” exhibited a negative coefficient
“-0.6315397” with a significant p-value (0.0066120) indicating that
“social support” had negative impact on happiness score. However,
variables “perception of corruption” and “generosity” appear to have
p-values(\>0.05) indicating that it was not significant enough to tell
that they had impacts on happiness score.

\#Multiple linear Regression for Happiness Score Prediction in Middle
East and Northern Africa

``` r
east_africa = happiness_score|>
  filter(region == "Middle East and Northern Africa")

model4 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = east_africa)

model4|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  1.5472699 | 0.2567575 |  6.0261911 | 0.0000000 |
| gdp_per_capita               |  1.5506958 | 0.2511559 |  6.1742356 | 0.0000000 |
| social_support               |  0.4364518 | 0.2065005 |  2.1135633 | 0.0373830 |
| health_life_expectancy       |  2.0670148 | 0.4620345 |  4.4737244 | 0.0000229 |
| freedom_to_make_life_choices |  1.3532650 | 0.4613435 |  2.9333131 | 0.0042752 |
| perception_of_corruption     |  0.3720748 | 0.6673868 |  0.5575099 | 0.5785945 |
| generosity                   | -1.4347645 | 0.4915314 | -2.9189679 | 0.0044584 |

From 2015 to 2019, in Middle Eastern and Northern African countries,
variables “health life expectancy” exhibited the largest coefficient
(2.0670148) with a significant p-value(0.0000229), while the variable
“gdp per capita” exhibited the second largest coefficient and more
significant p-values(\<0.0001) in comparison to variable “health life
expectancy”. The variables “generosity”,“freedom to make life choices”
and “social support” also exhibited relatively large coefficient with
significant p-value. This indicated that they all exhibited a very
outstanding impact on happiness score. However, variable “perception of
corruption” appeared to have p-values 0.5785945, which is larger than
0.05, indicating that it didn’t exhibit significant impact to the
happiness score.

\#Multiple linear Regression for Happiness Score Prediction in North
America

``` r
north_america = happiness_score|>
  filter(region == "North America")

model5 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = north_america)

model5|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  5.7461328 | 1.1568751 |  4.9669432 | 0.0156747 |
| gdp_per_capita               |  0.2906872 | 0.4853376 |  0.5989381 | 0.5914238 |
| social_support               | -0.2790530 | 0.1790157 | -1.5588190 | 0.2169302 |
| health_life_expectancy       |  0.4041180 | 0.4999375 |  0.8083371 | 0.4780512 |
| freedom_to_make_life_choices |  0.9868637 | 0.8267570 |  1.1936563 | 0.3183984 |
| perception_of_corruption     |  1.4273085 | 0.5532336 |  2.5799383 | 0.0817842 |
| generosity                   |  0.4721823 | 0.4867702 |  0.9700313 | 0.4035808 |

From 2015 to 2019, in North American countries, none of the 6 variables
exhibited significant p-value since all their p-values were greater than
0.05 indicating that none of the variables exhibit significant impact to
the happiness score.

\#Multiple linear Regression for Happiness Score Prediction in
Southeastern Asia

``` r
southeastern_asia = happiness_score|>
  filter(region == "Southeastern Asia")

model6 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = southeastern_asia)

model6|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  3.5557237 | 0.7682308 |  4.6284577 | 0.0000442 |
| gdp_per_capita               |  1.6457687 | 0.3325444 |  4.9490193 | 0.0000165 |
| social_support               |  0.3338875 | 0.3197225 |  1.0443039 | 0.3031200 |
| health_life_expectancy       |  0.7927899 | 0.5905490 |  1.3424625 | 0.1876252 |
| freedom_to_make_life_choices | -0.9335115 | 1.0718734 | -0.8709158 | 0.3894160 |
| perception_of_corruption     | -0.6520931 | 0.6148682 | -1.0605413 | 0.2957725 |
| generosity                   |  0.0954657 | 0.4326026 |  0.2206775 | 0.8265568 |

From 2015 to 2019, in Southeastern Asian countries, only variable “gdp
per capita” exhibited relatively high coefficient (1.6457687) with
significant p-value (0.0000165) indicating that “gdp per capita” had
substantial impact on happiness scrore. Despite “gdp per capita”, none
of other variables exhibited significant p-values indicating that we did
not have sufficient evidence to show their impact on happiness score.

\#Multiple linear Regression for Happiness Score Prediction in Southern
Asia

``` r
southern_asia = happiness_score|>
  filter(region == "Southern Asia")

model7 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = southern_asia)

model7|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  2.9020228 | 0.3943552 |  7.3589065 | 0.0000001 |
| gdp_per_capita               |  0.1307995 | 0.5138800 |  0.2545332 | 0.8009437 |
| social_support               |  0.3855854 | 0.3180950 |  1.2121705 | 0.2355768 |
| health_life_expectancy       |  1.5212822 | 0.8819818 |  1.7248452 | 0.0955793 |
| freedom_to_make_life_choices | -1.3276433 | 0.8445746 | -1.5719668 | 0.1271914 |
| perception_of_corruption     |  8.8312157 | 2.1766502 |  4.0572507 | 0.0003603 |
| generosity                   |  0.4649207 | 0.9043858 |  0.5140734 | 0.6112343 |

From 2015 to 2019, in Southern Asian countries, only one variable,
“perception of corruption”, exhibited significant p-value (0.0003603)
with extremely high coefficient (8.8312157) indicating that among all 6
variables, “perception of corruption” had the largest impact on
happiness score.

\#Multiple linear Regression for Happiness Score Prediction in
Sub-Saharan Africa

``` r
sub_africa = happiness_score|>
  filter(region == "Sub-Saharan Africa")

model8 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = sub_africa)

model8|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |   estimate | std.error |  statistic |   p.value |
|:-----------------------------|-----------:|----------:|-----------:|----------:|
| (Intercept)                  |  2.6862788 | 0.1737404 | 15.4614553 | 0.0000000 |
| gdp_per_capita               |  0.6020415 | 0.1539281 |  3.9111867 | 0.0001280 |
| social_support               |  0.6583385 | 0.1332112 |  4.9420649 | 0.0000017 |
| health_life_expectancy       | -0.0725829 | 0.2444136 | -0.2969676 | 0.7668173 |
| freedom_to_make_life_choices |  0.9605341 | 0.2729970 |  3.5184786 | 0.0005437 |
| perception_of_corruption     | -0.6583010 | 0.4600525 | -1.4309258 | 0.1541028 |
| generosity                   |  2.0618730 | 0.4998157 |  4.1252665 | 0.0000555 |

From 2015 to 2019, in Sub- Saharan African countries, variable
“generosity” exhibited coefficient (2.0618730) with significant p-value
(0.0000555) indicating that it had the most substantial impact on
happiness score. Following this, variables “freedom to make life
choice”, “social support” and “gdp per capita” also had essential impact
on happiness score since they had relatively large coefficient and
significant p-value(\<0.001). However, “health life expectancy” and
“perception of corruption” did not appear to have significant p-value
(\>0.05) indicating that they did not have significant impact on
happiness score. \#Multiple linear Regression for Happiness Score
Prediction in Western Europe

``` r
west_europe = happiness_score|>
  filter(region == "Western Europe")

model9 = lm(formula = score ~ gdp_per_capita + social_support + health_life_expectancy + freedom_to_make_life_choices + perception_of_corruption + generosity, 
                     data = west_europe)

model9|>
  broom::tidy()|>
  knitr::kable()
```

| term                         |  estimate | std.error | statistic |   p.value |
|:-----------------------------|----------:|----------:|----------:|----------:|
| (Intercept)                  | 2.1188003 | 0.7020829 |  3.017878 | 0.0032440 |
| gdp_per_capita               | 0.8563506 | 0.3675997 |  2.329574 | 0.0218810 |
| social_support               | 0.7417660 | 0.1914877 |  3.873701 | 0.0001936 |
| health_life_expectancy       | 1.1950538 | 0.5049965 |  2.366459 | 0.0199258 |
| freedom_to_make_life_choices | 0.8686215 | 0.3881809 |  2.237672 | 0.0275070 |
| perception_of_corruption     | 2.7240472 | 0.3798791 |  7.170826 | 0.0000000 |
| generosity                   | 1.2352683 | 0.3167298 |  3.900070 | 0.0001763 |

From 2015 to 2019, in Western European countries, all 6 variables had
significant impact on happiness score since their p-value were all
smaller than 0.05. Among these variables, “perception of corruption”
exhibited the largest coefficient and the smallest p-valu indicating
that it had the most significant impact on happiness score.

According to the linear regression model performed separately according
to their regions, factors that impacted the happiness score varied a
lot. One extreme case took place in North American countries since none
of the factors were playing significant roles in impacting their
happiness score; another extreme case took place in western- European
countries since all 6 variables had significant impact on the happiness
score while “perception of corruption” was the main factors. The linear
regression models also indicate that both Southern Asian countries and
Southeastern Asian countries had only one factor, “perception of
corruption” and “gdp per capita” respectively with extremely significant
p-values. “freedom to make life choices” appeared to make the greatest
impact on the happiness score in Central and Eastern European and Latin
American and Caribbean countries countries.”perception of corruption”
played a substantial role on impacting the happiness score in southern
Asian countries and eastern Asian countries.
