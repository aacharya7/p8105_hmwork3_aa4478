Homework 3
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
```

\#\#Problem 1

``` r
data('instacart')
```

The `instacart` dataset contains 1384617 observations (representing
products from orders) and 15 variables. The key variables in the
`instacart` dataset include `reordered`, which describes if this product
has been ordered by this user in the past,`order_dow`, which describes
the day in which the order was placed, and `order_hour_of_day`, the hour
of the day on which the order was placed. There are also item variables
- name, aisle, department, and some numeric codes.

How many aisles and which are most items from?

``` r
instacart %>%
  count(aisle)%>%
  arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

The aisles that the most items are ordered from are “fresh vegetables”
(150609 orders) and “fresh fruit” (150473 orders).

Making a plot

``` r
instacart %>%
  count(aisle) %>%
  filter (n>1000) %>%
  mutate(
    aisle = factor(aisle),
    aisle = fct_reorder(aisle,n)
  ) %>%
  ggplot(aes(x=aisle,y=n)) +
  geom_point()
```

![](p8105_hmwork3_aa4478Rmd_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1 ))
```

    ## List of 1
    ##  $ axis.text.x:List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : num 0.5
    ##   ..$ angle        : num 270
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

Making a table

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients","dog food care","packaged vegetables fruits")) %>%
  group_by(aisle) %>%
  count(product_name) %>%
  mutate(rank = min_rank(desc(n))) %>%
  filter(rank < 4) %>%
  arrange(aisle,rank) %>%
  knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

## Problem 2

Loading, tidying and wrangling data:

``` r
accel_data = 
  read_csv("./data/accel_data.csv",col_names = TRUE ) %>%
  janitor::clean_names() %>%
  mutate(
    weekend = recode(day, "Saturday" = 1, "Sunday" = 1, "Monday" = 0, "Tuesday" = 0, "Wednesday" = 0, "Thursday" = 0, "Friday" = 0),
    weekday = recode(day, "Saturday" = 0, "Sunday" = 0, "Monday" = 1, "Tuesday" = 1, "Wednesday" = 1, "Thursday" = 1, "Friday" = 1),
    day = ordered(day, c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
    week = ordered(week, c("1", "2", "3", "4", "5"))) %>% 
  select(week, day_id, day, weekend, weekday, everything()) %>% 
  pivot_longer(
    cols = starts_with("activity_"),
    names_to = "activity_minute_num",
    names_prefix = "activity_",
    values_to = "activity_counts") %>% 
  mutate(activity_minute_num = as.numeric(activity_minute_num))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

The `accel_data` dataset contains 50400 observations and 7 variables.
The 7 variables in the dataset are `week`, `day_id`, `day`, `weekend`,
`weekday`, `activity_minute_num`, and `activity_counts`.

## Aggregate accross minutes to create a total activity variable for each day, and create a table showing these totals, check for trends.

``` r
total_act = accel_data %>% 
  group_by(week, day_id, day) %>% 
  summarize (total_min = sum(activity_counts))
```

    ## `summarise()` regrouping output by 'week', 'day_id' (override with `.groups` argument)

``` r
knitr::kable(total_act)
```

| week | day\_id | day       | total\_min |
| :--- | ------: | :-------- | ---------: |
| 1    |       1 | Friday    |  480542.62 |
| 1    |       2 | Monday    |   78828.07 |
| 1    |       3 | Saturday  |  376254.00 |
| 1    |       4 | Sunday    |  631105.00 |
| 1    |       5 | Thursday  |  355923.64 |
| 1    |       6 | Tuesday   |  307094.24 |
| 1    |       7 | Wednesday |  340115.01 |
| 2    |       8 | Friday    |  568839.00 |
| 2    |       9 | Monday    |  295431.00 |
| 2    |      10 | Saturday  |  607175.00 |
| 2    |      11 | Sunday    |  422018.00 |
| 2    |      12 | Thursday  |  474048.00 |
| 2    |      13 | Tuesday   |  423245.00 |
| 2    |      14 | Wednesday |  440962.00 |
| 3    |      15 | Friday    |  467420.00 |
| 3    |      16 | Monday    |  685910.00 |
| 3    |      17 | Saturday  |  382928.00 |
| 3    |      18 | Sunday    |  467052.00 |
| 3    |      19 | Thursday  |  371230.00 |
| 3    |      20 | Tuesday   |  381507.00 |
| 3    |      21 | Wednesday |  468869.00 |
| 4    |      22 | Friday    |  154049.00 |
| 4    |      23 | Monday    |  409450.00 |
| 4    |      24 | Saturday  |    1440.00 |
| 4    |      25 | Sunday    |  260617.00 |
| 4    |      26 | Thursday  |  340291.00 |
| 4    |      27 | Tuesday   |  319568.00 |
| 4    |      28 | Wednesday |  434460.00 |
| 5    |      29 | Friday    |  620860.00 |
| 5    |      30 | Monday    |  389080.00 |
| 5    |      31 | Saturday  |    1440.00 |
| 5    |      32 | Sunday    |  138421.00 |
| 5    |      33 | Thursday  |  549658.00 |
| 5    |      34 | Tuesday   |  367824.00 |
| 5    |      35 | Wednesday |  445366.00 |

``` r
trends_wk = total_act %>% 
  ggplot(aes(x = day, y = total_min, color = week, group = week)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(~week) +
  labs(
    title = "Total daily activity over 5 weeks",
    x = "Day of week",
    y = "Total activity per day (min)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

trends_wk
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](p8105_hmwork3_aa4478Rmd_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
trends_day = total_act %>% 
  ggplot(aes(x = week, y = total_min, color = day, group = day)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(~day) +
  labs(
    title = "Total daily activity over 5 weeks",
    x = "Week",
    y = "Total activity per day (min)") 
trends_day
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

![](p8105_hmwork3_aa4478Rmd_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
There are no obvious trends or patterns that stand out when looking at
the `accel_data` dataset. Once we take a closer look, we can see that
for the first two weeks(week 1 and week 2), the total activity per day
is lowest on Mondays and then increases throughout the week.
Additionally, for weeks 4 and 5, we can see that Saturdays and Sundays
have the lowest total activity per day.

\#\#Make a single-panel plot that shows the 24-hour activity time
courses for each day and use color to indicate day of the week. Describe
in words any patterns or conclusions you can make based on this graph.

``` r
accel_plot = accel_data %>% 
  group_by(week, day_id, day) %>% 
  summarize (average_min = mean(activity_counts)) %>% 
  ggplot(aes(x = day_id, y = average_min, color = day)) +
  geom_line() +
  labs(
    title = "24-hour activity of Male Accelerometer Data Over 5-Week Period",
    x = "Day in 5-Week Period",
    y = "Average Activity in Minutes "
  )
```

    ## `summarise()` regrouping output by 'week', 'day_id' (override with `.groups` argument)

``` r
accel_plot
```

![](p8105_hmwork3_aa4478Rmd_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Similar to the previous plot, we can see here that the average activity
for Monday was very low, and increased throughout the week. However,
from this plot we also notice that the average activity for Saturday and
Sunday were high and then decreased throughout the week.
