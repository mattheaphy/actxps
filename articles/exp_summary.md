# Experience summaries

After experience data has been prepared for analysis, the next step is
to summarize results. The actxps package’s workhorse function for
summarizing termination experience is
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).
This function returns an `exp_df` object, which is a type of data frame
containing additional attributes about the experience study.

At a minimum, an `exp_df` includes:

- The number of claims (termination events, `n_claims`)
- The amount of claims (weighted by a variable of the user’s choice;
  more on this below, `claims`)
- The total exposure (`exposure`)
- The observed termination rate (`q_obs`)

Optionally, an `exp_df` can also include:

- Any grouping variables attached to the input data
- Expected termination rates and actual-to-expected (A/E) ratios
  (`ae_*`)
- Limited fluctuation credibility estimates (`credibility`) and
  credibility-adjusted expected termination rates (`adj_*`)

To demonstrate this function, we’re going to use a data frame containing
simulated census data for a theoretical deferred annuity product that
has an optional guaranteed income rider. Before
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
can be used, we must convert our census data into exposure records using
the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
function[^1]. In addition, let’s assume we’re interested in studying
surrender rates, so we’ll pass the argument
`target_status = 'Surrender'` to
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md).

``` r

library(actxps)
library(dplyr)

exposed_data <- expose(
  census_dat,
  end_date = "2019-12-31",
  target_status = "Surrender"
)
```

## The `exp_stats()` function

To use
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md),
pass it a data frame of exposure-level records, ideally of type
`exposed_df` (the object class returned by the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
family of functions).

``` r

exp_stats(exposed_data)
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#>      <int>  <int>    <dbl>  <dbl>
#> 1     2869   2869  132634. 0.0216
```

The results show us that we specified no groups, which is why the output
data is a single row. In addition, we can see that we’re looking at
surrender rates through the end of 2019, which
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
inferred from `exposed_data`.

The number of claims (`n_claims`) is equal to the number of “Surrender”
statuses in `exposed_data`. Since we didn’t specify any weighting
variable, the amount of claims (`claims`) equals the number of claims.

``` r

(amount <- sum(exposed_data$status == "Surrender"))
#> [1] 2869
```

The total exposure (`exposure`) is equal to the sum of the exposures in
`exposed_data`. Had we specified a weighting variable, this would be
equal to the sum of weighted exposures.

``` r

(sum_expo <- sum(exposed_data$exposure))
#> [1] 132633.9
```

Lastly, the observed termination rate (`q_obs`) equals the amount of
claims divided by the exposures.

``` r

amount / sum_expo
#> [1] 0.02163097
```

### Grouped data

If the data frame passed into
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
is grouped using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
the resulting output will contain one record for each unique group.

In the following, `exposed_data` is grouped by policy year before being
passed to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).
This results in one row per policy year found in the data.

``` r

exposed_data |>
  group_by(pol_yr) |>
  exp_stats()
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 15 × 5
#>    pol_yr n_claims claims exposure   q_obs
#>     <int>    <int>  <int>    <dbl>   <dbl>
#>  1      1      102    102   19252. 0.00530
#>  2      2      160    160   17715. 0.00903
#>  3      3      124    124   16097. 0.00770
#>  4      4      168    168   14536. 0.0116 
#>  5      5      164    164   12916. 0.0127 
#>  6      6      152    152   11376. 0.0134 
#>  7      7      164    164    9917. 0.0165 
#>  8      8      190    190    8448. 0.0225 
#>  9      9      181    181    6960. 0.0260 
#> 10     10      152    152    5604. 0.0271 
#> 11     11      804    804    4390. 0.183  
#> 12     12      330    330    2663. 0.124  
#> 13     13       99     99    1620. 0.0611 
#> 14     14       62     62     872. 0.0711 
#> 15     15       17     17     268. 0.0634
```

Multiple grouping variables are allowed. Below, the presence of an
income guarantee (`inc_guar`) is added as a second grouping variable.

``` r

exposed_data |>
  group_by(inc_guar, pol_yr) |>
  exp_stats()
#> 
#> ── Experience study results ──
#> 
#> • Groups: inc_guar and pol_yr
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 30 × 6
#>    inc_guar pol_yr n_claims claims exposure   q_obs
#>    <lgl>     <int>    <int>  <int>    <dbl>   <dbl>
#>  1 FALSE         1       56     56    7720. 0.00725
#>  2 FALSE         2       92     92    7103. 0.0130 
#>  3 FALSE         3       67     67    6447. 0.0104 
#>  4 FALSE         4      123    123    5799. 0.0212 
#>  5 FALSE         5       97     97    5106. 0.0190 
#>  6 FALSE         6       96     96    4494. 0.0214 
#>  7 FALSE         7       92     92    3899. 0.0236 
#>  8 FALSE         8      103    103    3287. 0.0313 
#>  9 FALSE         9       87     87    2684. 0.0324 
#> 10 FALSE        10       60     60    2156. 0.0278 
#> # ℹ 20 more rows
```

### Target status

The `target_status` argument of
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
specifies which status levels count as claims in the experience study
summary. If the data passed to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
is an `exposed_df` object that already has a specified target status
(via a prior call to
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)),
then this argument is not necessary because the target status is
automatically inferred.

Even if the target status exists on the input data, it can be
overridden. However care should be taken to ensure that exposure values
in the data are appropriate for the new status.

Using the example data, a total termination rate can be estimated by
including both death and surrender statuses in `target_status`. To
ensure exposures are accurate, an adjustment is made to fully expose
deaths prior to calling
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)[^2].

``` r

exposed_data |>
  mutate(exposure = ifelse(status == "Death", 1, status)) |>
  group_by(pol_yr) |>
  exp_stats(target_status = c("Surrender", "Death"))
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender and Death
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 15 × 5
#>    pol_yr n_claims claims exposure  q_obs
#>     <int>    <int>  <int>    <dbl>  <dbl>
#>  1      1      290    290    20199 0.0144
#>  2      2      325    325    18754 0.0173
#>  3      3      292    292    17054 0.0171
#>  4      4      329    329    15602 0.0211
#>  5      5      329    329    13946 0.0236
#>  6      6      334    334    12371 0.0270
#>  7      7      297    297    10869 0.0273
#>  8      8      340    340     9510 0.0358
#>  9      9      308    308     7953 0.0387
#> 10     10      260    260     6489 0.0401
#> 11     11      894    894     6505 0.137 
#> 12     12      398    398     3753 0.106 
#> 13     13      131    131     2135 0.0614
#> 14     14       89     89     1306 0.0681
#> 15     15       23     23      544 0.0423
```

## Weighted results

Experience studies often weight output by key policy values. Examples
include account values, cash values, face amount, premiums, and more.
Weighting can be accomplished by passing the name of a weighting column
to the `wt` argument of
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).

Our sample data contains a column called `premium` that we can weight
by. When weights are supplied, the `claims`, `exposure`, and `q_obs`
columns will be weighted. If expected termination rates are supplied
(see below), these rates and A/E values will also be weighted.[^3]

``` r

exposed_data |>
  group_by(pol_yr) |>
  exp_stats(wt = 'premium')
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Weighted by: premium
#> 
#> # A tibble: 15 × 8
#>    pol_yr n_claims claims  exposure   q_obs  .weight  .weight_sq .weight_n
#>     <int>    <int>  <dbl>     <dbl>   <dbl>    <dbl>       <dbl>     <dbl>
#>  1      1      102  83223 25312812. 0.00329 26301746 60742993234     19995
#>  2      2      160 170058 23352461. 0.00728 24275265 56232848027     18434
#>  3      3      124 123554 21246765. 0.00582 22201817 51746834383     16806
#>  4      4      168 176751 19270856. 0.00917 20200019 47142441689     15266
#>  5      5      164 173273 17228978. 0.0101  18134795 42887920479     13618
#>  6      6      152 163034 15246504. 0.0107  16192950 38828949428     12067
#>  7      7      164 153238 13328777. 0.0115  14159437 34291451913     10541
#>  8      8      190 174200 11476433. 0.0152  12346124 30121310640      9130
#>  9      9      181 187337  9546247. 0.0196  10420172 25781142118      7591
#> 10     10      152 157603  7707062. 0.0204   8543150 20882643976      6185
#> 11     11      804 856379  6093168. 0.141    6783273 16219955859      4897
#> 12     12      330 383055  3883534. 0.0986   4525027 11191462577      3093
#> 13     13       99 123357  2450266. 0.0503   2891573  7075934189      1937
#> 14     14       62  75534  1339240. 0.0564   1821026  4655661820      1182
#> 15     15       17  19168   401169. 0.0478    783146  1944755748       510
```

## Expected values and A/E ratios

A common metric in experience studies is the actual-to-expected, or A/E
ratio.

``` math
A/E\ ratio = \frac{observed\ value}{expected\ value}
```

If the data passed to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
has one or more columns containing expected termination rates, A/E
ratios can be calculated by passing the names of these columns to the
`expected` argument.

Let’s assume we have two sets of expected rates. The first set is a
vector that varies by policy year. The second set is either 1.5% or 3.0%
depending on whether the policy has a guaranteed income benefit. First,
we need to attach these assumptions to our exposure data. We will use
the names `expected_1` and `expected_2`. Then we pass these names to the
`expected` argument when we call
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).

In the output, 4 new columns are created for expected rates and A/E
ratios.

``` r

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

# using 2 different expected termination assumption sets
exposed_data <- exposed_data |>
  mutate(
    expected_1 = expected_table[pol_yr],
    expected_2 = ifelse(exposed_data$inc_guar, 0.015, 0.03)
  )

exp_res <- exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"))


exp_res |>
  select(
    pol_yr,
    inc_guar,
    q_obs,
    expected_1,
    expected_2,
    ae_expected_1,
    ae_expected_2
  )
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1 and expected_2
#> 
#> # A tibble: 30 × 7
#>    pol_yr inc_guar   q_obs expected_1 expected_2 ae_expected_1 ae_expected_2
#>     <int> <lgl>      <dbl>      <dbl>      <dbl>         <dbl>         <dbl>
#>  1      1 FALSE    0.00725    0.005        0.03          1.45          0.242
#>  2      1 TRUE     0.00399    0.005        0.015         0.798         0.266
#>  3      2 FALSE    0.0130     0.00778      0.03          1.67          0.432
#>  4      2 TRUE     0.00641    0.00778      0.015         0.824         0.427
#>  5      3 FALSE    0.0104     0.0106       0.03          0.985         0.346
#>  6      3 TRUE     0.00591    0.0106       0.015         0.560         0.394
#>  7      4 FALSE    0.0212     0.0133       0.03          1.59          0.707
#>  8      4 TRUE     0.00515    0.0133       0.015         0.386         0.343
#>  9      5 FALSE    0.0190     0.0161       0.03          1.18          0.633
#> 10      5 TRUE     0.00858    0.0161       0.015         0.532         0.572
#> # ℹ 20 more rows
```

As noted above, if weights are passed to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
then A/E ratios will also be weighted.

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"), wt = "premium") |>
  select(
    pol_yr,
    inc_guar,
    q_obs,
    expected_1,
    expected_2,
    ae_expected_1,
    ae_expected_2
  )
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1 and expected_2
#> • Weighted by: premium
#> 
#> # A tibble: 30 × 7
#>    pol_yr inc_guar   q_obs expected_1 expected_2 ae_expected_1 ae_expected_2
#>     <int> <lgl>      <dbl>      <dbl>      <dbl>         <dbl>         <dbl>
#>  1      1 FALSE    0.00471    0.005        0.03          0.942         0.157
#>  2      1 TRUE     0.00235    0.005        0.015         0.470         0.157
#>  3      2 FALSE    0.0105     0.00778      0.03          1.36          0.351
#>  4      2 TRUE     0.00513    0.00778      0.015         0.660         0.342
#>  5      3 FALSE    0.00737    0.0106       0.03          0.698         0.246
#>  6      3 TRUE     0.00479    0.0106       0.015         0.453         0.319
#>  7      4 FALSE    0.0174     0.0133       0.03          1.30          0.579
#>  8      4 TRUE     0.00377    0.0133       0.015         0.283         0.252
#>  9      5 FALSE    0.0146     0.0161       0.03          0.907         0.487
#> 10      5 TRUE     0.00710    0.0161       0.015         0.441         0.473
#> # ℹ 20 more rows
```

### Control variables

Control variables are a related concept to expected values. Control
variables are used to estimate the impact of any grouping variables on
observed experience *after accounting for* the impact of other (control)
variables.

Control variables can help answer questions like, “How much lower are
surrender rates by policy year for contracts with a guaranteed income
rider relative to contracts without a rider?”. Here, the presence of a
guaranteed income rider is a grouping variable and policy year is a
control variable.

Control variables are specified using the optional `control_vars`
argument. If provided, this argument must be `".none"` (more on this
below) or a character vector with values corresponding to column names
in `.data`.

To answer the question above, we can group the data by `inc_guar` and
add `control_vars = "pol_yr"` in a call to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).

``` r

exposed_data |>
  group_by(inc_guar) |>
  exp_stats(control_vars = "pol_yr") |>
  select(inc_guar, q_obs, control, ae_control)
#> 
#> ── Experience study results ──
#> 
#> • Groups: inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Control variables: pol_yr
#> • Expected values: control
#> 
#> # A tibble: 2 × 4
#>   inc_guar  q_obs control ae_control
#>   <lgl>     <dbl>   <dbl>      <dbl>
#> 1 FALSE    0.0307  0.0209      1.47 
#> 2 TRUE     0.0157  0.0221      0.712
```

In the resulting output two new columns appeared:

- `control`: Observed surrender rates considering the control variables
  (`pol_yr`) only. The fact that the two values of `control` above do
  not match is not surprising and simply represents the fact that the
  distributions of `pol_yr` across the levels of `inc_guar` are not
  identical.
- `ae_control`: The A/E ratio of observed experience versus `control`.
  This is an estimate of the impact of `inc_guar` after accounting for
  `pol_yr` effects.

These results show that the presence of a guaranteed income rider
decreases surrender rates by a very significant amount. The converse is
true for contracts without a rider.

As an alternative, if `".none"` is passed to `control_vars`, a single
aggregate termination rate is calculated for the entire data set and
used to compute `control` and `ae_control`.

``` r

exposed_data |>
  group_by(inc_guar) |>
  exp_stats(control_vars = ".none") |>
  select(inc_guar, q_obs, control, ae_control)
#> 
#> ── Experience study results ──
#> 
#> • Groups: inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Control variables: None
#> • Expected values: control
#> 
#> # A tibble: 2 × 4
#>   inc_guar  q_obs control ae_control
#>   <lgl>     <dbl>   <dbl>      <dbl>
#> 1 FALSE    0.0307  0.0216      1.42 
#> 2 TRUE     0.0157  0.0216      0.728
```

Note that:

- `control` is now a constant value
- Different results are yielded for `ae_control`

The `control_distinct_max` argument places an upper limit on the number
of unique values that a control variable is allowed to have. This limit
exists to prevent an excessive number of groups on continuous or
high-cardinality features.

It should be noted that usage of control variables is a rough
approximation and not a substitute for rigorous statistical models. The
impact of control variables is calculated in isolation and does consider
other features or possible confounding variables. As such, control
variables are most useful for exploratory data analysis.

## Credibility

If the `credibility` argument is set to `TRUE`,
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
will produce an estimate of partial credibility under the Limited
Fluctuation credibility method (also known as Classical Credibility)
assuming a binomial distribution of claims.[^4]

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE) |>
  select(pol_yr, inc_guar, claims, q_obs, credibility)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 30 × 5
#>    pol_yr inc_guar claims   q_obs credibility
#>     <int> <lgl>     <int>   <dbl>       <dbl>
#>  1      1 FALSE        56 0.00725       0.192
#>  2      1 TRUE         46 0.00399       0.173
#>  3      2 FALSE        92 0.0130        0.246
#>  4      2 TRUE         68 0.00641       0.211
#>  5      3 FALSE        67 0.0104        0.210
#>  6      3 TRUE         57 0.00591       0.193
#>  7      4 FALSE       123 0.0212        0.286
#>  8      4 TRUE         45 0.00515       0.172
#>  9      5 FALSE        97 0.0190        0.254
#> 10      5 TRUE         67 0.00858       0.210
#> # ℹ 20 more rows
```

Under the default arguments, credibility calculations assume a 95%
confidence of being within 5% of the true value. These parameters can be
overridden using the `conf_level` and `cred_r` arguments, respectively.

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE, conf_level = 0.98, cred_r = 0.03) |>
  select(pol_yr, inc_guar, claims, q_obs, credibility)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 30 × 5
#>    pol_yr inc_guar claims   q_obs credibility
#>     <int> <lgl>     <int>   <dbl>       <dbl>
#>  1      1 FALSE        56 0.00725      0.0969
#>  2      1 TRUE         46 0.00399      0.0876
#>  3      2 FALSE        92 0.0130       0.125 
#>  4      2 TRUE         68 0.00641      0.107 
#>  5      3 FALSE        67 0.0104       0.106 
#>  6      3 TRUE         57 0.00591      0.0976
#>  7      4 FALSE       123 0.0212       0.145 
#>  8      4 TRUE         45 0.00515      0.0867
#>  9      5 FALSE        97 0.0190       0.128 
#> 10      5 TRUE         67 0.00858      0.106 
#> # ℹ 20 more rows
```

If expected values are passed to
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
and `credibility` is set to `TRUE`, then the output will also contain
credibility-weighted expected values:

``` math
q^{adj} = Z^{cred} \times q^{obs} + (1-Z^{cred}) \times q^{exp}
```
where,

- $`q^{adj}`$ = credibility-weighted estimate
- $`Z^{cred}`$ = partial credibility factor
- $`q^{obs}`$ = observed termination rate
- $`q^{exp}`$ = expected termination rate

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE, expected = "expected_1") |>
  select(
    pol_yr,
    inc_guar,
    claims,
    q_obs,
    credibility,
    adj_expected_1,
    expected_1,
    ae_expected_1
  )
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1
#> 
#> # A tibble: 30 × 8
#>    pol_yr inc_guar claims   q_obs credibility adj_expected_1 expected_1
#>     <int> <lgl>     <int>   <dbl>       <dbl>          <dbl>      <dbl>
#>  1      1 FALSE        56 0.00725       0.192        0.00543    0.005  
#>  2      1 TRUE         46 0.00399       0.173        0.00482    0.005  
#>  3      2 FALSE        92 0.0130        0.246        0.00905    0.00778
#>  4      2 TRUE         68 0.00641       0.211        0.00749    0.00778
#>  5      3 FALSE        67 0.0104        0.210        0.0105     0.0106 
#>  6      3 TRUE         57 0.00591       0.193        0.00966    0.0106 
#>  7      4 FALSE       123 0.0212        0.286        0.0156     0.0133 
#>  8      4 TRUE         45 0.00515       0.172        0.0119     0.0133 
#>  9      5 FALSE        97 0.0190        0.254        0.0168     0.0161 
#> 10      5 TRUE         67 0.00858       0.210        0.0145     0.0161 
#> # ℹ 20 more rows
#> # ℹ 1 more variable: ae_expected_1 <dbl>
```

## Confidence intervals

If `conf_int` is set to `TRUE`,
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
will produce lower and upper confidence interval limits for the observed
termination rate.

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(conf_int = TRUE) |>
  select(pol_yr, inc_guar, q_obs, q_obs_lower, q_obs_upper)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 30 × 5
#>    pol_yr inc_guar   q_obs q_obs_lower q_obs_upper
#>     <int> <lgl>      <dbl>       <dbl>       <dbl>
#>  1      1 FALSE    0.00725     0.00544     0.00920
#>  2      1 TRUE     0.00399     0.00286     0.00520
#>  3      2 FALSE    0.0130      0.0104      0.0156 
#>  4      2 TRUE     0.00641     0.00490     0.00801
#>  5      3 FALSE    0.0104      0.00807     0.0129 
#>  6      3 TRUE     0.00591     0.00446     0.00746
#>  7      4 FALSE    0.0212      0.0176      0.0250 
#>  8      4 TRUE     0.00515     0.00366     0.00675
#>  9      5 FALSE    0.0190      0.0153      0.0229 
#> 10      5 TRUE     0.00858     0.00666     0.0106 
#> # ℹ 20 more rows
```

If no weighting variable is passed to `wt`, confidence intervals will be
constructed assuming a binomial distribution of claims. However, if a
weighting variable is supplied, a normal distribution for aggregate
claims will be assumed with a mean equal to observed claims and a
variance equal to:

``` math
Var(S) = E(N) \times Var(X) + E(X)^2 \times Var(N)
```

Where `S` is the aggregate claim random variable, `X` is the weighting
variable assumed to follow a normal distribution, and `N` is a binomial
random variable for the number of claims.

The default confidence level is 95%. This can be changed using the
`conf_level` argument. Below, tighter confidence intervals are
constructed by decreasing the confidence level to 90%.

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(conf_int = TRUE, conf_level = 0.9) |>
  select(pol_yr, inc_guar, q_obs, q_obs_lower, q_obs_upper)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 30 × 5
#>    pol_yr inc_guar   q_obs q_obs_lower q_obs_upper
#>     <int> <lgl>      <dbl>       <dbl>       <dbl>
#>  1      1 FALSE    0.00725     0.00570     0.00894
#>  2      1 TRUE     0.00399     0.00303     0.00494
#>  3      2 FALSE    0.0130      0.0108      0.0152 
#>  4      2 TRUE     0.00641     0.00518     0.00773
#>  5      3 FALSE    0.0104      0.00838     0.0126 
#>  6      3 TRUE     0.00591     0.00466     0.00725
#>  7      4 FALSE    0.0212      0.0181      0.0243 
#>  8      4 TRUE     0.00515     0.00389     0.00641
#>  9      5 FALSE    0.0190      0.0159      0.0221 
#> 10      5 TRUE     0.00858     0.00691     0.0104 
#> # ℹ 20 more rows
```

If expected values are passed to `expected`, the output will also
contain confidence intervals around any actual-to-expected ratios.

``` r

exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(conf_int = TRUE, expected = "expected_1") |>
  select(pol_yr, inc_guar, starts_with("ae_"))
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1
#> 
#> # A tibble: 30 × 5
#>    pol_yr inc_guar ae_expected_1 ae_expected_1_lower ae_expected_1_upper
#>     <int> <lgl>            <dbl>               <dbl>               <dbl>
#>  1      1 FALSE            1.45                1.09                1.84 
#>  2      1 TRUE             0.798               0.572               1.04 
#>  3      2 FALSE            1.67                1.34                2.01 
#>  4      2 TRUE             0.824               0.630               1.03 
#>  5      3 FALSE            0.985               0.764               1.22 
#>  6      3 TRUE             0.560               0.422               0.707
#>  7      4 FALSE            1.59                1.32                1.88 
#>  8      4 TRUE             0.386               0.275               0.506
#>  9      5 FALSE            1.18                0.948               1.42 
#> 10      5 TRUE             0.532               0.413               0.660
#> # ℹ 20 more rows
```

Lastly, if `credibility` is `TRUE` *and* expected values are passed to
`expected`, confidence intervals will also be calculated for any
credibility-weighted termination rates.

## Miscellaneous

### Summary method

As noted above, the result of
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
is an `exp_df` object. If the
[`summary()`](https://rdrr.io/r/base/summary.html) function is applied
to an `exp_df` object, the data will be summarized again and return a
higher level `exp_df` object.

If no additional arguments are passed,
[`summary()`](https://rdrr.io/r/base/summary.html) returns a single row
of aggregate results.

``` r

summary(exp_res)
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1 and expected_2
#> 
#> # A tibble: 1 × 8
#>   n_claims claims exposure  q_obs expected_1 expected_2 ae_expected_1
#>      <int>  <int>    <dbl>  <dbl>      <dbl>      <dbl>         <dbl>
#> 1     2869   2869  132634. 0.0216     0.0242     0.0209         0.892
#> # ℹ 1 more variable: ae_expected_2 <dbl>
```

If additional variable names are passed to the
[`summary()`](https://rdrr.io/r/base/summary.html) function, then the
output will group the data by those variables. In our example, if
`pol_yr` is passed to
[`summary()`](https://rdrr.io/r/base/summary.html), the output will
contain one row per policy year.

``` r

summary(exp_res, pol_yr)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1 and expected_2
#> 
#> # A tibble: 15 × 9
#>    pol_yr n_claims claims exposure   q_obs expected_1 expected_2 ae_expected_1
#>     <int>    <int>  <int>    <dbl>   <dbl>      <dbl>      <dbl>         <dbl>
#>  1      1      102    102   19252. 0.00530    0.005       0.0210         1.06 
#>  2      2      160    160   17715. 0.00903    0.00778     0.0210         1.16 
#>  3      3      124    124   16097. 0.00770    0.0106      0.0210         0.730
#>  4      4      168    168   14536. 0.0116     0.0133      0.0210         0.867
#>  5      5      164    164   12916. 0.0127     0.0161      0.0209         0.788
#>  6      6      152    152   11376. 0.0134     0.0189      0.0209         0.707
#>  7      7      164    164    9917. 0.0165     0.0217      0.0209         0.763
#>  8      8      190    190    8448. 0.0225     0.0244      0.0208         0.920
#>  9      9      181    181    6960. 0.0260     0.0272      0.0208         0.955
#> 10     10      152    152    5604. 0.0271     0.03        0.0208         0.904
#> 11     11      804    804    4390. 0.183      0.2         0.0208         0.916
#> 12     12      330    330    2663. 0.124      0.15        0.0200         0.826
#> 13     13       99     99    1620. 0.0611     0.05        0.0197         1.22 
#> 14     14       62     62     872. 0.0711     0.05        0.0195         1.42 
#> 15     15       17     17     268. 0.0634     0.05        0.0191         1.27 
#> # ℹ 1 more variable: ae_expected_2 <dbl>
```

Similarly, if `inc_guar` is passed to
[`summary()`](https://rdrr.io/r/base/summary.html), the output will
contain a row for each unique value in `inc_guar`.

``` r

summary(exp_res, inc_guar)
#> 
#> ── Experience study results ──
#> 
#> • Groups: inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Expected values: expected_1 and expected_2
#> 
#> # A tibble: 2 × 9
#>   inc_guar n_claims claims exposure  q_obs expected_1 expected_2 ae_expected_1
#>   <lgl>       <int>  <int>    <dbl>  <dbl>      <dbl>      <dbl>         <dbl>
#> 1 FALSE        1601   1601   52123. 0.0307     0.0235      0.03          1.31 
#> 2 TRUE         1268   1268   80511. 0.0157     0.0247      0.015         0.637
#> # ℹ 1 more variable: ae_expected_2 <dbl>
```

### Column names

As a default,
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
assumes the input data frame uses the following naming conventions:

- The exposure column is called `exposure`
- The status column is called `status`

These default names can be overridden using the `col_exposure` and
`col_status` arguments.

For example, if the status column was called `curr_stat` in our data, we
could write:

``` r

exposed_data |>
  exp_stats(col_status = "curr_stat")
```

### Applying exp_stats to a non-`exposed_df` data frame

[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
can still work when given a non-`exposed_df` data frame. However, it
will be unable to infer certain attributes like the target status and
the study dates. For target status, all statuses except the first level
are assumed to be terminations. Since this may not be desirable, a
warning message will appear informing what statuses were assumed to be
terminated.

``` r

not_exposed_df <- data.frame(exposed_data)

exp_stats(not_exposed_df)
#> Warning: ✖ No target status was provided.
#> ℹ "Death" and "Surrender" were assumed.
#> 
#> ── Experience study results ──
#> 
#> • Target status: Death and Surrender
#> • Study range: to
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#>      <int>  <int>    <dbl>  <dbl>
#> 1     4639   4639  132634. 0.0350
```

If `target_status` is provided, no warning message will appear.

``` r

exp_stats(not_exposed_df, target_status = "Surrender")
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: to
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#>      <int>  <int>    <dbl>  <dbl>
#> 1     2869   2869  132634. 0.0216
```

### Limitations

The
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
function only supports termination studies. It does not contain support
for transaction studies or studies with multiple changes from an active
to an inactive status. For information on transaction studies, see
[`vignette("transactions")`](https://mattheaphy.github.io/actxps/articles/transactions.md).

[^1]: See
    [`vignette('exposures')`](https://mattheaphy.github.io/actxps/articles/exposures.md)
    for more information on creating exposure records.

[^2]: This adjustment is not necessary on surrenders because the
    [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
    function previously did this for us.

[^3]: When weights are supplied, additional columns are created
    containing the sum of weights, the sum of squared weights, and the
    number of records. These columns are used for re-summarizing the
    data (see the “Summary method” section on this page).

[^4]: See Herzog, Thomas (1999). Introduction to Credibility Theory for
    more information on Limited Fluctuation Credibility.
