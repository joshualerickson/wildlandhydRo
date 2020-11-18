
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildlandhydRo

<!-- badges: start -->

<!-- badges: end -->

The goal of wildlandhydRo is to create wrapper functions around commonly
used packages like [streamstats](https://github.com/markwh/streamstats),
[nhdplusTools](https://github.com/USGS-R/nhdplusTools),
[snotelr](https://github.com/bluegreen-labs/snotelr),[dataRetrieval](https://github.com/USGS-R/dataRetrieval)
and [AOI](https://github.com/mikejohnson51/AOI) and provide accessible
functions for culvert sizing, quick reports and EDA into extreme events.
Mostly being able to batch (get more than one) in a more direct
approach. Basically, a package for me but worth sharing with others.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/wildlandhydRo")
```

## Example

This is a basic example which shows you how to solve a common problem:
getting more than one culvert sized.

``` r
library(wildlandhydRo)
## basic example code
```

This delineates the basins and then computes basin characteristics
(e.g. precip, basin area, percent forested, etc.). The advantage of
`batch_StreamStats()` is you can do more than one at a time. See example
below.

``` r
# data frame with 3 different pour point locations
data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
                  Lon = c(-115.54327, -114.75546, -116.05935),
                   Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))

three_sites <- batch_StreamStats(lon = data$Lon, lat = data$Lat, group = data$Site,
                                  crs = 4326)
```

You can see that ‘WF Blue Creek’ is right on the border of Montana and
Idaho. The function accounts for that and provides the necessary stream
stats.

<img src="man/figures/README-unnamed-chunk-3-1.png" width="75%" /> <br>

Now we can use `batch_RRE()` to get regional regression estimates. Here
we’ll use the `three_sites` named object as the parameter input for the
function. Note you don’t have to use a `batch_StreamStats()` object and
could just manually enter the correct parameters.

``` r
peak_RRE <- batch_RRE(state = three_sites$state, wkID = three_sites$wkID, group = three_sites$group)
```

Then we can plot together. Below are the three different sites and the
regional regression estimates (RRE) plotting for these pour points.

``` r
peak_RRE %>% ggplot(aes(parse_number(Name), Value)) + geom_line() + geom_point() + facet_wrap(~group, scales = "free")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

The next step is to generate recommended culvert sizes for a set of
reoccurrence intervals (e.g. 2,25,50,100) based on the stream stats
collected in the previous steps. It’s important to note that this is
only available for western Montana right now. Also, we don’t have
bankfull width measurements and it is highly recommended to include if
you can.

``` r
culverts_all <- batch_culverts(ss = three_sites, rre = peak_RRE)
```

Then we can plot and see the difference between regressions.

``` r
culverts_all %>% ggplot(aes(RI, value, color = source)) + geom_point() + geom_line() + facet_grid(group~Method, scales = "free")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

No we can see what the recommended culvert sizes are in a table, only
showing the first 10.

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

ReturnInterval

</th>

<th style="text-align:left;">

source

</th>

<th style="text-align:left;">

group

</th>

<th style="text-align:right;">

RI

</th>

<th style="text-align:left;">

Method

</th>

<th style="text-align:right;">

value

</th>

<th style="text-align:left;">

Size

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

basin\_char

</td>

<td style="text-align:right;">

408

</td>

<td style="text-align:left;">

(Bridge or Big Culvert\!)

</td>

</tr>

<tr>

<td style="text-align:left;">

2 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

bankfull\_width\_regression

</td>

<td style="text-align:right;">

342

</td>

<td style="text-align:left;">

(96 in)

</td>

</tr>

<tr>

<td style="text-align:left;">

25 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

basin\_char

</td>

<td style="text-align:right;">

865

</td>

<td style="text-align:left;">

(Bridge or Big Culvert\!)

</td>

</tr>

<tr>

<td style="text-align:left;">

25 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

bankfull\_width\_regression

</td>

<td style="text-align:right;">

733

</td>

<td style="text-align:left;">

(Bridge or Big Culvert\!)

</td>

</tr>

<tr>

<td style="text-align:left;">

50 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

basin\_char

</td>

<td style="text-align:right;">

958

</td>

<td style="text-align:left;">

(Bridge or Big Culvert\!)

</td>

</tr>

<tr>

<td style="text-align:left;">

50 Year Peak Flood

</td>

<td style="text-align:left;">

Omang, Parrett and Hull

</td>

<td style="text-align:left;">

Granite Creek

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

bankfull\_width\_regression

</td>

<td style="text-align:right;">

836

</td>

<td style="text-align:left;">

(Bridge or Big Culvert\!)

</td>

</tr>

</tbody>

</table>
