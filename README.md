
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/raymondben/peranavolley.svg?branch=master)](https://travis-ci.org/raymondben/peranavolley)

# peranavolley

An R package for reading [Perana Sports VBStats](http://peranasports.com/software/vbstatshd/) volleyball
scouting files. Note that this package is in a relatively early stage of development, and
functionality may change without warning.

See also [datavolley](https://github.com/raymondben/datavolley), which
provides similar functionality for reading DataVolley files into R. The
conventions (data structures, variable naming, etc) used in the
peranavolley package follow those of the datavolley package.

## Installation

You can install peranavolley from GitHub with:

``` r
library(remotes)
install_github("raymondben/peranavolley")
```

## Example

Read one of the example data files bundled with the package:

``` r
library(peranavolley)
x <- pv_read(pv_example_file())
summary(x)
#> Match summary:
#> Date: 2017-09-23
#> League: 2017 AVL Men's
#> Teams: Canberra Heat Men's
#>        vs
#>        UTSSU Men's
#> Result: 3-0 (25-23, 31-29, 25-15)
#> Duration: unknown
```

Number of serves by team:

``` r
serve_idx <- which(plays(x)$skill == "Serve")
table(plays(x)$team[serve_idx])
#> 
#> Canberra Heat Men's         UTSSU Men's 
#>                  77                  70
```

Attack kill rate by player, using `dplyr`:

``` r
library(dplyr)
plays(x) %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, player_name, player_id) %>%
  dplyr::summarize(`N attacks` = n(), `Kill rate` = mean(evaluation == "Winning attack", na.rm = TRUE)) %>%
  ungroup %>%
  dplyr::arrange(desc(`Kill rate`)) %>% dplyr::select(-player_id)
#> # A tibble: 16 x 4
#>    team                player_name     `N attacks` `Kill rate`
#>    <chr>               <chr>                 <int>       <dbl>
#>  1 UTSSU Men's         Brian Cho                 3       0.667
#>  2 Canberra Heat Men's Cameron Steer             7       0.571
#>  3 UTSSU Men's         Nathan Lovett             7       0.571
#>  4 Canberra Heat Men's Murch Malachi             9       0.556
#>  5 Canberra Heat Men's Nick Borgeaud             2       0.5  
#>  6 Canberra Heat Men's Samuel Walker            30       0.467
#>  7 UTSSU Men's         C Beasley                28       0.429
#>  8 Canberra Heat Men's Keiran Ackhurst           5       0.4  
#>  9 UTSSU Men's         Nathan Healey            31       0.387
#> 10 UTSSU Men's         Adam Penman               3       0.333
#> 11 Canberra Heat Men's Jordan Power             28       0.286
#> 12 UTSSU Men's         Artem Ipatyev            19       0.211
#> 13 Canberra Heat Men's David Darcy              10       0.2  
#> 14 UTSSU Men's         Alex Minchin              1       0    
#> 15 UTSSU Men's         Mick Werner               2       0    
#> 16 UTSSU Men's         Samuel Hansen             3       0
```
