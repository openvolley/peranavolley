
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/openvolley/peranavolley.svg?branch=master)](https://travis-ci.org/openvolley/peranavolley)
![openvolley](https://img.shields.io/badge/openvolley-darkblue.svg?logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiNmZmYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+)

# peranavolley

An R package for reading [Perana Sports
VBStats](http://peranasports.com/software/vbstatshd/) volleyball
scouting files as well as [Perana Sports
VideoTagger](http://peranasports.com/software/videotagger/) files.

See also [datavolley](https://github.com/openvolley/datavolley), which
provides similar functionality for reading DataVolley files into R. The
conventions (data structures, variable naming, etc) used in the
peranavolley package follow those of the datavolley package.

## Installation

You can install peranavolley from GitHub with:

``` r
library(remotes)
install_github("openvolley/peranavolley")
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
#>  7 Canberra Heat Men's Keiran Ackhurst           5       0.4  
#>  8 UTSSU Men's         C Beasley                28       0.393
#>  9 UTSSU Men's         Adam Penman               3       0.333
#> 10 UTSSU Men's         Nathan Healey            31       0.323
#> 11 Canberra Heat Men's Jordan Power             28       0.214
#> 12 UTSSU Men's         Artem Ipatyev            19       0.211
#> 13 Canberra Heat Men's David Darcy              10       0.2  
#> 14 UTSSU Men's         Alex Minchin              1       0    
#> 15 UTSSU Men's         Mick Werner               2       0    
#> 16 UTSSU Men's         Samuel Hansen             3       0
```
