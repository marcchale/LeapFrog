LeapFrog
================
Huf
2/12/2020

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bjhufstetler/LeapFrog.svg?branch=master)](https://travis-ci.org/bjhufstetler/LeapFrog)
<!-- badges: end -->

## Overview

LeapFrog is a single agent metaheuristic search algorithm for solving
the Travelling Salesman Problem. LeapFrog can be used with
non-symmetrical distance matrices, coordinate matrices, or both. The
solution method can easily be expanded to solve for other types of
shortest-path problems.

Given a distance matrix, coordinate matrix, or both, the
`LeapFrog::ImportData()` function will create a LeapFrog class object
that can then be used with the LeapFrog algorithm function
`LeapFrog::LF()`.

## Parameters

The LeapFrog algorithm offers the following tunable parameters:

  - m: The number of games to be played by the algorithms \[1, inf)
  - r: The number of rounds in each game \[1, inf)
  - p: The percentage of available nodes removed in each round (0, 1\]
  - s: The accuracy of player landing positions in the first round of
    each game (0, 1\]
  - a: The decay rate of p as a game is played (0, 1\]
  - monitor: If TRUE, will print a status message after each game \[T,
    F\]

## Example

### Create LeapFrog Class Object

LeapFrog comes with 13 different known TSP instances, sourced from the
TSPLIB (Reinelt 1991). They can be called using

``` r
LeapFrogObject <- LeapFrog::GetData(1)
```

or by using the
function

``` r
LeapFrog::ImportData(distances = yourDistanceMatrix, coordinates = yourCoordinateMatrix)
```

### Run the LeapFrog Algorithm

Run the LeapFrog algorithm by passing the LeapFrog class object and any
parameters.

``` r
LeapFrogOutput <- LeapFrog::LF(LeapFrogObject, m = 10, r = 20, a = .5)
```

    ## LF | Game =     1 | Best = 2026
    ## LF | Game =     2 | Best = 2020
    ## LF | Game =     3 | Best = 2020
    ## LF | Game =     4 | Best = 2020
    ## LF | Game =     5 | Best = 2020
    ## LF | Game =     6 | Best = 2020
    ## LF | Game =     7 | Best = 2020
    ## LF | Game =     8 | Best = 2020
    ## LF | Game =     9 | Best = 2020
    ## LF | Game =    10 | Best = 2020

![](README_files/figure-gfm/LF-1.png)<!-- -->![](README_files/figure-gfm/LF-2.png)<!-- -->

## References

<div id="refs" class="references">

<div id="ref-reinelt1991tsplib">

Reinelt, Gerhard. 1991. “TSPLIB-a Travelling Salesman Problem Library.”
*ORSA Journal on Computing* 3 (4). INFORMS: 376–84.

</div>

</div>
