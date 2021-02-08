
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulatr

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of simulatr is to facilitate simulation of data for simulation
studies. Broadly speaking, the package facilitates the following
approach to data simulation:

**Estimation of parameters**

1\) Choose a very large number `N`, eg `N = 1e6`.

2\) Simulate `N` observations of (correlated) covariate data, where the
distribution of each separate covariate, and a correlation matrix is
specified.

3\) Using the simulated data from 1), estimate parameter values in a
logistic regression model that can be used to simulate a binary exposure
variable, inducing a target proportion of exposed observations, and with
the desired associations between covariates and exposure.

4\) Using the expanded simulated data from 3), estimate parameter values
in an outcome model that can be used to simulate an outcome variable,
inducing a target conditional effect of exposure and covariates on the
outcome, or a target marginal exposure effect.

5\) Optional: repeat 3) to estimate parameters needed to simulate a
missingness indicator, that can be used to induce MCAR/MAR/MNAR
missingness.

**Simulation of data**

6\) We now a have a set of chosen and very precisely estimated parameter
values (if `N` is large) that can be used to simulate a large number of
smaller datasets, where each simulated dataset has the desired
properties and can be analyzed in a simulation study. Step 1) -5) can be
repeated for each different set of parameter values that is of interest.

## Installation

You can install the development version of simulatr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thomas-rasmussen/simulatr")
```

## Example

Provide a “simple” example here? Or is references to vignettes going to
be needed because of the complexity of using the package?

## TODO

-   Implement remaining code from the simulater-test repository

-   Design of functions. Should each major function take data from the
    prior step and output an list(dat, par)? Include functionality so
    that only dat or par is done in output for efficiency gains. So when
    we simulating data as described we would use the fucntions for the
    par parameter, and then manually simulate many small datasets in the
    end using all the estimated parametrs, but the functions could also
    be used for their dat output, egif you have data and you simply want
    to add a new variable to it with some exact properties. This kind of
    flexibility might be useful for other users/scenarios where we are
    only working with a single dataset that is used for something more
    simple than a formal simulation study? This approach would also kind
    of standardize how all the main fucntions of the package work in an
    intuitive way?

-   Example in README? Or makes vignettes?

-   Try implementing an inverse cumulative hazard function in both R and
    C++ and see how much work it is relative to how much efficiency is
    gained. It might make a lot of sense to try to implement (parts of)
    the package in C++ for efficiency gains.

-   When simulating a time-to-evnet outcome, should the function also be
    able to induce censoring? And how should that be done? Maybe
    something simple as providing a target proportion of observation to
    be censured, and then if a patient is censored it is censored at a
    point in timeaccording to a uniform distribution on the interval
    \[0; time-to-event\]?
