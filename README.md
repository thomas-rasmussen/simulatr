
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulatr

<!-- badges: start -->

<!-- badges: end -->

## Overview

The goal of simulatr is to facilitate simulation of data for simulation
studies. Broadly speaking, the package facilitates the following
approach to data simulation:

**Estimation of parameters**

1)  Choose a very large number `N`, eg `N = 1e6`.

2)  Simulate `N` observations of correlated covariate data, where the
    distribution of each separate covariate is specified.

3)  Estimate parameter values in a logistic regression model that can be
    used to simulate a binary exposure variable, inducing a target
    proportion of exposed observations, and with the desired
    associations between covariates and exposure.

4)  Estimate parameter values in an outcome model that can be used to
    simulate an outcome variable, inducing a target conditional effect
    of exposure and covariates on the outcome, or a target marginal
    exposure effect.

5)  Optional: repeat 3) to estimate parameters needed to simulate a
    missingness indicator, that can be used to induce MCAR/MAR/MNAR
    missingness.

**Simulation of data**

6)  We now a have a set of chosen and very precisely estimated parameter
    values (if `N` is large) that can be used to simulate a large number
    of smaller datasets, where each simulated datasets with the desired
    properties that can be analyzed in a simulation study. Step 1) -5)
    can be repeated for each different set of parameter values that is
    of interest.

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

  - Implement remaining code from the simulater-test repository

  - Naming of functions. Names like sim\_indicator are not that
    intuitive, since we are not using the function to simulate the
    variable directly, but to estimate parameters so we can simulate the
    variable manually. est\_par\_indicator maybe? Whih begs the
    question: should separate wrapper functions be designed for the
    actual simulation? eg sim\_indicator to actually simulate an
    indicator variable based on input parameters? Generally unclear how
    to design the package at this point.

  - Example in README? Or makes vignettes?

  - Try implementing an inverse cumulative hazard function in both R and
    C++ and see how much work it is relative to how much efficiency is
    gained. It might make a lot of sense to try to implement (parts of)
    the package in C++ for efficiency gains.

  - When simulating a time-to-evnet outcome, should the function also be
    able to induce censoring? And how should that be done? Maybe
    something simple as providing a target proportion of observation to
    be censured, and then if a patient is censored it is censored at a
    point in timeaccording to a uniform distribution on the interval
    \[0; time-to-event\]?
