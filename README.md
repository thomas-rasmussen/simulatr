
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

## Design ideas/plans

Makes sense for each step/function to take an input dataset with data
that has been simulated so far (expect sim\_vars), and outputs a new
dataset with added simulated variables. Input/output could/should also
include the parameters that was used to simulate the data, and
parameters that are induced by the simulated data. This way an object
could be iteratively expanded to include simulated data and the
corresponding set/induced parameters. If the simulated data is very
large, the parameters set/induced by the simulated data is going to be
very precise, and can be used to simulate new (smaller datasets) where
the underlying true parameters are (almost) identical to those
parameters. This enables a two-step strategy where we first use one very
large dataset of simulated data to set/estimate “true” paramters, and
then we use these parameters to simulate many smaller datasets to be
used in the simulation study.

The functions described above would also enable more simpler task like
taking a single dataset and adding a simulated variable with exact
properties in that particular data, eg add an indicator variable with
the exact associations and proportions of the indicator that is needed.

So how is this done in a meaningful way? sim\_data could create list of
legnth 2, with elements “data” and “par”. Then each time sim\_xxx is
used, variables are added to the “data” part, and the corresponding
parameters to “par”. Objects could/should be given a class, eg
“simulatr”, so that OOP can be used to faciliate exploration of the
object, eg there should be a meaningful print method for objects with
class “simulatr” that prints the parameter part?

Objects of class “simulatr” could also be used as an input in a wrapper
function that takes the “par” part of the object and use it to simulate
datasets with the parameters.

Is it possible/reasonable to make “simulatr” objects that only store the
“par” part, so that we can effciently store parameters needed to
simulate data without carryig a huge “data” part that was only needed to
estimate the parameters in the first part?

How should the “par” part be structured? Should there be a sub-list for
each time something was added, eg when sim\_data is used to create the
object par has one element called sim\_data? But then what if sim\_xx is
used multiple times doing the process of setting/estimating parameters,
then there would be multiple elements with the same name. No bueno.
Sub-list name can (and should be) named as input parameters, so that
unique names can be given, eg sim\_vars, sim\_exp, sim\_miss\_ind,
sim\_out? If no names are given then just default to sim\_data,
sim\_data2, etc, if sim\_data has been used to simulate two sets of
variables that are independt of each other (can’t see why this would be
done so, but it might be relevant). Then when wrapper function is used
on “simulatr” object to simulate data, variables are simulated in the
order they appear in “par”.

So if “par” is to be used to automatically simulate new data, how should
things be stores in “par”. How does the function know how to handle
sublists in “par”? They are going to be very different. Maybe each
sublist needs to have a “type”, that tells the function how the info
needs to used to simulate data. Eg for sim\_data, par element needs a
type of “ovariates”, so it knows to look for inverse CDF functions for
each covariates together with a correlation matrix. Function can then
call the same lines of code that is used inside sim\_vars to simulate
data from the paraters. This means that the inner workings of sim\_vars
should be refactored, so that the actual clculations is done by calling
a function.

Start by just working with list element objects and figure out if
approach makes sense.

## TODO

-   Many/all of the utility/helper functions, eg is\_integer, are
    general purpose functions that are likely going to be reused in
    other packages/projects in the future. Probably better to make a
    tbrmisc package for such functions.

-   Implement remaining code from the simulater-test repository

-   Example in README? Or makes vignettes?

-   Try implementing an inverse cumulative hazard function in both R and
    C++ and see how much work it is relative to how much efficiency is
    gained. It might make a lot of sense to try to implement (parts of)
    the package in C++ for efficiency gains.

-   When simulating a time-to-event outcome, should the function also be
    able to induce censoring? And how should that be done? Maybe
    something simple as providing a target proportion of observation to
    be censured, and then if a patient is censored it is censored at a
    point in timeaccording to a uniform distribution on the interval
    \[0; time-to-event\]?
