README
================
LGD
13/01/2022

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Notes on developing lifeTimes

## Refactored code

This version of lifeTimes is refactored. The main function is now designed to take use input. The core functions are written with the aim to be able to take a range of user input, and adjust dynamically.

------------------------------------------------------------------------

## User exposed functions

There are 4 user exposed functions. One is for user input, and reamaining three are for plotting lifeTimes output.

**User input function** `lts_inputs()` Inputs and calculations function.When called without arguments this function runs on a default dataset, and ouputs calculated cross correlations (CCFs). The function can also take user input and output CCFs.

**Two types of User input**.
Type 1: A series dataframe.
Type 2: Labels for the different variables in the dataframe, so that they can be mapped to arguments in the internal lifeTimes package functions. Internal functions called by lts\_input() are sourced in the script "lts\_wide\_ccf\_df\_cluster\_diff.R".

**Plotting functions** There are three functions that plot the results of `lts_inputs()`.
They are:
i. lts\_clusterPlot()
ii. lts\_leadLagClusterPlot()
iii. lts\_couplingPlot()

## Internal functions

An overview of the purpose of each internal function:

`lts_tsToWide()` converts "tidy" data into wide data, with each time series as a vector. `lts_wide_ts_to_ccf()` calculated cross correlations between selected paris of wide time series. `lts_ccf_df()` converts lists of calculated cross correlations into dataframe. `lts_metaData_ccf_join()` matches categorical variables and metadata to cross correlations. `lts_clusterCCFs()` clusters the calculated cross correlations, by chosen metrics. `leadLagCorr_diffs()` calculates assymetries in cross correlations between negative and positive lags.

## Conventions in lifeTimes code

There are some loose conventions I am trying to use to help organise the code and make it easier to understand and debug.

Functions and objects created by lifeTimes are prefixed by "lts\_" to distinguish them from objects created by R or other packages. Function arguments and variables in function created by lifeTimes, are prefixed by "." in the function arguments definition, and throughout the body of the function. The idea is that the "." prefix helps distinguish "algebraic variables" from "concrete""objects in the body of the function. Examples are ".lts\_variables". The "." has not special meaning it is an arbitrary symbol.
