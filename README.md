README
================
LGD
9/01/2022

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Notes on developming lifeTimes

## **Refactored code**

This version of lifeTimes is refactored.
The main function is now designed to take use input.
The core functions are written with the aim to be able to take a range of user input, and adjust dynamically.

## **Description of functions**

The package currently has 4 user exposed functions.

**Inputs and calculations function** `lts_lifeTimesChain()` When called without arguments this function runs on a default dataset, and ouputs calculated cross correlations (CCFs). The function can also take user input and output CCFs.

There are two types of user input. The first is a series dataframe. The second, is a set of labels for the different variables in the dataframe, so that they can be mapped to arguments in the internal lifeTimes package functions. Internal functions called by `lts_lifeTimesInput()` are sourced in the script `"lts_wide_ccf_df_cluster_diff.R"`.

**Plotting functions** There are three functions that plot the results of `lts_lifeTimesChain()`. They are `lts_clusterPlot(), lts_leadLagClusterPlot(), lts_couplingPlot()`.

## **Conventions in code**

There are some loose conventions I am trying to use to help organise the code and make it easier to understand and debug.

1.  Functions and objects created by lifeTimes are prefixed by "lts\_" to distinguish them from objects created by R or other packages.
2.  Function arguments and variables in function created by lifeTimes, are prefixed by "." in the function arguments definition, and throughout the body of the function. The idea is that the "." prefix helps distinguish "algebraic variables" from "concrete""objects in the body of the function. Examples are ".lts\_variables". The "." has not special meaning it is an arbitrary symbol.
