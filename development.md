README
================
LGD
13/01/2022

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Notes on developing lifeTimes

## Conventions in lifeTimes code

There are some loose conventions I am trying to use to help organise the
code and make it easier to understand and debug.  

<p>

1.  `lts_` prefix. Functions and objects created by lifeTimes are
    (mostly) prefixed by “lts\_” to distinguish them from objects
    created by R or other packages.  
    <p>
2.  `.` prefix for function arguments and variables. This is intended to
    distinguish “algebraic variables” from “concrete” objects in the
    body of the function. Examples are “.lts\_variables”. The “.” has
    not special meaning it is an arbitrary symbol.

## User exposed functions

There are 5 user exposed functions. `lts_in()` is for **user input**,
`lts_pairsMaker()` is a helper function for generating a list of sets of
**paired variables** that a user can then input to ‘lts\_in()’, and
reamaining three are **plotting functions** for lifeTimes output.

**User input function**  

<p>

`lts_in()` is the main user input function, which calls the rest of the
lifeTimes internal functions to return calculated cross correlations
(CCFs) and summary statistics of CCFs. When called without arguments
this function runs on a default dataset. There are two types of User
input for `lts_in()`. **1. User data input:** Is the data. Time series
data for the parameter `.tsData =`. The format is a “tidy” series
dataframe. Each row is an observation (measurements from a unit of
obversation at a particular timepoint). Each column is a variables
measurements or categorical metadata. **2. User label input:** Users
supply labels for the different variables in the time series so that
these can be mapped to arguments in the internal functions of the
lifeTimes package. These include: `.in_time`, `.in_compare_categorical`,
`.in_pairedComparisons`.  

<p>

Example of user input:

``` r
lts_in(.in_tsData = catchmentsAndRivers,
                       .in_time = c("dayOfseason"),
                       .in_compare_categorical = c("season","catchmentRegion"), #Categorical variables
                       .in_plot_measured_variables = FALSE,
                       .in_pairedComparisons = list(
                         pair_1 =list(y ="rainfall_cm",x ="flow_m3s")), #pairedVarCCF
                       .in_uniqueID_colname = "key_num",
                       .in_metaData = NULL)
```

**Plotting functions**  

<p>

There are three functions that plot the results of `lts_in()`.These
functions are prefixed by `lts_plot`. Each function takes the data
returned by `lts_in()`, as an argument. The functions are:

``` r
lts_plot_ccfs() #plot clustered correlations

lts_plot_ClustSum() #plot direction of correlations

lts_plot_coupled() # plot strength and direction of correlation
```

## Internal functions

An overview of the purpose of the internal functions sourced in the
script `lts_in()` function.

``` r
 lts_tsToWide(lts_inputVars) %>%
  lts_wide_ts_to_ccf(.lts_variables = lts_inputVars) %>%
  lts_ccf_df(.lts_variables = lts_inputVars) %>%
  lts_metaData_ccf_join(.lts_variables = lts_inputVars) %>%
  lts_summarise_ccf(.lts_variables = lts_inputVars) %>%
  lts_cluster_ccf_summs(.lts_variables = lts_inputVars) -> lts_Output


lts_tsToWide() # converts "tidy" data into wide data, with each time series as a vector.  

lts_wide_ts_to_ccf() #calculated cross correlations between selected paris of wide time series.  

lts_ccf_df() #converts lists of calculated cross correlations (CCFs) into dataframe.  

lts_metaData_ccf_join() #matches categorical variables and metadata to cross correlations.  

lts_summarise_ccf() #generates summary statistics of CCFs.  

lts_cluster_ccf_summs() #clusters summary statistics of CCFs with categorical variables as rows and columns.  
```

## Internal data for testing functions during development

The structure of lifeTimes is currently a pipeline of data
transformations. The input of each function is output from the previous
function, which is then processed further. To make it easier to develop
and test each function in isolation, intermediate outputs from each
function in the pipeline can be accessed and their output stored. These
intermediate steps can be examined and supplied directly as input to the
function you would like to test or develop. Following R’s syntax for
accessing the internal namespace of a package, these outputs can be
accessed by specifying the internal lifeTimes namespace with three
colons “:::” eg. `lifeTimes:::lts_tsToWide` gives the output from the
`lts_tsToWide()` function, that would be produced if it were called on
the default built in dataset.  

<p>

Catalogue of the internal functions, internal outputs, and examples of
their use as arguments to internal functions:

``` r
#After installing lifeTimes

#The following reproduces the chain of calls that occur when lts_inputs() is called on the default dataset:

lts_inputVars <- lifeTimes:::lts_input() #generate default input data

lts_wide <- lifeTimes:::lts_tsToWide(lts_inputVars) #pass default input to function, to make default input data wider

lts_ccf <- lifeTimes:::lts_wide_ts_to_ccf(lts_wide, .lts_variables = lts_inputVars) #pass wider

lts_ccf_df <- lifeTimes:::lts_ccf_df(lts_ccf, .lts_variables = lts_inputVars) # ...as above

lts_ccf_meta <- lifeTimes:::lts_metaData_ccf_join(lts_ccf_df, .lts_variables = lts_inputVars) # ...as above

lts_ccf_sums <- lifeTimes:::lts_summarise_ccf(lts_ccf_meta, .lts_variables = lts_inputVars) # ...as above

lts_clust_ccf_sums <- lifeTimes:::lts_cluster_ccf_summs(lts_ccf_sums, .lts_variables = lts_inputVars) #this is equivalent to the final output of lts_inputs(), the main user input function 
```

## Refactored code as of Jan 2022

This version of lifeTimes is refactored. The main function is now
designed to take use input. The core functions are written with the aim
to be able to take a range of user input, and adjust dynamically.
