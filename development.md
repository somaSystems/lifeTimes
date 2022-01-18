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
    <p>
3.  `dev_` prefix for internal functions that have been modified to help
    with development and testing.

## User exposed functions

There are 4 user exposed functions. One is for **user input**, and
reamaining three are **plotting functions** for lifeTimes output.

**User input function**  

<p>

`lts_inputs()` is a user input function, that then calls the rest of the
lifeTimes internal functions to return calculations. When called without
arguments this function runs on a default dataset, and ouputs calculated
cross correlations (CCFs). The function can also take user input and
output CCFs. There are two types of User input for `lts_inputs()`.
**Type 1:** is time series data for the parameter `.tsData =`. The
format is a “tidy” time series dataframe. Each row is an observation
(measurements from a unit of obversation at a particular timepoint) and
variables measurements are in columns. **Type 2:** is a set of labels
for the different variables in the time series so that these can be
mapped to arguments in the internal lifeTimes package functions. These
include: `.time`, `.compare_categorical`, `.pairedComparisons`,
`.uniqueID_colname`, and `.metaData`. These parameters are supplied to
the functions sourced in the script
`lts_internalFunctions_generalised.R`.  

<p>

Example of user input:

``` r
lts_input(.tsData = catchmentsAndRivers,
          .time = c("dayOfseason"),
          .compare_categorical = c("season","catchmentRegion"), #Categorical variables
          .pairedComparisons = list(pair_1 =list(x = "rainfall_cm", y = "flow_m3s")), #pairedVarCCF
          .uniqueID_colname = "key_num",
          .metaData = NULL)
```

**Plotting functions**  

<p>

There are three functions that plot the results of `lts_inputs()`.These
arguments take the data returned by `lts_input()`, as an argument. The
functions
are:

``` r
lts_clusterPlot()  # plots CCFs clustered by magnitude of correlation at the most correlated lag across the dataset

lts_leadLagClusterPlot()  # clusters and plots asymmetry in CCFs at negative and positive lags

lts_couplingPlot()  # combines strength of correlation and asymmetry in correlation into a single plot
```

## Internal functions

An overview of the purpose of the internal functions sourced in the
script
`lts_internalFunctions_generalised.R`.

``` r
lts_tsToWide() # converts "tidy" data into wide data, with each time series as a vector.  

lts_wide_ts_to_ccf() #calculated cross correlations between selected paris of wide time series.  

lts_ccf_df() #converts lists of calculated cross correlations into dataframe.  

lts_metaData_ccf_join() #matches categorical variables and metadata to cross correlations.  

lts_clusterCCFs() #clusters the calculated cross correlations, by chosen metrics.  

leadLagCorr_diffs() #calculates assymetries in cross correlations between negative and positive lags.  
```

## Internal data for testing functions during development

The structure of lifeTimes is currently a pipeline of data
transformations. The input of each function is output from the previous
function, which is then processed further. To make it easier to develop
and test each function in isolation, I have stored the intermediate
outpus from each function in the pipeline as an internal dataset. These
intermediate steps can be supplied as input to the function you would
like to develop. Following R’s syntax for accessing the internal
namespace of a package, these outputs can be accessed by specifying the
internal lifeTimes namespace with three colons “:::” eg.
`lifeTimes:::lts_OUT_cast_ts` gives the output from the `lts_tsToWide()`
function, that would be produced if it were called on the default built
in dataset.  

<p>

Catalogue of the internal functions, internal outputs, and examples of
their use as arguments to internal functions:

``` r
#after installing lifeTimes

library(lifeTimes)

#The following reproduces the chain of calls that occur when lts_inputs() is called on the default dataset:

lts_inputVars <- lifeTimes:::lts_defaultVariables #access and assign internal set of default input variables stored in lifeTimes namespace

lts_OUT_cast_ts <- lifeTimes:::lts_tsToWide(lts_inputVars) #access and call internal function, with argument created by internal data

lts_OUT_ccf_list_out <- lifeTimes:::lts_wide_ts_to_ccf(lts_OUT_cast_ts, .lts_variables = lts_inputVars) #output from previous functions is used as input for next

lts_OUT_dfccf <- lifeTimes:::lts_ccf_df(lts_OUT_ccf_list_out) # ...as above

lts_OUT_ccfWithMetaData_compareBy <- lifeTimes:::lts_metaData_ccf_join(lts_OUT_dfccf, .lts_variables = lts_inputVars) # ...as above

lts_OUT_clusterOutput <- lifeTimes:::lts_clusterCCFs(lts_OUT_ccfWithMetaData_compareBy, .lts_variables = lts_inputVars) # ...as above

lts_OUT_lts_clusterOutput_LAGranges <- lifeTimes:::lts_leadLagCorr_diffs(lts_OUT_clusterOutput, .lts_variables = lts_inputVars) #this is equivalent to the final output of lts_inputs(), the main user input function 
```

## Refactored code as of Jan 2022

This version of lifeTimes is refactored. The main function is now
designed to take use input. The core functions are written with the aim
to be able to take a range of user input, and adjust dynamically.

-----
