README
================
LGD
19/12/2021

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

## lifeTimes: correlations in biological series data

This is a package for **detecting** and **visualising** correlations between objects in biological series data.

## Citation

**lifeTimes** is available for everyone. If you find it useful for your research please cite the work that motivated its development: **Environmentally dependent and independent control of cell shape determination by Rho GTPase regulators in melanoma.** doi: <https://doi.org/10.1101/2021.10.11.463377>

## **Quick start**

Download the code for lifeTimes and run these commands to try the workflow on a built-in dataset

``` r
# Extract and unzip the code
# navigate into the top level directory 
# run installation:

install()

#copy and paste to run on test data
lts <- lts_input() #calculate cross correlation
lts_clusterPlot(lts) #plot clustered correlations
lts_leadLagClusterPlot(lts) #plot direction of correlations
lts_couplingPlot(lts) # plot strength and direction of correlation
```

## **How to use**

To calculate cross correlation, just load the package, assign, and call the `lts <- lts_input()` function. This will chain together the lifeTimes workflow on a built in set of default data and return calculated cross correlations.
<p>
To plot cross correlations, just call `clusterPlot(lts)`.

To change the plot style just adjust the `plotType` argument. The options are `c( "compoundPlot", "draw_treatmentDendrogram", "plt_dendr", "heatmapLagZero", "rawTraces", "clusteredLines")`. For example, `clusterPlot(ltc, plotType = "rawTraces")`

<p>
To plot clustered lead vs lag metrics which emphasise the direction of coupling, just call `leadLagClusterPlot(lts)`.

<p>
Finally, to plot coupling plots, incorporating both strength and direction of coupling, just call `couplingPlot(lts)`.

## **Examples**

Example data are yearly cross correlation functions (CCFs) of rainfall and river flow rates for two different rivers in the United Kingdom, the Ash and Thames rivers. CCFs are calculated and plotted separately based on two explanatory variables, season (summer, autumn, winter, spring), and catchment region (Mardock, Kingston).

**Fig 1** Calculated cross correlations clustered by strength at lag zero

<p>
<img src="man/figures/clusterPlots.png" alt="Fig 1A: example of clusterPlot(), output" style="width:60.0%" />
<p>
<p>
**Fig 2** Calculated asymmetries between past and future lags, clustered by strength of asymmetry

<p>
<img src="man/figures/leadLagPlot.png" alt="Fig 2: example of leadLagClusterPlot(), output" style="width:50.0%" />

<p>
<p>
**Fig 3** 'Coupling plots' representing the strength of correlation at lag zero, and the direction of correlation.

<p>
<img src="man/figures/couplingPlot.png" alt="Fig 3: example of couplingPlot(), output" style="width:40.0%" />

**Fig 4** Ariel images of the Ash, and Thames rivers.

<img src="man/figures/riverCatchments.png" alt="Fig 4: example of couplingPlot(), output" style="width:60.0%" />

## **Inputs**

1.  Evenly spaced series data (eg. can be time or space series)

2.  Label of the measurements taken. (eg. can be shape or signal intensity)

3.  Labels for the objects to be compared (eg. cytoplasm and nuclear compartments)

4.  Higher level groupings for comparing objects (eg. compare objects per cell/organism/treatment/community)

## **Outputs**

1.  Calculated correlations clustered by strength at lag zero  

2.  Calculated asymmetries between past and future lags, clustered by strength of asymmetry  

3.  'Coupling plots' representing the strength of correlation at lag zero, and the direction of correlation
