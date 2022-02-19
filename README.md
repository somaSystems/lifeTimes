README
================
LGD
19/12/2021

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![codecov](https://codecov.io/gh/somaSystems/lifeTimes/branch/main/graph/badge.svg?token=4LFWpvvLOq)](https://codecov.io/gh/somaSystems/lifeTimes)

[![test-coverage](https://github.com/somaSystems/lifeTimes/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/somaSystems/lifeTimes/actions/workflows/test-coverage.yaml)

## lifeTimes: correlations in biological series data

This is a package for **detecting** and **visualising** correlations
between objects in biological series data.

## Citation

**lifeTimes** is available for everyone. If you find it useful for your
research please cite the work that motivated its development:
**Environmentally dependent and independent control of cell
shdepre4cated)READNEAape determination by Rho GTPase regulators in
melanoma.** doi: <https://doi.org/10.1101/2021.10.11.463377>

## **Quick start**

Install lifeTimes

``` r
#install devtools if needed
if(!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install from github, using package access token
install_github("somaSystems/lifeTimes", 
auth_token = "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") 
```

Run lifeTimes on default data

``` r
#copy and paste to run on test data
library(lifeTimes)

lts <- lts_in() #calculate cross correlation

lts_plot_ccfs(lts) #plot clustered correlations

lts_plot_ClustSum(lts) #plot direction of correlations

lts_plot_coupled(lts) # plot strength and direction of correlation
```

## **How to use**

To calculate cross correlation, just load the package, assign, and call
the `lts <- lts_in()` function. This will chain together the lifeTimes
workflow on a built in set of default data and return calculated cross
correlations.

<p>

To plot cross correlations, just call `lts_plot_ccfs(lts)`.

<p>

To plot clustered lead vs lag metrics which emphasise the direction of
coupling, just call `lts_plot_ClustSum(lts)`.

<p>

Finally, to plot coupling plots, incorporating both strength and
direction of coupling, just call `lts_plot_coupled(lts)`.

## **Example data**

Example data are yearly cross correlation functions (CCFs) of rainfall
and river flow rates for two different rivers in the United Kingdom, the
Ash and Thames rivers. CCFs are calculated and plotted separately based
on two explanatory variables, season (summer, autumn, winter, spring),
and catchment region (Mardock, Kingston).

**Fig 1** Calculated cross correlations clustered by strength at lag
zero

<p>

![Fig 1A: example of clusterPlot(),
output](man/figures/clusterPlots.png)  

<p>

<p>

**Fig 2** Calculated asymmetries between past and future lags, clustered
by strength of asymmetry

<p>

![Fig 2: example of leadLagClusterPlot(),
output](man/figures/leadLagPlot.png)

<p>

<p>

**Fig 3** ‘Coupling plots’ representing the strength of correlation at
lag zero, and the direction of correlation.

<p>

![Fig 3: example of couplingPlot(),
output](man/figures/couplingPlot.png)

**Fig 4** Ariel images of the Ash, and Thames rivers.

![Fig 4: example of couplingPlot(),
output](man/figures/riverCatchments.png)

## **Inputs**

1.  ``` 
     Evenly spaced series data (eg. can be time or space series)
    ```

2.  ``` 
    Label of the measurements taken. (eg. can be shape or signal intensity)
    ```

3.  Labels for the objects to be compared (eg. cytoplasm and nuclear
    compartments)

4.  ``` 
    Higher level groupings for comparing objects (eg. compare objects per cell/organism/treatment/community)
    ```

## **Outputs**

1.  ``` 
     Calculated correlations clustered by strength at lag zero  
    ```

2.  ``` 
    Calculated asymmetries between past and future lags, clustered by strength of asymmetry  
    ```

3.  ‘Coupling plots’ representing the strength of correlation at lag
    zero, and the direction of correlation

© 2022 GitHub, Inc. Terms Privacy Security Status
