README
================
LGD
19/12/2021

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

## lifeTimes: correlations in biological series data

This is a package for **detecting** and **visualising** correlations
between objects in biological series data.

## Citation

**lifeTimes** is available for everyone. If you find it useful for your
research please be so kind as to cite the work that motivated its
development: **Environmentally dependent and independent control of cell
shape determination by Rho GTPase regulators in melanoma** doi:
<https://doi.org/10.1101/2021.10.11.463377>

## **How to use** 

To calculate cross correlation, just load the package, assign, and call
the `outputCCF <- lifeTimesChain()` function. This will chain together
the lifeTimes workflow on a built in set of default data and return
calculated cross correlations.<br>
<p>

To plot cross correlations, just call `clusterPlot(outputCCF)` <br> To
change the plot style just adjust the `plotType` argument. <br> For
example, `clusterPlot(outputCCF, plotType = "rawTraces")` <br>

## **Inputs** 

1.   Evenly spaced series data (eg. can be time or space series)

2.  Label of the measurements taken. (eg. can be shape or signal intensity)

3.  Labels for the objects to be compared (eg. cytoplasm and nuclear
    compartments)

4.  Higher level groupings for comparing objects (eg. compare objects per cell/organism/treatment/community)

## **Outputs** 

1.   Calculated correlations clustered by strength at lag zero\n

2.  Calculated asymmetries between past and future lags, clustered by strength of asymmetry\n

3.  ‘Coupling plots’ representing the strength of correlation at lag
    zero, and the direction of correlation

## **Examples** 

**Fig 1** Calculated correlation clustered by strength at lag zero  
<p>

<figure>
<img src="README_figs/README-clusteredCorrelations.png" style="width:50.0%" alt="Fig 1: An example of clusterPlot(), output" /><figcaption aria-hidden="true">Fig 1: An example of clusterPlot(), output</figcaption>
</figure>

**Fig 2** Calculated asymmetries between past and future lags, clustered
by strength of asymmetry  
<p>

<figure>
<img src="README_figs/README-clusteredCorrelationLags.png" style="width:50.0%" alt="Fig 2: An example of leadLagClusterPlot(), output" /><figcaption aria-hidden="true">Fig 2: An example of leadLagClusterPlot(), output</figcaption>
</figure>

**Fig 3** ‘Coupling plots’ representing the strength of correlation at
lag zero, and the direction of correlation.  
<p>

<figure>
<img src="README_figs/README-couplingPlot.png" style="width:50.0%" alt="Fig 3: An example of couplingPlot(), output" /><figcaption aria-hidden="true">Fig 3: An example of couplingPlot(), output</figcaption>
</figure>
