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

**How to use**  
Currently, just source, assign, and call the `outputCCF <- timesChain()`
function.  
This will chain together the lifeTimes workflow on a built in set of
default data and return calculated cross correlations.  
  
To plot cross correlations, just call `clusterPlot() <- timesChain()`  
To change the plot style just adjust the `plotType` argument.  
For example, `clusterPlot(, plotType = "rawTraces")`  

**Inputs**  
i. Evenly spaced series data (eg. can be time or space series)  
ii. Label of the measurements taken. (eg. can be shape or signal
intensity)  
iii. Labels for the objects to be compared (eg. cytoplasm and nuclear
compartments)  
iv. Higher level groupings for comparing objects (eg. compare objects
per cell/organism/treatment/community)  

**Outputs**  
i. Calculated correlations clustered by strength at lag zero  
ii. Calculated asymmetries between past and future lags, clustered by
strength of asymmetry  
iii. ‘Coupling plots’ representing the strength of correlation at lag
zero, and the direction of correlation  

**Output examples**  

**Fig 1** Calculated correlation clustered by strength at lag zero  
  
<img src="README_figs/README-clusteredCorrelations.png" style="width:50.0%" alt="an image caption Source: Ultimate Funny Dog Videos Compilation 2013." />

**Fig 2** Calculated asymmetries between past and future lags, clustered
by strength of asymmetry  
  
<img src="README_figs/README-clusteredCorrelationLags.png" style="width:50.0%" alt="an image caption Source: Ultimate Funny Dog Videos Compilation 2013." />

**Fig 3** ‘Coupling plots’ representing the strength of correlation at
lag zero, and the direction of correlation.  
  
<img src="README_figs/README-couplingPlot.png" style="width:50.0%" alt="an image caption Source: Ultimate Funny Dog Videos Compilation 2013." />
