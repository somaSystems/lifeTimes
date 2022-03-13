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
**Environmentally dependent and independent control of cell shape
determination by Rho GTPase regulators in melanoma.** doi:
<https://doi.org/10.1101/2021.10.11.463377>

## **Quick start**

Install lifeTimes

``` r
#install devtools if needed
if(!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install dependency from github
install_github("jokergoo/ComplexHeatmap")

#install lifeTimes from github, using package access token
install_github("somaSystems/lifeTimes", 
auth_token = "<paste your github token as a string here>") 

# If you need a github token you make one with the commented code below:
# usethis::create_github_token() 
```

**Run lifeTimes on default data**

``` r
#copy and paste to run on test data
library(lifeTimes)

lts <- lts_in() #calculate cross correlation

lts_plot_ccfs(lts) #plot clustered correlations

lts_plot_ClustSum(lts) #plot direction of correlations

lts_plot_coupled(lts) # plot strength and direction of correlation
```

**description of default data** lifeTimes makes it easy to detect
coupling between different components of a biological systems from the
scale of cells, to organisms, to ecosystems. Along these lines,
lifeTimes comes loaded with subcellular and landscape scale datasets.

**Example one: Landscape scale data** lifeTimes can be used to find
coupling between processes at the landscape or ecosystem scale. For this
example, we can use data from the National River Flow Archive
(<https://nrfa.ceh.ac.uk/web-download-service>). We will look at two
types of data, i) Gauged Daily Flows (GDF), and ii) Catchment Daily
Rainfall (CDR). River flows are in cubic meters per second, and rainfall
is in mm across the catchment in the given reference period
(<https://nrfa.ceh.ac.uk/data-formats-types>).  
<p>
For this example I have downloaded data from the Thames river, at
Kingston (<https://nrfa.ceh.ac.uk/data/station/info/39001>), and the Ash
river, at Mardock <https://nrfa.ceh.ac.uk/data/station/info/38002>. I
have subset the data between the years 1983 and 2017. This is a period
where both stations were recording measurements.  
<p>

After some data wrangling, these two datsets produce the following csv:

``` r
rain_flow <- read.csv(file ="data-raw/rain_flow_Thames_Ash.csv")
colnames(rain_flow)
```

    ##  [1] "date"                "dateAsInteger"       "catchmentRegion"    
    ##  [4] "flow_m3s"            "rainfall_cm"         "year"               
    ##  [7] "month"               "day"                 "yearNumber"         
    ## [10] "dayNumber"           "Reset_SeasonYearDay" "dayOfYear"          
    ## [13] "season"              "dayOfseason"         "catchRep"           
    ## [16] "uniqueID"            "key_num"

``` r
head(rain_flow)
```

    ##         date dateAsInteger catchmentRegion flow_m3s rainfall_cm year month day
    ## 1 1983-06-01          4899  Ash at Mardock    4.720           3 1983     6   1
    ## 2 1983-06-02          4900  Ash at Mardock    0.756          12 1983     6   2
    ## 3 1983-06-03          4901  Ash at Mardock    0.543           8 1983     6   3
    ## 4 1983-06-04          4902  Ash at Mardock    0.450           0 1983     6   4
    ## 5 1983-06-05          4903  Ash at Mardock    0.392           0 1983     6   5
    ## 6 1983-06-06          4904  Ash at Mardock    0.363           0 1983     6   6
    ##   yearNumber dayNumber Reset_SeasonYearDay dayOfYear season dayOfseason
    ## 1          1       365                   0         1 summer           1
    ## 2          1       366                   1         2 summer           2
    ## 3          1       367                   2         3 summer           3
    ## 4          1       368                   3         4 summer           4
    ## 5          1       369                   4         5 summer           5
    ## 6          1       370                   5         6 summer           6
    ##                catchRep              uniqueID key_num
    ## 1 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 2 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 3 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 4 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 5 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 6 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83

To make data suitable for lifeTimes, we need the raw data, and four
labels:  
<p>

**1. A unique identified for each thing we are measuring: ** So we need
to find, or create a column in the data frame with unique identifier for
each observation: In this dataset the column is called “key\_num”. An
observation is some unit of interest, tracked over time. In this
dataset, it’s a river measuring station, in a given season, in a given
year. For example, “Ash river in summer in 1995”, or “Thames river in
winter of 2001”. So in this dataset I have grouped the data by,
River,Season and Year, and given a unique ID to each observation and
labelled the column the column “key\_num”.

**2. A unit of time:** So we need to find the column in the dataset that
indicates this. In this data we have years, months and days, but the
unit of time I am interested in is at the resolution of days. The column
in the datset with days is called “dayOfseason”. On the first day of
each season (summer, autumn, winter, spring) this value starts at one
and increments up until the end of the season.

**3. Pairs of variables to compare **

**landscape data**

**TIPS**

**creating a unique identifier**

**creating pairs of variables to compare**

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
<img src="man/figures/clusterPlots.png" style="width:60.0%" alt="Fig 1A: example of clusterPlot(), output" />  
<p>
<p>

**Fig 2** Calculated asymmetries between past and future lags, clustered
by strength of asymmetry

<p>

<figure>
<img src="man/figures/leadLagPlot.png" style="width:50.0%" alt="Fig 2: example of leadLagClusterPlot(), output" /><figcaption aria-hidden="true">Fig 2: example of leadLagClusterPlot(), output</figcaption>
</figure>

<p>
<p>

**Fig 3** ‘Coupling plots’ representing the strength of correlation at
lag zero, and the direction of correlation.

<p>

<figure>
<img src="man/figures/couplingPlot.png" style="width:40.0%" alt="Fig 3: example of couplingPlot(), output" /><figcaption aria-hidden="true">Fig 3: example of couplingPlot(), output</figcaption>
</figure>

**Fig 4** Ariel images of the Ash, and Thames rivers.

<figure>
<img src="man/figures/riverCatchments.png" style="width:60.0%" alt="Fig 4: example of couplingPlot(), output" /><figcaption aria-hidden="true">Fig 4: example of couplingPlot(), output</figcaption>
</figure>

## **Inputs**

1.   Evenly spaced series data (eg. can be time or space series)

2.  Label of the measurements taken. (eg. can be shape or signal intensity)

3.  Labels for the objects to be compared (eg. cytoplasm and nuclear
    compartments)

4.  Higher level groupings for comparing objects (eg. compare objects per cell/organism/treatment/community)

## **Outputs**

1.   Calculated correlations clustered by strength at lag zero  

2.  Calculated asymmetries between past and future lags, clustered by strength of asymmetry  

3.  ‘Coupling plots’ representing the strength of correlation at lag
    zero, and the direction of correlation

© 2022 GitHub, Inc. Terms Privacy Security Status
