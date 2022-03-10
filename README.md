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

**Install lifeTimes**

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

## **Quick how to use**

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

## **Example data and Output**

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

**Quick notes on
    inputs**

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

**Quick notes on outputs** i. Plots of calculated correlations clustered
by strength at lag zero  
ii. Plots of calculated asymmetries between past and future lags,
clustered by strength of asymmetry  
iii. Plots representing both the strength of correlation at lag zero,
and the direction of correlation  
iv. Calculated summary statistics of CCFs that can be used in downstream
analysis (e.g to improve the performance of classification tasks).

## **Detailed examples**

**description of default data** lifeTimes makes it easy to detect
“coupling” between different components of biological systems from the
scale of cells, to organisms, to ecosystems. Along these lines,
lifeTimes comes with examples of:  
i.subcellular,  
ii.organism, and  
iii.landscape scale datasets.

**Dataset 1: Rainfall and river flow in the United Kingdom**

**Landscape scale data** lifeTimes can be used to find coupling between
processes at the landscape or ecosystem scale. For this example, I have
used data from the Unied Kingdom, National River Flow Archive
(<https://nrfa.ceh.ac.uk/web-download-service>). We will look at two
types of data, i) Gauged Daily Flows (GDF) which measure how much water
is in a river, and ii) Catchment Daily Rainfall (CDR) which measure the
amount of rain on the landscape around the river. River flows are in
cubic meters per second, and rainfall is in mm across the catchment in
the given reference period
(<https://nrfa.ceh.ac.uk/data-formats-types>).  

<p>

For this example I have downloaded data from the Thames river, at
Kingston (<https://nrfa.ceh.ac.uk/data/station/info/39001>), and the Ash
river, at Mardock <https://nrfa.ceh.ac.uk/data/station/info/38002>. I
have also subset the data between the years 1983 and 2017. This is a
period where both stations were operating, and recording measurements.  

<p>

After some data wrangling, these two datsets produce a dataframe, where
the first five observations look like this:

``` r
rain_flow <- read.csv(file ="data-raw/rain_flow_Thames_Ash.csv")
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
    ## 1          1       365                   0         0 summer           0
    ## 2          1       366                   1         1 summer           1
    ## 3          1       367                   2         2 summer           2
    ## 4          1       368                   3         3 summer           3
    ## 5          1       369                   4         4 summer           4
    ## 6          1       370                   5         5 summer           5
    ##                catchRep              uniqueID key_num
    ## 1 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 2 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 3 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 4 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 5 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83
    ## 6 year_1_Ash at Mardock 1Ash at Mardocksummer  key_83

Plotting the river flow and rainfall measures over a 1000 day interval
looks like this: ![Raw data example:](man/figures/raw_flow_rain.png)

The main funtion for user input in lifeTimes is `lts_in()`. To test that
lifeTimes is working you can run this function without any arguments.
This will run the program on a default set of data.

**Example 1**

``` r
library(lifeTimes) #load lifeTimes namespace

#use the lts_pairsMaker hrlper function 
#to prepare variables to be compared in a list of lists format 
lts_pairedVars <- lts_pairsMaker(c("rainfall_cm","flow_m3s"))
lts_pairedVars
```

    ## $rainfall_cm
    ## $rainfall_cm[[1]]
    ## [1] "rainfall_cm"
    ## 
    ## $rainfall_cm[[2]]
    ## [1] "flow_m3s"

``` r
#arguments for lts_in() with default inputs (the "river catchments "rain_flow" dataset) shown
lts_in(.in_tsData = rain_flow  , #1. the dataset
         .in_time = c("dayOfseason"), #2. the time column
       .in_compare_categorical =  c("season", "catchmentRegion"), #3. A vector, with the names of categorical columns
       .in_plot_measured_variables = FALSE , #4. Whether to CCFs for multiple pairs of variables 
       .in_pairedComparisons =  list(pair_1 = list(y = "rainfall_cm", 
        x = "flow_m3s")), #5. A list of lisys, holding the pairs of variables to be compared
       .in_uniqueID_colname = "key_num", #6. The column with unique ID name for your observations
       .in_metaData = NULL) #7. Column names of any attributes you would like to append to data
```

<p>

I will now give a walkthrough that explains how the arguments above, and
how to run lifeTimes, on your own data\! To make any data suitable for
lifeTimes, we need the raw data, and four labels, :  

<p>

**The Arguments**  

<p>

**1:`.in_tsDat =` A series dataframe:** Time series measurements. These
should have:  
i. at least two variables,  
ii. evenly spaced intervals,  
iii. Complete sets of observations (e.g NAs, can be imputed.).  
iv. Time series of equal length.  
v. At least two categorical variables (e.g Treatment vs Control)

TODO: lifeTimes will be compatible with single categorical variable, and
missing observations. A helper function to impute NAs will also be
included.

<p>

**2: `.in_time =` A unit of time:** So we need to find the column in the
dataset that indicates this. In this data we have years, months and
days, but the unit of time I am interested in is at the resolution of
days. The column in the datset with days is called `"dayOfseason"`. On
the first day of each season (summer, autumn, winter, spring) this value
starts at one and increments up until the end of the season.

<p>

**3: `.in_compare_categorical =` Categorical Variables:** This is where
you tell lifeTimes which colums hold the labels for categorical
variables. For example these might be experimental treatments or
conditions. Currently you can have 1 or 2 categorical variables. For the
rainflow dataset there are 2 columns with categorical variables, and
they are “season” (e.g a label that is ‘summer’, ‘autumn’, ‘winter’ or
‘spring’) and “catchmentRegion” e.g “Ash at Mardock” or “Thames at
Kingston”. LifeTimes will cluster your categorical data based on
correlations between measured variables.  

<p>

**4: `.in_plot_measured_variables =` Whether to plot multiple
variables?** This parameter takes a logical argument and can be either
“TRUE” or “FALSE”. The choice depends on whether you are analysing two
categorical variables, in which case set it to “FALSE”. If you are
studying one categorical variable but would like to calculate and plot
the cross correlations of multiple mesured variables, set this to
“TRUE”. A detailed explanation is below.  

<p>

Currently in lifeTimes, categorical and measured variables can be used
in two ways:  

<p>

1.  You can analyse 2 categorical variables and one pair of measured
    variables. This is shown with the “rain\_flow” dataset. The two
    categorical variables are “catchment” and “season”, and the pair of
    measured variables are “rainfall\_cm” and “flow\_m3s”.  
2.  alternatively, if you are using one categorical variable, but more
    than one pair of measured variablesyou can set this to
    `.in_plot_measured_variables = TRUE`  
    <p>

**5: `.in_pairedComparisons =` Pairs of variables to compare to one
another at different lags:** Under the hood, these are passed to the
lifeTimes internal functions in the format of a list of lists. There is
one master list, holding lists of paired variables to compare. In
practice, there is a helper fuction `lts_pairsMaker()`, that takes a
vector with a list of column names, and returns all possible non
redundant pairs of variables in this list. So you can give a list of
variables you would like to compare to the `lts_pairsMaker` function,
and assign the output to an object. Then just pass this object as an
argument to the `.in_pairedComparisons =` parameter (See example 1
above).  

<p>

TODO: In addition to iterating all possible combinations of pairings
between variables, lts\_pairsMaker will be adapte to be passed a table
of pre-specified pairings, and return these in list of list format.

**6:`.in_uniqueID_colname =` A unique identified for each thing we are
measuring:** We need to give lifeTimes the column namr which holds the
unique identifier for each observation. If there isn,t one, we can
create a column in the data frame with unique identifier for each
observation.  

<p>

In this dataset the column is called “key\_num”. In general, an unit of
observation is some unit of interest, measured over time. In this
dataset, it’s a river measuring station, in a given season, in a given
year. For example, “Ash river in summer in 1995”, or “Thames river in
winter of 2001”. So in this dataset I have grouped the data by,
River,Season and Year, and given a unique ID to each observation and
labelled the column `"key_num"`.  

<p>

TODO: Future updates to lifetimes will include a helper function that
inputs a set of columns defining a unique observation, and outputs
creates a new column of uniqueIDs.  

<p>

**7: `.in_metaData =` Column names of metadata:** This parameter takes
column names of any attributes you would like to append to data

© 2022 GitHub, Inc. Terms Privacy Security Status
