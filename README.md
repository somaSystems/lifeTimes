README
================
LGD
17/12/2021

-   [**Quick start**](#quick-start)
-   [**Detailed examples**](#detailed-examples)
-   [**Dataset 1: Rainfall and river
    flow**](#dataset-1-rainfall-and-river-flow)
    -   [**Two categorical variable with single pair of measured
        variables**](#two-categorical-variable-with-single-pair-of-measured-variables)
-   [**Dataset 2: Cell and nucleus**](#dataset-2-cell-and-nucleus)
    -   [**Single categorical variable with multiple measured
        variables**](#single-categorical-variable-with-multiple-measured-variables)
-   [**Downstream analysis: Rainfall and river
    flow**](#downstream-analysis-rainfall-and-river-flow)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![codecov](https://codecov.io/gh/somaSystems/lifeTimes/branch/main/graph/badge.svg?token=4LFWpvvLOq)](https://codecov.io/gh/somaSystems/lifeTimes)

[![test-coverage](https://github.com/somaSystems/lifeTimes/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/somaSystems/lifeTimes/actions/workflows/test-coverage.yaml)
<p>

<img src="man/figures/lifeTimesLogo.png" style="width:40.0%" />

This is a package for **detecting** and **visualising** correlations
between objects in biological series data.

**Citation**

**lifeTimes** is available for everyone. If you find it useful for your
research please cite the work that motivated its development:
**Environmentally dependent and independent control of cell shape
determination by Rho GTPase regulators in melanoma.** doi:
<https://doi.org/10.1101/2021.10.11.463377>

## **Quick start**

\#**Install lifeTimes**

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
```

**Visualise results from lifeTimes on default data**

``` r
lts_plot_ccfs(lts) #plot clustered correlations
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
lts_plot_ClustSum(lts) #plot direction of correlations
```

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
lts_plot_coupled(lts) # plot strength and direction of correlation
```

![](README_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

**Fig 1** Ariel images of the Ash, and Thames rivers.

<figure>
<img src="man/figures/riverCatchments.png" style="width:60.0%" alt="Fig 1: Ariel images of the Ash, and Thames rivers" /><figcaption aria-hidden="true"><strong>Fig 1:</strong> Ariel images of the Ash, and Thames rivers</figcaption>
</figure>

**Quick summary on inputs**  
i. Evenly spaced series data (eg. can be time or space series)  
ii. Label of the measurements taken. (eg. can be shape or signal
intensity)  
iii. Labels for the objects to be compared (eg. cytoplasm and nuclear
compartments)  
iv. Higher level groupings for comparing objects (eg. compare objects
per cell/organism/treatment/community)

**Quick notes on outputs**  
i. Plots of calculated correlations clustered by strength at lag zero  
ii. Plots of calculated asymmetries between past and future lags,
clustered by strength of asymmetry  
iii. Plots representing both the strength of correlation at lag zero,
and the direction of correlation  
iv. Calculated summary statistics of CCFs that can be used in downstream
analysis (e.g to improve the performance of classification tasks)

## **Detailed examples**

**description of default data** lifeTimes makes it easy to detect
“coupling” between different components of biological systems from the
scale of cells, to organisms, to ecosystems. Along these lines,
lifeTimes comes with examples of:  
i.landscape scale datasets, ii.subcellular scale datasets.

## **Dataset 1: Rainfall and river flow**

**Landscape scale data** lifeTimes can be used to find coupling between
processes at the landscape or ecosystem scale. For this example, I have
used data from the United Kingdom, National River Flow Archive
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

After some data wrangling, these two datsets produce a csv which I read
into a dataframe. The first five observations look like this:

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
looks like this:
<img src="man/figures/raw_flow_rain.png" style="width:100.0%" alt="Raw data example:" />

The main funtion for user input in lifeTimes is `lts_in()`. To test that
lifeTimes is working you can run this function without any arguments.
This will run the program on a default set of data.However, it is more
interesting to run lifeTimes on your own data. Below, I have included a
worked example of how to use run lifeTimes on a dataset. For this
example, I am still usingthe rain\_flow dataset, except that the steps
to run this data in `lts_in()` are now clearly shown.

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

### **Two categorical variable with single pair of measured variables**

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
I will now give a walkthrough that explains the arguments above, and how
to run lifeTimes, on your own data! To make any data suitable for
lifeTimes, we need the raw data, and four types labels which identify
the “time”,“unique ID”, “categorical variable” and “measured variable”
columns. There is also an option to identify metadata:  
<p>
**The Arguments**  
<p>

**1:`.in_tsData =` A series dataframe:** Time series measurements. These
should have:  
i. at least two variables,  
ii. evenly spaced intervals,  
iii. Complete sets of observations (e.g NAs, can be imputed).  
iv. Time series of equal length.  
v. At least two categorical variables (e.g Treatment vs Control)

TODO: lifeTimes will be compatible with single categorical variable, and
missing observations. A helper function to impute NAs will also be
included.

**2: `.in_time =` A unit of time:** So we need to find the column in the
dataset that indicates this. In this data we have years, months and
days, but the unit of time I am interested in is at the resolution of
days. The column in the datset with days is called `"dayOfseason"`. On
the first day of each season (summer, autumn, winter, spring) this value
starts at one and increments up until the end of the season.

**3: `.in_compare_categorical =` Categorical Variables:** This is where
you tell lifeTimes which colums hold the labels for categorical
variables. For example these might be experimental treatments or
conditions. Currently you can have 1 or 2 categorical variables. For the
rainflow dataset there are 2 columns with categorical variables, and
they are “season” (e.g a label that is ‘summer’, ‘autumn’, ‘winter’ or
‘spring’) and “catchmentRegion” e.g “Ash at Mardock” or “Thames at
Kingston”. LifeTimes will cluster your categorical data based on
correlations between measured variables.

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
above). In addition to iterating all possible combinations of pairings
between variables, lts\_pairsMaker can be passed a table of
pre-specified pairings, and return these in list of list format (this is
demonstrated in the examples below).

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

**7: `.in_metaData =` Column names of metadata:** This parameter takes
column names of any attributes you would like to append to data

## **Dataset 2: Cell and nucleus**

**Subcellular scale data** We have seen that lifeTimes can be used to
detect the coupling between lanscape scale processes such as rainfall
and riverflow, and to highlight how this coupling varies between seasons
and geographical regions. In this next example, I am using lifeTimes to
look at coupling between the overall geometry of a living cell, and the
nucleus compartment within that cell. For this example I have used data
from melanoma cells imaged in a 3D collagen matrix, treated with
different drugs that affect cell function. The data here are just 30
cells (5 cells for each drug treatment), sampled for demonstration
purposes, from a larger dataset.  
<p>

First I read in the ‘sampleCells.csv’ data, that is included in the
lifeTimes package.

``` r
###start example
lts_cells <- read.csv(file = "data-raw/sampleCells.csv")
```

<p>

Next, I look at the column names to see the categorical variables, and
measured variables in the dataset

``` r
#look at colum names
colnames(lts_cells)
```

    ##  [1] "Volume_cell"                "EquivDiameter"             
    ##  [3] "SurfaceArea_cell"           "MeanIntensity"             
    ##  [5] "MinIntensity"               "MaxIntensity"              
    ##  [7] "MajorAxis_cell"             "MinorAxis_cell"            
    ##  [9] "secondMajor_cell"           "Eccentricity_cell"         
    ## [11] "secondEccentricity_cell"    "Sphericity_cell"           
    ## [13] "Vol2surf_cell"              "runNumber"                 
    ## [15] "timeTrack"                  "yDim"                      
    ## [17] "xDim"                       "zDim"                      
    ## [19] "xCoord_cell"                "yCoord_cell"               
    ## [21] "zCoord_cell"                "Orbit"                     
    ## [23] "AngleBetween"               "serialNumber"              
    ## [25] "nProtrusions_cell"          "CoverslipPresent"          
    ## [27] "CoverslipDistance"          "OnCoverslip"               
    ## [29] "fieldNumber"                "Treatment"                 
    ## [31] "Row"                        "Column"                    
    ## [33] "Polarity_cell"              "Spreading_cell"            
    ## [35] "Protrusivity_cell"          "cellNumber"                
    ## [37] "NucleusVolumeFraction"      "Volume_nucleus"            
    ## [39] "SurfaceArea_nucleus"        "MajorAxis_nucleus"         
    ## [41] "MinorAxis_nucleus"          "secondMajor_nucleus"       
    ## [43] "Eccentricity_nucleus"       "secondEccentricity_nucleus"
    ## [45] "Sphericity_nucleus"         "Vol2surf_nucleus"          
    ## [47] "xCoord_nucleus"             "yCoord_nucleus"            
    ## [49] "zCoord_nucleus"             "nProtrusions_nucleus"      
    ## [51] "Polarity_nucleus"           "Spreading_nucleus"         
    ## [53] "Protrusivity_nucleus"       "Plate"                     
    ## [55] "cluster"

Here, I can see that `"runNumber"`, will be the column with the time
information, and `"cellNumber"` will be the unique ID for each
observation. `"Treatment"` is a categorical variable.
<p>
For this dataset, I am only interested in one categorical variable,
called “Treatment”. This variable describes the drug that each cell has
been exposed to. This is in contrast to the “rain\_flow” dataset
(example 1 above) where there were two categorical variables of inerest
(“season” and “catchmentRegion”).  
<p>
Also, I can see that there are many measured variables for both the cell
and nucleus that I could compare (eg. “Polarity\_cell”,
“Eccentricity\_nucleus”, “Volume\_cell” etc.) . This is in contrast to
the “rain\_flow” dataset where there were only two measured variables.  
<p>

**User defined paired comparisons** For this dataset, instead of using
`lts_pairsMaker` to permute all the possible comparisons between
measured variables, I will specifically define the variables to compare.
In the cell dataset, we can use some domain specific knowledge to
recognise that comparing the coupling of geometry between the cell and
nucleus under different drug treatments is meaningful and interesting.
To make this comparisons, I have just listed some of the features I
would like to directly compare, and then joined this into a matrix that
I named `my_pairs` by using `rbind()`:

``` r
#choose three pairs of column names
pair1 <- c("Polarity_cell","Polarity_nucleus")
pair2 <- c("Eccentricity_cell", "Eccentricity_nucleus")
pair3 <- c("Volume_cell", "Volume_nucleus")

# use rbind to make these pairs into a matrix
my_pairs <- rbind(pair1,pair2,pair3)
```

<p>

I passed `my_pairs` to `lts_pairsMaker` with `defined = TRUE`, to create
a list of specific paired comparisons that I would like to make. This
contrasts with the less default use of `lts_pairsMaker`, where a list of
column names is entered, and all possible non-redundant combinations of
these are returned.

``` r
#make these into list of pairs using helper function with "defined = TRUE"
lts_pairs <- lts_pairsMaker(my_pairs, defined = TRUE)
```

<p>

### **Single categorical variable with multiple measured variables**

The data, relevant column names and list of pairs can all be entered
into `lts_in()`. The important and interesting difference between this
example and the “rain\_flow” example, is that we are now using a
**single categorical variable**, “Treatment”. In this situation you must
set `.in_plot_measured_variables = TRUE`. This means lifeTimes will
cluster and plot the different measured variables.  
<p>

TODO: Improve UI by either removing `.in_plot_measured_variables = TRUE`
argu,ent, and making it automatic when only one categorical is entered,
OR make it possible to enter only one categorical, while
`.in_plot_measured_variables = FALSE`.

``` r
# enter arguments into lts_in
# here there is one categorical variable and multiple measured variables
lts_oneCat <- lts_in(.in_tsData = lts_cells,
                     .in_compare_categorical = "Treatment",
                     .in_time = "runNumber",
                     .in_plot_measured_variables = TRUE,
                     .in_pairedComparisons = lts_pairs,
                     .in_uniqueID_colname = "cellNumber",
                     .in_metaData = NULL )
```

<p>

After running `lts_in()`, the output can be plotted with the three
lifeTimes plotting functions. Examples are below:

``` r
#View results
lts_plot_ccfs(lts_oneCat)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
lts_plot_ClustSum(lts_oneCat)
```

![](README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
<p>

This example of `lts_plot_coupled()` shows two plots of the same data,
and how the colouring and faceting variables can be switched with the
`.lts_colour_by =` and \`.lts\_facet\_by =\`\`\` parameters.

``` r
lts_plot_coupled(lts_oneCat, 
                 .lts_colour_by = "cat1", 
                 .lts_facet_by = "cat2")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
lts_plot_coupled(lts_oneCat, 
                 .lts_colour_by = "cat2", 
                 .lts_facet_by = "cat1")
```

![](README_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

## **Downstream analysis: Rainfall and river flow**

Cross correlations, or lagged correlations between features have many
uses. Two of these are:  
i. generating hypotheses about cause and effect between parts of a
system  
ii. improving the results of classification tasks indynamic datasets  
<p>

The code below shows how you can access summary statistics about each
ovbservation, for use in downstream tasks.

``` r
#load lideTimes
library(lifeTimes)

#run lifeTimes
lts_demo <- lts_in()
```

``` r
#access the summary statistics output from lifeTimes
#there are a range of these in a list called "lts_ccf_summaries"

#ccf summary statistics are stored at the level of individual observations 
#these are in "lts_singleton_summ_metadata"
#here is an example ofthe first 9 summaries
head(lts_demo$lts_ccf_summaries$lts_singleton_summ_metadata, 9)
```

    ##   key_num   lag_range mean_corr_by_lag_range max_corr_by_lag_range
    ## 1   key_1 negativeLAG            -0.03899845            0.09724116
    ## 2   key_1 positiveLAG             0.02162797            0.73764478
    ## 3   key_1     zeroLAG             0.15692657            0.15692657
    ## 4  key_10 negativeLAG            -0.06618308            0.18799905
    ## 5  key_10 positiveLAG             0.16726856            0.55213014
    ## 6  key_10     zeroLAG             0.19257648            0.19257648
    ## 7 key_100 negativeLAG            -0.02128817            0.25414531
    ## 8 key_100 positiveLAG             0.08202752            0.59733411
    ## 9 key_100     zeroLAG             0.15889929            0.15889929
    ##   min_corr_by_lag_range var_corr_by_lag_range sd_corr_lag_by_range
    ## 1           -0.10770390           0.002185072           0.04674475
    ## 2           -0.11536337           0.041744477           0.20431465
    ## 3            0.15692657                    NA                   NA
    ## 4           -0.21163487           0.016040207           0.12664994
    ## 5           -0.01048577           0.023601916           0.15362915
    ## 6            0.19257648                    NA                   NA
    ## 7           -0.12270161           0.008115395           0.09008549
    ## 8           -0.11054365           0.030432511           0.17444916
    ## 9            0.15889929                    NA                   NA
    ##   median_corr_by_lag_range mean_corr_by_lag_range_maxLag_0.25
    ## 1              -0.04764419                        -0.06310762
    ## 2              -0.04439647                         0.27158059
    ## 3               0.15692657                         0.15692657
    ## 4              -0.10506722                         0.08492328
    ## 5               0.13816208                         0.35193834
    ## 6               0.19257648                         0.19257648
    ## 7              -0.02960596                        -0.05110460
    ## 8               0.07451034                         0.34682505
    ## 9               0.15889929                         0.15889929
    ##   max_corr_by_lag_range_maxLag_0.25 min_corr_by_lag_range_maxLag_0.25
    ## 1                       -0.01969124                      -0.107703901
    ## 2                        0.73764478                      -0.057178837
    ## 3                        0.15692657                       0.156926569
    ## 4                        0.18799905                       0.004409065
    ## 5                        0.55213014                       0.166652559
    ## 6                        0.19257648                       0.192576482
    ## 7                        0.01341341                      -0.095055712
    ## 8                        0.59733411                       0.219826212
    ## 9                        0.15889929                       0.158899293
    ##   var_corr_by_lag_range_maxLag_0.25 sd_corr_lag_by_range_maxLag_0.25
    ## 1                       0.001937601                       0.04401819
    ## 2                       0.172075594                       0.41481995
    ## 3                                NA                               NA
    ## 4                       0.008808088                       0.09385142
    ## 5                       0.037314883                       0.19317061
    ## 6                                NA                               NA
    ## 7                       0.003258636                       0.05708446
    ## 8                       0.047069134                       0.21695422
    ## 9                                NA                               NA
    ##   median_corr_by_lag_range_maxLag_0.25   mean_corr  max_corr   min_corr
    ## 1                          -0.06192772 -0.00366670 0.7376448 -0.1153634
    ## 2                           0.13427582 -0.00366670 0.7376448 -0.1153634
    ## 3                           0.15692657 -0.00366670 0.7376448 -0.1153634
    ## 4                           0.06236172  0.05484679 0.5521301 -0.2116349
    ## 5                           0.33703232  0.05484679 0.5521301 -0.2116349
    ## 6                           0.19257648  0.05484679 0.5521301 -0.2116349
    ## 7                          -0.07167151  0.03426451 0.5973341 -0.1227016
    ## 8                           0.22331483  0.03426451 0.5973341 -0.1227016
    ## 9                           0.15889929  0.03426451 0.5973341 -0.1227016
    ##     var_corr   sd_corr  median_corr mean_corr_maxLag_0.25 max_corr_maxLag_0.25
    ## 1 0.02234200 0.1494724 -0.047202941             0.1117636            0.7376448
    ## 2 0.02234200 0.1494724 -0.047202941             0.1117636            0.7376448
    ## 3 0.02234200 0.1494724 -0.047202941             0.1117636            0.7376448
    ## 4 0.03281848 0.1811587  0.040444165             0.2147373            0.5521301
    ## 5 0.03281848 0.1811587  0.040444165             0.2147373            0.5521301
    ## 6 0.03281848 0.1811587  0.040444165             0.2147373            0.5521301
    ## 7 0.02123847 0.1457342  0.003918053             0.1494372            0.5973341
    ## 8 0.02123847 0.1457342  0.003918053             0.1494372            0.5973341
    ## 9 0.02123847 0.1457342  0.003918053             0.1494372            0.5973341
    ##   min_corr_maxLag_0.25 var_corr_maxLag_0.25 sd_corr_maxLag_0.25
    ## 1         -0.107703901           0.08640505           0.2939474
    ## 2         -0.107703901           0.08640505           0.2939474
    ## 3         -0.107703901           0.08640505           0.2939474
    ## 4          0.004409065           0.03329408           0.1824666
    ## 5          0.004409065           0.03329408           0.1824666
    ## 6          0.004409065           0.03329408           0.1824666
    ## 7         -0.095055712           0.05638033           0.2374454
    ## 8         -0.095055712           0.05638033           0.2374454
    ## 9         -0.095055712           0.05638033           0.2374454
    ##   median_corr_maxLag_0.25 posNegDiffmean_corr_by_lag_range
    ## 1             -0.01969124                       0.06062643
    ## 2             -0.01969124                       0.06062643
    ## 3             -0.01969124                       0.06062643
    ## 4              0.18799905                       0.23345164
    ## 5              0.18799905                       0.23345164
    ## 6              0.18799905                       0.23345164
    ## 7              0.15889929                       0.10331570
    ## 8              0.15889929                       0.10331570
    ## 9              0.15889929                       0.10331570
    ##   posNegDiffmax_corr_by_lag_range posNegDiffmin_corr_by_lag_range
    ## 1                       0.6404036                    -0.007659471
    ## 2                       0.6404036                    -0.007659471
    ## 3                       0.6404036                    -0.007659471
    ## 4                       0.3641311                     0.201149107
    ## 5                       0.3641311                     0.201149107
    ## 6                       0.3641311                     0.201149107
    ## 7                       0.3431888                     0.012157960
    ## 8                       0.3431888                     0.012157960
    ## 9                       0.3431888                     0.012157960
    ##   posNegDiffvar_corr_by_lag_range posNegDiffsd_corr_lag_by_range
    ## 1                      0.03955941                     0.15756990
    ## 2                      0.03955941                     0.15756990
    ## 3                      0.03955941                     0.15756990
    ## 4                      0.00756171                     0.02697921
    ## 5                      0.00756171                     0.02697921
    ## 6                      0.00756171                     0.02697921
    ## 7                      0.02231712                     0.08436368
    ## 8                      0.02231712                     0.08436368
    ## 9                      0.02231712                     0.08436368
    ##   posNegDiffmedian_corr_by_lag_range
    ## 1                        0.003247717
    ## 2                        0.003247717
    ## 3                        0.003247717
    ## 4                        0.243229303
    ## 5                        0.243229303
    ## 6                        0.243229303
    ## 7                        0.104116299
    ## 8                        0.104116299
    ## 9                        0.104116299
    ##   posNegDiffmean_corr_by_lag_range_maxLag_0.25
    ## 1                                    0.3346882
    ## 2                                    0.3346882
    ## 3                                    0.3346882
    ## 4                                    0.2670151
    ## 5                                    0.2670151
    ## 6                                    0.2670151
    ## 7                                    0.3979297
    ## 8                                    0.3979297
    ## 9                                    0.3979297
    ##   posNegDiffmax_corr_by_lag_range_maxLag_0.25
    ## 1                                   0.7573360
    ## 2                                   0.7573360
    ## 3                                   0.7573360
    ## 4                                   0.3641311
    ## 5                                   0.3641311
    ## 6                                   0.3641311
    ## 7                                   0.5839207
    ## 8                                   0.5839207
    ## 9                                   0.5839207
    ##   posNegDiffmin_corr_by_lag_range_maxLag_0.25
    ## 1                                  0.05052506
    ## 2                                  0.05052506
    ## 3                                  0.05052506
    ## 4                                  0.16224349
    ## 5                                  0.16224349
    ## 6                                  0.16224349
    ## 7                                  0.31488192
    ## 8                                  0.31488192
    ## 9                                  0.31488192
    ##   posNegDiffvar_corr_by_lag_range_maxLag_0.25
    ## 1                                  0.17013799
    ## 2                                  0.17013799
    ## 3                                  0.17013799
    ## 4                                  0.02850679
    ## 5                                  0.02850679
    ## 6                                  0.02850679
    ## 7                                  0.04381050
    ## 8                                  0.04381050
    ## 9                                  0.04381050
    ##   posNegDiffsd_corr_lag_by_range_maxLag_0.25
    ## 1                                 0.37080176
    ## 2                                 0.37080176
    ## 3                                 0.37080176
    ## 4                                 0.09931919
    ## 5                                 0.09931919
    ## 6                                 0.09931919
    ## 7                                 0.15986976
    ## 8                                 0.15986976
    ## 9                                 0.15986976
    ##   posNegDiffmedian_corr_by_lag_range_maxLag_0.25 season catchmentRegion
    ## 1                                      0.1962035 autumn  Ash at Mardock
    ## 2                                      0.1962035 autumn  Ash at Mardock
    ## 3                                      0.1962035 autumn  Ash at Mardock
    ## 4                                      0.2746706 spring  Ash at Mardock
    ## 5                                      0.2746706 spring  Ash at Mardock
    ## 6                                      0.2746706 spring  Ash at Mardock
    ## 7                                      0.2949863 winter  Ash at Mardock
    ## 8                                      0.2949863 winter  Ash at Mardock
    ## 9                                      0.2949863 winter  Ash at Mardock

TODO: A tutorial on how to use these summary statistics in PCA
visualisation and PLSR will follow.

© 2022 GitHub, Inc. Terms Privacy Security Status
