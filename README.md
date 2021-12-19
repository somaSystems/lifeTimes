lifeTimes: biological time series cross correlation in R
================
somaSystems
19/12/2021

#Coupled time series analysis in r:

### i. [input](#data-cleanup)

### ii. [output](#initial-results)

# input

### Analysis to Remove low intensity cells

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

    ## Warning: Removed 400 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-8-2.png)

    ## Warning: Removed 400 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-8-3.png)![](README_files/figure-markdown_github/unnamed-chunk-8-4.png)![](README_files/figure-markdown_github/unnamed-chunk-8-5.png)

### Analysis to identify base of nuclei and coverslip position

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)![](README_files/figure-markdown_github/unnamed-chunk-9-2.png)![](README_files/figure-markdown_github/unnamed-chunk-9-3.png)![](README_files/figure-markdown_github/unnamed-chunk-9-4.png)

### Method to put lowest cell in each FIELD (regions of wells) on coverslip

### View of adjusted coverslip positions

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

    ## geom_pointdensity using method='kde2d' due to large number of points (>20k)

![](README_files/figure-markdown_github/unnamed-chunk-11-2.png)

    ## geom_pointdensity using method='kde2d' due to large number of points (>20k)

    ## Warning: Removed 27126 rows containing missing values (stat_pointdensity).

![](README_files/figure-markdown_github/unnamed-chunk-11-3.png)![](README_files/figure-markdown_github/unnamed-chunk-11-4.png)

    ## Warning: Removed 25102 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-11-5.png)

# output

### Look at effect of Proximal vs Distal on protrusivity, at single cell, well, field

![](README_files/figure-markdown_github/unnamed-chunk-18-1.png)![](README_files/figure-markdown_github/unnamed-chunk-18-2.png)![](README_files/figure-markdown_github/unnamed-chunk-18-3.png)![](README_files/figure-markdown_github/unnamed-chunk-18-4.png)

### Compare each treatment to controls

![](README_files/figure-markdown_github/unnamed-chunk-19-1.png)![](README_files/figure-markdown_github/unnamed-chunk-19-2.png)![](README_files/figure-markdown_github/unnamed-chunk-19-3.png)
