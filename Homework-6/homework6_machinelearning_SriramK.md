Homework - 6
================
Sriram Kannan

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.1.3

    ## randomForest 4.7-1

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

1.  Convert the response variable in the “vowel.train” data frame to a
    factor variable prior to training, so that “randomForest” does
    classification rather than regression.

2.  Review the documentation for the “randomForest” function.

``` r
?randomForest()
```

    ## starting httpd help server ... done

3.  Fit the random forest model to the vowel data using all of the 11
    features using the default values of the tuning parameters.

4.  Use 5-fold CV and tune the model by performing a grid search for the
    following tuning parameters: 1) the number of variables randomly
    sampled as candidates at each split; consider values 3, 4, and 5,
    and 2) the minimum size of terminal nodes; consider a sequence (1,
    5, 10, 20, 40, and 80).

5.  With the tuned model, make predictions using the majority vote
    method, and compute the misclassification rate using the
    ‘vowel.test’ data.
