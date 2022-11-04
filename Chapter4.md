Chapter4 Applied Exercise
================
8/28/2022

## ISLR2 Chapter4

Q13 This question should be answered using the Weekly data set, which is
part of the ISLR2 package. This data is similar in nature to the Smarket
data from this chapter’s lab, except that it contains 1, 089 weekly
returns for 21 years, from the beginning of 1990 to the end of 2010.

1.  Produce some numerical and graphical summaries of the Weekly data.
    Do there appear to be any patterns?

``` r
Weekly = ISLR2::Weekly
```

``` r
names(Weekly)
```

    ## [1] "Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"      "Lag5"     
    ## [7] "Volume"    "Today"     "Direction"

``` r
dim(Weekly)
```

    ## [1] 1089    9

``` r
summary(Weekly)
```

    ##       Year           Lag1               Lag2               Lag3         
    ##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
    ##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
    ##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
    ##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
    ##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
    ##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
    ##       Lag4               Lag5              Volume            Today         
    ##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747   Min.   :-18.1950  
    ##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202   1st Qu.: -1.1540  
    ##  Median :  0.2380   Median :  0.2340   Median :1.00268   Median :  0.2410  
    ##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462   Mean   :  0.1499  
    ##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373   3rd Qu.:  1.4050  
    ##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821   Max.   : 12.0260  
    ##  Direction 
    ##  Down:484  
    ##  Up  :605  
    ##            
    ##            
    ##            
    ## 

``` r
pairs(Weekly)
```

![](Chapter4_files/figure-gfm/cars-1.png)<!-- -->

``` r
cor(Weekly[, -9])
```

    ##               Year         Lag1        Lag2        Lag3         Lag4
    ## Year    1.00000000 -0.032289274 -0.03339001 -0.03000649 -0.031127923
    ## Lag1   -0.03228927  1.000000000 -0.07485305  0.05863568 -0.071273876
    ## Lag2   -0.03339001 -0.074853051  1.00000000 -0.07572091  0.058381535
    ## Lag3   -0.03000649  0.058635682 -0.07572091  1.00000000 -0.075395865
    ## Lag4   -0.03112792 -0.071273876  0.05838153 -0.07539587  1.000000000
    ## Lag5   -0.03051910 -0.008183096 -0.07249948  0.06065717 -0.075675027
    ## Volume  0.84194162 -0.064951313 -0.08551314 -0.06928771 -0.061074617
    ## Today  -0.03245989 -0.075031842  0.05916672 -0.07124364 -0.007825873
    ##                Lag5      Volume        Today
    ## Year   -0.030519101  0.84194162 -0.032459894
    ## Lag1   -0.008183096 -0.06495131 -0.075031842
    ## Lag2   -0.072499482 -0.08551314  0.059166717
    ## Lag3    0.060657175 -0.06928771 -0.071243639
    ## Lag4   -0.075675027 -0.06107462 -0.007825873
    ## Lag5    1.000000000 -0.05851741  0.011012698
    ## Volume -0.058517414  1.00000000 -0.033077783
    ## Today   0.011012698 -0.03307778  1.000000000

The correlation between Today’s return and lag variables are close to 0.

2.  Use the full data set to perform a logistic regression with
    Direction as the response and the five lag variables plus Volume as
    predictors. Use the summary function to print the results. Do any of
    the predictors appear to be statistically significant? If so, which
    ones?

``` r
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Weekly, family = binomial
  )
summary(glm.fits)
```

    ## 
    ## Call:
    ## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    ##     Volume, family = binomial, data = Weekly)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6949  -1.2565   0.9913   1.0849   1.4579  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)  0.26686    0.08593   3.106   0.0019 **
    ## Lag1        -0.04127    0.02641  -1.563   0.1181   
    ## Lag2         0.05844    0.02686   2.175   0.0296 * 
    ## Lag3        -0.01606    0.02666  -0.602   0.5469   
    ## Lag4        -0.02779    0.02646  -1.050   0.2937   
    ## Lag5        -0.01447    0.02638  -0.549   0.5833   
    ## Volume      -0.02274    0.03690  -0.616   0.5377   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1496.2  on 1088  degrees of freedom
    ## Residual deviance: 1486.4  on 1082  degrees of freedom
    ## AIC: 1500.4
    ## 
    ## Number of Fisher Scoring iterations: 4

Only Lag2 is statistically significant.

3.  Compute the confusion matrix and overall fraction of correct
    predictions. Explain what the confusion matrix is telling you about
    the types of mistakes made by logistic regression.

``` r
glm.probs <- predict(glm.fits, type = "response")
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Weekly$Direction)
```

    ##         
    ## glm.pred Down  Up
    ##     Down   54  48
    ##     Up    430 557

``` r
mean(glm.pred == Weekly$Direction)
```

    ## [1] 0.5610652

However, Down’s precision is `54/(54+430)*100`% while Up one is
`557/(48+557)`.

4.  Now fit the logistic regression model using a training data period
    from 1990 to 2008, with Lag2 as the only predictor. Compute the
    confusion matrix and the overall fraction of correct predictions for
    the held out data (that is, the data from 2009 and 2010).

``` r
train <- (Weekly$Year < 2009)
Weekly.2009 <- Weekly[!train, ]
dim(Weekly.2009)
```

    ## [1] 104   9

``` r
Direction.2009 <- Weekly$Direction[!train]
```

``` r
glm.fits <- glm(
    Direction ~ Lag2,
    data = Weekly, family = binomial, subset = train
  )
glm.probs <- predict(glm.fits, Weekly.2009,
    type = "response")
```

``` r
glm.pred <- rep("Down", 104)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2009)
```

    ##         Direction.2009
    ## glm.pred Down Up
    ##     Down    9  5
    ##     Up     34 56

``` r
mean(glm.pred == Direction.2009)
```

    ## [1] 0.625

``` r
9/(34+9)
```

    ## [1] 0.2093023

``` r
56/(5+56)
```

    ## [1] 0.9180328

The overall precision goes up.

5.  Repeat (d) using LDA.

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:ISLR2':
    ## 
    ##     Boston

``` r
lda.fit <- lda(Direction ~ Lag2, data = Weekly,
    subset = train)
lda.pred <- predict(lda.fit, Weekly.2009)
```

``` r
lda.class <- lda.pred$class
table(lda.class, Direction.2009)
```

    ##          Direction.2009
    ## lda.class Down Up
    ##      Down    9  5
    ##      Up     34 56

``` r
mean(lda.class == Direction.2009)
```

    ## [1] 0.625

The same result as logistic regression.

6.  Repeat (d) using QDA.

``` r
qda.fit <- qda(Direction ~ Lag2, data = Weekly,
    subset = train)
qda.pred <- predict(qda.fit, Weekly.2009)
```

``` r
qda.class <- qda.pred$class
table(qda.class, Direction.2009)
```

    ##          Direction.2009
    ## qda.class Down Up
    ##      Down    0  0
    ##      Up     43 61

``` r
mean(qda.class == Direction.2009)
```

    ## [1] 0.5865385

The precision goes down.

7.  Repeat (d) using KNN with K = 1.

``` r
library(class)
train.X <- Weekly$Lag2[train]
test.X <- Weekly$Lag2[!train]
train.Direction <- Weekly$Direction[train]
```

``` r
set.seed(1)
knn.pred <- knn(data.frame(train.X), data.frame(test.X), train.Direction, k = 1)
table(knn.pred, Direction.2009)
```

    ##         Direction.2009
    ## knn.pred Down Up
    ##     Down   21 30
    ##     Up     22 31

``` r
mean(knn.pred == Direction.2009)
```

    ## [1] 0.5

8.  Repeat (d) using naive Bayes.

``` r
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag2, data = Weekly,
    subset = train)
nb.fit
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##      Down        Up 
    ## 0.4477157 0.5522843 
    ## 
    ## Conditional probabilities:
    ##       Lag2
    ## Y             [,1]     [,2]
    ##   Down -0.03568254 2.199504
    ##   Up    0.26036581 2.317485

``` r
nb.class <- predict(nb.fit, Weekly.2009)
table(nb.class, Direction.2009)
```

    ##         Direction.2009
    ## nb.class Down Up
    ##     Down    0  0
    ##     Up     43 61

``` r
mean(nb.class == Direction.2009)
```

    ## [1] 0.5865385

1.  Which of these methods appears to provide the best results on this
    data? The logistic regression and LDA had the highest accuracy =
    0.625.

Q14 In this problem, you will develop a model to predict whether a given
car gets high or low gas mileage based on the Auto data set.

1.  Create a binary variable, mpg01, that contains a 1 if mpg contains a
    value above its median, and a 0 if mpg contains a value below its
    median. You can compute the median using the median() function. Note
    you may find it helpful to use the data.frame() function to create a
    single data set containing both mpg01 and the other Auto variables.

``` r
Auto = ISLR2::Auto
```

``` r
Auto.median = median(Auto$mpg)
```

``` r
Auto$mpg01 = ifelse(Auto$mpg >= Auto.median, 1, 0)
summary(Auto)
```

    ##       mpg          cylinders      displacement     horsepower        weight    
    ##  Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0   Min.   :1613  
    ##  1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0   1st Qu.:2225  
    ##  Median :22.75   Median :4.000   Median :151.0   Median : 93.5   Median :2804  
    ##  Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5   Mean   :2978  
    ##  3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0   3rd Qu.:3615  
    ##  Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0   Max.   :5140  
    ##                                                                                
    ##   acceleration        year           origin                      name    
    ##  Min.   : 8.00   Min.   :70.00   Min.   :1.000   amc matador       :  5  
    ##  1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000   ford pinto        :  5  
    ##  Median :15.50   Median :76.00   Median :1.000   toyota corolla    :  5  
    ##  Mean   :15.54   Mean   :75.98   Mean   :1.577   amc gremlin       :  4  
    ##  3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000   amc hornet        :  4  
    ##  Max.   :24.80   Max.   :82.00   Max.   :3.000   chevrolet chevette:  4  
    ##                                                  (Other)           :365  
    ##      mpg01    
    ##  Min.   :0.0  
    ##  1st Qu.:0.0  
    ##  Median :0.5  
    ##  Mean   :0.5  
    ##  3rd Qu.:1.0  
    ##  Max.   :1.0  
    ## 

2.  Explore the data graphically in order to investigate the associ-
    ation between mpg01 and the other features. Which of the other
    features seem most likely to be useful in predicting mpg01?
    Scatterplots and boxplots may be useful tools to answer this ques-
    tion. Describe your findings.

``` r
library("GGally")
```

    ## Loading required package: ggplot2

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'pillar'

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
ggcorr(Auto, label=TRUE)
```

    ## Warning in ggcorr(Auto, label = TRUE): data in column(s) 'name' are not numeric
    ## and were ignored

![](Chapter4_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
pairs(Auto)
```

![](Chapter4_files/figure-gfm/unnamed-chunk-18-2.png)<!-- --> It seems
displacement, cylinders, weight and horsepower are negatively correlated
with mpg01.

3.  Split the data into a training set and a test set. Split into 80:20.

``` r
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Auto), replace=TRUE, prob=c(0.8, 0.2))
train <- Auto[sample, ]
test  <- Auto[!sample, ]
```

4.  Perform LDA on the training data in order to predict mpg01 using the
    variables that seemed most associated with mpg01 in (b). What is the
    test error of the model obtained?

``` r
library(MASS)
lda.fit <- lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = train)
lda.pred <- predict(lda.fit, test)
```

``` r
lda.class <- lda.pred$class
table(lda.class, test$mpg01)
```

    ##          
    ## lda.class  0  1
    ##         0 29  2
    ##         1  7 33

``` r
mean(lda.class == test$mpg01)
```

    ## [1] 0.8732394

``` r
mean(lda.class != test$mpg01)
```

    ## [1] 0.1267606

The test error is 12.7%.

5.  Perform QDA on the training data in order to predict mpg01 using the
    variables that seemed most associated with mpg01 in (b). What is the
    test error of the model obtained?

``` r
qda.fit <- qda(mpg01 ~ cylinders + displacement + horsepower + weight, data = train)
qda.pred <- predict(qda.fit, test)
```

``` r
qda.class <- qda.pred$class
table(qda.class, test$mpg01)
```

    ##          
    ## qda.class  0  1
    ##         0 30  6
    ##         1  6 29

``` r
mean(qda.class == test$mpg01)
```

    ## [1] 0.8309859

``` r
mean(qda.class != test$mpg01)
```

    ## [1] 0.1690141

The result is the same as LDA.

6.  Perform logistic regression on the training data in order to pre-
    dict mpg01 using the variables that seemed most associated with
    mpg01 in (b). What is the test error of the model obtained?

``` r
glm.fits <- glm(
    mpg01 ~ cylinders + displacement + horsepower + weight,
    data = train, family = binomial
  )
summary(glm.fits)
```

    ## 
    ## Call:
    ## glm(formula = mpg01 ~ cylinders + displacement + horsepower + 
    ##     weight, family = binomial, data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3731  -0.1308   0.0671   0.3159   3.4325  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  14.5648724  2.1915319   6.646 3.01e-11 ***
    ## cylinders    -0.2550816  0.3947836  -0.646  0.51820    
    ## displacement -0.0040600  0.0091959  -0.442  0.65885    
    ## horsepower   -0.0519746  0.0169482  -3.067  0.00216 ** 
    ## weight       -0.0026909  0.0008272  -3.253  0.00114 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 445.00  on 320  degrees of freedom
    ## Residual deviance: 157.64  on 316  degrees of freedom
    ## AIC: 167.64
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
dim(test)
```

    ## [1] 71 10

``` r
glm.probs <- predict(glm.fits, test,
    type = "response")
glm.pred <- rep(0, 71)
glm.pred[glm.probs > .5] <- 1
table(glm.pred, test$mpg01)
```

    ##         
    ## glm.pred  0  1
    ##        0 30  3
    ##        1  6 32

``` r
mean(glm.pred == test$mpg01)
```

    ## [1] 0.8732394

``` r
mean(glm.pred != test$mpg01)
```

    ## [1] 0.1267606

7.  Perform naive Bayes on the training data in order to predict mpg01
    using the variables that seemed most associated with mpg01 in (b).
    What is the test error of the model obtained?

``` r
library(e1071)
nb.fit <- naiveBayes(mpg01 ~ cylinders + displacement + horsepower + weight, data = train)
nb.class <- predict(nb.fit, test)
table(nb.class, test$mpg01)
```

    ##         
    ## nb.class  0  1
    ##        0 30  3
    ##        1  6 32

``` r
mean(nb.class == test$mpg01)
```

    ## [1] 0.8732394

``` r
mean(nb.class != test$mpg01)
```

    ## [1] 0.1267606

8.  Perform KNN on the training data, with several values of K, in order
    to predict mpg01. Use only the variables that seemed most associated
    with mpg01 in (b). What test errors do you obtain? Which value of K
    seems to perform the best on this data set? K=1

``` r
library(class)
set.seed(1)
knn.pred <- knn(train[, c("cylinders", "displacement", "horsepower", "weight")], test[, c("cylinders", "displacement", "horsepower", "weight")], train$mpg01, k = 1)
table(knn.pred, test$mpg01)
```

    ##         
    ## knn.pred  0  1
    ##        0 29  4
    ##        1  7 31

``` r
mean(knn.pred == test$mpg01)
```

    ## [1] 0.8450704

``` r
mean(knn.pred != test$mpg01)
```

    ## [1] 0.1549296

K=3

``` r
set.seed(1)
knn.pred <- knn(train[, c("cylinders", "displacement", "horsepower", "weight")], test[, c("cylinders", "displacement", "horsepower", "weight")], train$mpg01, k = 3)
table(knn.pred, test$mpg01)
```

    ##         
    ## knn.pred  0  1
    ##        0 30  3
    ##        1  6 32

``` r
mean(knn.pred == test$mpg01)
```

    ## [1] 0.8732394

``` r
mean(knn.pred != test$mpg01)
```

    ## [1] 0.1267606

K=5

``` r
set.seed(1)
knn.pred <- knn(train[, c("cylinders", "displacement", "horsepower", "weight")], test[, c("cylinders", "displacement", "horsepower", "weight")], train$mpg01, k = 5)
table(knn.pred, test$mpg01)
```

    ##         
    ## knn.pred  0  1
    ##        0 28  5
    ##        1  8 30

``` r
mean(knn.pred == test$mpg01)
```

    ## [1] 0.8169014

``` r
mean(knn.pred != test$mpg01)
```

    ## [1] 0.1830986

K=3 is the lowest.

Q15 This problem involves writing functions.

1.  Write a function, Power(), that prints out the result of raising 2
    to the 3rd power. In other words, your function should compute 23
    and print out the results. Hint: Recall that x^a raises x to the
    power a. Use the print() function to output the result.

``` r
Power <- function() {
  p <- 2^3
  print(p)
}
Power()
```

    ## [1] 8

2.  Create a new function, Power2(), that allows you to pass any two
    numbers, x and a, and prints out the value of x^a.

``` r
Power2 <- function(x, a) {
  p <- x^a
  print(p)
}
Power2(3, 8)
```

    ## [1] 6561

3.  Using the Power2() function that you just wrote, compute 10^3, 8^17,
    and 131^3.

``` r
Power2(10, 3)
```

    ## [1] 1000

``` r
Power2(8, 17)
```

    ## [1] 2.2518e+15

``` r
Power2(131, 3)
```

    ## [1] 2248091

(d)Now create a new function, Power3(), that actually returns the result
x^a as an R object, rather than simply printing it to the screen.

``` r
Power3 <- function(x, a) {
  p <- x^a
  return(p)
}
```

(e)Now using the Power3() function, create a plot of f(x) = x2. The
x-axis should display a range of integers from 1 to 10, and the y-axis
should display x2. Label the axes appropriately, and use an appropriate
title for the figure. Consider displaying either the x-axis, the y-axis,
or both on the log-scale.

``` r
data.frame(x = 1:10, y = Power3(1:10, 2)) %>%
  ggplot(aes(x = x, y = log(y))) + 
  geom_line() 
```

![](Chapter4_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

6.  Create a function, PlotPower(), that allows you to create a plot of
    x against x^a for a fixed a and for a range of values of x.

``` r
PlotPower <- function(x, a) {
  data.frame(x = x, y = Power3(x, a)) %>%
  ggplot(aes(x = x, y = log(y))) + 
  geom_line() 
}
PlotPower(1:10, 3)
```

![](Chapter4_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
