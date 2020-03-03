---
title: 'Team CCAD Case Study 8: Crox stock ARIMA analysis'
author: "David Josephs, Andy Heroy, Carson Drake, Che' Cobb"
date: '2020-03-02'
output:
  html_document:
    df_print: paged
    fig_caption: true
#    fig_height: 10
    fig_retina: yes
#    fig_width: 10
    highlight: haddock
    keep_md: yes
    number_sections: yes
    theme: readable
    toc: yes
  pdf_document:
    toc: yes
---





```r
library(quantmod)
library(tswge)
library(tswgewrapped)
library(dplyr)
library(magrittr)
library(tidyverse)
set.seed(666)


# this set of functions allows us to autoincrement our figures
counter <- function() {
  x <- 0
  return (
          function() {
            # Assigning outside of scope! The real purpose of <-
            x <<- x+1
            return(x)
          }

  )
}

# initialize a new counter
cnt <- counter()

# call the counter with a bolded figure caption
cap <- function(str) {
  paste("**Figure", cnt(), ":**",str)
}
```

# Introduction
The purpose of this case study is to fit an ARIMA time series model to a stock of our choosing.   Since we enjoy the finer consumer products avaiable on the market, we chose to write our case study about CROX stock.  Yes, that long forgotten footwear loved by all those who spend too much time on their feet in a given day.  To accomplish this task, we've chosen to utilize the library (TSWGE) which was written and composed by Dr. Woodward, Gray, and Elliot, along the whamo brilliance of Dr. Bivin Sadler.  This library contains many useful functions for time series analysis which we describe and explore in the analysis below.


# Background
To begin, we'll explain what an ARIMA model is comprised of, and how they are useful in time series forecasting techniques. An ARIMA model is comprised of two main components, one consisting of the autoregressive (AR) component, and one consisting of the moving average (MA) component. As we will see shortly, the integrated (I in ARIMA) as well as the seasonal portion are just special cases of the autoregressive model. 

## AR Models

The basic form of the AR part of ARIMA (with order $1$) is written as: 

$$
X_t\sum_{j=0}^p\phi_jB^j = \epsilon_t
$$

Where $\mathrm{B}$ is the backshift operator such that $\mathrm{B}^nX_t = X_{t-n}$, $\phi_0 = 1$, and $\epsilon_t \sim N(0, v)$. For our analysis, we can define the **charactersitic polynomial** of an AR model in the following manner:

$$
\sum_{j=0}^p\phi_jZ^j = 0
$$

We can constrain this equation with the assumption that our AR processes are stationary. **An AR process is stationary if and only if all the roots of its characteristic equation lie within the unit circle on the complex plane**. That is to say for all roots of a charactarestic AR polynomial, the AR process is stationary if for every root $r$, $\| \mathbf{r} \| < 1$. Visually this can be explained by viewing the unit circle on the complex plane:



<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-1-1.svg" alt="**Figure 1 :** Unit circle in the complex plane, all complex roots must be WITHIN this circle"  />
<p class="caption">**Figure 1 :** Unit circle in the complex plane, all complex roots must be WITHIN this circle</p>
</div>


There is interesting behavior when the roots of the characteristic equation lie exactly on the unit circle. This is how autoregressive models capture integrated/wandering components and seasonal components.

### The I in ARIMA

The Integrated (I) component of ARIMA occurs when the AR portion of the model has a linear root at exactly 1. This means that, using ARIMA(p,d,q) form, we can write a $\left(0,1,0\right)$ model ($d=1$) as:
$$
\left(1 - B\right)X_t = \epsilon_t
$$

or more generally as 

$$
\left(1-B\right)^dX_t = \epsilon_t
$$

Where $d$ is the integrated order of the model. It is important to note that in general, if you have $d=2$, you are pushing it, while $d \geq 3$ processes rarely occur on this earth. If you have $d$ at a high value it is likely better to look to other models, for example high order AR models.

### Seasonal ARIMA

Seasonal components also appear when the roots to the AR characterstic equations are at one, however instead of linear unit roots, Seasonal components have polynomial roots at one. That is, giving a seasonality $s$:

$$
(1-B^s)X_t = \epsilon_t
$$

## MA models

While AR models are focused on the relationship between a realization $X$ at time $t$ and at time $t-n$, MA models are focused on the noise portion of the equation. In general, MA models alone are not used for modeling real data, as they have some strange properties and are not quite as useful, but in conjunction with with AR, integrated, and seasonal models you can build more powerful models of time series. An MA model is expressed just the same as an arima model, using polynomials:

$$
X_t = \epsilon_t\sum_{k=0}^q\theta_kB^k
$$

Unlike the AR component, an MA model is a model of noise, which we will refer to as a General Linear Process (GLP). GLPs have a special property in that they are always stationary (in comparison to, for example, autoregressive processes, which are only stationary when their roots are bounded by the unit circle). However, MA processes  have an undesirable characteristic: multiple processes (different sets of $\theta$) can have the same autocorrelation structure, that is they exhibit model multiplicity. That means we can have different models producing the same effects, which is not ideal. To get around this, we constrain the possible MA models, we must only use models with unique solutions. This characteristic is known as **invertibility**, and we can define the criteria for invertibility as having characteristic roots *outside* the unit circle.

## General ARIMA models

To introduce the most general form of the ARIMA model, we will introduce some new notation for brevity:

\begin{align}
\Phi_p(B) = \sum_{j=0}^p\phi_pB^p \\
\Theta_q(B) = \sum_{k=0}^q \theta_qB^q
\end{align}

This allows us to write seasonal ARIMA with order $(p,d,q)$ and seasonality $s$ as:

$$
\Phi_p(B)(1-B)^d(1-B^s)X_t = \Theta_q(B)\epsilon_t
$$


# TSWGE

In this study we will be using the `tswge` library, which is paired with the book `Applied Time Series Analysis with R`, mentioned in the above paragraphs. The library contains a few main utilities. First, it has a series of functions for generating time series, `gen.TYPEOFSERIES.wge`. These allow for practice and developing intuition. Second, it has utilities for forecasting time series, `fore.TYPEOFSERIES.wge`. Finally, it has two irrepreplacable sets of utilities, those for estimating the order of a time series as well as exploring them(model identification), and those for estimating the coefficients once the model has been identified (parameter estimation). This library, while brilliant, is sometimes syntactically painful, so it is extended by the student made (and in active development!) `tswgewrapped` package (maintained by one of the authors of this report). We will use a mix of both libraries, and share source code for the functions used when appropriate, in order to explain exactly what we are doing.


# Stock Analysis

## Data Setup

As mentioned in our Introduction section, we are chiefly interested in the stock ticker CROX, for the company that makes crocs, everyone's favorite footwear. Please note, due to the ongoing financial turmoil, we will be ignoring the last two weeks with coronavirus, as that is outside the scope of ARIMA. Lets first load in the data, and take a brief look at a candle chart.


```r
crox <- getSymbols("CROX",src = "yahoo")
cstock <- CROX
cstock_2years <-cstock['2018-02-01::2020-02-01']
candleChart(cstock_2years, theme = "white")
# assign to a data frame!
cstock_2years %<>% data.frame
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/getStockData-1.svg" alt="**Figure 2 :** Candle stock of Croc's stock over the last two years"  />
<p class="caption">**Figure 2 :** Candle stock of Croc's stock over the last two years</p>
</div>

The first thing we must do is pick a **forecast horizon**. As we are forecasting stocks, it is unlikely and inappropriate to forecast stocks 15-20 days ahead. Therefore we will focus on just one business week (5 days). To assess our model, we will use the following cross validation structure:


  * cut the data at one forecast horizon (5 days), using the last 5 days as a holdout set
  * Validate by using *backcasting* and getting the ASE. It can be shown that forecasts hold both forwards and backwords (see applied time series with R), and so a good trick to assess model validity without dirtying your test set is to make your forecasts backwards
  * Assess models on the holdout set

<!-- end of list -->

Lets go ahead and split our data, and isolate the closing price.


```r
train_indices <- 1:(nrow(cstock_2years) - 5)
crox_train <- cstock_2years[train_indices,]$CROX.Close
crox_test <- cstock_2years[-train_indices,]$CROX.Close
```

## Exploratory Analysis

The first key part of doing any time series analysis is to look at the series, its ACF, and a periodigram (parzen window), which measures the frequency density of the series in decibels from 0 to the nyquist frequency (0.5):



```r
x <- plotts.sample.wge(crox_train)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-3-1.svg" alt="**Figure 3 :** Raw Data, ACF, on top, frequency periodigrams on bottom"  />
<p class="caption">**Figure 3 :** Raw Data, ACF, on top, frequency periodigrams on bottom</p>
</div>

What can we tell from this? We will work right to left. First, we do not see any clear seasonality in the line plot. This is expected. Crocs are a commodity which is in demand year round, so we do not expect any seasonalities. It is unclear as to whether or not there is an integrated or wandering trend, it could either have a low order integrated trend or a higher order AR term. The ACF tells the same story. These barely damping lines are highly indicative of integrated trends, but also of strong autoregressions (which if you recall, an integrated term can be viewed as an exceptionally strong autoregression). The parzen window on the bottom left tells the same story as the periodigram, but is in the authors' opinion more digestible. This inverse curve shape is indicative of a wandering (integrated component), or of a higher order AR component (but more likely wandering trend).

We will continue our analysis as follows. We will consider two cases, the first that CROX stock is stationary, or that it stays the same over time. We will also consider the case that CROX is wandering ($d>0$). With these two setups, we will evaluate two techniques: visual identification of the model, and automated using an AIC and BIC based grid search technique, as favored by Woodward, Gray, and Elliot. First lets go ahead and difference the series, to see if that helps. We will use the `artrans.wge` function to difference it away


```r
# define a function for a single order difference
diff1  <- . %>% artrans.wge(phi.tr = 1)
train_adj <- diff1(crox_train)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-4-1.svg" alt="**Figure 4 :** First order ARIMA difference of CROX stock"  />
<p class="caption">**Figure 4 :** First order ARIMA difference of CROX stock</p>
</div>

```r
par(mfrow=c(1,1))
```

Here we see the series before and after the  transformation. Lets talk about the ACF before the transformation first. This slowly damping ACF implies an AR model with real, positive roots, so we will look for that in our by hand analysis. The transformed ACF implies either an AR model or a simple ARMA model, or even simply noise, which would imply that CROX stock is simply performing a random walk. We will keep this in mind for our analysis.


## Analysis of untransformed series

First, we will attempt to perform our analysis by hand. We said we wanted a high order AR model with positive, real roots. To confirm this, lets make a sample time series with a strong value of phi (a strongly autoregressive model). We will use the tswgewrapped `generate` function, which is a thin wrapper for the tswge version:


```r
generate
```

```
#> function (type, ...) 
#> {
#>     phrase <- paste0("tswge::gen.", rlang::enexpr(type), ".wge")
#>     func <- rlang::parse_expr(phrase)
#>     eval(rlang::call2(func, ...))
#> }
#> <bytecode: 0xbe3ba58>
#> <environment: namespace:tswgewrapped>
```


```r
acf(generate(arma, n=100, phi=(0.99), plot=FALSE))  # 
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-6-1.svg" alt="**Figure 5 :** A strong autoregressive model looks fairly appropriate. In the wild, it would be a higher order AR model, but this is difficult to generate."  />
<p class="caption">**Figure 5 :** A strong autoregressive model looks fairly appropriate. In the wild, it would be a higher order AR model, but this is difficult to generate.</p>
</div>

### By hand analysis of the untransformed series

The first thing, looking at the ACF, which never goes negative, is we can definitely tell the AR process generating the data has strong, positive roots. This is apparent due to the lack of sign change and overall high value of ACF. Let us go ahead and try our first guess of model, and see if the roots match our assumptions. We will start with $p = 3$. This is our choice because our generation of a random series with $phi_1=0.99$ looked very similar to the autocorrelation of the original series. However, we would much prefer if the roots were a bit further away from the unit circle, while also exhibiting a stronger autoregressive effect. Therefore 3 is a good, simple start. We will first estimate the AR coeffecients using the `estimate` function from tswgewrapped. The source code for the function is below:


```r
estimate
```

```
#> function (xs, p, q = 0, type = "mle", ...) 
#> {
#>     if (q > 0) {
#>         return(tswge::est.arma.wge(xs, p, q, ...))
#>     }
#>     else {
#>         return(tswge::est.ar.wge(xs, p, type, ...))
#>     }
#> }
#> <bytecode: 0xc6a7cf0>
#> <environment: namespace:tswgewrapped>
```

Again, it is a thin wrapper for tswge, where it uses maximum likelihood estimation to quickly determine the ideal values for phi and theta.


```r
train_hand_est <- estimate(crox_train, p=3)
```

```
#> 
#> Coefficients of Original polynomial:  
#> 0.8452 0.1966 -0.0434 
#> 
#> Factor                 Roots                Abs Recip    System Freq 
#> 1-0.9985B              1.0015               0.9985       0.0000
#> 1+0.2988B             -3.3466               0.2988       0.5000
#> 1-0.1455B              6.8740               0.1455       0.0000
#>   
#> 
```

This also conveniently prints out the roots of the polynomial. We see we have real roots, with two positive and one negative. The first root is rather close to the unit circle, which may not be desirable. Differencing is likely more appropriate, but we will continue for the sake of knowledge! We can also plot the ACF of this estimate, and see how it compares to our original data:


```r
acf(generate(arma, n=length(crox_train), phi = train_hand_est$phi, plot=FALSE))
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-9-1.svg" alt="**Figure 6 :** ACF of a sample series"  />
<p class="caption">**Figure 6 :** ACF of a sample series</p>
</div>

This looks pretty good! We can check goodness of fit with two methods: first using residual plotting, and second using the `ljung_box` function, which performs the ljung_box test. The ljung box test has a null hypothesis which the residuals are white noise, and an alternative that the residuals are correlated in some way. It is a portmanteau test. First we will look at the residuals visually. It is important to do this because the ljung box test, while the best of statistical tests for autocorrelation, has low statistical power.


```r
plot_res <-  function (res) {
     par(mfrow = c(1, 2))
     plot(res, type = "b")
     title(main="Residual plot")
     acf(res)
     par(mfrow = c(1, 1))
 }

plot_res(train_hand_est$res)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-10-1.svg" alt="**Figure 7 :** The residuals of the estimate. If it looks like noise, it is better. In this case, we still have those two extreme values."  />
<p class="caption">**Figure 7 :** The residuals of the estimate. If it looks like noise, it is better. In this case, we still have those two extreme values.</p>
</div>

This looks like an *alright* fit, however to double check we will go ahead with the portmanteau test. It is advisible to conduct this test at multiple lags, particularly 24 and 48 (suggested by box). Luckily, tswgewrapped has this all covered for us (source code included, no smoke and mirrors).


```r
ljung_box
```

```
#> function (x, p, q, k_val = c(24, 48)) 
#> {
#>     ljung <- function(k) {
#>         hush(tswge::ljung.wge(x = x, p = p, q = q, K = k))
#>     }
#>     sapply(k_val, ljung)
#> }
#> <bytecode: 0x4bfbc18>
#> <environment: namespace:tswgewrapped>
```

```r
ljung_box(train_hand_est$res, 3,0)
```

```
#>            [,1]             [,2]            
#> test       "Ljung-Box test" "Ljung-Box test"
#> K          24               48              
#> chi.square 34               61              
#> df         21               45              
#> pval       0.033            0.06
```

So this is in agreement with our plot. Our fit is pretty close, however it is probably not a perfect fit. We did not model all the autocorrelation out of the data, thus this is not the best model. For a baseline, we will backcast and get the ASE of our model. For the sake of transparency, we will show the tswgewrapped source code below:


```r
assess
```

```
#> function (x, ...) 
#> {
#>     bcast <- fcst(x = x, ..., lastn = T)
#>     ASE <- ase(x, bcast)
#>     return(ASE)
#> }
#> <bytecode: 0x29217d8>
#> <environment: namespace:tswgewrapped>
```

```r
fcst
```

```
#> function (type, ...) 
#> {
#>     phrase <- paste0("tswge::fore.", rlang::enexpr(type), ".wge")
#>     func <- rlang::parse_expr(phrase)
#>     eval(rlang::expr((!!func)(...)))
#> }
#> <bytecode: 0x2b3c8c8>
#> <environment: namespace:tswgewrapped>
```

```r
ase
```

```
#> function (x, xhat) 
#> {
#>     s <- length(x) - length(xhat$f) + 1
#>     n <- length(x)
#>     mean((xhat$f - x[s:n])^2)
#> }
#> <bytecode: 0x8e638b0>
#> <environment: namespace:tswgewrapped>
```

Again, it is just a thin wrapper for tswge, we forecast the series backwards and get the ASE. Lets test it out:


```r
ase_hand_un <- assess(crox_train, arma, n.ahead=5, phi = train_hand_est$phi)
title(main="5 step back forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-13-1.svg" alt="**Figure 8 :** Looks like a pretty good forecast"  />
<p class="caption">**Figure 8 :** Looks like a pretty good forecast</p>
</div>

```r
ase_hand_un
```

```
#> [1] 1.1
```

This is a pretty good forecast! Lets go ahead and automate one too:

### Grid search analysis of the untransformed data

First, we will use AIC and BIC to determine the optimal values of p and q. Note the aic5.wge and tswgewrapped::aicbic are just criterion based grid searches. Lets go ahead and run it (note, we are using the tswgewrapped::hush function to avoid some annoying tswge output):


```r
aics <- hush(aic5.wge(crox_train))
bics <- hush(aic5.wge(crox_train, type='bic'))
pander::pander(list(aics, bics))
```



  *

    --------------------------
     &nbsp;   p   q     aic
    -------- --- --- ---------
     **11**   3   1   -0.6896

     **10**   3   0   -0.6871

     **14**   4   1   -0.6858

     **13**   4   0   -0.6853

     **16**   5   0   -0.6849
    --------------------------

  *

    --------------------------
     &nbsp;   p   q     bic
    -------- --- --- ---------
     **10**   3   0   -0.6533

     **4**    1   0   -0.6503

     **11**   3   1   -0.6474

     **6**    1   2   -0.6472

     **13**   4   0   -0.643
    --------------------------


<!-- end of list -->


It looks like we were pretty close! We will go ahead and try out an ARMA(3,1) model as well, for the sake of thoroughness. The analysis will be less verbose this time, as there is nothing new under the sun.


```r
est_auto_un <- estimate(crox_train, p=3, q = 1)
```

```
#> 
#> Coefficients of Original polynomial:  
#> 1.6557 -0.4727 -0.1834 
#> 
#> Factor                 Roots                Abs Recip    System Freq 
#> 1-0.9979B              1.0021               0.9979       0.0000
#> 1-0.8692B              1.1504               0.8692       0.0000
#> 1+0.2114B             -4.7306               0.2114       0.5000
#>   
#> 
```

```r
plot_res(est_auto_un$res)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-15-1.svg" alt="**Figure 9 :** These residuals do not look much better, at a higher model complexity..."  />
<p class="caption">**Figure 9 :** These residuals do not look much better, at a higher model complexity...</p>
</div>

Lets go ahead and check with our Ljung Box too:


```r
ljung_box(est_auto_un$res, 3, 1)
```

```
#>            [,1]             [,2]            
#> test       "Ljung-Box test" "Ljung-Box test"
#> K          24               48              
#> chi.square 31               57              
#> df         20               44              
#> pval       0.05             0.086
```

This represents a very slight improvement, however at the risk of slightly overfitting. Lets see how much better our backtested forecast got:


```r
ase_auto_un <- assess(crox_train, arma, n.ahead=5, phi = est_auto_un$phi, theta = est_auto_un$theta)
title(main="5 step back forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-17-1.svg" alt="**Figure 10 :** Auto generated backtesting"  />
<p class="caption">**Figure 10 :** Auto generated backtesting</p>
</div>

```r
ase_auto_un
```

```
#> [1] 1.3
```

Interestingly, this model did slightly worse in backtesting. It is likely that the MA term is not totally appropriate with the series, as we did not see any indicators in the plots or anywhere else in the data that suggested MA, and we are just modeling noise. We will keep it for comparison. Lets move on to the differenced series now.

## Analysis of the differenced series

We will now look at the first order differenced data in more detail. First lets look at the sample plot:



```r
# assignment to save the grader from tswge output
x <- plotts.sample.wge(train_adj)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-18-1.svg" alt="**Figure 11 :** The differenced series is suggestive of MA or AR with complex conjugate and/or negative roots"  />
<p class="caption">**Figure 11 :** The differenced series is suggestive of MA or AR with complex conjugate and/or negative roots</p>
</div>

As the caption states, the oscillitory nature of the ACF, suggests AR with either conjugate or negative roots. What is more interesting is the parzen window on the bottom left. This sharply lumpy state can be indicative of a couple of things: first, it is possible we are left with just white noise and not enough data to flatten out the parzen window, and second: a mixture of AR and MA components. AR with complex conjugate roots would make the peaks, while MA would make the troughs. Since conjugate roots come in pairs, and the oscillations continue for a long time, we can assume both the AR and the MA components have an even number. The autocorrelations continue to extend and oscillate for a long period of time, with a slow sinusoidal damping pattern. This suggests to us that the AR component outweighs the MA component (and in general we prefer more AR than MA, as pure AR models are in general simpler and should generalize better). We will go with ARMA(4,2) because of these reasons (the parzen window is such a clear sign of an ARMA model).


```r
est_hand_diff <- estimate(train_adj, p=4,q=2)
```

```
#> 
#> Coefficients of Original polynomial:  
#> -0.8924 -0.9981 -0.0707 0.0710 
#> 
#> Factor                 Roots                Abs Recip    System Freq 
#> 1+0.7623B+0.9720B^2   -0.3921+-0.9354i      0.9859       0.3132
#> 1+0.3431B             -2.9147               0.3431       0.5000
#> 1-0.2131B              4.6937               0.2131       0.0000
#>   
#> 
```

These roots make a lot of sense. First of all, we have the complex conjugate, which explains the peak in the parzen window at 0.3132. The second root is negative, with a peak near the nyquist frequency in the parzen window. The third root is relatively weak, and shows the remaining wandering component to the series. We can also check the roots of the MA component of the model using the brilliant `factor.wge` function (note this doesnt explain the interaction between the two sets of roots, but it definitely helps)


```r
factor.wge(est_hand_diff$theta)
```

```
#> 
#> Coefficients of Original polynomial:  
#> -0.7350 -0.9402 
#> 
#> Factor                 Roots                Abs Recip    System Freq 
#> 1+0.7350B+0.9402B^2   -0.3909+-0.9544i      0.9697       0.3119
#>   
#> 
```

We see another complex conjugate root, with a frequencey of 0.3119. This is slightly less than the peak from the AR part, which probably explains the sharpness of the central peak. Again, we cannot model the interplay between the two sets of roots with simple factoring. Lets go ahead and run our tests:


```r
plot_res(est_hand_diff$res)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-21-1.svg" alt="**Figure 12 :** Not much change..."  />
<p class="caption">**Figure 12 :** Not much change...</p>
</div>

```r
ljung_box(est_hand_diff$res, p=4,q=2)
```

```
#>            [,1]             [,2]            
#> test       "Ljung-Box test" "Ljung-Box test"
#> K          24               48              
#> chi.square 25               47              
#> df         18               42              
#> pval       0.13             0.29
```

Our residual plot looks about the same, however finally we can safely accept the null hypothesis and say our residuals are stationary! Lets test the forecast out:


```r
ase_hand_diff <- assess(crox_train, aruma, n.ahead=5, phi = est_hand_diff$phi, theta =est_hand_diff$theta, d=1)
title(main="Backtesting ARIMA")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-22-1.svg" alt="**Figure 13 :** Worst forecast so far!"  />
<p class="caption">**Figure 13 :** Worst forecast so far!</p>
</div>

```r
ase_hand_diff
```

```
#> [1] 1.5
```

This is not a brilliant forecast, relative to what we have done previously. This is largely in part due to some of the facts regarding ARIMA. The integrated component of ARIMA is more or less a random walk, and the expected value of a random walk is just wherever you started. So, a forecast with d=1 and nothing else is just going to predict the last value over and over and over. When we add in the ARMA components, we are just adding a little wiggle to that same pattern. This means even if it is the most appropriate model, ARIMA is not always the best forecast. We will investigate this further in the conclusion section. Lets now try to automate the choice of model using information criterion again.

### Automated Analysis of differenced series:


```r
aics <- hush(aic5.wge(train_adj))
bics <- hush(aic5.wge(train_adj, type='bic'))
pander::pander(list(aics, bics))
```



  *

    --------------------------
     &nbsp;   p   q     aic
    -------- --- --- ---------
     **15**   4   2   -0.7054

     **4**    1   0   -0.7002

     **3**    0   2   -0.6984

     **2**    0   1   -0.6974

     **7**    2   0   -0.6972
    --------------------------

  *

    --------------------------
     &nbsp;   p   q     bic
    -------- --- --- ---------
     **4**    1   0   -0.6832

     **2**    0   1   -0.6804

     **3**    0   2   -0.673

     **7**    2   0   -0.6718

     **5**    1   1   -0.6715
    --------------------------


<!-- end of list -->

It looks like a simple ARMA(1,0) model is most appropriate (other than our 4,2 model, which did not fare so well). I like the idea of this simple model, lets estimate the parameters and check out the residuals



```r
est_auto_diff <- estimate(train_adj, p =1)
```

```
#> 
#> Coefficients of Original polynomial:  
#> -0.1690 
#> 
#> Factor                 Roots                Abs Recip    System Freq 
#> 1+0.1690B             -5.9183               0.1690       0.5000
#>   
#> 
```

```r
plot_res(est_auto_diff$res)
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-24-1.svg" alt="**Figure 14 :** Final residual plot looks slightly worse than the previous"  />
<p class="caption">**Figure 14 :** Final residual plot looks slightly worse than the previous</p>
</div>

```r
ljung_box(est_auto_diff$res, p=1, q=0)
```

```
#>            [,1]             [,2]            
#> test       "Ljung-Box test" "Ljung-Box test"
#> K          24               48              
#> chi.square 34               58              
#> df         23               47              
#> pval       0.072            0.12
```

This got us slightly worse than the previous model as far as modeling the noise, but these portmanteau tests are simply guidelines, as they are pretty weak. We can proceed with assessing the model:


```r
ase_auto_diff <- assess(crox_train, aruma, n.ahead=5, phi = est_auto_diff$phi, d=1)
title(main="Backtesting ARIMA")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-25-1.svg" alt="Final backtest, looks like a classic d=1 model"  />
<p class="caption">Final backtest, looks like a classic d=1 model</p>
</div>

```r
ase_auto_diff
```

```
#> [1] 1.3
```

This model performed all right. It is simpler than the previous model, and performed better than it, so I am more likely to choose it over the 4,2 model. Lets go ahead and test all the models on the holdout set:

# Conclusions

We will start by testing the first model made, by hand using no seasonal differencing on the test set:


```r
f_un_h <- fcst( arma, crox_train, n.ahead=5, phi = train_hand_est$phi)
lines(x=(length(train_indices)):nrow(cstock_2years), y=c(crox_train[length(crox_train)],crox_test), col='red')
title(main="Hand made forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-26-1.svg" alt="**Figure 15 :** An alright forecast"  />
<p class="caption">**Figure 15 :** An alright forecast</p>
</div>

```r
ase(crox_test, f_un_h)
```

```
#> [1] 3.3
```

This is not a brilliant forecast, but it is not horrible, we have to keep in mind that at this point in time there is already worry over a global pandemic. Lets look at the automatically made forecast and then discuss ARMA forecasts in general:


```r
f_a_h <- fcst(arma,crox_train, n.ahead=5, phi = est_auto_un$phi, theta = est_auto_un$theta)
lines(x=(length(train_indices)):nrow(cstock_2years), y=c(crox_train[length(crox_train)],crox_test), col='red')
title(main="Grid/Criterion made forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-27-1.svg" alt="**Figure 16 :** A better forecast!"  />
<p class="caption">**Figure 16 :** A better forecast!</p>
</div>

```r
ase(crox_test, f_a_h)
```

```
#> [1] 3.1
```

This did slightly better! We were able to capture more of the downward trend more quickly with the stronger fit by including the MA term. Lets briefly discuss the other advantages of not including the I term. With ARMA models, in general we are suggesting that things are going to trend towards the mean, as they are stationary models. For goods which do not explode in demand, but are still pretty constantly demanded, such as crocs, this is a reasonable assumption. We can display this by making a very long forecast with our ARMA model:


```r
f_l <- fcst(arma,crox_train, n.ahead=1600, phi = est_auto_un$phi, theta = est_auto_un$theta)
abline(h=mean(crox_train), col='red')
title(main="Extreme ARMA forecasting")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-28-1.svg" alt="**Figure 17 :** Trending towards the mean(in red)..."  />
<p class="caption">**Figure 17 :** Trending towards the mean(in red)...</p>
</div>

As t approaches infinity, we will eventually just start to forecast the mean over and over. So ARMA models are conservating, saying things will stay the same as they have in the past. Lets check out our ARIMA models now.


```r
f_d_h <- fcst(aruma, crox_train, n.ahead=5, phi = est_hand_diff$phi, theta =est_hand_diff$theta, d=1)
lines(x=(length(train_indices)):nrow(cstock_2years), y=c(crox_train[length(crox_train)],crox_test), col='red')
title(main="Hand made first order difference forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-29-1.svg" alt="**Figure 18 :** The slope appears way off"  />
<p class="caption">**Figure 18 :** The slope appears way off</p>
</div>

```r
ase(crox_test, f_d_h)
```

```
#> [1] 3.5
```

Our worst forecast so far. We will also go ahead and check out the automated one:


```r
f_a_d <- fcst(aruma, crox_train, n.ahead=5, phi = est_auto_diff$phi, d=1)
lines(x=(length(train_indices)):nrow(cstock_2years), y=c(crox_train[length(crox_train)],crox_test), col='red')
title(main="BIC made first order difference forecast")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-30-1.svg" alt="**Figure 19 :** Our dead simple model had an even worse forecast..."  />
<p class="caption">**Figure 19 :** Our dead simple model had an even worse forecast...</p>
</div>

```r
ase(crox_test, f_a_d)
```

```
#> [1] 3.8
```

This is the worst model we made. Why did the differenced models perform so poorly? Isnt ARIMA a powerful tool?? The reason is this: When you make an ARIMA forecast, you are trying to predict a random walk, or an AR process at with a root at 1. When you do this, the math happens to fall in place such that the model just predicts the last value seen repeatedly (with d=2, the slope of the last 2 values, with d=3, you are doing something wrong). We can test this by making another extremely long forecast:


```r
f_l <- fcst(aruma, crox_train, n.ahead=1600, phi = est_auto_diff$phi, d=1)
abline(h=mean(crox_train), col='red')
title(main="Extreme ARIMA forecasting")
```

<div class="figure" style="text-align: center">
<img src="d_scratch_files/figure-html/unnamed-chunk-31-1.svg" alt="**Figure 20 :** I predict things will be exactly the same as they are right now for the rest of time"  />
<p class="caption">**Figure 20 :** I predict things will be exactly the same as they are right now for the rest of time</p>
</div>

ARIMA is a powerful tool, and incorporating the random walk/integrated component can help us build powerful models (especially with seasonality involved), however when the only thing that seems to be going on is an integrated component (or strong AR), it is often better to underfit that as a strong AR model than overfit it as an ARIMA model. All in all, the toolset given by tswge is a powerful one. For further reference, please refer to the study guide written by the authors of this report ([link](https://josephsdavid.github.io/tstest/)), the tswgewrapped documnetation on [rdrr.io](https://rdrr.io/github/josephsdavid/tswgewrapped/), the Applied Time Series Analysis with R book, and the brilliant book by Rob J Hyndman, the [bible for forecasting, fpp2](https://otexts.com/fpp2/).
