# shiny app for Repeated Measurements Course
This repository contains a [shiny](http://shiny.rstudio.com/) application that replicates 
all analyses presented in the course 
**Statistical Analysis of Repeated Measurements Data**, including also some additional 
illustrations. The app requires [R](http://cran.r-project.org/) 
(version >= 4.2.2) and the following packages:

* [nlme](http://cran.r-project.org/package=nlme) (version >= 3.1-161)
* [lme4](http://cran.r-project.org/package=lme4) (version >= 1.1-31)
* [geepack](http://cran.r-project.org/package=geepack) (version >= 1.3-9)
* [GLMMadaptive](http://cran.r-project.org/package=GLMMadaptive) (version >= 0.8-6)
* [MASS](http://cran.r-project.org/package=MASS) (version >= 7.3-58.2)
* [shiny](http://cran.r-project.org/package=shiny) (version >= 1.7.4)
* [lattice](http://cran.r-project.org/package=lattice) (version >= 0.20-45)
* [splines](http://cran.r-project.org/) (available within base R)
* [corrplot](http://cran.r-project.org/package=corrplot) (version >= 0.92)

These packages can be installed using the following function call:
```r
install.packages(c("shiny", "nlme", "lattice", "lme4", "GLMMadaptive", "MASS",
                   "geepack", "corrplot"), dependencies = TRUE)
```
and then the app can be directly invoked using the command:
```r
shiny::runGitHub("Repeated_Measurements", "drizopoulos")
```

The app will automatically load these packages and also load the data sets used in the 
course. If you would like to interactively run the code in your own R session, then you 
will need first to load the packages using the commands:
```r
library("shiny")
library("lattice")
library("nlme")
library("lme4")
library("geepack")
library("GLMMadaptive")
library("MASS")
library("splines")
library("corrplot")
```
and also load the data sets from [GitHub](https://github.com/drizopoulos/Repeated_Measurements) 
using the commands:
```r
con <- url("https://raw.github.com/drizopoulos/Repeated_Measurements/master/Data.RData")
load(con)
close(con)
```
