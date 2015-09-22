# Courses
<p>This repository contains a <a href="http://shiny.rstudio.com/" target="_blank">shiny app</a> that replicates all analyses presented in the course <strong>Statistical Analysis of Repeated Measurements Data</strong>, including also some additional illustrations. The app requires <a href="http://cran.r-project.org/" target="_blank">R</a> (version >= 3.2.2) and the following packages:</p>
<ul style="list-style-type:circle">
  <li>
     <a href="http://cran.r-project.org/package=nlme" target="_blank">nlme</a> (version >= 3.1-121)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=lme4" target="_blank">lme4</a> (version >= 1.1-8)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=geepack" target="_blank">geepack</a> (version >= 1.2-0)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=MCMCglmm" target="_blank">MCMCglmm</a> (version >= 2.21)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=shiny" target="_blank">shiny</a> (version >= 0.12.1)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=lattice" target="_blank">lattice</a> (version >= 0.20-33)
  </li>
  <li>
     splines (available within base <a href="http://cran.r-project.org/" target="_blank">R</a>)
  </li>
  <li>
     <a href="http://cran.r-project.org/package=corrplot" target="_blank">corrplot</a> (version >= 0.73)
  </li>
</ul>
<p>These packages can be installed using the following function call:</p>
```r
install.packages(c("shiny", "nlme", "lattice", "lme4", "MCMCglmm", 
                   "geepack", "corrplot"), dependencies = TRUE)
```
and then the app can be directly invoked using the command:
```r
shiny::runGitHub("Repeated_Measurements", "drizopoulos")
```
<br/>
<p>The app will automatically load these packages and also load the data sets used in 
the course. If you would like to interactively run the code in your own R session, then you will need first 
to load the packages using the commands:</p>
```r
library("shiny")
library("lattice")
library("nlme")
library("lme4")
library("geepack")
library("MCMCglmm")
library("splines")
library("corrplot")
```
and also load the data sets from 
<a href="https://github.com/drizopoulos/Courses" target="_blank">GitHub</a> using the commands:
```r
con <- url("https://raw.github.com/drizopoulos/Repeated_Measurements/master/Data.RData")
load(con)
close(con)
```
