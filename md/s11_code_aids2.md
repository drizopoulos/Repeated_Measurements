```r
# some comments
xyplot(sqrt(CD4) ~ obstime | drug, group = patient, data = aids,
       type = "l", col = 1, xlab = "Time (months)", ylab = "square root CD4 cell count")
```
