# MechaCar_Statistical_Analysis

## Project Overview

## Linear Regression to Predict MPG
```
Call:
lm(formula = mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + 
    vehicle_length, data = mecha)

Residuals:
     Min       1Q   Median       3Q      Max 
-19.4701  -4.4994  -0.0692   5.4433  18.5849 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -1.040e+02  1.585e+01  -6.559 5.08e-08 ***
AWD              -3.411e+00  2.535e+00  -1.346   0.1852    
ground_clearance  3.546e+00  5.412e-01   6.551 5.21e-08 ***
spoiler_angle     6.877e-02  6.653e-02   1.034   0.3069    
vehicle_weight    1.245e-03  6.890e-04   1.807   0.0776 .  
vehicle_length    6.267e+00  6.553e-01   9.563 2.60e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.774 on 44 degrees of freedom
Multiple R-squared:  0.7149,	Adjusted R-squared:  0.6825 
F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11
```


## Summary Statistics on Suspension Coils

### Total Suspension Summary
```

```
### Individual Lot Suspension Summary
```

```

## T-Tests on Suspension Coils

### Results

#### Total T-Test Summary
```
	One Sample t-test

data:  (suspension$PSI)
t = -1.8931, df = 149, p-value = 0.06028
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1497.507 1500.053
sample estimates:
mean of x 
  1498.78 
```

#### Lot 1 T-Test Summary
```
	One Sample t-test

data:  subset(suspension, Manufacturing_Lot == "Lot1")$PSI
t = 0, df = 49, p-value = 1
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1499.719 1500.281
sample estimates:
mean of x 
     1500 
```

#### Lot 2 T-Test Summary
```
	One Sample t-test

data:  subset(suspension, Manufacturing_Lot == "Lot2")$PSI
t = 0.51745, df = 49, p-value = 0.6072
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1499.423 1500.977
sample estimates:
mean of x 
   1500.2 
```

#### Lot 3 T-Test Summary
```
	One Sample t-test

data:  subset(suspension, Manufacturing_Lot == "Lot3")$PSI
t = -2.0916, df = 49, p-value = 0.04168
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1492.431 1499.849
sample estimates:
mean of x 
  1496.14 
```



