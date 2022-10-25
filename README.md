# MechaCar Statistical Analysis

## Project Overview

The purpose of this project is to analyze data for a new prototype car, the MechaCar. There are production issues that are blocking the manufacturing team's progress and we are tasked to review the production data for insights. Using the programming language R, we will perform multiple linear regressions, summary statistics, and t-tests to produce statistical interpretations.

## Linear Regression to Predict MPG
The `MechaCar_mpg.csv` dataset contains mpg test results for 50 prototype MechaCars. The MechaCar prototypes were produced using multiple design specifications to identify ideal vehicle performance. We will perform a multiple linear regression model to predict the `mpg` of MechaCar prototypes. In our analysis we will use:
+ All Wheel Drive: `AWD`
+ Ground Clearance: `ground_clearance`
+ Spoiler Angle: `spoiler_angle`
+ Vehicle Weight: `vehicle_weight`
+ Vehicle Length: `vehicle_length`

#### Hypothesis
+ $H_0$: The slope of the linear model is zero, or m = 0.
+ $H_a$: The slope of the linear model is not zero, or m ≠ 0.

#### Multiple Linear Regression Model
```
lm(mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data=mecha)
summary(lm(mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data=mecha)) 
```
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
In our analysis, we can see that `ground_clearance` and `vehicle_length` have a p-value of 5.21e-08 and 2.60e-12, respectively. Since both values are smaller than the significance level of 0.05, therefore both variables are statistically significant resulting in rejecting the null hypothesis. In other words, the slope of the linear model is not zero and these coefficients provided a non-random amount of variance to the mpg values in the dataset. Additionally, the R-Squared value of this multiple linear regression model is 0.71. The coefficient of determination represents how well the regression model approximates real-world data points. In our case, we have a strong likelihood that the model can be used to approximate the `mpg` values in the dataset.

## Summary Statistics on Suspension Coils

The MechaCar `Suspension_Coil.csv` dataset contains the results from multiple production lots. In this dataset, the weight capacities of multiple suspension coils were tested to determine if the manufacturing process is consistent across production lots. We created summary statistics for the suspension coil’s PSI continuous variable across all manufacturing lots and the PSI metrics for each lot.

#### Total Suspension Summary
```
total_summary <- suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')
```
| Mean    | Median | Variance | SD       |
| ------- | ------ | -------- | -------- |
| 1498.78 | 1500   | 62.29356 | 7.892627 |

In the summary of the entire suspension coil dataset, we can see that the mean PSI is 1498.78 and the median PSI is 1500. Additionally, the variance is 62.29356 and the standard deviation is 7.892627. Overall, since the variance of the suspension coils does not exceed 100 pounds per square inch, it meets the design specifications for the MechaCar suspension coils. 

#### Individual Lot Suspension Summary
```
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')
```
| Manufacturing_Lot | Mean    | Median   | Variance    | SD          |
| ----------------- | ------- | -------- | ----------- | ----------- |
| Lot1              | 1500.00 | 1500.00  | 0.9795918   | 0.9897433   |
| Lot2              | 1500.20 | 1500.00  | 7.4693878   | 2.7330181   |
| Lot3              | 1496.14 | 1498.50  | 170.2861224 | 13.0493725  |

In the summary of the individual lots, we can see that the mean and median between the lots are similar. The variance for Lot 1 is 0.98, Lot 2 is 7.47, and Lot 3 is 170.29. Since the variance of the suspension coils for Lot 1 and Lot 2 are below 100 pounds per square inch, it meets the design specifications for the MechaCar suspension coils. However, the Lot 3 variance exceeds the 100 pounds per square inch, so it does not meet the design specifications.

## T-Tests on Suspension Coils
To further our analysis, we will perform t-tests to determine if all manufacturing lots and each lot individually are statistically different from the population mean of 1,500 pounds per square inch.

#### Hypothesis
+ $H_0$: There is no statistical difference between the observed sample mean and its presumed population mean.
+ $H_a$: There is a statistical difference between the observed sample mean and its presumed population mean.

#### Total T-Test Summary
```
t.test((suspension$PSI),mu=1500)
```
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
Based on our results for the t-test that compares all manufacturing lots against the mean PSI of the population, we can interpret that there is no statistical difference between the observed sample mean and its presumed population mean since the p-value is 0.06. It is not statistically significant as the p-value is greater than our significance level of 0.05. 

#### Lot 1 T-Test Summary
```
t.test(subset(suspension, Manufacturing_Lot == "Lot1")$PSI, mu=1500)
```
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
Based on our results for the t-test that compares manufacturing lot 1 against the mean PSI of the population, we can interpret that there is no statistical difference between the observed sample mean and its presumed population mean since the p-value is 1. It is not statistically significant as the p-value is greater than our significance level of 0.05.

#### Lot 2 T-Test Summary
```
t.test(subset(suspension, Manufacturing_Lot == "Lot2")$PSI, mu=1500)
```
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
Based on our results for the t-test that compares manufacturing lot 2 against the mean PSI of the population, we can interpret that there is no statistical difference between the observed sample mean and its presumed population mean since the p-value is 0.6. It is not statistically significant as the p-value is greater than our significance level of 0.05.

#### Lot 3 T-Test Summary
```
t.test(subset(suspension, Manufacturing_Lot == "Lot3")$PSI, mu=1500)
```
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
Based on our results for the t-test that compares manufacturing lot 3 against the mean PSI of the population, we can interpret that there is a statistical difference between the observed sample mean and its presumed population mean since the p-value is 0.04. It is statistically significant as the p-value is smaller than our significance level of 0.05.

## Study Design: MechaCar vs Competition

To further our analysis, we will design a statistical study to compare vehicle performance of the MechaCar vehicles against vehicles from other manufacturers.

#### Test Metrics

+ Fuel Efficiency: Measured by calculating the distance traveled by a vehicle and the amount of fuel consumed.
+ Vehicle Price: The price of vehicle sold in the market.

#### Hypothesis
+ $H_0$: There is no statistical difference between the observed mean of fuel efficiency and price with the observed mean of its competitors.
+ $H_a$: There is a statistical difference between the observed mean of fuel efficiency and price with the observed mean of its competitors.

#### Statistical Test
Since we are determining whether the means of two-samples (MechaCar vs Competitors) are statistically different, we will be utilizing the two-sample t-test to perform this analysis. The two dependent variables in the two analyses would be fuel efficiency and vehicle price.

#### Data Source
Since data would be difficult to obtain before the release of competitor cars, we can collect or scrape the data from outside sources on previous car models in the same class as the MechaCar. The data could potentially be found through competitor webpages since the information is public after the car's release. We would need the fuel efficiency and vehicle price data from the competitors. 

