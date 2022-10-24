library(dplyr)

# Read CSV
mecha <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Linear Regression to Predict MPG
lm(mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data=mecha)
summary(lm(mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data=mecha)) 

# Create Visualizations for the Trip Analysis
total_summary <- suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

# T-Tests on Suspension Coils
t.test((suspension$PSI),mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot1")$PSI, mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot2")$PSI, mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot3")$PSI, mu=1500)