library(dplyr)

#Deliverable 1

#import csv file
mpg_data <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
#perform linear regression
lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,mpg_data)
#summarize
summary(lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, mpg_data) ) 

#Deliverable 2

#import csv file into dataframe
sus_data <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

#total summary dataframe
total_summary <- sus_data %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups = 'keep')

#create lot summary dataframe
lot_summary <- sus_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups = 'keep')

#Deliverable 3
# t test for all the lots
t.test(sus_data$PSI,mu=mean(sus_data$PSI))

# t test for lot 1
t.test(subset(sus_data$PSI,sus_data$Manufacturing_Lot == "Lot1"),mu=mean(sus_data$PSI))

# t test for lot 2
t.test(subset(sus_data$PSI,sus_data$Manufacturing_Lot == "Lot2"),mu=mean(sus_data$PSI))

# t test for lot 3
t.test(subset(sus_data$PSI,sus_data$Manufacturing_Lot == "Lot3"),mu=mean(sus_data$PSI))