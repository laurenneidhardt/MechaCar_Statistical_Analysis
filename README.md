# MechaCar_Statistical_Analysis
AutosRUs’ newest prototype, the MechaCar, is suffering from production troubles that are blocking the manufacturing team’s progress. AutosRUs’ upper management has called on the data analytics team to review the production data for insights that may help the manufacturing team.

## Linear Regression to Predict MPG

![Deliverable1](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/Deliverable1.PNG)

*Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
   
   Ground clearance and vehicle length had the most non-random amounts of variance meaning they most likely effect the mpg. The other variables, vehicle weight, spoiler angle, and AWD have p-Values that indicate a random amount of variance.


*Is the slope of the linear model considered to be zero? Why or why not? 
    
    It is not considered to be zero because all of the variables are directly proportional to the mpg values and the p-value is far lower than 0.05 (reject the null hypothesis)


*Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
    
    It does not, 

## Summary Statistics on Suspension Coils
*The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?

![Deliverable2_TotalSummary](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/Deliverable2_TotalSummary.PNG)

When taking the total, the variance is around 62 which is well within the 100 PSI limit.


![Deliverable2_LotSummary](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/Deliverable2_LotSummary.PNG)

When looking at the three lots individually, we can see that the first two are within the limits of 100PSI but the third is far over.


## T-Tests on Suspension Coils

Lot 1, we cannot reject the null hypothesis as the p-value is greater than 0.05.
![TTEST_Lot1](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/TTEST_Lot1.PNG)

Lot 2, we  reject the null hypothesis as the p-value is less than 0.05.
![TTEST_Lot2](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/TTEST_Lot2.PNG)

Lot 3, we cannot reject the null hypothesis as the p-value is greater than 0.05.
![TTESTLot3](https://github.com/laurenneidhardt/MechaCar_Statistical_Analysis/blob/main/TTESTLot3.PNG)


## Study Design: MechaCar vs Competition

*What metric or metrics are you going to test?
   
   It would beneficial to have additional data such as what competitor vehicles are comparable, vehicle cost, annual maintenance costs, mpg city vs highway, safety ratings, fuel type, engine type.

*What is the null hypothesis or alternative hypothesis?

*What statistical test would you use to test the hypothesis? And why? 

   Linear Regression models because we could look at multiple metrics against one another and the hypothesis.

*What data is needed to run the statistical test?
