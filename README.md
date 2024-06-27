# EDA-Virtual_Internship-Quantium-Part2
During my Virtual Internship at Quantium Company, I gained valuable skills in data analysis, hypothesis testing, and data visualization using the R programming language. I worked on various tasks that enhanced my ability to analyze complex datasets
## What exactly I learned
- Understand experimentation and uplift testing, comparing trial and control stores.
- Learn control store selection based on defined metrics.
- Gain experience in data visualization.
- Perform statistical analysis to assess sales differences and formulate recommendations.
## Project Objectives:
- Define metrics to select control stores.
- Analyze trial stores against controls.
- Use R/Python for data analysis and visualization and summarise findings and provide recommendations.
## Method Used:
- Data Analysis
- Statistical Analysis
- uplift testing
- data Visualization
## Tools Used:
- R studio
## Libraries:
- data.table
- ggplot2
- tidyr
- dplyr
## Dataset:
The data table has been cleaned as described in Part 1
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/6dfd172c-9cc9-4a6b-9dc8-7cfa7528a71e)

## Step-by-step:
### Select control stores

The client has selected store numbers 77, 86 and 88 as trial stores and want control stores to be established stores that are operational for the entire observation period. We would want to match trial stores to control stores that are similar to the trial store prior to the trial period of Feb 2019 in terms of : - Monthly overall sales revenue - Monthly number of customers - Monthly number of transactions per customer Let’s first create the metrics of interest and filter to stores that are present throughout the pre-trial period.
By creating a new table (measureOvertime) with new columns(below):

![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/f82d5c1c-1750-4c5f-bbc1-a5c14b97ac45)

we need to work out a way of ranking how similar each potential control store is to the trial store. We can calculate how correlated the performance of each store is to the trial store. Let’s write a function for this so that we don’t have to calculate this for each trial store and control store pair

```R
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
          calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure =numeric())
          storeNumbers <- unique(inputTable[,STORE_NBR])
          for (i in storeNumbers) {
          calculatedMeasure = data.table("Store1" = storeComparison,
                                          "Store2" = i ,
                                          "corr_measure" =cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)],
                                          inputTable[STORE_NBR == i, eval(metricCol)]))
          
          calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
          }
          return(calcCorrTable)
}
```

Calculate a standardised metric based on the absolute difference between the trial store's performance and each control store's performance.
```R
#### Create a function to calculate a standardised magnitude distance for a measure,
#### looping through each control store
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
    calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
    numeric(), measure = numeric())
    storeNumbers <- unique(inputTable[, STORE_NBR])
    for (i in storeNumbers) {
        calculatedMeasure = data.table("Store1" = storeComparison
        , "Store2" = i, "YEARMONTH" = inputTable[STORE_NBR ==storeComparison, YEARMONTH]
        , "measure" = abs(inputTable[STORE_NBR ==storeComparison, eval(metricCol)]- inputTable[STORE_NBR == i,
        eval(metricCol)])
        )
        calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    }
#### Standardise the magnitude distance so that the measure ranges from 0 to 1
minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                  by = c("Store1", "YEARMONTH")]
                  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
                  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
                  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                  .(Store1, Store2)]
                  return(finalDistTable)
}
```

Let’s take a simple average of the correlation and magnitude scores for each driver. Note that if we consider it more important for the trend of the drivers to be similar, we can increase the weight of the correlation score (a simple average gives a weight of 0.5 to the corr_weight) or if we consider the absolute size of the drivers to be more important, we can lower the weight of the correlation score.
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/d5f8fb17-d82b-4657-a7d2-ad2cbf226a80)
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/928fe759-8823-4782-8700-61e980602d40)
The store with the highest score is then selected as the control store since it is most similar to the trial store.

Now that we have found a control store, let’s check visually if the drivers are indeed similar in the period before the trial. We’ll look at total sales first.
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/2c099db2-e029-4f3a-bb77-f17e3a5f7592)

Next, the number of customers.
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/cb3c778c-d4b7-4588-98b9-ee5382d1e4b3)
###  Assessment of trial
The trial period goes from the start of February 2019 to April 2019. We now want to see if there has been an uplift in overall chip sales.
We’ll start with scaling the control store’s sales to a level similar to control for any differences between the two stores outside of the trial period.
```{R}
#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]
```
Now that we have comparable sales figures for the control store, we can calculate the percentage difference between the scaled control sales and the trial store's sales during the trial period.
```R
#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[,c("YEARMONTH","controlSales")], measureOverTime[STORE_NBR == trial_store,.SD,.SDcols = 1:3][,c("YEARMONTH","totSales")],
by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]
```

```R
#### As our null hypothesis is that the trial period is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
#### Note that there are 8 months in the pre-trial period
#### hence 8 - 1 = 7 degrees of freedom 
degreesOfFreedom <- 7
#### We will test with a null hypothesis of there being 0 difference between trial and control stores.
#### Calculate the t-values for the trial months. After that, find the 95th percentile of the t distribution with the appropriate degrees of freedom
#### to check whether the hypothesis is statistically significant.
#### Hint: The test statistic here is (x - u)/standard deviation
percentageDiff[, tValue :=(percentageDiff-0)/stdDev
][, TransactionMonth := as.Date(paste(substr(YEARMONTH, 1, 4), substr(YEARMONTH, 5, 6), "01", sep = "-"), "%Y-%m-%d")][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]
```
We can observe that the t-value is much larger than the 95th percentile value of the t-distribution for March and April - i.e. the increase in sales in the trial store in March and April is statistically greater than in the control store.

Let's create a more visual version of this by plotting the sales of the control store, the sales of the trial stores and the 95th percentile value of sales of the control store.

```R
measureOverTimeSales <- measureOverTime
#### Trial and control store total sales
#### Create new variables Store_type, totSales and TransactionMonth in the data table.
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales :=mean(totSales), by=c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(substr(YEARMONTH, 1, 4), substr(YEARMONTH, 5, 6), "01", sep = "-"), "%Y-%m-%d")][Store_type %in% c("Trial", "Control"),]
#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence
interval"]
#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
```
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/36e9007d-78c9-49e6-991f-8220d8295b26)
The results show that the trial in store 77 is significantly different to its control store in the trial period as the trial store performance lies outside the 5% to 95% confidence interval of the control store in two of the three trial months.

Let's have a look at assessing this for number of customers as well
![image](https://github.com/KeithDang1610/EDA-Virtual_Internship-Quantium-Part2/assets/167521177/399d64a9-eeb7-4b1f-90f4-437af9e14f4f)
Let's again see if the difference is significant visually!

Let's repeat finding the control store and assessing the impact of the trial for
each of the other two trial stores (86,88)

## Conclusion
We’ve found control stores 233, 155, 237 for trial stores 77, 86 and 88 respectively.
The results for trial stores 77 and 88 during the trial period show a significant difference in at least two of the three trial months but this is not the case for trial store 86. We can check with the client if the implementation of the trial was different in trial store 86 but overall, the trial shows a significant increase in sales. Now that we have finished our analysis, we can prepare our presentation to the Category Manager.




