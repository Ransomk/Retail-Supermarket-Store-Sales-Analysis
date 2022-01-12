# Project 1 - Walmart Data

# Read csv file to Data fram SalesData
SalesData <- read.csv("~/Documents/Data Science Program/Data Science with R Course files/Assignment 1/Walmart_Store_sales.csv")

# Set the Store as a factor from type int > We got 45 Stores as seen in the data
SalesData$Store <- as.factor(SalesData$Store)
summary(SalesData)

library(dplyr)
# Q1 > Basic Statistics tasks > Which store has maximum sales
# Step 1: First take the Sum of Weekly_Sales data and group it by store. Saved this result in SumSales
sumSales <- summarize(group_by(SalesData,Store), sumOfSales = sum(Weekly_Sales))
summary(sumSales)
# Step 2: Next we find the store with max sales by using filter and providing the logical check where
# the sumOfSales is equal to the max value. 
storeWithMaxSales <- filter(sumSales,sumSales$sumOfSales==max(sumSales$sumOfSales))
storeWithMaxSales
# Solution : We get the answer is store 20 which has the max total sales. 
#Store sumOfSales
# 20    301397792

# Q2 > Basic Statistics tasks > Which store has maximum Std deviation in Sales and coeff of mean to StdDeviation
# Step 1: find the std deviation and mean on the sales data using summarize again group by store
stdDevSales <- summarize(group_by(SalesData,Store),stdDev = sd(Weekly_Sales), meanSales = mean(Weekly_Sales))
# Step 2: Now we find the store with max Std deviation of sales by using filter and 
# applying condition for stdDev being equal to max among all stores
storeWithHighSalesVariation <- filter(stdDevSales,stdDevSales$stdDev==max(stdDevSales$stdDev))
storeWithHighSalesVariation
# Solution : We get the answer is store 14 which has the max variation in sales as per the max Standard deviation found. 
#Store stdDev
# 14    317570.
# Step 3: Find the coefficient of mean to std Deviation. Created a new variable Coefficient in the table and divided SD with mean
stdDevSales$Coefficient <- stdDevSales$stdDev/stdDevSales$meanSales
stdDevSales[,c("Store","Coefficient")]
storeWithMaxCoeff_SdtoMean <- filter(stdDevSales,stdDevSales$Coefficient==max(stdDevSales$Coefficient))
storeWithMaxCoeff_SdtoMean 
# As per the Coefficient of standard Deviation to Sales , We see that Store 35 is having a higher deviation by this metric.
# The Coefficient is a good measure to compare as this scales the SD based on the average. 
# It creates a more standard scale to compare the deviation between the different stores.
# Store  stdDev meanSales Coefficient
# 35    211243.   919725.       0.230

#Q3 > Basic Statistics tasks > Which store/s has good quarterly growth rate in Q3’2012
# Step 1 : Convert the Date field in the SalesData frame to date type instead of char
library(lubridate)
SalesData$Date <- as.Date(SalesData$Date,format = "%d-%m-%Y")
# Step 2 : Identify dates in Q2 and Q3 of 2012 to compare sales growth and measure relative Growth rates.
# Note growth rate for Q3 measured as (Sales Q3 - Sales Q2 / Sales Q2)
str(SalesData$Date)
SalesData$month <- month(SalesData$Date)
# There's an easier way to do this identification of Quarter by using the luridate-Quarter function
?quarter
# Have updated the quarter to start from the second month since this is possible with the quarter function of lubridate package.
# Also starting from the second month as the Data starts from Feb-2010.
SalesData$QTR <- quarter(SalesData$Date,with_year = T, fiscal_start = 2)
# Note the output of these values has the next year tagged to the quarter since fiscal start was 2, Hence the need to subtract 1 from the output
SalesData$QTR <- SalesData$QTR - 1 
SalesData$QTR <- as.factor(SalesData$QTR)
summary(SalesData$QTR)
#Now extract the sales data for Q2 and Q3 of 2012 out of all the observations
Q2SalesData <- SalesData[SalesData$QTR=="2012.2",]
Q3SalesData <- SalesData[SalesData$QTR=="2012.3",]
# Next summarize the Sales for the Quarters by store. We need the total sales for each store in the Quarter.
# Using the total sales we can compare the sales between Q2 and Q3 for each store and obtain the growth rate.
SummedSalesDataforQ2of2012 <- summarize(group_by(Q2SalesData,Store),Q2Sales = sum(Weekly_Sales))
SummedSalesDataforQ3of2012 <- summarize(group_by(Q3SalesData,Store),Q3Sales = sum(Weekly_Sales))
CombinedQoQData2012 <- cbind(SummedSalesDataforQ2of2012,SummedSalesDataforQ3of2012[2])
CombinedQoQData2012$SalesGrowth <- ((CombinedQoQData2012$Q3Sales - CombinedQoQData2012$Q2Sales)/CombinedQoQData2012$Q2Sales)*100
maxSalesGrowthinQ3of2012 <- filter(CombinedQoQData2012,SalesGrowth==max(SalesGrowth))
maxSalesGrowthinQ3of2012
# In this scenario when fiscal start was set to 2 to split data into Quarters. 
# We note that the answer shows that most of the stores had a decline in sales in Q3 2012 when compared to Q2 2012
# Finally store which had max Sales growth is Store 39
#   Store  Q2Sales  Q3Sales SalesGrowth
# 1    39 20178609 20730481    2.734934

### --  Here we can try to do the same execution if the Fiscal start to decide quarters was set to 1 and we get a different result.
SalesData$Quarter <- quarter(SalesData$Date,with_year = T, fiscal_start = 1)
# In this case the Quarter years are correctly assigned since Fiscal start was set to 1.
SalesData$Quarter <- as.factor(SalesData$Quarter)
summary(SalesData$Quarter)
#Now extract the sales data for Q2 and Q3 of 2012 out of all the observations
Q2SalesData <- SalesData[SalesData$Quarter=="2012.2",]
Q3SalesData <- SalesData[SalesData$Quarter=="2012.3",]
# Next summarize the Sales for the Quarters by store. We need the total sales for each store in the Quarter.
# Using the total sales we can compare the sales between Q2 and Q3 for each store and obtain the growth rate.
SummedSalesDataforQ2of2012 <- summarize(group_by(Q2SalesData,Store),Q2Sales = sum(Weekly_Sales))
SummedSalesDataforQ3of2012 <- summarize(group_by(Q3SalesData,Store),Q3Sales = sum(Weekly_Sales))
CombinedQoQData2012 <- cbind(SummedSalesDataforQ2of2012,SummedSalesDataforQ3of2012[2])
CombinedQoQData2012$SalesGrowth <- ((CombinedQoQData2012$Q3Sales - CombinedQoQData2012$Q2Sales)/CombinedQoQData2012$Q2Sales)*100
maxSalesGrowthinQ3of2012 <- filter(CombinedQoQData2012,SalesGrowth==max(SalesGrowth))
maxSalesGrowthinQ3of2012
#  In this scenario when using calendar year for deciding the Quarter > 
# We get following result showing store 7 had the maximum Sales growth in Q3 2012 compared to previous quarter its sales grew by 13.3%
#   Store Q2Sales Q3Sales SalesGrowth
# 1     7 7290859 8262787    13.33078


#Q4 > Basic Statistics tasks > Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
# Compare holiday sales average to non holiday sales average . First we split to get non-holiday sales average
# step 1: Exclude the holiday data for the stores
NonHolidaySalesData <- SalesData[SalesData$Holiday_Flag==0,]
averageNonHolidaySales <- mean(NonHolidaySalesData$Weekly_Sales)
averageNonHolidaySales
# We see average NonHolidaySales is 1041256. Next we pick each of the holidays for the stores and compare with average sales.
# [1] 1041256

# For Superbowl the dates are : 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13 > Excluded 2013 date as data only upto max date (2012-10-26)
SuperBowlSalesData <- SalesData[SalesData$Date == "2010-02-12" | SalesData$Date == "2011-02-11" | 
                                SalesData$Date == "2012-02-10", ]
sum(SuperBowlSalesData$Holiday_Flag)
averageSuperBowlSales <- mean(SuperBowlSalesData$Weekly_Sales)
averageSuperBowlSales
# [1] 1079128
averageSuperBowlSales>=averageNonHolidaySales
# TRUE

# For Labour Day the dates are : 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13 >Excluded 2013 date as data only upto max date (2012-10-26)
LabourDaySalesData <- SalesData[SalesData$Date == "2010-09-10" | SalesData$Date == "2011-09-09" | 
                                SalesData$Date == "2012-09-07", ]
sum(LabourDaySalesData$Holiday_Flag)
averageLabourDaySales <- mean(LabourDaySalesData$Weekly_Sales)
averageLabourDaySales
# [1] 1042427
averageLabourDaySales>=averageNonHolidaySales
# TRUE

# For Thanksgiving the dates are : 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13 > Excluded 2013 date as data only upto max date (2012-10-26)
ThanksgivingSalesData <- SalesData[SalesData$Date == "2010-11-26" | SalesData$Date == "2011-11-25" | 
                                SalesData$Date == "2012-11-23", ]
sum(ThanksgivingSalesData$Holiday_Flag)
averageThanksgivingSales <- mean(ThanksgivingSalesData$Weekly_Sales)
averageThanksgivingSales
# [1] 1471273
averageThanksgivingSales>=averageNonHolidaySales
# TRUE

# For Christmas the dates are : 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13 > Excluded 2013 date as data only upto max date (2012-10-26)
ChristmasSalesData <- SalesData[SalesData$Date == "2010-12-31" | SalesData$Date == "2011-12-30" | 
                                SalesData$Date == "2012-12-28", ]
sum(ChristmasSalesData$Holiday_Flag)
averageChristmasSales <- mean(ChristmasSalesData$Weekly_Sales)
averageChristmasSales
# [1] 960833.1
averageChristmasSales>=averageNonHolidaySales
# FALSE

# To conclude we see that only for Christmas the Sales are below the Non holiday average.
# Of all the Holidays Thanksgiving has the max Holiday sales.
# One must note that the total dollar-value of the sales is not a fair comparison between Non-Holiday v/s Holidays > Since Holidays normally have a markdown
# Due to the markdown and discounts during holiday the $ dollar amount (Price) of any given product is lesser. 
# This means that while there could be a possibility that actual units sold were higher during holidays 
# The final dollar value of the Sales can actually be lesser or equal the Non discounted sales value during the normal period.
# So this comparison is dependent upon the amount of Holiday discount.
# A better measure to compare the impact of Holiday discounts on the Sales compared to Non-Holiday weeks would be 
# to compare actual units of goods sold across categories between the two periods(Holiday v/s Non-Holiday). 
# But this information is not currently available in the data-set provided for this analysis.

#Q5 > Basic Statistics tasks > Provide a monthly and semester view of sales in units and give insights

# Part 1 > Monthly sales split
MonthlySalesSummary <- summarize(group_by(SalesData,month),AverageSales = mean(Weekly_Sales), SalesDeviation = sd(Weekly_Sales),
                                 MinSales = min(Weekly_Sales), MaxSales=max(Weekly_Sales), MedianSales = median(Weekly_Sales))
View(MonthlySalesSummary)
# plot graph of average sales by month
barplot(AverageSales~month,data=MonthlySalesSummary, main="Monthly Average Sales",
        col="lavender",xlab = "Month", ylab = "Average Sales",ylim=c(0,1400000),cex.axis = 0.8)
?barplot
# box plot for average monthly sales
boxplot(Weekly_Sales~month, data=SalesData,main = "Boxplot Monthly Sales", col = "lightblue",
        xlab = "Month", ylab = "Weekly Sales",ylim=c(200000,3800000),cex.axis = 0.9)
# Insight the average sales are highest in last part (i.e. months 11 and 12) of the year.
# January has lowest average sales (923884.6) and December average sales(1281863.6) is the highest. 
# Also note that December has a higher Standard Deviation in the sales. 
#    > The box plot also confirms this as there are plenty of outlier points on the higher side of the scale in December.
# Also there appears to be a dip in sales during September and October where the average sales is below 1000000


#Part 2 > Semester wise split >
# Classify sales data into semesters. Adding semester variable to data.
SalesData$Semester <- semester(SalesData$Date,with_year = T)
SalesData$Semester <- as.factor(SalesData$Semester)
summary(SalesData$Semester)

SemesterSalesSummary <- summarize(group_by(SalesData,Semester),AverageSemesterSales = mean(Weekly_Sales), DeviationSemesterSales = sd(Weekly_Sales), 
                                  MinSales = min(Weekly_Sales), MaxSales=max(Weekly_Sales), MedianSales = median(Weekly_Sales))
View(SemesterSalesSummary)
boxplot(Weekly_Sales~Semester, data=SalesData,main = "Boxplot Semester Sales", col = "lightgreen",
        xlab = "Semester", ylab = "Weekly Sales",ylim=c(200000,3800000),cex.axis = 0.9)
barplot(AverageSemesterSales~Semester,data=SemesterSalesSummary, main="Semester Average Sales",
        col="gold",xlab = "Semester", ylab = "Average Weekly Sales",ylim=c(0,1250000),cex.axis = 0.8)
# Insights based on the Semester classification of data indicates following points:
# 1. On Average the 2nd Semester sales are higher than 1st Semester. Except for in the year 2012.  
#    This is logically conclusive as well since the monthly analysis validates that sales on average is higher in Nov and Dec months which fall in 2nd Semester.
#    Note that for 2012-2nd Semester data is incomplete as data was available only till October 2012. And as noted in monthly analysis the November and December month average sales tend to be the highest of all months in the year.
#    But there is not a large difference/disparity in the average weekly sales between the semesters.
#    Range of Average weekly sales is 1002080 to 1087128.
#    The lowest average weekly sales was recorded in the 1st Semester of 2011 and The highest was recorded in the 2nd Semester of 2011.
# 2. Note that again we see a high variance/sd of weekly sales in the 2nd Semester with a large number of outliers.
#    This again follows from our conclusion in Monthly sales analysis which revealed high number of outliers for weekly sales figures in Nov and Dec.


#### Task 1 : For Store 1 – Build  prediction models to forecast demand ####
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). 
# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
 
# Step1 > Let's filter the data for Store 1 only

Store1SalesData <- filter(SalesData,SalesData$Store==1)
# Remove first column as all values are referring to store number 1 itself
Store1SalesData <- Store1SalesData[-1]
# Ensuring that Holiday Flag and month are treated as factors to assess them based on the factor levels rather than as numeric data which would be incorrect.
Store1SalesData$Holiday_Flag <- as.factor(Store1SalesData$Holiday_Flag)
Store1SalesData$month <- as.factor(Store1SalesData$month)
str(Store1SalesData)

# Step 2 > Restructuring of Dates to a sequence > Update dates as 1,2 ....for 5 Feb 2010 (starting from the earliest date in order). 
Store1SalesData$Date <- as.numeric(Store1SalesData$Date)
itr = 1
for (date in Store1SalesData$Date) {
  Store1SalesData$Date[itr]=itr
  itr=itr+1
}
summary(Store1SalesData$Date)

# Step 3: 
# Attempt to Build regression models with all of the variables first and test various methods to improve models. 
Model0 <- lm(Weekly_Sales~.,data = Store1SalesData)
summary(Model0)
# As the levels of Semester and Quarter and QTR factor variables depend entirely on Past dates (Quarter/Semesters for years 2010 to 2012 based on the given past data).
# Further the values of these variables are derived based on the Date variable.
# So for future dates which we would like to predict demand for would never fall in the categories of factor variables Semester,Quarter and QTR which are currently defined.
# Predictor variables(factor) Semester and Quarter and QTR do not make sense to help predict demand on future data hence are removed from the revised model.
Model0 <- lm(Weekly_Sales~Date+Holiday_Flag+Temperature+Fuel_Price+CPI+Unemployment+month,data = Store1SalesData)
summary(Model0)
# After this Model0 has Adjusted R-squared:  0.3219 but still has variables that are statistically insignificant.
# Running the AIC Model on the updated model
Model0_AIC <- step(Model0)
summary(Model0_AIC)
# Final model with AIC is : Weekly_Sales ~ CPI + month
# This improved Adjusted R-squared to 0.3384 but model has statistically insignificant levels(month7,month9,month10) of the Month factor at 5% Level of significance.
# However we may consider this a better predictive model than the initial one Model0 as other months do significantly determine weekly sales.


#Checking for multi-collinearity using VIF
library(sp)
library(raster)
library(usdm) 
vif(Store1SalesData)
vif(Store1SalesData[,c(1,4,5,6)])
vifstep(Store1SalesData[,c(1,4,5,6)],th=5)
# As per VIF 1 variables from the 5 input variables have collinearity problem: Date
#---------- VIFs of the remained variables -------- 
#  Variables      VIF
#1 Temperature 1.062715
#2  Fuel_Price 2.439081
#3         CPI 2.344666
ModelVIF <- lm(Weekly_Sales~Temperature+Fuel_Price+CPI,data = Store1SalesData)
summary(ModelVIF)
# Fuel price is not statistically significant in the model and model has a Adjusted R-squared: 0.09505 
# which means these variables do not explain most of the Weekly_Sales variation.
ModelVIF1 <- lm(Weekly_Sales~Temperature+CPI,data = Store1SalesData)
summary(ModelVIF1)
# Removing Fuel price yields ModelVIF1 with where the remaining variables are statistically significant at 1% LoS
# But the model again has a low Adjusted R-squared: 0.1012 

# Step 4: Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# For this our Null Hypothesis : Weekly_Sales is not affected by CPI, unemployment, and fuel price

# Removing the variables other than Weekly_Sales, CPI, unemployment, and fuel price. 
Store1SalesReduced <- Store1SalesData[c("Weekly_Sales","Fuel_Price", "CPI","Unemployment")]
# Performing a base exploration of any patterns for weekly sales based on these variables
plot(Store1SalesReduced)
# Plots revealed no pattern for Weekly sales based on the numeric parameter variables.
cor(Store1SalesReduced)
# This is further reflected in the correlation matrix as the Correlation of Weekly sales with most independent factor variables is below 0.3 implying weak correlation.
#               Weekly_Sales 
# Fuel_Price     0.12459158  
# CPI            0.22540766
# Unemployment  -0.09795539

Model1 <- lm(Weekly_Sales~CPI+Unemployment+Fuel_Price, data = Store1SalesReduced)
summary(Model1)
# As per the linear Model1 Weekly sales cannot be well explained by CPI, unemployment, and fuel price
# Multiple R-squared:  0.08499,	Adjusted R-squared:  0.06524 
# The Adjusted R-Squared is just 6% which implies only 6% of the variation of weekly sales is effectively explained by  CPI, unemployment, and fuel price.
# Further Fuel price is highly insignificant to the model with a p-value of 0.16851
# Unemployment and CPI are statistically significant in terms of their impact on Weekly_Sales
# While the model itself has statistical significance at 5% LOS. Model has p-value: 0.006162
# Variables CPI, unemployment, and fuel price have lesser impact on Weekly_Sales.
# OR we could say that only a small part of variation of Weekly_Sales is explained by the variables.
# This model would not be recommended for making Sales/Demand predictions.

# Task 2 : Change dates into days by creating new variable.
SalesData$Day <- as.numeric(SalesData$Date)
itr = 1
min = min(SalesData$Day)
for(itr in 1:length(SalesData$Day)){
  if(SalesData$Day[itr]==min){
   SalesData$Day[itr] = 1 
  }
  else{
    SalesData$Day[itr] = SalesData$Day[itr]-min
  }
  itr=itr+1
}
summary(SalesData$Day)



