This dataset entails the property sales in the counties in Ireland in the year 2020 solely for the month of January. 
The dataset displays sales of apartments across all the  26 counties. The average house price was roughly around 256,000 Euros in 2020 (Slater, S. 2021). 
When it comes to the most expensive place to buy a house in Ireland, Dublin tops the chart. (Holland, L. Retrieved on 2021, June 25). 
The 26 counties have been divided into 4 Provinces – Connacht, Leinster, Munster, and Ulster (Republic of Ireland)  for better analysis of this dataset. 
I also plan to analyze the most expensive and the least expensive counties in terms of property prices. 
Accordingly, one-sample t tests and  2 sample t tests would be conducted to compare the means of  two groups, while conducting  hypothesis testing. 

###"R Practice-Module 4"###

#installing the required packages for the program
library(tidyverse)
library(janitor)
library(stringr)
library(dplyr)

#Getting the path of the CSV input file for property data 
path <- "C:\\Users\\dsouz\\Downloads\\Property_Sales_Ireland_sample_January_20210617.csv"

#Reading CSV file
property <- read.csv(path, header =TRUE, sep = ',')

#Checking Summary and structure of dataset
summary(property)
str(property)


#Renaming the column names in the dataframe 'property'
property <- property %>% dplyr::rename(date_of_sale = Date.of.Sale..dd.mm.yyyy.,
                                       postal_code = Postal.Code, 
                                       price = Price..â...,
                                       not_full_market_price = Not.Full.Market.Price,
                                       vat_exclusive = VAT.Exclusive,
                                       property_description = Description.of.Property)

  
#Using the 'replace' function to cut out the non numeric special characters in the price column  
property$price = str_replace(property$price,"â,¬","")

#Removing commas in the price column using the gsub function to ease with type conversion
property$price <- gsub(",","",property$price)  

#Converting the price column to numeric
property$price <- as.numeric(as.character(property$price))


#Using the tabyl function to gain a better understanding of the following variables
janitor::tabyl(property, property_description)#Second Hand Dwelling apartments sold the most
janitor::tabyl(property,County)#Highest number of Properties are sold in Dublin
janitor::tabyl(property, vat_exclusive)#Majority are VAT Inclusive
janitor::tabyl(property, not_full_market_price)#Majority of the Property sold are full market price

#Using the split function to check if if makes sense to divide the dataset county wise
A<-split(property, property$County)
A$Dublin

#Creating a subset 'x' to check certain variables
x<-property[c(4, 5, 6, 7, 8)]

#Created subset 'y' to check the property prices above 200,000
y <- subset(x, price >200000)

#Created subset 'z' to check the property prices below or equal to 200,000
z <- subset(x, price <=200000)

#Mean price of Housing in Ireland
summary(x) # Mean price of apartments is 298998 per the given dataset


#Making subsets to view the most expensive counties in Ireland based on the data for Jan 2020
dublin<- subset(x, County == "Dublin") #Mean price is 467033, has 1146 observations
summary(dublin)

wicklow<- subset(x, County == "Wicklow") #Mean price is 492518
summary(wicklow)

kildare<- subset(x, County == "Kildare") #Mean price is 282019
summary(kildare)

galway<- subset(x, County == "Galway") #Mean price is 267329

meath<- subset(x, County == "Meath") #Mean price is 261995

#Made subsets to check the remaining counties as well
louth<- subset(x, County == "Louth") # Mean   : 241581
cork<- subset(x, County == "Cork") #Mean price is 240565 
kilkenny<- subset(x, County == "Kilkenny") #Mean   : 237702
limerick<- subset(x, County == "Limerick") #Mean price is 208770 
clare<- subset(x, County == "Clare") # Mean   : 206279
carlow<- subset(x, County == "Carlow") # Mean   :197333
westmeath<- subset(x, County == "Westmeath") # Mean   : 194506
wexford<- subset(x, County == "Wexford") #Mean price is 189815
kerry<- subset(x, County == "Kerry") # Mean   :189693
waterford<- subset(x, County == "Waterford") # Mean   :188694
monaghan<- subset(x, County == "Monaghan") #  Mean   :175328 
laois<- subset(x, County == "Laois") # Mean   :169257 
offaly<- subset(x, County == "Offaly") # Mean   :151280 
mayo<- subset(x, County == "Mayo") # Mean   :147076
tipperary<- subset(x, County == "Tipperary") # Mean   :138465
cavan<- subset(x, County == "Cavan") # Mean   :133439


#Making subsets to view the  5 least expensive counties in Ireland based on the data for Jan 2020

leitrim<- subset(x, County == "Leitrim") #Mean   :101709 
summary(leitrim)

roscommon<- subset(x, County == "Roscommon") #Mean   :108214
summary(roscommon) 

longford<- subset(x, County == "Longford") #Mean   :131550
summary(longford)

cavan<- subset(x, County == "Cavan") # Mean   :133439

sligo<- subset(x, County == "Sligo") #Mean   :134321 
summary(sligo)

donegal<- subset(x, County == "Donegal") #Mean   :135648 
summary(donegal)

#Combining the 5 most expensive counties into dataframe 'df1'
df1 <- rbind(dublin, wicklow, kildare, galway, meath)
summary(df1) # Mean   :  409708

#Combining the 5 least expensive counties into dataframe 'df2'
df2 <- rbind(longford, roscommon, leitrim, cavan, sligo)
summary(df2) # Mean   :123060

#Created samples for hypothesis testing.
sample1<-dplyr::sample_n(df1, 50, replace=T)
sample2<-dplyr::sample_n(df2, 50, replace=T)

#combining the counties into the 4 Provinces of Ireland
connacht<-rbind(galway, leitrim, mayo, roscommon, sligo) #Mean   : 193598
leinster<-rbind(carlow, dublin, kildare, kilkenny, laois, longford,louth, meath,offaly,westmeath, wexford, wicklow) #Mean   :  372332
munster<-rbind(clare,cork, kerry, limerick, tipperary, waterford) #Mean   : 210772
ulster<-rbind(cavan, donegal, monaghan) #Mean   :141712


## t-tests

# One sample t tests

#H0: The average property price in Dublin is  290,000 Euros
#H1: The average property price in Dublin is greater than 290,000 Euros

t.test(dublin$price, mu=290000, alternative = "greater")

One Sample t-test

data:  dublin$price
t = 9.4193, df = 1145, p-value < 2.2e-16
alternative hypothesis: true mean is greater than 290000
95 percent confidence interval:
  436093    Inf
sample estimates:
  mean of x 
467032.6 

#H0: The average property price in Leitrim is 290,000 Euros
#H1: The average property price in Leitrim is lesser than 290,000 Euros

t.test(leitrim$price, mu=290000, alternative = "less")

One Sample t-test

data:  leitrim$price
t = -14.017, df = 26, p-value = 6.226e-14
alternative hypothesis: true mean is less than 290000
95 percent confidence interval:
  -Inf 124620.4
sample estimates:
  mean of x 
101708.6 

#2 sample t tests
#H0:The difference between the average property prices in the most expensive and Least expensive counties is 0
#H1:The difference between the average property prices in the most expensive and Least expensive counties is not equal to 0

t.test(df1$price, df2$price, mu=0, var.eq = F, paired=F)

Welch Two Sample t-test

data:  df1$price and df2$price
t = 17.9, df = 1966.2, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  255242.2 318055.6
sample estimates:
  mean of x mean of y 
409708.5  123059.6 

#Conducted the 2 sample t-test with random sampling as well.
sample1<-dplyr::sample_n(df1, 50, replace=T)
sample2<-dplyr::sample_n(df2, 50, replace=T)
t.test(sample1$price, sample2$price, mu=0, var.eq = F, paired=F)

Welch Two Sample t-test

data:  sample1$price and sample2$price
t = 7.4108, df = 52.602, p-value = 1.027e-09
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  226211.7 394139.7
sample estimates:
  mean of x mean of y 
420189.0  110013.3 


#Created random samples of 50 observations from each province
connacht1<-dplyr::sample_n(connacht, 50, replace=T)
leinster1<-dplyr::sample_n(leinster, 50, replace=T)
munster1<-dplyr::sample_n(munster, 50, replace=T)
ulster1<-dplyr::sample_n(ulster, 50, replace=T)

#H0:The difference between the average property price in Connacht and Leinster is 0
#H1:The difference between the average property price in Connacht and Leinster is not equal to 0
t.test(connacht1$price, leinster1$price, mu=0, var.eq = F, paired = F)

Welch Two Sample t-test

data:  connacht1$price and leinster1$price
t = -3.5451, df = 61.46, p-value = 0.0007571
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -316929.26  -88359.45
sample estimates:
  mean of x mean of y 
208196.4  410840.8 

#H0:The difference between the average property price in Connacht and Munster is 0
#H1:The difference between the average property price in Connacht and Munster is not equal to 0
t.test(connacht1$price, munster1$price, mu=0, var.eq = F, paired = F)

Welch Two Sample t-test

data:  connacht1$price and munster1$price
t = -0.67783, df = 84.902, p-value = 0.4997
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -93577.06  45995.76
sample estimates:
  mean of x mean of y 
208196.4  231987.1 

#H0:The difference between the average property price in Connacht and Ulster is 0
#H1:The difference between the average property price in Connacht and Ulster is not equal to 0
t.test(connacht1$price, ulster1$price, mu=0, var.eq = F, paired = F)

Welch Two Sample t-test

data:  connacht1$price and ulster1$price
t = 3.5101, df = 68.256, p-value = 0.0007983
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  32154.34 116864.82
sample estimates:
  mean of x mean of y 
208196.4  133686.9 

#H0:The difference between the average property price in Leinster and Ulster is 0
#H1:The difference between the average property price in Leinster and Ulster is not equal to 0
t.test(leinster1$price, ulster1$price, mu=0, var.eq = F, paired = F)

Welch Two Sample t-test

data:  leinster1$price and ulster1$price
t = 5.0856, df = 51.592, p-value = 5.184e-06
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  167775.2 386532.7
sample estimates:
  mean of x mean of y 
410840.8  133686.9 

#H0:The difference between the average property price in Munster and Ulster is 0
#H1:The difference between the average property price in Munster and Ulster is not equal to 0
t.test(munster1$price, ulster1$price, mu=0, var.eq = F, paired = F)

Welch Two Sample t-test

data:  munster1$price and ulster1$price
t = 3.2157, df = 57.678, p-value = 0.002134
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  37102.74 159497.72
sample estimates:
  mean of x mean of y 
231987.1  133686.9 


#Boxplot for the Provinces.
par(mfrow=c(2,2))
boxplot(ulster1$price, main= "Average property price in Ulster")
boxplot(leinster1$price, main= "Average property price in Leinster")
boxplot(munster1$price, main= "Average property price in Munster")
boxplot(connacht1$price, main= "Average property price in Connacht")
