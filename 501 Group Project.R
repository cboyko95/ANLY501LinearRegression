#Setting Directory
setwd("C:\\Users\\cboyk\\OneDrive\\Desktop\\Analytics")


#Libraries
library(readxl)



#Bringing in Master file and creating working set
master <- read_excel("Economic_Freedom_Index.xlsx")


#Cleaning Set
#To import correctly, missing values were set to 1,000,000
#Now these need to be replaced
summary(master)
master[, 30:32] [master[, 30:32] > 999999] = NA
summary(master)

###Outliers
zscored = scale(master$`2019 Score`)
outlier <- subset(master, abs(zscored) >= 3)
#2 outliers
master2 <- subset(master, abs(zscored) <= 3)
#no outliers

#Linear Regression - P-value Method
lm <- lm(`2019 Score` ~ Region + `Tariff Rate` + `Income Tax Rate` +
             `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
             `Govt Expenditure Percent of GDP` + Population + GDP + 
             `GDP Growth Rate` + `5 Year GDP Growth Rate`+
             `GDP per Capita` + Unemployment + Inflation + `FDI Inflow` +
             `Public Debt`, data = master2)
summary(lm)
#Rsquared:0.6782
#Get rid of: Population

lm2 <- lm(`2019 Score` ~ Region + `Tariff Rate` + `Income Tax Rate` +
           `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
           `Govt Expenditure Percent of GDP` + GDP + 
           `GDP Growth Rate` + `5 Year GDP Growth Rate`+
           `GDP per Capita` + Unemployment + Inflation + `FDI Inflow` +
           `Public Debt`, data = master2)
summary(lm2)
#Rsquared of 0.6782
#Get rid of Region

lm3 <- lm(`2019 Score` ~ `Tariff Rate` + `Income Tax Rate` +
            `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `GDP Growth Rate` + `5 Year GDP Growth Rate`+
            `GDP per Capita` + Unemployment + Inflation + `FDI Inflow` +
            `Public Debt`, data = master2)
summary(lm3)
#Rsquared of 0.6693
#Get rid of GDP Growth Rate

lm4 <- lm(`2019 Score` ~ `Tariff Rate` + `Income Tax Rate` +
            `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `5 Year GDP Growth Rate`+
            `GDP per Capita` + Unemployment + Inflation + `FDI Inflow` +
            `Public Debt`, data = master2)
summary(lm4)
#Rsquared 0.6685
#Get rid of: Unemployment

lm5 <- lm(`2019 Score` ~ `Tariff Rate` + `Income Tax Rate` +
            `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `5 Year GDP Growth Rate`+
            `GDP per Capita` + Inflation + `FDI Inflow` +
            `Public Debt`, data = master2)
summary(lm5)
#Rsquared: 0.6385
#Get rid of Public Debt

lm6 <- lm(`2019 Score` ~ `Tariff Rate` + `Income Tax Rate` +
            `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `5 Year GDP Growth Rate`+
            `GDP per Capita` + Inflation + `FDI Inflow` , data = master2)
summary(lm6)
#Rsquared 0.6359
#Get rid of Income Tax Rate

lm7 <- lm(`2019 Score` ~ `Tariff Rate` +
            `Corporate Tax Rate` +  `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `5 Year GDP Growth Rate`+
            `GDP per Capita` + Inflation + `FDI Inflow` , data = master2)
summary(lm7)
#Rsquared of 0.6327
#Get rid of Corporate Tax Rate

lm8 <- lm(`2019 Score` ~ `Tariff Rate` +
            `Tax Burden Percent of GDP` + 
            `Govt Expenditure Percent of GDP` + GDP + 
            `5 Year GDP Growth Rate`+
            `GDP per Capita` + Inflation + `FDI Inflow` , data = master2)
summary(lm8)
#Rsquared 0.6272
#ALL STATISTICALLY SIGNIFICANT


correlations <- cor(master2[, c(7,20,23,24, 26,28, 29, 31, 32)], use = 'pairwise.complete.obs')
correlations
symnum(correlations)
#None are multicolinear

