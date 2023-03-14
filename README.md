>*In this project I'm going to use VAR model and Garch multivariate model to analyze financial time series. In this project I decide to create the Japan capital financial account as a function of Japan money and US interest rate as well as GDP in both countries, which of these variable can effect the Japan capital financial account*.


Title:    Importing data from the database
File:      yahooo
Project:  VAR MODEL IN R 


ABSTRACT::: I decided to create a model using Japanese capital flows in this project.In this case, the JAPAN capital financial account is represented as a function of JAPAN money and US interest rate as well as GDP in both countries. After pulling data directly from FRED and creating variables for our dataset, we estimate a model of Japan's capital flows. In this project we will use Granger Causality tests, orthogonalized Impulse Response Functions, and Forecast Error Variance Decompositions to show the impact of income and monetary variables. 


**INSTALL AND LOAD PACKAGES**

IMPORTANT PACKAGES 1."VARS"--MOST IMPORTANT
-casuality,
- fanchart,
- fevd,
- irf,
- arch.test,
- normality.test,
-roots,
- serial.test,
- stability,
- VARselect. 

```
rm(list=ls())
```
Lets now load the required packages that we need for our model.
```
install.packages("quantmod")
install.packages("tikzDevice")
library(tikzDevice)
library(quantmod)
install.packages("imputeTS")
library(imputeTS)
install.packages("xts")
library(xts)
library(urca)
install.packages("vars")
library(vars)
library(mfilter)
library(tseries)
library(forecast)
library(tidyverse)#contain ggplot 
install.packages("stargazer")
library(stargazer)# to create tables 
library(readxl)
```
The step we need to do in order to estimate the VAR model:

1. To check for the integration of the series to make sure our series is stationarity I(0)
2. Check the appropriate (OPTIMAL) lag length, too many lag we loss observations too many parameters to estimate using few lags that will cause some serial problems. 
3.Estimate the model, the number of independent variable constitute the number of equations of the model we use OLS but we go equation by equation the package vars will do it for us.
4. The model is stable, we look at the eigenvalues of the coefficient to see if their moduli are less than one. If the series is stationary it should be less than one. 
5.Granger Causality test 
6. Impulse Function IRFs
7.Forecasting

For this project I decidet to download data directly from FRED 


#JAPAN KFA = diff(RES) - CA; M1; U.S. Fed funds rate; US and JP Real GDP 
#CA as share of GDP, but Reserves in dollars
#Need to get JP GDP in dollars
#Get NGDP in yen and exchange rate
```
setDefaults(getSymbols,src='FRED')
getSymbols("JPNB6BLTT02STSAQ",src='FRED')
```


JPNB6BLTT02STSAQ=JPNB6BLTT02STSAQ["/2022-01-01"]# bases time series Current account Balance: Total: Total Balance as % of GDP for Japan 
CA<-ts(JPNB6BLTT02STSAQ,end=c(2022,4),freq=4)# make teh courrent account by seting this as a time series with the end date 
mean(CA)# it is in percentage of GRdp so it is a surplus of  2.78

# we need to use reserves to model the capital flows which are in Us dollars . We have to do a couple of steps as teh gdp is in yen to turn it in percetage points
```getSymbols("JPNGDPNQDSMEI",src='FRED') # we gonna pull nominal gdp# Current Price Gross Domestic Product in Japan 
JPNGDPNQDSMEI=JPNGDPNQDSMEI["/2022-01-01"]
NGDP<-ts(JPNGDPNQDSMEI,end=c(2022,4),freq=4)```

```getSymbols("CCUSMA02JPM618N",src='FRED')# National Currency to US Dollar Exchange Rate: Average of Daily Rates for Japan
CCUSMA02JPM618N=CCUSMA02JPM618N["/2022-07-01"]
JPNUSD<-ts(CCUSMA02JPM618N,end=c(2022,1),freq=4)
NGDPUSD<-NGDP/JPNUSD ```# divide the exchange rate to turn yen to dollars

#KFA = diff(Reserves) - CA# pull reserves to get my series in dollars 
```getSymbols("JPNB6FARA01CXCUQ",src='FRED')# Reserve assets Net for Japan
JPNB6FARA01CXCUQ=JPNB6FARA01CXCUQ["/2022-04-01"]
RES<-ts(JPNB6FARA01CXCUQ,end=c(2022,4),freq=4)
KFA<-(100*diff(RES)/NGDPUSD-CA)
mean(100*diff(RES)/NGDPUSD)
mean(KFA)``` 
#So now we have the variable of interest which is capital fiancial account
### If the capital and financial accounts are negative, the country has a net financial outflow. 
#It has more claims than it does liabilities, either because of an increase in claims by the economy abroad or a reduction in liabilities from foreign economies.
#The current account should be recording a surplus at this stage. 
#That indicates the economy is a net creditor, providing funds to the world


# To get our explanotery varibles we  have M1
```
getSymbols("MANMM101JPM189S",src='FRED')# M1 for Japan 
MANMM101JPM189S=MANMM101JPM189S["/2022-06-01"]
JPM1<-ts(MANMM101JPM189S,end=c(2022,4),freq=4)```
#US intrest rate 
```
getSymbols("BOGZ1FL072052006Q",src='FRED')  
BOGZ1FL072052006Q=BOGZ1FL072052006Q["/2022-01-01"]
USR<-ts(BOGZ1FL072052006Q,end=c(2022,4),freq=4)```
#Us GDP
```
getSymbols("GDPC1",src='FRED') 
GDPC1=GDPC1["/2022-04-01"]
USY<-ts(GDPC1,end=c(2022,1),freq=4)```
#Japan real GDP
```
getSymbols("NAEXKP01JPQ189S",src='FRED')   
NAEXKP01JPQ189S=NAEXKP01JPQ189S["/2022-01-01"]
JPY<-ts(NAEXKP01JPQ189S,end=c(2022,4),freq=4)```

#Take logs of 3 variables
#Combine; notice different lengths
```
data<-cbind(log(USY),log(USR),log(JPY),log(JPM1),KFA)
view(data)
names<-c("USY","USR","JPY","JPM1","KFA")
colnames(data)<-names
par(mfrow=c(3,2))
ts.plot(USY)
ts.plot(USR)
ts.plot(JPY)
ts.plot(JPM1)
ts.plot(KFA)
par(mfrow=c(1,1))#### to bigggg omg
#plot(data)
is.na(data)
sum(is.na(data))
#apply(is.na(data),2,which)
#data<-na_ma(data,k=3, weighting="exponential")
na_interpolation(data)-> data
#data<-na.omit(data)
sum(is.na(data))
#plot(data)
dim(data)

str(data)
head(data)
summary(data)
```
###Again, it is important to assess whether the variables under study are stationary or not. As we 
#have said, having stationary variables is of an ideal case in our VAR even if we can run it without 
#these.

#Phillips-Perron stationarity test
```pptab<-NULL
for(i in 1:ncol(data)){
  pp<-PP.test(data[,i])
  pptab<-rbind(pptab,pp$p.value)
}
pptab```


### non stationary If the series are not stationary, 
#WE could first test for cointegration and estimate a vector error correction model if series were cointegrated.
#Otherwise,  we may estimate a VAR model on data integrated of first-order i.e., I(1) after taking the first difference to make it stationary.

#redo with differences
```
data<-cbind(diff(log(USY)),diff(USR),diff(log(JPY)),diff(log(JPM1)),KFA)
data<-na.omit(data)
colnames(data)<-names

pptab<-NULL
for(i in 1:ncol(data)){
  pp<-PP.test(data[,i])
  pptab<-rbind(pptab,pp$p.value)
}
pptab  ```### our data is now stationary



######Determine the persistence of the model acf or pacf#####
```
par(mfrow=c(2,1))
acf(USY, main="ACF for USY") #describe the graph
pacf(USY, main="PACF for USY") 
par(mfrow=c(1,1))
```
#describe the graph
```acf(USR, main="ACF for USR") #describe the graph
pacf(USR, main="PACF for USR") #describe the graph
```
#plot IRFs: Make VAR (LOWER triangular: Order with most exogenous on left and most endogenous on right)
#Loop to only make response of CA
#Still default settings
```var<-data
library(vars)
```
####Finding the Optimal lags###
```
Lagselect<-VARselect(var, lag.max=10, type="const")
Lagselect$selection # print the optimal lag we are going to use in this case  all criteria AIC HQ AND SC FPE agree for one lag 
```

##### Estimate the model by using OLS equations.  The number of variables determine the number of equations also.
```
Modeldataset1<-VAR(var, p=1, type="const", season=NULL, exog=NULL)
summary(Modeldataset1)

```
#####Stable model ####
```
stability<-roots(Modeldataset1, modulus=TRUE)
stability# should be less than one to be stable, in this case is stable 
```

#VAR estimation result: the more lag we include the more observation we will use 
#The first equations ols estimation see if is significant or not 
#The Second  equations ols estimation see if is significant or not più stelline più significant 
#To make a table in a more beautiful way we can use stargazer as a function that produces nice tables 

#stargazer(Modeldataset1("varresults"),type= "text")# put together all variables 

##### Diagnosing the VAR model#####

#####Serial Autocorrelation####(Portmanteau Test)
```
Serial1<-serial.test(Modeldataset1, lags.pt=12,type="PT.asymptotic")
Serial1
```

#### it gives  me chi squared no sense of it
#Describing the table of Portmanteau test 
#If the p.value of the test is greater then 0.05 it suggests that there is no serial correlation so this is a good news. 

####Heteroscekasticity### (Multivariate ARCH-LM test) which in time series takes the form of a arch effects essentially those are period of volatility so we are trying to test for volatility here .###
```
Arch1<-arch.test(Modeldataset1, lags.multi=12, multivariate.only=TRUE)
Arch1```
#If the pvalue of the test is greater then 0.05 it suggests that so the model does not sufferes from heteroscedasticity this is a good news. 



#####Normal Distribution#### (Jarque-Bera test) of residuals another assumption we want our residuals to be normal distributed###
```
Norm1<-normality.test(Modeldataset1, multivariate.only=TRUE)
Norm1
```
stargazer(Norm1[["jb.mul"]], type = "latex", title="Table 6:Jarque-Bera Test Results")
## problem with this one 
#Here we have 3 test the 
#First one is the JB-Test. If the pvalue of the test is lower then 0.05(for example p.value<2.2e-16) it suggests that our residuals are not normally distributed 
#Second one is the Skewness. If the pvalue of the test is lower then 0.05(for example p.value<2.2e-16) it suggests that our residuals are not normally distributed 
#Third one is the Kurtosis Test. If the pvalue of the test is lower then 0.05(for example p.value<2.2e-16) it suggests that our residuals are not normally distributed 
#Our model does not pass this diagnosis but is not a problem. This is excatly the case here 



#Testing for Structural beaks in the residuals 
```Stabilty1<-stability(Modeldataset1, type="OLS-CUSUM")

plot(Stabilty1)```
### In this case we can describe the result of the graph.
#Describing the graph we have to see if there are points In the graph that exceeded the two red line in the bottom and in the top so the system is stable. 

######Granger Casuality  TEST##### association of two variable variable1 causes variable 2 or variable 2 causes variable 1 or both direction.

#Granger Causality tests: I make pairwise vars, then a nice table
****
gctab<-NULL
var<-data
library(vars)
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 1:4){
  var2<-VAR(var[,c(i,5)],type = c("const"),lag.max = 4,ic="SC")
  gc<-causality(var2,cause = colnames(var1$datamat[i]))
  gc1<-cbind(as.numeric(gc$Granger$statistic),gc$Granger$p.value)
  gctab<-rbind(gctab,gc1)
}
colnames(gctab)<-c("Statistic","p-val.")
rownames(gctab)<-colnames(var1$datamat[c(1:4)])
gctab<-round(gctab,3)
print(gctab)```
# JPY and USR  are indeed significant  are the variables that has an effect on Japan capital flows



######ALTERNATIVE######
```
Grangervariable1<-causality(Modeldataset1, cause="USY")
Grangervariable1 
#Describing the result
#H0:variable1 do not granger cause variable2 il pvalue is greater than 0.05 we cannot reject the null hypothesis  if is smaller we reject the null hypothesis 
Grangervariable2<-causality(Modeldataset1, cause="USR")# kjo ok
Grangervariable2
Grangervariable3<-causality(Modeldataset1, cause="JPY")# problem ktu se del poooo uff
Grangervariable3
Grangervariable4<-causality(Modeldataset1, cause="JPM1")
Grangervariable4
Grangervariable5<-causality(Modeldataset1, cause="KFA")
Grangervariable5
```
#####Describing the result###
#H0:variable2 do not granger cause variable1 il pvalue is greater than 0.05 we cannot reject the null hypothesis 

#### Impulse response function#### how a variable will behave if one variable shocks(increase positive shock)
```
var<-data
library(vars)
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 1:4){
  irf1<-irf(var1,impulse = colnames(var1$datamat[i]),response = colnames(var1$datamat[5]),n.ahead = 6,ortho = TRUE,ci=0.95,boot=TRUE,runs=100,cumulative = FALSE)
  plot(irf1)# four explonatory variable effecting the fith 
}
``````
#####Variance DEcomposition#### how much these variable are influences by the shocks
#Get FEVDs at 1,4,8,12-quarter horizons
```fevd1<-fevd(var1,n.ahead = 12)
fevd2<-fevd1$KFA
fevdtab<-round(100*fevd2[c(1,4,8,12),],2)
rownames(fevdtab)<-c(1,4,8,12)
print(fevdtab)# Kfa is mostly affected by itself but also by USR AND JPY
plot(fevd1)
```
#####ALTERNATIVE#### SAMEEEEEEE 
```
FEVD1<-fevd(var1, n.ahead=12)
plot(FEVD1)
```
#Describe the graph the percentage of the shock from what variable they come basically 

####VAR Forecasting###
```
par(mfrow=c(2,2))
Forecast<-predict(Modeldataset1, n.ahead=4, ci=0.95)
fanchart(Forecast, names="USY")
#describe the graph
Forecast<-predict(Modeldataset1, n.ahead=4, ci=0.95)
fanchart(Forecast, names="USR")
Forecast<-predict(Modeldataset1, n.ahead=4, ci=0.95)
fanchart(Forecast, names="JPY")
Forecast<-predict(Modeldataset1, n.ahead=4, ci=0.95)
fanchart(Forecast, names="JPM1")
Forecast<-predict(Modeldataset1, n.ahead=4, ci=0.95)
fanchart(Forecast, names="KFA")
```
In conclusion, we can say that JAPAN capital financial account is affected mainly by US interest rate Japan money. 
 
