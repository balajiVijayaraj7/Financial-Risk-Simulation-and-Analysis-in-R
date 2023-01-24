install.packages("quantmod")
install.packages("rvest")
install.packages("tidyverse")
install.packages("plotly")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
set.seed(630829)
library(quantmod) # get stock prices; useful stock analysis functions
library(xts)
library(rvest)# web scraping
library(tidyverse) # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr) # working with strings
library(forcats)
library(lubridate) # working with dates 
library(plotly) # interactive plots
library(corrplot)
library(dplyr)
library(PerformanceAnalytics) # evaluating the performance and  risk  characteristics  of  financial  assets  or  funds

#Loads the company stock using ticker

getSymbols("MSFT",from="2009-10-01",to="2021-10-01",src="yahoo") # Microsoft 
getSymbols("AAPL",from="2009-10-01",to="2021-10-01",src="yahoo")   # Apple
getSymbols("INTC",from="2009-10-01",to="2021-10-01",src="yahoo") #Intel
getSymbols("KO",from="2009-10-01",to="2021-10-01",src="yahoo")  # Coca Cola
getSymbols("WMT",from="2009-10-01",to="2021-10-01",src="yahoo") #Walmart

#Stock returns in log
MSFT_log_returns<-dailyReturn(MSFT,type='log')
INTC_log_returns<-dailyReturn(INTC,type='log')
APPL_log_returns<-dailyReturn(AAPL,type='log')
KO_log_returns<-dailyReturn(KO,type='log')
WMT_log_returns<-dailyReturn(WMT,type='log')

#--------------------------QUESTION 1-Mean of log stock returns -----------------------

MSFT_mean_log<-mean(MSFT_log_returns)
INTC_mean_log<-mean(INTC_log_returns)
APPL_mean_log<-mean(APPL_log_returns)
KO_mean_log<-mean(KO_log_returns)
WMT_mean_log<-mean(WMT_log_returns)

MSFT_mean_log
INTC_mean_log
APPL_mean_log
KO_mean_log
WMT_mean_log

#round it to 4 decimal places
# !!! change the order MSFT vs KO

mean_log<-c(INTC_mean_log,APPL_mean_log,MSFT_mean_log, KO_mean_log, WMT_mean_log)
mean_log<-round(mean_log,4)
sort(mean_log, decreasing=TRUE)
View(mean_log)

#----------------QUESTION 2-standard deviation of log stock returns -----------

MSFT_sd_log<-sd(MSFT_log_returns)
INTC_sd_log<-sd(INTC_log_returns)
APPL_sd_log<-sd(APPL_log_returns)
KO_sd_log<-sd(KO_log_returns)
WMT_sd_Log<-sd(WMT_log_returns)

MSFT_sd_log
INTC_sd_log
APPL_sd_log
KO_sd_log
WMT_sd_Log

#round it to 4 decimal places 
# !!! change the order MSFT vs KO
sd_log<-c(INTC_sd_log,APPL_sd_log,MSFT_sd_log, KO_sd_log, WMT_sd_Log)
sd_log<-round(sd_log,4)
sd_log

#--------------------QUESTION 3- create data frame -----------------------------

graphic1<-data.frame(rbind(c("INTC",INTC_mean_log,INTC_sd_log),c("AAPL",APPL_mean_log,APPL_sd_log),c("MSFT",MSFT_mean_log,MSFT_sd_log),c("KO",KO_mean_log,KO_sd_log),c("WMT",WMT_mean_log,WMT_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("INTC","APPL","MSFT","KO","WMT")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")
View(graphic1) 

#Data frame contains the 5 companies with each company's average log return and standard deviation.
#!!! Don't use ordering, othervwise dots on plot are messed up
plot(Sd_Log_Return~Mean_Log_Return,data=graphic1,type="p",pch=(
  substring(row.names(graphic1),1)))#[order(graphic1$Mean_Log_Return)]

#Alternative plot:
#Used plotly to create a visualization of each stock's risk v reward. 
#Risk: standard deviation of log returns
#Reward: mean of log returns

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

xlab<-list(title="Reward", titlefont=f)
ylab<-list(title="Risk", titlefont=f)

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)


#-------------------QUESTION 4--Use R to observe a stock's performance ----------------

#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

#MSFT plot for 2020
chartSeries(MSFT,bar.type="ohlc",theme='white',subset='2020',multi.col=T,TA='addBBands(n=20,sd=2)')

#Alternative MSFT plot for 2020
MSFT%>%Ad()%>%chartSeries()
MSFT%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')


#APPL plot for 2020
chartSeries(AAPL,bar.type="ohlc",theme='white',subset='2020',multi.col=T,TA='addBBands(n=20,sd=2)')

#Alternative APPL plot for 2020:
AAPL%>%Ad()%>%chartSeries()
AAPL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')


#INTC plot for 2020
chartSeries(INTC,bar.type="ohlc",theme='white',subset='2020',multi.col=T,TA='addBBands(n=20,sd=2)')

#Alternative INTC plot for 2020:
INTC%>%Ad()%>%chartSeries()
INTC%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')


#KO plot for 2020
chartSeries(KO,bar.type="ohlc",theme='white',subset='2020',multi.col=T,TA='addBBands(n=20,sd=2)')

#Alternative KO plot for 2020:
KO%>%Ad()%>%chartSeries()
KO%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')


#WMT plot for 2020
chartSeries(WMT,bar.type="ohlc",theme='white',subset='2020',multi.col=T,TA='addBBands(n=20,sd=2)')

#Alternative WMT plot for 2020:
WMT%>%Ad()%>%chartSeries()
WMT%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')


#---------------------QUESTION 5- check correlation of different companies-------------- 

#check correlation of different companies
data2<-cbind(diff(log(Cl(INTC))),diff(log(Cl(WMT))),diff(log(Cl(MSFT))),diff(log(Cl(AAPL))),diff(log(Cl(KO))))
View(data2)
corrplot(cor(na.omit(data2)))

#Alternative plot to illustrate the correlation:
chart.Correlation(data2)

#----------------------Question 6 Monte Carlo Simulations for MSFT--------------------------------

#Monte Carlo/random walk: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy

# define mu and sd for daily returnbased on 10 years observations

muMSFT<-MSFT_mean_log
sigMSFT<-MSFT_sd_log

#generate random daily exponent increase rate using MSFT's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days
T<-252*4
priceMSFT<-rep(NA,T)

#most recent colsing price for every company
priceMSFT[1]<-as.numeric(MSFT[(dim(MSFT))[1],4])
priceMSFT[1]

#start simulating prices - create 5 vectors of prices day by day for the next 4 years 
#with normaly distributed r.v. using mu and sd from previous colculations

for(i in 2:T){
  priceMSFT[i]<-priceMSFT[i-1]*exp(rnorm(1,muMSFT,sigMSFT))
}

# build plot for vectors with predictions of prices for the next 4 years
plot(priceMSFT~seq(1,T),xlab="Day",ylab="Price")


#Alternative plot with predictions of prices for the next 4 years for MSFT
random_dataMSFT<-cbind(priceMSFT,1:(252*4))
colnames(random_dataMSFT)<-c("Price","Day")
random_dataMSFT<-as.data.frame(random_dataMSFT)
random_dataMSFT
random_dataMSFT%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Microsoft Corporation (MSFT) price simulation for 4 years")+theme_bw()


#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables

#Monte Carlo simulations for MSFT
N<-300 #number of simulations

#create a matrix of 300 MC simulations for MSFT(days in rows - 1008, simulations in columns - 300)
mc_matrixMSFT<-matrix(nrow=T,ncol=N)
mc_matrixMSFT[1,1]<-as.numeric(MSFT[(dim(MSFT))[1],4])

# fill in the empty matrix with predictions of price for every day for next 4 years. First row is the last actual price (2021-09-30)
for(j in 1:ncol(mc_matrixMSFT)){
  mc_matrixMSFT[1,j]<-as.numeric(MSFT[(dim(MSFT))[1],4])
  for(i in 2:nrow(mc_matrixMSFT)){
    mc_matrixMSFT[i,j]<-mc_matrixMSFT[i-1,j]*exp(rnorm(1,muMSFT,sigMSFT))
  }
}

# define the names of rows
name<-str_c("S",seq(1,300))
name<-c("Day",name)

# create a table in "tibble" format including row names, column names and results of 300 simulations
final_matMSFT<-cbind(1:T,mc_matrixMSFT)
final_matMSFT<-as.tibble(final_matMSFT)
final_matMSFT
colnames(final_matMSFT)<-name

#check the dimension
dim(final_matMSFT) 

# build a plot of 300 simulations
final_matMSFT%>%gather("Simulation","Price",2:301)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="MICROSOFT CORPORATION Stock (MSFT): 300 Monte Carlo Simulations for 4 Years")+theme_bw()

# define a vector of prices of 300 simulations at the last day of 4 years prediction
MSFTlastday <- as.numeric(final_matMSFT[1008,2:301])

# calculate the mean of prices of 300 simulations at the last day of 4 years prediction
meanlastdayMSFT <- mean(MSFTlastday)
meanlastdayMSFT

#filter how many of the last day values are less than 282 i.e. lower than initial value

less_than_282 <- MSFTlastday<282
less_than_282
length(less_than_282[less_than_282==TRUE]) #11 OF 300 SIMS were less than 282 (start price)

# calculate the standard deviation of prices of 300 simulations at the last day of 4 years prediction
sdlastdayMSFT <- sd(MSFTlastday)
sdlastdayMSFT

# calculating the difference 
meanlastdayMSFT - priceMSFT[1]

#ratio of growth 
meanlastdayMSFT / priceMSFT[1]


#---------------------------------Question 7-----------------------------------

#Code Question 7 here

# creating a portfolio of  + 25% of WMT + 25% KO stocks + 25% of MSFT +25% of APPL

PORTFOLIO <-  WMT*0.25 + KO*0.25 + MSFT*0.25 + AAPL*0.25 

#Stock returns in log for portfolio 
PORTFOLIO_log_returns<-dailyReturn(PORTFOLIO,type='log')

#Mean of log stock returns for portfolio 
PORTFOLIO_mean_log <- mean(PORTFOLIO_log_returns)
PORTFOLIO_mean_log

#standard deviation of log stock returns for portfolio
PORTFOLIO_sd_log <- sd(PORTFOLIO_log_returns)
PORTFOLIO_sd_log

#Monte Carlo/random walk
# define mu and sd for daily return of PRTF based on 10 years observations 
muPORTFOLIO<-PORTFOLIO_mean_log
sigPORTFOLIO<-PORTFOLIO_sd_log

#generate random daily exponent increase rate using mean and sd log returns
#one year 252 trading days, simulate for 4 years 
# 4*252 trading days
T<-252*4

# create empty vector for 4 years price prediction
pricePORTFOLIO<-rep(NA,T)

#most recent colsing price for PRTF
pricePORTFOLIO[1]<-as.numeric(PORTFOLIO[(dim(PORTFOLIO))[1],4])
pricePORTFOLIO[1]

#start simulating prices - create a vector of prices day by day for the next 4 years 
#with normaly distributed r.v. using mu and sd from previous colculations
for(i in 2:T){
  pricePORTFOLIO[i]<-pricePORTFOLIO[i-1]*exp(rnorm(1,muPORTFOLIO,sigPORTFOLIO))
}

# build a plot for vectors with predictions of prices for the next 4 years
random_dataPORTFOLIO<-cbind(pricePORTFOLIO,1:(252*4))
colnames(random_dataPORTFOLIO)<-c("Price","Day")
random_dataPORTFOLIO<-as.data.frame(random_dataPORTFOLIO)

#Price simulation for 4 years
random_dataPORTFOLIO%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Portfolio price simulation for 4 years")+theme_bw()

#CHecking starting and ending price
head(random_dataPORTFOLIO)
tail(random_dataPORTFOLIO)

#Monte Carlo simulations for Portfolio
N<-300 #number of simulations

#create a matrix of 300 MC simulations for Portfolio(days in rows - 1008, simulations in columns - 300)
mc_matrixPORTFOLIO<-matrix(nrow=T,ncol=N)
mc_matrixPORTFOLIO[1,1]<-as.numeric(PORTFOLIO[(dim(PORTFOLIO))[1],4])

# fill in the empty matrix with predictions of price for every day for next 4 years. First row is the last actual price (2021-09-30)
for(j in 1:ncol(mc_matrixPORTFOLIO)){
  mc_matrixPORTFOLIO[1,j]<-as.numeric(PORTFOLIO[(dim(PORTFOLIO))[1],4])
  for(i in 2:nrow(mc_matrixPORTFOLIO)){
    mc_matrixPORTFOLIO[i,j]<-mc_matrixPORTFOLIO[i-1,j]*exp(rnorm(1,muPORTFOLIO,sigPORTFOLIO))
  }
}

# define the names of rows
name<-str_c("S",seq(1,300))
name<-c("Day",name)

# create a table in "tibble" format including row names, column names and results of 300 simulations
final_matPORTFOLIO<-cbind(1:T,mc_matrixPORTFOLIO)
final_matPORTFOLIO<-as.tibble(final_matPORTFOLIO)
colnames(final_matPORTFOLIO)<-name
final_matPORTFOLIO

#Monte Carlo Simulation
final_matPORTFOLIO%>%gather("Simulation","Price",2:301)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="Portfolio Stock (PORTFOLIO): 300 Monte Carlo Simulations for 4 Years")+theme_bw()

#defining a vector of prices of 300 simulations at the last day of 4 years prediction
PORTFOLIOlastday <- as.numeric(final_matPORTFOLIO[1008,2:301])

#calculate the mean of prices of 300 simulations at the last day of 4 years prediction
meanlastdayPORTFOLIO <- mean(PORTFOLIOlastday)
meanlastdayPORTFOLIO

#calculate the standard deviation of prices of 300 simulations at the last day of 4 years prediction
sdlastdayPORTFOLIO <- sd(PORTFOLIOlastday)
sdlastdayPORTFOLIO

#Difference between initial and average final price
diff <- meanlastdayPORTFOLIO - pricePORTFOLIO[1]

#Percentage increase
Per_inc <- diff/pricePORTFOLIO[1]

#The ratio of growth from the last actual price (2021-10-01) to the predicted price 
meanlastdayPORTFOLIO / pricePORTFOLIO[1]

#filter how many of the last day values are less than 153 i.e. lower than starting value
less_than_153 <- PORTFOLIOlastday<153
less_than_153
length(less_than_153[less_than_153==TRUE]) #7 OF 300 Simulations were less than starting price


#Comparing Portfolio with other stocks
mean_log_with_PORTFOLIO<-c(INTC_mean_log,APPL_mean_log,MSFT_mean_log, KO_mean_log, WMT_mean_log,PORTFOLIO_mean_log)
mean_log_with_PORTFOLIO<-round(mean_log_with_PORTFOLIO,4)

sd_log_with_PORTFOLIO<-c(INTC_sd_log,APPL_sd_log,MSFT_sd_log, KO_sd_log, WMT_sd_Log,PORTFOLIO_sd_log)
sd_log_with_PORTFOLIO<-round(sd_log_with_PORTFOLIO,4)

#creating data frame
graphic_p<-data.frame(rbind(c("INTC",INTC_mean_log,INTC_sd_log),c("AAPL",APPL_mean_log,APPL_sd_log),c("MSFT",MSFT_mean_log,MSFT_sd_log),c("KO",KO_mean_log,KO_sd_log),c("WMT",WMT_mean_log,WMT_sd_Log),c("PORTFOLIO",PORTFOLIO_mean_log,PORTFOLIO_sd_log)),stringsAsFactors = FALSE)

rownames(graphic_p)<-c("INTC","APPL","MSFT","KO","WMT","PORTFOLIO")
colnames(graphic_p)<-c("Stock","Mean_Log_Return", "Sd_Log_Return")
View(graphic_p) 

#Plot in relation to other stocks
plot(Sd_Log_Return~Mean_Log_Return,data=graphic_p,type="p",pch=(substring(row.names(graphic_p),1)))

