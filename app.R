#installing packages & loading libraries
library(tidymodels)
library(tidymodels)
library(dplyr)
library(lubridate)
library(ggstatsplot)
library(tidyverse)
library (GGally)
library(forecast)
library(tseries)
library(forecast)
library(tseries)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shiny)

#set working directory and get csv file with price paid data
pp.monthly.update.new.version.txt <- read.csv("pp-monthly-update-new-version.txt.csv")

#convert character to date format
pp.monthly.update.new.version.txt$date <- ymd(substr(pp.monthly.update.new.version.txt$column3, 1, 10))

#change price data type to numeric
pp.monthly.update.new.version.txt$saleprice <- as.numeric(pp.monthly.update.new.version.txt$column2)
class(pp.monthly.update.new.version.txt$saleprice)

#get first day of month
pp.monthly.update.new.version.txt$date <- floor_date(ymd(pp.monthly.update.new.version.txt$date), 'month')


#exclude outliers
Q <- quantile(pp.monthly.update.new.version.txt$saleprice, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(pp.monthly.update.new.version.txt$saleprice,na.rm = TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
data<- subset(pp.monthly.update.new.version.txt, pp.monthly.update.new.version.txt$saleprice > (Q[1] - 1.5*iqr) & pp.monthly.update.new.version.txt$saleprice < (Q[2]+1.5*iqr))



#grouping by month

groupedavg <- aggregate(saleprice~date+column14+column7+column5+column6,data = data,mean)
groupedbymonth <- aggregate(saleprice~date,data = data,mean)


#price prediction vs the actual price 
set.seed(123)
split_data<-initial_split(groupedavg, prop=0.75)
train_data<-training(split_data)
dim(train_data)
test_data<-testing(split_data)
test_data$column14[which(!(test_data$column14 %in% unique(train_data$column14)))] <- NA
dim(test_data)


lm_spec<-linear_reg()%>%
  set_engine(engine = "lm")


train_fit<- lm_spec %>% 
  fit(saleprice~., data = train_data,na.rm=true)
train_fit

test_results<-train_fit%>%
  predict(new_data=test_data)%>%
  mutate(truth=test_data$saleprice)
test_results



rmse(test_results, truth=truth, estimate = .pred)
rsq(test_results, truth=truth, estimate = .pred)

##############################################################

#Predicting prices using ARIMA model
head(groupedbymonth)
names(groupedbymonth)[2] <- "Price"
Price <- groupedbymonth$Price

#creating time series object
tCaseSchiller <- ts(Price,start = c(1995,1),frequency = 12)
tCaseSchiller

#creating model, testing accuracy & plotting results
arima<- auto.arima(tCaseSchiller)
arima
accuracy(arima)

#creating qq plot & checking for auto-correlation
qqnorm(arima$residuals)
qqline(arima$residuals)
Box.test(arima$residuals,type = "Ljung-Box")


#putting forecasted values in a table & putting date in first column
predvalues <- forecast(arima,24)
Date<-row.names(as.data.frame(predvalues))
df <- cbind(Date,as.data.frame(predvalues))
df




#seasonal decomposition
ltCaseShiller <- log(tCaseSchiller)
Fit2 <- stl(ltCaseShiller,s.window = "period")


#creating seasons plot 

#ggseasonplot(tCaseSchiller,year.labels = TRUE,col = rainbow(28))



######################################################################
#server
ui <- dashboardPage(
  dashboardHeader(title = "House Prices"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1",width="100%")),
      box(plotOutput("plot2",width="100%")),
      column(6,box(plotOutput("plot3"),width="100%",height = 535)),
      column(6,box(tableOutput("table"), width = "100%",title = h1("Predicted Price (ARIMA)", align="center",style = "color: black; font-size: 15px")))
      
      
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot2 <- renderPlot(test_results %>% 
                               ggplot(mapping = aes(x = .pred, y = truth)) +
                               geom_point(size = 1.6, color = "blue") +
                               # Overlay a regression line
                               geom_smooth(method = "lm", se = F, color = 'red') +
                               ggtitle("House Price Predictions") +
                               xlab("Actual price") +
                               ylab("Predicted price") +
                               theme(plot.title = element_text(hjust = 0.5)),
                             width = "auto",
                             height = "auto" )
  
  output$plot1 <- renderPlot(ggplot(groupedbymonth,aes(x=date,y=Price)) + 
                               ggtitle("House Prices By Month")+
                               theme(plot.title = element_text(hjust = 0.5))+
                               geom_point() + geom_smooth(method="lm"),
                             width = "auto",
                             height = "auto")
  
  
  output$table <- renderTable(df %>% filter(grepl('2024', Date)),
                              width = "auto",
                              height = "auto")
  
  
  output$plot3 <- renderPlot(plot(Fit2,main = "Seasonal Decomposition (ARIMA)"),
                             width = "auto")
  #height = "auto")
}


shinyApp(ui, server)

