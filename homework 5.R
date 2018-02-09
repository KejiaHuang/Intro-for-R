#Assignment 5: Realized Volatility
#1input data 
CSV <- read.csv("zye2@stevens.edu-MS_N-N99265651.csv", head = TRUE) 
head(CSV)

# use package lubridate install.packages("lubridate")
library(lubridate)

# make a time window between 11:00 am and 2:30 pm

CSV$Time.L. <- hms(CSV$Time.L.)
CSV$Time.L. <- CSV$Time.L.@hour+CSV$Time.L.@minute/60
idx <-CSV$Time.L.>=11 & CSV$Time.L.<=14.5
CSV_rtn <- diff(log(CSV$Last))[idx]
Date <- CSV$Date.L.[idx]
everyday_rtn <- split(CSV_rtn, Date)

#Calculate the volatility
calvol <- function(rtn){
    return (sum(rtn^2) / length(rtn))
}
realized_var <- sapply (everyday_rtn, calvol)
realized_vol <- sqrt(realized_var)

#ggplot2
library(ggplot2)
date <- ymd(unique(Date))
volatility <- realized_vol
ggplot(data.frame(date,realized_vol), aes(x = date , y = volatility)) + 
    geom_point(color = "firebrick")




