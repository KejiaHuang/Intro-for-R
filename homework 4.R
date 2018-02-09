# Question 1 : Ruturns and Autocorrelations
# download data
library(quantmod)
library(lubridate)
APLL <- getSymbols("APLL", auto.assign = F)
APLL <- data.frame(APLL)
APLL_price <- APLL$APLL.Adjusted
head(APLL_price)
# simple return
apl_pt <- APLL_price[-1]     # delete the first element
apl_pt_1 <- APLL_price[-length(APLL_price)]
apl_simple_rtn <- apl_pt/apl_pt_1 - 1
# log return
apll_log_rtn <- diff(log(APLL_price))
# verify the relationship between single 
# period and multiple periods simple returns
# simple return of multiperiods
apll_simple_rtn_kperiods <- APLL_price[length(APLL_price)]/APLL_price[1] - 1
apll_simple_rtn_kperiods  # -0.9171875
# the relationship between  multiperiods and single period for simple return
# the product of (1+ simple return ) and minus 1
apll_simple_product = 1
i <- 1
for ( i in 1 :(length(apl_simple_rtn)-1)){
    apll_simple_product <- apll_simple_product * (1+apl_simple_rtn[i])
}
apll_simple_product
apll_simple_product-1# -0.9171875
# log return of multiperiods
apll_log_rtn_kperiods <-log(APLL_price[length(APLL_price)])-log(APLL_price[1])
apll_log_rtn_kperiods #-2.491176
# the relationship between  multiperiods and single period for log return
# sum of all single period log return 
apll_log_sum = 0
i <- 1
for ( i in 1 :(length(apll_log_rtn)-1)){
    apll_log_sum <- apll_log_sum + apll_log_rtn[i]
}
apll_log_sum #-2.491176  
# calculate the auto correlation function 
acf(apll_log_rtn)
acf(apll_log_rtn^2)
#Question 2: Geometrical Brownian Motion
rm(list = ls())
## parameters
r <- 0.05
sigma <- 0.2
Maturity <- 1
steps <- 252
S0 <- 100
result <- vector() # show the difference bewteen two methods
j <- 0
for(j in 1:20){ #try 20 different seed
    ## method 1: Euler Method
    dt <- Maturity / steps
    set.seed(j)
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    dwt_vec <- epsilon_t_vec * sqrt(dt)
    St_vec <- c()
    St_vec[1] <- S0
    for(i in 1:steps){
     dwt <- dwt_vec[i+1]
     St_vec[i+1] <- St_vec[i] + r * St_vec[i] * dt + sigma * St_vec[i] * dwt
    }
   # St_vec[steps+1] result of method 1

## method 2: Solution to GBM
    dt <- Maturity / steps
    set.seed(j)
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    cum_wt_vec <- cumsum(epsilon_t_vec) * sqrt(dt)
    ST <- S0 * exp(r*Maturity + cum_wt_vec[steps + 1] * sigma)
    #ST  result of method 2
    result <- c(result, (ST-St_vec[steps+1]))
}
#print out the difference between 2 methods
result  
acf(result)
