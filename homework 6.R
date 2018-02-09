library(quantmod)
library(bizdays)
# Use package quantmod download the Option Chain of SPY,
# the maturity date is Dec-042015. 
spyop <- getOptionChain("SPY", Exp = "2015/2016")
spyop$`12月.04.2015` # don't know why there is Chinese character in opion chain 
callprice <- spyop$`12月.04.2015`$calls$Last
putprice <- spyop$`12月.04.2015`$puts$Last
callstrike <- spyop$`12月.04.2015`$calls$Strike
putstrike <- spyop$`12月.04.2015`$puts$Strike

# Download the market quote data as spot 
spyq <- getQuote( "SPY", src = "yahoo")
S <- spyq$Last[1]

# set r as 0.002
r <- 0.02

# Maturity
m <- bizdays( "2015-11-13" , "2015-12-4" )/ 252

# Black-scholes
call.BS<-function( S, K, r, Maturity,sigma )
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity));
    d2<-d1-sigma*sqrt(Maturity);
    S*pnorm(d1)-K*exp(-r*Maturity)*pnorm(d2)
}

put.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity));
    d2<-d1-sigma*sqrt(Maturity);
    K*exp(-r*Maturity)*pnorm(-d2)-S*pnorm(-d1)
}

# Newton for call
fnewtoncall <- function(calls, callp){
    # f(x) 
    fx <- function(x)
    {
     y = call.BS( S, calls, r, m,x ) - callp
      return (y)
    }

    # f'(x) 
    dfx <- function(x)
    {
        d1<-(log(S/calls)+(r+(x^2)/2)*m)/(x*sqrt(m))
        y <- sqrt(m)*S*dnorm(d1)
        return(y)
    }

    x0 =  1     # initial guess
    epsilon = 0.0001  # convergent condition
    step = 1        # step count

    while(1)
    {
       tmp = x0
       deltaX = fx(x0) / dfx(x0)
       x0 = x0 - deltaX
       step = step + 1
        if (abs(x0-tmp) < epsilon)
        {
           #print("Converged!")
           #print(paste("x0 = ", x0, ", step = ", step, sep=""))
            break
        }
    }
    return(x0)
}

# Newton for put
fnewtonput <- function(calls, callp){
    # f(x) 
    fx <- function(x)
    {
        y = put.BS( S, calls, r, m,x ) - callp
        return (y)
    }
    
    # f'(x) 
    dfx <- function(x)
    {
        d1<-(log(S/calls)+(r+(x^2)/2)*m)/(x*sqrt(m))
        y <- sqrt(m)*S*dnorm(d1)
        return(y)
    }
    
    x0 =  5    # initial guess
    epsilon = 0.0001  # convergent condition
    step = 1        # step count
    
    while(1)
    {
        tmp = x0
        deltaX = fx(x0) / dfx(x0)
        x0 = x0 - deltaX
        step = step + 1
        if (abs(x0-tmp) < epsilon)
        {
            #print("Converged!")
            #print(paste("x0 = ", x0, ", step = ", step, sep=""))
            break
        }
    }
    return(x0)
}

# get volatility for put
length(putprice)#57
putstrike <- putstrike[-38]
putprice <- putprice[-38]
putvol <- fnewtonput (putstrike[4], putprice[4])
for ( i in  5: 33){
   a <- fnewtonput( putstrike[i], putprice[i])
   putvol <- c(putvol,a)
}
# get plot for call
plot(putvol, main = (" volatility smile for put"), ylab = (" volatility "), xlab = (" "))

# get volatility for call
callvol <- fnewtoncall (callstrike[13], callprice[13])
for ( i in  14: 65){
    a <- fnewtoncall( callstrike[i], callprice[i])
    callvol <- c(callvol,a)
}

# delete abnormal number
callvol <- callvol[-22]
callvol <- callvol[-30]
callvol <- callvol[-42]
callvol <- callvol[-40]
callvol <- callvol[-48]

# get plot for call
plot(callvol, main = (" volatility smile for call"), ylab = (" volatility "), xlab = (" "))


