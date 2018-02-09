#question 1       Loops
Dow <- read.csv("DOW.csv", head = TRUE)          #input csv
SP500 <- read.csv("SP500.csv", head = TRUE)

names(Dow)                   #look for the type of the Dow and SP500
names(SP500)
mode(Dow)
class(Dow$Company)

Dow$Ticker <- as.character(Dow$Ticker)         #change factor to character
Dow$Company <- as.character(Dow$Company)
SP500$Ticker.symbol <- as.character(SP500$Ticker.symbol)
SP500$Company <- as.character(SP500$Company)

result1 <- NULL               #loops for traverse Dow  
for (i in 1:30 )                      
    {
        for (j in 1:500)                   #loops for traverse SP500
        {
            if (Dow$Ticker[i] == SP500$Ticker.symbol[j])            #check if the name is same
            {
                result1 <- c( result1, paste(SP500$Ticker.symbol[j], j, sep = "--"))  #if same, save in the result
            }
        }
    }
result1
#question 2 user defined functions
firstfun <- function( n )    # first function 
{
    sum1 <- 0
    result2 <- NULL
    for (rolls in 1 : n)    #loops for n rolls
    { 
        tmp1 <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                  prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum1 <- sum1 + tmp1 
        result2 <- c( result2, sum1 / rolls)
    }
    plot(1:n, result2, type = "l", ylim = c(1, 5), xlab =   "roll times ", ylab = "mean" )    # produce a figure
}
firstfun(100)
firstfun(200)
firstfun(500)
num
secondfun <- function ( num , pro)  #second function 
{
    sum2 <- 0
    for (rolls in 1 : num)
    {
        sum2 <- sum2 + sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, prob = pro)
    }
    result3 <- sum2/rolls
    return (result3)
}
vec1 <- rep (1/6, 6)
vec2 <- c( 0.2, 0.2, 0.2, 0.2, 0.1, 0.1)
secondfun( 500, vec1)

secondfun( 500, vec2)

#question 3 optimal strategy
# try some possible strategy to find out which is the optimal strategy
# because 1 is the min of the 6 result, will try another roll surely
# also 6 is the max of the 6 result, will not try another roll 
# so we will have try 5 strategy and for each one we will try roll 50000 times to simulate the result

# first strategy
# first  roll           1   2   3   4   5   6 
# if try another time   Y   N   N   N   N   N
sum <- 0
for (rolls in 1 : 50000)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                   prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR == 1)
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                         prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean1 = sum / 50000
mean1   #mean1 = 3.90988
# second strategy
# first  roll           1   2   3   4   5   6
# if try another time   Y   Y   N   N   N   N
sum <- 0
for (rolls in 1 : 50000)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR <= 2 )
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                          prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean2 = sum / 50000
mean2   #mean2 = 4.15946
# third strateg
# first  roll           1   2   3   4   5   6
# if try another time   Y   Y   Y   N   N   N
sum <- 0
for (rolls in 1 : 50000)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR <= 3)
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                          prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean3 = sum / 50000
mean3   #mean3 = 4.25868
# fourth strategy
# first  roll           1   2   3   4   5   6
# if try another time   Y   Y   Y   Y   N   N
sum <- 0
for (rolls in 1 : 50000)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR <= 4)
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                          prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean4 = sum / 50000
mean4   #mean4 = 4.18408
# fifth strategy
# first  roll           1   2   3   4   5   6
# if try another time   Y   Y   Y   Y   Y   N
sum <- 0
for (rolls in 1 : 50000)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR <= 5)
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                          prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean5 = sum / 50000
mean5   #mean5 = 3.91828
#because mean3 is the biggest one, so the thrid strategy is the optimal strategy
# third strateg
# first  roll           1   2   3   4   5   6
# if try another time   Y   Y   Y   N   N   N

# the expectation of the optimal strategy:
# E= (4*1/6) + (5*1/6) + (6* 1/6) + ((1*1/6) + (2*1/6) + (3*1/6) + (4*1/6) + (5*1/6) +(6*1/6))*1/2 
E <- (4*1/6) + (5*1/6) + (6* 1/6) + ((1*1/6) + (2*1/6) + (3*1/6) + (4*1/6) + (5*1/6) +(6*1/6))*1/2
E #E = 4.25
#the function of the strategy

optimalfun <- function ( nn )
{
sum <- 0
for (rolls in 1 : nn)    
{ 
    firstR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
    firstR
    if (firstR <= 3)
    {
        secondR <- sample(x = c(1, 2, 3, 4, 5, 6), size = 1, replace = T, 
                          prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ))
        sum <- sum + secondR
    }
    else 
    {
        sum <- sum +firstR
    }
}
mean = sum / nn
return (mean) 
}
optimalfun (10)
optimalfun (100)
optimalfun (1000)
optimalfun (10000)
optimalfun (100000)
# the expectation of the optimal strategy:
# E= (4*1/6) + (5*1/6) + (6* 1/6) + ((1*1/6) + (2*1/6) + (3*1/6) + (4*1/6) + (5*1/6) +(6*1/6))*1/2 
E <- (4*1/6) + (5*1/6) + (6* 1/6) + ((1*1/6) + (2*1/6) + (3*1/6) + (4*1/6) + (5*1/6) +(6*1/6))*1/2
E #E = 4.25  


