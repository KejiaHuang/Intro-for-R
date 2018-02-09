# question 1
rm(list=ls())   # remove all existing variables

?rnorm  # tells your how to use this function
?hist

xx <- rnorm(10000)  # generate 10000 standard normal random samples
hist(xx)            # draw a histogram for vector xx

??basicStats
basicStats  

basicStats(xx)

# question 2
    #Three ways to create vector
v1 <- seq(-10, 10, by = 0.1) 
v2 <- rep(c('0', '1', '2'), 3)
v1
v2
    #Expilict coercion
v2num <- as.numeric(v2)
v2NA <- as.logical(v2)
v2logical <- as.logical(v2num)
v2num
v2NA
v2logical
    #Matrix and list
m2 <- matrix( v2num, nrow = 3, ncol = 3, byrow = T )
m2
solve(m2)
m2[1, 1] = 1
m2[2, 2] = 2
m2
solve(m2)
myFirstList <- list(char = v2, integer = v2num, NAs = v2NA, bool = v2logical , mat = m2)
myFirstList[4]
myFirstList$bool
#question 3
    # Fibonacci with repeat
a <- 0
b <- 1
MAX = 1000
repeat 
{
    print (a)
    if (a > MAX)
        {
            break
        }
    tmp <- a
    a <- b
    b <- tmp + b
}

