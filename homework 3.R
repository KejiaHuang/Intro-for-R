#Question 1: Coin Simulation and For Loop
sum <- 0
toss <- function()              #fucntion for toss a coin
{
    y=sample(x = c(1, 0), size = 1, replace = T, prob = c(1/2, 1/2))
    return (y)                  #get the result if head 1, if tail 0
}
for (n in 1: 90000)
{  
    time <- 0
    while (1)
    {
        x<-toss()
        sum <- sum + 1
        if (x == 1)             #if it is a head
            time <- time + 1
        else                    #if it is a tail, recount the heads time
            time <- 0
        if (time == 3)          #if get three consecutive heads
            break
    }
}    
E=sum/90000                     #  the result is E = 14.03869
E
#mathematical proof :
# the sample space of the first three tosses :{HHH, HHT, HTH, HTT, THH, THT, TTH, TTT}
#       E = 1/2(E + 1) + 1/4(E + 2) +1/8(E + 3) +(1/8)*3
# begin with   T           HT         HHT        HHH
# SO E = 14

#Question 2: Removing NA values
x <- read.csv(file = "GOOGwNA.csv", header = T)     #read cvs
head(x)
xna <- is.na(x)                                     #find Na in x
Nas <- na.omit(x)                                   #remove the row with Na
head(Nas)
a <- write.csv(Nas, file = "No NA.csv",row.names = FALSE, col.names = TRUE ,append = False)                  #creat a Nas.cvs without Na
new <-read.csv(file = "No NA.csv", header = T)
head(new)
Eopen <- mean(new$Open)                             #calculate mean of each column
Ehigh <- mean(new$High)
Elow <- mean(new$Low)
Eclose <- mean(new$Close)
Evolume <- mean(new$Volume)
Eadj <- mean(new$Adj.Close)
Eopen                                               #show mean 
Ehigh 
Elow 
Eclose 
Evolume 
Eadj

#Question 3 : The Monty Hall Simulation

doors <- c(1, 2, 3)                                     
Nswitch <- 0
Yswitch <- 0
opendoor <- 0
for (i in 1 : 1000000)
{
    price <- sample(doors, size = 1, prob = c(1/3, 1/3, 1/3))  
    pick <- sample(doors, size = 1, prob = c(1/3, 1/3, 1/3))
    if (pick == price)
        opendoor <- sample(doors[which(doors != price & doors != pick)], size = 1, prob = c(1/2, 1/2))
    if (pick != price)
    opendoor <- doors[which(doors != price & doors != pick)]
    change <- doors[which(doors != opendoor & doors != pick)]
    if (change == price) Yswitch <- Yswitch + 1
    if (pick == price) Nswitch <- Nswitch + 1
}

cat("win after switch expectation",Yswitch/1000000 )
cat("win without switch expectation", Nswitch/1000000)






