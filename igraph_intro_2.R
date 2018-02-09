# igraph presentation
library("igraph")

# creat network from data frame 
# A simple example with a couple of actors
# The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g)
###################################
# creat network from adjacency matrix
# simple
adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
g1 <- graph_from_adjacency_matrix( adjm )
plot(g1)

# weight
adjm <- matrix(sample(0:5, 100, replace=TRUE,
                      prob=c(0.9,0.02,0.02,0.02,0.02,0.02)), nc=10)
g2 <- graph_from_adjacency_matrix(adjm, weighted=TRUE)
E(g2)$weight
plot(g2, edge.label=E(g2)$weight)

# distance matrix ( undirected )
points <- rbind(sample(0:100,10,replace=TRUE), sample(0:100,10,replace=TRUE) )
distmat <- as.matrix(dist(t(points)))
g3 <- graph_from_adjacency_matrix(distmat, weighted=TRUE,  mode="undirected")
plot(g3,edge.label=round(E(g3)$weight,2))
MST <- minimum.spanning.tree(g3)
plot(MST,edge.label=round(E(MST)$weight,2))
