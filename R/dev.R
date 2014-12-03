# Starting point for this implementation http://bit.ly/1tK0XrE

library("magrittr")
library("digest")
library("R6")
source("R/edge.R")
source("R/node.R")


node1 <- Node$new()
node2 <- Node$new()


connect(node1, node2, 4)
node1$edges_in
node1$edges_out


consonantal <- FeatureDetector("Consonantal")
vocalic <- FeatureDetector("Vocalic")

