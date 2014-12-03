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

FeatureNode <- R6Class("FeatureNode",
 inherit = Node,
 public = list(
   type = NA,
   value = NA,
   initialize = function(type, value) {
     # Randomized name to help tell them apart
     self$tag = rnorm(1) %>% digest() %>% substr(1, 6)
     self$type = type
     self$value = value
   }
  )
)





node3 <- Node$new()


choose(8,2, )
expand.grid(1:8, 1:8)

FeatureNode$new("Consonantal", 1)
type = "Consonantal"



list(node1, node2) %>% lapply(., function(x) x$last_input = 13) %>% invisible

node1$edges_in
node1$edges_out
node2$last_input <- 1


node2$tag



connect(node2, node3)
connect(node1, node3, 5)


e1 <- Edge(node1, node2, 4)
e2 <- Edge(node1, node2, 5)
e1
e2
list() %>% c(list(e1)) %>% c(list(e2)) %>% str
e
str(e)
%>% str

for (x in 1:8) {
  connect(node1, Node$new(), 3)
}

node1$edges_in

%>% print
unique(node1$edges_in)
unique(node3$edges_out)

node$last_input <- 1
str(node)
ann <- Person$new("Ann", "black")

Queue <- R6Class("Queue",
 public = list(
   initialize = function(...) {
     for (item in list(...)) {
       self$add(item)
     }
   },
   add = function(x) {
     private$queue <- c(private$queue, list(x))
     invisible(self)
   },
   remove = function() {
     if (private$length() == 0) return(NULL)
     # Can use private$queue for explicit access
     head <- private$queue[[1]]
     private$queue <- private$queue[-1]
     head
   }
 ),
 private = list(
   queue = list(),
   length = function() base::length(private$queue)
 )
)

q <- Queue$new(5, 6, "foo")





e <- Edge(0, node1, node2)
node1$edges_out
node1$tag <- "YES"

  def __init__(self, source, target):
    self.weight = random.uniform(0,1)
    self.source = source
    self.target = target

  # attach the edges to its nodes
  source.outgoingEdges.append(self)
  target.incomingEdges.append(self)
