# Starting point for this implementation http://bit.ly/1tK0XrE

library("magrittr")
library("digest")
library("R6")
source("R/edge.R")
source("R/node.R")





def evaluate(self, inputVector):
  self.lastInput = []
weightedSum = 0

for e in self.incomingEdges:
  theInput = e.source.evaluate(inputVector)
self.lastInput.append(theInput)
weightedSum += e.weight * theInput

self.lastOutput = activationFunction(weightedSum)
return self.lastOutput






bias$cache <- 1
bias$uptick()
bias

bias$cache <- 1
bias$uptick()


node1 <- Node$new()
node2 <- Node$new()
node3 <- Node$new()
node4 <- Node$new()

bias <- Node$new()
bias$activation <- 1


connect(bias, node1, 1)
connect(bias, node2, 1)
connect(bias, node3, 1)
connect(bias, node4, 1)

node1$receive()
node2$receive()
node3$receive()
node4$receive()


node1$uptick()
node2$uptick()
node3$uptick()
node4$uptick()


connect(node1, node2, 1)
connect(node1, node3, 2)
connect(node1, node4, 3)

node1$edges_in
node1$receive()
node1$uptick()


edge <- node1$edges_in[[1]]
edges <- node1$edges_in

node1$receive() %>% sum
visit_sender(edge)
visit_sender(edges)
visit_sender(edge, f = add)
visit_sender(edges, f = add)

e1 %>% lapply(. %>% extract("weight"))
e2 %>% lapply(. %>% extract2("weight"))
node1$edges_in %>%


node1$edges_out


consonantal <- FeatureDetector("Consonantal")
vocalic <- FeatureDetector("Vocalic")

class Network:
  def __init__(self):
  self.inputNodes = []
self.outputNode = None

class InputNode(Node):
  def __init__(self, index):
  Node.__init__(self)
self.index = index; # the index of the input vector corresponding to this node

class Network:
  ...

def evaluate(self, inputVector):
  return self.outputNode.evaluate(inputVector)

class Node:
  ...



class InputNode(Node):
  ...

def evaluate(self, inputVector):
  self.lastOutput = inputVector[self.index]
return self.lastOutput


class BiasNode(InputNode):
  def __init__(self):
  Node.__init__(self)

def evaluate(self, inputVector):
  return 1.0


