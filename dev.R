# Starting point for this implementation http://bit.ly/1tK0XrE

library("magrittr")
library("digest")
library("R6")
source("R/edge.R")
source("R/node.R")

# Minimizing the Number of Parameters (p. 21)
#
# At the expense of considerable realism, we have tried to keep TRACE II simple
# by using homogeneous parameters wherever possible. Thus, as already noted, the
# feature specifications of all phonemes were spread out over the same number of
# time slices, effectively giving all phonemes the same duration. The strength
# of the total excitation coming into a particular phoneme unit from the feature
# units was normalized to the same value for all phonemes, thus making each
# phoneme equally excitable by its own canonical pattern. Other simplifying
# assumptions should be noted as well. For example, there were no differences in
# connections or resting levels for words of dfierent frequency. It would have
# been a simple matter to incorporate frequency as McClelland and Rumelhart
# (1981) did, and a complete model would, of course, include some account for
# the ubiquitous effects of word frequency. We left it out here to facilitate an
# examination of the many other factors that appear to influence the process of
# word recognition in speech perception.


# Details of Processing Dynamics (pg. 20)
#
# The interactive activation process in the Trace model follows the dynamic
# assumptions laid out in McClelland and Rumelhart (1981). Each unit has a
# resting activation value arbitrarily set at 0, a maximum activation value
# arbitrarily set at 1.0, and a minimum activation set at -.3. On every time
# cycle of processing, all the weighted excitatory and inhibitory signals
# impinging upon a unit are added together. The signal from one unit to another
# is just the extent to which its activation exceeds 0; if its activation is
# less than 0, the signal is 0.* Global level-specific excitatory, inhibitory,
# and decay parameters scale the relative magnitudes of different types of
# influences on the activation of each unit. [...]
#
# After the net input to each unit has been determined based on the prior
# activations of the units, the activations of the units are all updated for the
# next processing cycle. The new value of the activation of the unit is a
# function of its net input from other units and its previous activation value.
# The exact function used (see McClelland & Rumelhart, 1981) keeps unit
# activations bounded between their maximum and minimum values. Given a constant
# input, the activation of a unit will stabilize at a point between its maximum
# and minimum that depends on the strength and sign (excitatory or inhibitory)
# of the input.
#
# With a net input of 0, the activation of the unit will gradually return to its
# resting level. Each processing time cycle corresponds to a single time slice
# at the feature level. This is actually a parameter of the model-there is no
# intrinsic reason why there should be a single cycle of the interactive
# activation process synchronized with the arrival of each successive slice of
# the input. A higher rate of cycling would speed the percolation of effects of
# new input through the network relative to the rate of presentation.
#
# *At the word level, the inhibitory signal from one word to another is just the
# square of the extent to which the sender’s activation exceeds zero. This tends
# to smooth the effects of many units suddenly becoming slightly activated, and
# of course it also increases the dominance of one active word over many weakly
# activated ones.





bias$cache <- 1
bias$uptick()
bias

bias$cache <- 1
bias$uptick()


node1 <- Node$new()
node1$activation <- .1
node1$send_activation()
node1$receive()
node1$uptick()
node1$compute_activation()


trace_params$decay_feat <- .2
FeatureNode$new("Consonantal", 8)

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


