#' Create a weighted connection
#' @export
Edge <- function(sender, weight) {
  structure(
    list(s_tag = sender$tag,
         sender = sender,
         weight = weight),
    class = c("Edge"))
}

#' Simplified print
print.Edge <- function(edge, ...) {
  edge %>% str(give.head = FALSE, ...)
}


#' Visit a Node over an Edge
#' @param x an Edge or a list of Edges.
#' @param f a binary function used to combine the visited node's activation and
#'   the edge's weight. Default is multiplication, so f(activation, weight) =
#'   activation * weight.
#' @return a vector of f(activation, weight) values
#' @export
visit <- function(x, f = multiply_by) UseMethod("visit")

visit.Edge <- function(x, f = multiply_by) {
  weight <- x$weight
  x[["sender"]]$send_activation() %>% f(weight)
}

# Vectorized version: Visit a whole list of Edges.
visit.list <- function(x, f = multiply_by) {
  lapply(x, visit, f) %>% unlist(use.names = FALSE)
}





#' Reciprocally connect two nodes
#' @export
connect <- function(x, y, weight) {
  connect_onto(x, y, weight)
  connect_onto(y, x, weight)
}


#' Create one-way connection between pair of nodes
#'
#' Two nodes can be connected (by a non-zero weight) if they overlap in time
#'
#' @param x the sending Node
#' @param y the receiving Node
#' @param weight weight of the connection.
#' @return nothing; the receiving node is updated.
#' @export
connect_onto <- function(x, y, weight) {
  if (overlap(x, y) & weight != 0) {
    x_onto_y <- Edge(x, weight)
    y$attach_input(x_onto_y)
  }
  invisible(NULL)
}

#' Do two nodes overlap in time?
#' @export
overlap <- function(x, y) {
  intersect(x$timeslices, y$timeslices) %>% is_not_empty
}

is_empty <- function(x) length(x) == 0
is_not_empty <- Negate(is_empty)

# Inhibition parameters are scaled by how much overlap there is between the
# nodes
determine_competition <- function(x, y) {
  length(intersect(x$timeslices, y$timeslices)) / 3
}



