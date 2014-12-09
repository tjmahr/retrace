#' Create a weighted connection
#' @export
Edge <- function(sender, receiver, weight) {
  structure(
    list(s_tag = sender$tag,
         r_tag = receiver$tag,
         sender = sender,
         receiver = receiver,
         weight = weight),
    class = c("Edge"))
}

#' Simplified print
print.Edge <- function(edge, ...) {
  edge %>% str(give.head = FALSE, ...)
}


#' Reciprocally connect two nodes
#' @export
connect <- function(x, y, weight) {
  connect_onto(x, y, weight)
  connect_onto(y, x, weight)
}


#' Create one-way connection between pair of nodes
#' @export
connect_onto <- function(x, y, weight) {
  if (overlap(x, y)) {
    x_onto_y <- Edge(x, y, weight)
    x$attach_output(x_onto_y)
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


#' Visit a Node over an Edge
#' @param x an Edge or a list of Edges.
#' @param direction the side of the edge to visit. Default is "sender";
#'   alternative is "receiver".
#' @param f a binary function used to combine the visited node's activation and
#'   the edge's weight. Default is multiplication, so f(activation, weight) =
#'   activation * weight.
#' @return a vector of f(activation, weight) values
#' @export
visit <- function(x, direction, f = multiply_by) UseMethod("visit")

visit.Edge <- function(x, direction, f) {
  weight <- x$weight
  x[[direction]]$send_activation() %>% f(weight)
}

# Vectorized version: Visit a whole list of Edges.
visit.list <- function(x, direction, f) {
  lapply(x, visit, direction, f) %>% unlist
}

#' @export
visit_sender <- function(x, f = multiply_by) {
  visit(x, direction = "sender", f)
}

