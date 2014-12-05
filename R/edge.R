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
  x_to_y <- Edge(x, y, weight)
  y_to_x <- Edge(y, x, weight)
  x$attach_input(y_to_x)$attach_output(x_to_y)
  y$attach_input(x_to_y)$attach_output(y_to_x)
}

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
  x[[direction]]$activation %>% f(weight)
}

# Vectorized version: Visit a whole list of Edges.
visit.list <- function(x, direction, f) {
  lapply(x, visit, direction, f) %>% unlist
}

#' @export
visit_sender <- function(x, f = multiply_by) {
  visit(x, direction = "sender", f)
}
