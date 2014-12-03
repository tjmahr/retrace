#' Create a weighted connection
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
connect <- function(x, y, weight) {
  x_to_y <- Edge(x, y, weight)
  y_to_x <- Edge(y, x, weight)
  x$add_input(y_to_x)$add_output(x_to_y)
  y$add_input(x_to_y)$add_output(y_to_x)
}
