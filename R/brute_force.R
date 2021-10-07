#' @export
#'
#'

brute_force_knapsack <- function(x, W) {

  stopifnot("x is not a dataframe" = is.data.frame(x))
  stopifnot("Dataframe has more/less than two variables." = (length(x) == 2))
  stopifnot("Wrong variables in dataframae" = setequal(colnames(x), c("w", "v")))
  stopifnot("W is not a number." = is.numeric(W))

  item_number <- nrow(x)
  highest_value <- 0
  combination <- NULL

  for (i in 1:2^item_number) {
    combi_vec <- (intToBits(i) == 1) [1:item_number]
    weight <- t(combi_vec) %*% x$w

    if (weight < W) {
      value <- t(combi_vec) %*% x$v
      if (value > highest_value) {
        highest_value <- value
        combination <- which(combi_vec)
      }
    }
  }

  return(list("value" = round(highest_value), "elements" = combination))
}
