#' brute_force_knapsack
#'
#' Solver for the knapsack problem based on the brute force algorithm.
#'
#' @param x Dataframe containing weight and value of objects
#' @param W maximum weight allowed in knapsack
#' @param parallel boolean value whether to use parallelization
#'
#' @return List with the optimal value and the respective elements,
#' that are included in the solution.
#'
#' @examples
#' knapsack_objects <- get_knapsack_objects(2000)
#'
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @import parallel
#'
#' @export

brute_force_knapsack <- function(x, W, parallel = FALSE) {

  stopifnot("x is not a dataframe" = is.data.frame(x))
  stopifnot("Dataframe has more/less than two variables." = (length(x) == 2))
  stopifnot("Wrong variables in dataframae" = setequal(colnames(x), c("w", "v")))
  stopifnot("W is not a number." = is.numeric(W))
  stopifnot("W negative." = W >= 0)

  # define function to use while parallelization
  multiply_vecs <- function(bit_vec, weights, values, max_weight) {
    result <- (bit_vec %*% weights)[1,1]
    if (result <= max_weight) {
      result_val <- (bit_vec %*% values)[1,1]
    }
    else {
      result_val <- 0
    }
    return(result_val)
  }


  item_number <- nrow(x)
  highest_value <- 0
  combination <- NULL

  weights <- x$w
  values <- x$v

  # check whether computations should be parallel
  if (parallel) {

    combinatory_vec <- mclapply(1:2^item_number, FUN = function(p)(intToBits(p) == 1)[1:item_number])

    result_list <- mclapply(combinatory_vec,
                          FUN = multiply_vecs,
                          weights = weights,
                          values = values,
                          max_weight = W)

    index <- which.max(unlist(result_list))
    highest_value <- result_list[[index]]
    combination <- which(combinatory_vec[[index]])

  }

  # no parallelization
  else {

    for (i in 1:2^item_number) {
      combi_vec <- (intToBits(i) == 1) [1:item_number]
      weight <- t(combi_vec) %*% x$w

      if (weight < W) {
        value <- t(combi_vec) %*% x$v
        value <- value[1,1]
        if (value > highest_value) {
          highest_value <- value
          combination <- which(combi_vec)
        }
      }
    }
  }

  return(list("value" = round(highest_value), "elements" = combination))
}



