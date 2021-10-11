#' knapsack_dynamic
#'
#' Solver for the knapsack problem based on dynamic programming.
#'
#' @param x Dataframe containing weight and value of objects
#' @param W maximum weight allowed in knapsack
#'
#' @return List with the optimal value and the respective elements,
#' that are included in the solution.
#'
#' @examples
#' knapsack_objects <- get_knapsack_objects(2000)
#'
#' knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @export

knapsack_dynamic <- function(x, W) {

  get_index <- function(i, j) {
    if (i == 0) {
      return(c())
    }

    if (res_mat[i+1, j+1] > res_mat[i, j+1]) {
      return(c(i, get_index(i-1, j - x$w[i])))
    }
    else {
      return(get_index(i-1, j))
    }
  }



  res_mat <- matrix(nrow = nrow(x) + 1, ncol = W + 1)
  res_mat[1, ] <- 0
  res_mat[, 1] <- 0

  for (i in 1:nrow(x)) {
    for (j in 1:W+1) {
      if (x$w[i] > j) {
        res_mat[i+1, j] <- res_mat[i, j]
      }
      else {
        res_mat[i+1, j] <- max(res_mat[i, j], res_mat[i, j-x$w[i]] + x$v[i])
      }
    }
  }

  index <- get_index(nrow(x), W)
  value <- res_mat[nrow(x)+1, W+1]
  result_list <- list("value" = round(value), "elements" = sort(index))

  return(result_list)
}





