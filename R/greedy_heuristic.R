#' greedy_heuristic
#'
#' Solver for the knapsack problem based on the greedy heuristic.
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
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' @import dplyr
#'
#' @export

greedy_knapsack <- function(x, W) {

  stopifnot("x is not a dataframe" = is.data.frame(x))
  stopifnot("Dataframe has more/less than two variables." = (length(x) == 2))
  stopifnot("Wrong variables in dataframae" = setequal(colnames(x), c("w", "v")))
  stopifnot("W is not a number." = is.numeric(W))
  stopifnot("W negative." = W >= 0)

  ordered_df <- x %>% mutate(ratio = .data$v/.data$w)
  ordered_df <- ordered_df[order(ordered_df$ratio, decreasing = TRUE), ]
  current_weight <- 0
  result_val <- 0
  result_index <- c()

  for (item in 1:nrow(x)) {
    if (current_weight + ordered_df[item, ]$w <= W) {
      current_weight <- current_weight + ordered_df[item, ]$w
      result_val <- result_val + ordered_df[item, ]$v
      result_index <- c(result_index, as.numeric(rownames(ordered_df)[[item]]))
    }
  }

  res_list = list("value" = round(result_val), "elements" = result_index)
  return(res_list)
}


