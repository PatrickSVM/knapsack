#'
#' @import dplyr
#'
#' @export
#'

greedy_knapsack <- function(x, W) {

  ordered_df <- x %>% mutate(ratio = v/w)
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


