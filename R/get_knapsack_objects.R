#' @export

get_knapsack_objects <- function(n) {
  suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))

  ##old sampler used for backward compatibility

  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
}
