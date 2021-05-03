# function to run calculate GGinis
ggini <- function (.data, outcome, grouping, weights, output=c("short", "long")) {
  # this is all just prep
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  if (!all(weights >= 0, na.rm = TRUE)) 
    stop("At least one weight is negative", call. = FALSE)
  if (all(weights == 0, na.rm = TRUE)) 
    stop("All weights are zero", call. = FALSE)
  # assigning var names and making sure format is right
  
  df <- data.frame(x = as.numeric(.data[[outcome]]), z = as.factor(.data[[grouping]]), w = as.numeric(.data[[weights]]))
  z <- factor(grouping)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  n <- as.numeric(nrow(df))
  n_weighted <- sum(df[, "w"])
  dfSplit <- split(df[, c("x", "w")], df[, "z"])
  n_group <- table(df[, "z"])
  n_group_weighted <- sapply(dfSplit, function(df) sum(df[, 
                                                          "w"]), simplify = TRUE)
  df[, "w"] <- df[, "w"]/sum(df[, "w"])
  xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
  xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,"x"], df[, "w"]), simplify = TRUE)
  share_group <- n_group_weighted/n_weighted
  share_group_income <- share_group * xMean_group/xMean
  
  n_cat <- length(n_group) 
    
  value_matrix <-  matrix(nrow = n_cat, ncol = n_cat)
  for (g in 1:n_cat) {
    for (k in 1:n_cat) {
      value_matrix[g,k] = share_group[g]*share_group[k]*abs(xMean_group[g]-xMean_group[k])
    }
  }
  k_sums <- rowSums(value_matrix, na.rm = T)
  g_sums <- sum(k_sums, na.rm = T)
  
  ggini_value <- g_sums/(2*xMean)
  
  if (output == "long") {
  return(list("group_shares" = share_group,
              "group_means" = xMean_group,
              "overall_mean" = xMean,
              "no_groups" = n_cat,
              "group_gini" = ggini_value)) } else {
                return(gini_value)
              }
}



