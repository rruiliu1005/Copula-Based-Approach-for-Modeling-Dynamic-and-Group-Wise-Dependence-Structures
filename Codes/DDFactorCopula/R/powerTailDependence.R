power_func = function(coin, d, p){
  out = 1 - coin/p
  out = out^d
  return(out)
}

#' Tail weighted measure of dependence
#'
#' This function computes the tail weighted measure of dependence as defined in Krupskyi, P. (2021). Tail-weighted measures of dependence. Dependence Modeling, 9(1), 92-113. https://doi.org/10.1515/demo-2021-0102
#'
#' @param df A data frame containing uniform variables with \code{nrow(df)}
#' observations and \code{ncol(df)} variables.
#' @param d the power used in the power function
#' @param p the truncation level satisfying 0 < p < 0.5
#' .
#' @return A ncol(df) x ncol(df) matrix containing the tail weighted measure of dependence between the variables
#' @export

powerTailDependence = function(df, d, p){
  coin_columns = colnames(df)
  #power_func_df = as.data.frame(lapply(df, function(col) lapply(col, power_func, d = d, p = p)))
  df[df > 0.5] = NA
  power_func_df = as.data.frame(apply(df,1:2, power_func, d = d, p = p))
  colnames(power_func_df) = colnames(df)
  power_tail_dependence_df = data.frame(matrix(NA, nrow = length(coin_columns), ncol = length(coin_columns)))
  colnames(power_tail_dependence_df) = coin_columns
  rownames(power_tail_dependence_df) = coin_columns

  for (i in 1:length(coin_columns)){
    #cat(sprintf("Completed %d out of %d iterations\n", i, length(coin_columns)))
    for (j in 1:length(coin_columns)){
      tmp1 = power_func_df[, coin_columns[i]]
      tmp2 = power_func_df[, coin_columns[j]]

      na_positions <- is.na(tmp1) | is.na(tmp2)
      tmp1 <- tmp1[!na_positions]
      tmp2 <- tmp2[!na_positions]
      power_tail_dependence_df[coin_columns[i], coin_columns[j]] = cor(tmp1, tmp2)
    }
  }
  return(power_tail_dependence_df)
}
