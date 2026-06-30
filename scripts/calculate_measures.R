# ------------------------------------------------- #
# summary statistics function
# ------------------------------------------------- #
calculate_measures <- function(true_mats, est_mats, d, n, marg, case, mod, bad){

  summary_stats <- lapply(seq_along(est_mats), function(j){

    # get auto and cross-regressive parameters
    est_auto   <- diag(est_mats[[j]])
    true_auto  <- diag(true_mats)
    est_cross  <- est_mats[[j]][row(est_mats[[j]]) != col(est_mats[[j]])]
    true_cross <- true_mats[row(true_mats) != col(true_mats)]

    # relative bias
    biasrel <- SimDesign::bias(
      estimate = c(est_mats[[j]]),
      parameter = c(true_mats),
      type = "relative",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    biasrel_auto <- SimDesign::bias(
      estimate = est_auto,
      parameter = true_auto,
      type = "relative",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    biasrel_cross <- SimDesign::bias(
      estimate = est_cross,
      parameter = true_cross,
      type = "relative",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    # absolute bias
    biasabs <- SimDesign::bias(
      estimate = c(est_mats[[j]]),
      parameter = c(true_mats),
      type = "bias",
      abs = TRUE,
      percent = FALSE,
      unname = FALSE
    )

    biasabs_auto <- SimDesign::bias(
      estimate = est_auto,
      parameter = true_auto,
      type = "bias",
      abs = TRUE,
      percent = FALSE,
      unname = FALSE
    )

    biasabs_cross <- SimDesign::bias(
      estimate = est_cross,
      parameter = true_cross,
      type = "bias",
      abs = TRUE,
      percent = FALSE,
      unname = FALSE
    )

    # bias
    bias <- SimDesign::bias(
      estimate = c(est_mats[[j]]),
      parameter = c(true_mats),
      type = "bias",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    bias_auto <- SimDesign::bias(
      estimate = est_auto,
      parameter = true_auto,
      type = "bias",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    bias_cross <- SimDesign::bias(
      estimate = est_cross,
      parameter = true_cross,
      type = "bias",
      abs = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    # rmse
    rmse <- SimDesign::RMSE(
      estimate = c(est_mats[[j]]),
      parameter = c(true_mats),
      type = "RMSE",
      MSE = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    rmse_auto <- SimDesign::RMSE(
      estimate = est_auto,
      parameter = true_auto,
      type = "RMSE",
      MSE = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    rmse_cross <- SimDesign::RMSE(
      estimate = est_cross,
      parameter = true_cross,
      type = "RMSE",
      MSE = FALSE,
      percent = FALSE,
      unname = FALSE
    )

    # bias_hand <-  mean( c(est_mats[[j]]) - c(true_mats), na.rm = TRUE )
    #
    biasabs_hand <- mean( abs(c(est_mats[[j]]) - c(true_mats)), na.rm = TRUE )
    #
    biasrel_med_hand <- median( (c(est_mats[[j]]) - c(true_mats)) / c(true_mats), na.rm = TRUE )
    #
    # est_mean <- mean(c(est_mats[[j]]), na.rm = TRUE)
    #
    # est_sd <- sd(c(est_mats[[j]]), na.rm = TRUE)
    #
    # mcsd_mean <- mean(c(true_mats), na.rm = TRUE)

    c(  "n" = n,
        "d" = d,
        "marg" = marg,
        "case" = case,
        "mod" = mod,
        "bad" = bad,
        "bias" = bias,
        "bias_auto" = bias_auto,
        "bias_cross" = bias_cross,
        "biasabs" = biasabs,
        "biasabs_auto" = biasabs_auto,
        "biasabs_cross" = biasabs_cross,
        "biasabs_hand" = biasabs_hand,
        "biasrel"= biasrel,
        "biasrel_auto"= biasrel_auto,
        "biasrel_cross"= biasrel_cross,
        "biasrel_med_hand" = biasrel_med_hand,
        "rmse" = rmse,
        "rmse_auto" = rmse_auto,
        "rmse_cross" = rmse_cross
        )

  })

  return(summary_stats)

}


