rm(list=ls())

library("SimDesign")
library("caret")
library("mclust")

# ------------------------------------------------- #
# Get functions
# ------------------------------------------------- #
source(file.path(scripts.dir,"calculate_measures.R"))

colSD <- function (x, na.rm = FALSE) apply(X = x, MARGIN = 2, FUN = sd, na.rm = na.rm)

stationary <- function(mat) {
  eig_vals <- eigen(mat)$values
  max_eig  <- max(Mod(eig_vals))

  is_stationary <- max_eig < 1 - 1e-4

  return(is_stationary)
}
# ------------------------------------------------- #
#  Analyze
# ------------------------------------------------- #
# conditions
reps  <- 500
ds    <- c(3, 5)
ns    <- c(50, 100, 200, 500)
margs <- c("small", "medium", "large")
cases <- c("bern", "pois", "mixed")
mods  <- c("cop", "var")

# for CI coverage
alpha <- 0.05
critical_value <- qnorm(1 - alpha / 2)

# value to filter out "bad" cases
flt <- 1

res_est <- list()
res_se  <- list()
var_est <- list()
var_se <- list()
ci <- list()
ci_var <- list()

cnt <- 1

for (d in ds) {
  for (n in ns) {
    for (case in cases) {
      for (marg in margs) {
        for (mod in mods) {

          # get true mat
          data_filename <- paste0("_d",d,"_n",n,"_",case,"_",marg,"_",1,".RDS")
          data          <- readRDS(file.path(data.dir, paste0("data", data_filename)))
          true_mat      <- data$A_true[,,1]

          # get results
          if (mod == "cop") {

            files    <- file.path( res.dir, sprintf("res_d%d_n%d_%s_%s_%d.RDS", d, n, case, marg, seq_len(reps)) )
            existing <- files[file.exists(files)]
            res      <- lapply(existing, readRDS)

            # get point estimates and ses
            est <- lapply(seq_along(res), function(i) res[[i]][[1]])
            ses <- lapply(seq_along(res), function(i) res[[i]][[2]])

            # remove "bad" cases
            bad_ses <- vapply(ses, function(i) any(abs(i) > 100, na.rm = TRUE), logical(1))
            bad_est <- vapply(est, function(i) !stationary(i), logical(1))

            keep <- !(bad_est | bad_ses)

            bad <- sum(!keep)

            est <- est[keep]
            ses <- ses[keep]

          } else {

            files    <- file.path( res.dir, sprintf("var_d%d_n%d_%s_%s_%d.RDS", d, n, case, marg, seq_len(reps)) )
            existing <- files[file.exists(files)]
            res      <- lapply(existing, readRDS)

            # get point estimates and ses
            est <- lapply(seq_along(res), function(i) vars::Acoef(res[[i]])[[1]])
            ses <- lapply(seq_along(res), function(i){
              t(sapply(coef(res[[i]]), function(tab) tab[grep("\\.l1$", rownames(tab)), "Std. Error"]))
            })

            bad_est <- vapply(est, function(i) !stationary(i), logical(1))

            # keep <- !(bad_est | bad_ses)
            keep <- !bad_est

            bad <- sum(!keep)

            est <- est[keep]
            ses <- ses[keep]

          }

          # get mcsd
          if (length(est) == 0){
            mcsd <- NA
          } else {
            mcsd  <- matrix( colSD(do.call("rbind", lapply(seq_along(est), function(i) c(est[[i]])))), d, d )
          }

          # calculate outcome measures
          if (length(est) == 0){
            res_est[[cnt]] <-  NA
          } else {
            res_est[[cnt]] <- as.data.frame(do.call("rbind", calculate_measures(
              true_mats = true_mat,
              est_mats = est,
              d = d,
              n = n,
              marg = marg,
              case = case,
              mod = mod,
              bad = bad
            )))
          }

          if (length(ses) == 0) {
            res_se[[cnt]] <- NA
          } else {
            res_se[[cnt]] <- as.data.frame(do.call("rbind", calculate_measures(
              true_mats = mcsd,
              est_mats = ses,
              d = d,
              n = n,
              marg = marg,
              case = case,
              mod = mod,
              bad = bad
            )))
          }

          if (length(est) == 0) {
            ci[[cnt]] <- NA
          } else {
            ci[[cnt]] <- do.call("rbind", lapply(seq_along(est), function(i){

              z        <- (est[[i]] - true_mat) / ses[[i]]
              coverage <- abs(z) <= critical_value

              c("n" = n,
                "d" = d,
                "marg" = marg,
                "case" = case,
                "mod" = mod,
                "ci" = mean(c(coverage), na.rm = TRUE)
              )

            }))
          }

          cnt <- cnt + 1

        }
      }
    }
  }
}

# ------------------------------------------------- #
#  Summarize results
# ------------------------------------------------- #
# combine results
df_est <- as.data.frame(do.call("rbind",res_est))
df_ses <- as.data.frame(do.call("rbind",res_se))

# make columns numeric
num_columns <- c("n","d","bad","bias","biasabs", "biasabs_hand", "biasrel","biasrel_med_hand", "rmse")

df_est[, num_columns] <- lapply(num_columns, function(x) as.numeric(df_est[[x]]))
df_ses[, num_columns] <- lapply(num_columns, function(x) as.numeric(df_ses[[x]]))

# summarize results across conditions
dt_est <- data.table::data.table(df_est)[, list(
  Nreps = .N,
  bad = mean(bad),
  biasabs = round(mean(biasabs), 4),
  biasabs_med = round(median(biasabs), 4),
  sd_biasabs = round(sd(biasabs), 4),
  biasabs_hand = round(mean(biasabs_hand), 4),
  sd_biasabs_hand = round(sd(biasabs_hand), 4),
  bias = round(mean(bias), 4),
  sd_bias = round(sd(bias), 4),
  biasrel = round(mean(biasrel), 4),
  sd_biasrel = round(sd(biasrel), 4),
  biasrel_med = round(median(biasrel), 4),
  biasrel_med_hand = round(mean(biasrel_med_hand), 4),
  sd_biasrel_med_hand = round(sd(biasrel_med_hand), 4),
  rmse = round(mean(rmse), 4),
  sd_rmse = round(sd(rmse), 4)
), by = c("n","d","marg","case","mod")]

dt_ses <- data.table::data.table(df_ses)[, list(
  Nreps = .N,
  bad = mean(bad),
  biasabs = round(mean(biasabs, na.rm = TRUE), 4),
  biasabs_med = round(median(biasabs, na.rm = TRUE), 4),
  sd_biasabs = round(sd(biasabs, na.rm = TRUE), 4),
  bias = round(mean(bias, na.rm = TRUE), 4),
  sd_bias = round(sd(bias, na.rm = TRUE), 4),
  biasrel = round(mean(biasrel, na.rm = TRUE), 4),
  biasrel_med = round(median(biasrel, na.rm = TRUE), 4),
  sd_biasrel = round(sd(biasrel, na.rm = TRUE), 4),
  biasrel_med_hand = round(mean(biasrel_med_hand), 4),
  sd_biasrel_med_hand = round(sd(biasrel_med_hand), 4),
  rmse = round(mean(rmse, na.rm = TRUE), 4),
  sd_rmse = round(sd(rmse, na.rm = TRUE), 4)
), by = c("n","d","marg","case","mod")]

# CI coverage
df_ci <- as.data.frame(do.call("rbind", ci))

num_columns <- c("n", "d")
df_ci[, num_columns] <- lapply(num_columns, function(x) as.numeric(df_ci[[x]]))
df_ci[,"ci"] <- as.logical(df_ci[,"ci"])

dt_ci <- data.table::data.table(df_ci)[, list(
  Nreps = .N,
  ci = mean(ci, na.rm = TRUE)
), by = c("n","d","marg","case","mod")]
