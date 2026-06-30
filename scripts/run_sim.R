rm(list=ls())

library(timecop)

# -------------------------------------------------- #
# info from longleaf
# -------------------------------------------------- #
args <- commandArgs(trailingOnly = TRUE)
print(args)

iter   <- as.numeric(args[1])
n      <- as.numeric(args[2])
d      <- as.numeric(args[3])
marg   <- as.character(args[4])
case   <- as.character(args[5])

set.seed(iter)

# ------------------------------------------------- #
#  longleaf
# ------------------------------------------------- #
main.dir <- file.path("")

# ------------------------------------------------- #
# Set up directories
# ------------------------------------------------- #
data.dir <- file.path(main.dir, "data")
res.dir  <- file.path(main.dir, "results")
mats.dir <- file.path(main.dir, "matrices")
obj.dir  <- file.path(main.dir, "objects")

# ------------------------------------------------- #
# Generate a dataset
# ------------------------------------------------- #
p    <- 1
corr <- FALSE

family     <- vector("list", d)
marg_param <- vector("list", d)

base_filename <- paste0("_d",d,"_n",n,"_",case,"_",marg,"_",iter,".RDS")
mats_filename <- paste0("_d",d,"_",case,"_",marg,".RDS")

# set up list of marginal distributions
if (case == "bern") {

  family[seq_len(d)] <- "Bernoulli"

} else if (case == "pois"){

  family[seq_len(d)] <- "Poisson"

} else {

  if (d == 3) {
    family[seq_len(d)] <- c("Bernoulli", "Poisson", "Gaussian")
  } else if (d == 5) {
    family[seq_len(d)] <- c( rep("Bernoulli", 2), rep("Poisson", 2), "Gaussian" )
  }

}

# set up list of marginal parameters
if (case == "bern"){

  if (marg == "small") {
    marg_param[seq_len(d)] <- 0.3
  } else if (marg == "medium") {
    marg_param[seq_len(d)] <- 0.5
  } else {
    marg_param[seq_len(d)] <- 0.7
  }

} else if (case == "pois") {

  if (marg == "small") {
    marg_param[seq_len(d)] <- 1
  } else if (marg == "medium") {
    marg_param[seq_len(d)] <- 5
  } else {
    marg_param[seq_len(d)] <- 10
  }

} else {

  if (marg == "small") {
    if (d == 3) {
      marg_param[seq_len(d)] <- c(0.3, 1, NA)
    } else if (d == 5) {
      marg_param[seq_len(d)] <- c( rep(0.3, 2), rep(1, 2), NA )
    }
  } else if (marg == "medium") {
    if (d == 3) {
      marg_param[seq_len(d)] <- c(0.5, 5, NA)
    } else if (d == 5) {
      marg_param[seq_len(d)] <- c( rep(0.5, 2), rep(5, 2), NA )
    }
  } else {
    if (d == 3) {
      marg_param[seq_len(d)] <- c(0.7, 10, NA)
    } else if (d == 5) {
      marg_param[seq_len(d)] <- c( rep(0.7, 2), rep(10, 2), NA )
    }
  }

}

# get transition matrix
mat <- readRDS(file.path(mats.dir, paste0("mat", mats_filename)))

# simulate data
sim  <- timeop::latent_var_sim(d, n, p, marg_param, mat, family)
data <- t(sim$X_t)

saveRDS(sim, file.path(data.dir, paste0("data", base_filename)))

# fit timecop model
obj <- timecop(data, family, corr = corr)
saveRDS(obj, file.path(obj.dir, paste0("obj", base_filename)))

fit <- fit_timecop(obj)
saveRDS(fit, file.path(res.dir, paste0("res", base_filename)))

# fit canonical VAR
fit_var <- vars::VAR(data)
saveRDS(fit_var, file.path(res.dir, paste0("var", base_filename)))



