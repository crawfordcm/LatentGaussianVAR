set.seed(92)

main.dir <- file.path("")
mats.dir <- file.path(main.dir, "matrices")

d    <- c(3, 5)
n    <- c(50, 100, 200, 500)
case <- c("pois", "bern", "mixed")
marg <- c("small", "medium", "large")

df_mat   <- expand.grid(d, case, marg)

# bounds for uniform draws
lower <- -0.4
upper <- 0.4

for (i in 1:nrow(df_mat)) {

  d <- df_mat[i,1]

  filename <- paste0("_d",df_mat[i,1],"_",df_mat[i,2],"_",df_mat[i,3],".RDS")

  nonstationary <- TRUE

  while (nonstationary) {

    mat <- matrix(runif(d*d, lower, upper), nrow = d, byrow = TRUE)

    max_eig <- max(abs(eigen(mat)$values))

    if (max_eig > .95) {
      nonstationary <- TRUE
    } else {
      nonstationary <-FALSE
    }

  }

  saveRDS(mat, file.path(mats.dir, paste0("mat", filename)))

}

