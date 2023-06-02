## Exemplary variance filter executed on three different matrices in parallel.
## Can be used in gene expression analysis as a prefilter
## for the number of covariates.
 
  library(parallel)
  n <- 300 # observations
  p <- 20000 # covariates
  ## Different sized matrices as filter inputs
  ## Matrix A and B form smaller work loads
  ## while matrix C forms a bigger workload (2*p)
  
  library(stats)
  
  A <- matrix(replicate( p, rnorm(n, sd = runif(1, 0.1, 10))), n, p)
  B <- matrix(replicate( p, rnorm(n, sd = runif(1, 0.1, 10))), n, p)
  C <- matrix(replicate(2*p, rnorm(n, sd = runif(1, 0.1, 10))), n, 2*p)
  varFilter <- function (X, nSim = 20) {
  for (i in 1:nSim) {
  train <- sample(nrow(X), 2 / 3 * nrow(X))
  colVars <- apply(X[train, ], 2, var)
  keep <- names(head(sort(colVars, decreasing = TRUE), 100))
  # myAlgorithm(X[, keep])
  }
  }
  ## Runtime comparison -----------------------------------
 
  ## mclapply with affinity.list
  ## CPU mapping: A and B run on CPU 1 while C runs on CPU 2:
  affinity <- c(1,1,2)
  system.time(
  mclapply(X = list(A,B,C), FUN = varFilter,
  mc.preschedule = FALSE, affinity.list = affinity))
  ## user system elapsed
  ## 34.909 0.873 36.720
 
 
  ## mclapply without affinity.list

  system.time(
  mclapply(X = list(A,B,C), FUN = varFilter, mc.cores = 2,
  mc.preschedule = FALSE) )
  ## user system elapsed
  ## 72.893 1.588 55.982
 
 
  ## mclapply with prescheduling
  system.time(
  mclapply(X = list(A,B,C), FUN = varFilter, mc.cores = 2,
  mc.preschedule = TRUE) )
  ## user system elapsed
  ## 53.455 1.326 53.399
#Instead of using affinity.list for runtime optimization, it can also be used to simply restrict3
#the computation to speciőc CPUs as in the following example.
  ## Restricts all elements of X to run on CPU 1 and 2.
  X <- list(1, 2, 3)
  affinity.list <- list(c(1,2), c(1,2), c(1,2))
  mclapply(X = X, FUN = function (i) i*i,
  mc.preschedule = FALSE, affinity.list = affinity.list)

