## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 4.5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(susieR)
library(Matrix)
library(microbenchmark)
library(ggplot2)
set.seed(1)

## -----------------------------------------------------------------------------
create_sparsity_mat <- function(sparsity, n, p) {
  nonzero          <- round(n*p*(1-sparsity))
  nonzero.idx      <- sample(n*p, nonzero)
  mat              <- numeric(n*p)
  mat[nonzero.idx] <- 1
  mat              <- matrix(mat, nrow=n,ncol=p)
  return(mat)
}
n <- 1000
p <- 10000

## -----------------------------------------------------------------------------
X.dense  <- create_sparsity_mat(0.99,n,p)
X.sparse <- as(X.dense,"dgCMatrix")
X.tilde  <- susieR:::set_X_attributes(X.dense) #returns a scaled X if input is a dense matrix
X <- susieR:::set_X_attributes(X.sparse) #return an unsacled sparse X if input is a sparse matrix 
                                     #but computes column means and standard deviations

## -----------------------------------------------------------------------------
b <- rnorm(p)
y <- rnorm(n)

## -----------------------------------------------------------------------------
res1 <- X.tilde %*% b
res2 <- susieR:::compute_Xb(X,b)
max(abs(res1 - res2))

## -----------------------------------------------------------------------------
compute_Xb_benchmark <- microbenchmark(
  dense  = (use.normal.Xb <- X.tilde%*%b),
  sparse = (use.sparse.Xb <- susieR:::compute_Xb(X,b)),
  times = 20,unit = "s")

## -----------------------------------------------------------------------------
autoplot(compute_Xb_benchmark)

## -----------------------------------------------------------------------------
res3 <- t(X.tilde) %*% y
res4 <- susieR:::compute_Xty(X,y)
max(abs(res3 - res4))

## -----------------------------------------------------------------------------
compute_Xty_benchmark = microbenchmark(
  dense  = (use.normal.Xty <- t(X.tilde)%*%y),
  sparse = (use.sparse.Xty <- susieR:::compute_Xty(X, y)),
  times = 20,unit = "s")

## -----------------------------------------------------------------------------
autoplot(compute_Xty_benchmark)

