## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 4.5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## ----warning=FALSE------------------------------------------------------------
library(Matrix)
library(susieR)
set.seed(1)

## -----------------------------------------------------------------------------
create_sparsity_mat = function(sparsity, n, p) {
  nonzero          <- round(n*p*(1-sparsity))
  nonzero.idx      <- sample(n*p, nonzero)
  mat              <- numeric(n*p)
  mat[nonzero.idx] <- 1
  mat              <- matrix(mat, nrow=n, ncol=p)
  return(mat)
}

## -----------------------------------------------------------------------------
n <- 1000
p <- 1000
beta <- rep(0,p)
beta[c(1,300,400,1000)] <- 10 
X.dense  <- create_sparsity_mat(0.99,n,p)
X.sparse <- as(X.dense,"CsparseMatrix")
y <- c(X.dense %*% beta + rnorm(n))

## -----------------------------------------------------------------------------
susie.dense <- susie(X.dense,y)

## -----------------------------------------------------------------------------
susie.sparse <- susie(X.sparse,y)

