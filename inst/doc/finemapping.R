## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
set.seed(1)

## -----------------------------------------------------------------------------
data(N3finemapping)
attach(N3finemapping)

## -----------------------------------------------------------------------------
dim(Y)

## -----------------------------------------------------------------------------
b <- true_coef[,1]
plot(b, pch=16, ylab='effect size')

## -----------------------------------------------------------------------------
which(b != 0)

## -----------------------------------------------------------------------------
sumstats <- univariate_regression(X, Y[,1])
z_scores <- sumstats$betahat / sumstats$sebetahat
susie_plot(z_scores, y = "z", b=b)

## -----------------------------------------------------------------------------
fitted <- susie(X, Y[,1],
                L = 10,
                estimate_residual_variance = TRUE, 
                estimate_prior_variance = FALSE,
                scaled_prior_variance = 0.1,
				verbose = TRUE)

## -----------------------------------------------------------------------------
print(fitted$sets)

## -----------------------------------------------------------------------------
sets <- susie_get_cs(fitted,
                     X = X,
	  	     coverage = 0.9,
                     min_abs_corr = 0.1)

## -----------------------------------------------------------------------------
print(sets)

## -----------------------------------------------------------------------------
susie_plot(fitted, y="PIP", b=b)

## -----------------------------------------------------------------------------
i  <- fitted$sets$cs[[3]]
z3 <- cbind(i,z_scores[i],fitted$pip[i])
colnames(z3) <- c('position', 'z-score', 'PIP')
z3[order(z3[,2], decreasing = TRUE),]

## -----------------------------------------------------------------------------
fitted = susie(X, Y[,1],
               L = 10,
               estimate_residual_variance = TRUE, 
               estimate_prior_variance = FALSE, 
               scaled_prior_variance = 0.2)
susie_plot(fitted, y='PIP', b=b)

## -----------------------------------------------------------------------------
fitted = susie(X, Y[,1],
               L = 10,
               estimate_residual_variance = TRUE)
susie_plot(fitted, y='PIP', b=b)

## ---- eval=FALSE--------------------------------------------------------------
#  remove.covariate.effects <- function (X, Z, y) {
#    # include the intercept term
#    if (any(Z[,1]!=1)) Z = cbind(1, Z)
#    A   <- forceSymmetric(crossprod(Z))
#    SZy <- as.vector(solve(A,c(y %*% Z)))
#    SZX <- as.matrix(solve(A,t(Z) %*% X))
#    y <- y - c(Z %*% SZy)
#    X <- X - Z %*% SZX
#    return(list(X = X,y = y,SZy = SZy,SZX = SZX))
#  }
#  
#  out = remove.covariate.effects(X, Z, Y[,1])
#  fitted_adjusted = susie(out$X, out$y,
#                 L = 10,
#                 estimate_residual_variance = TRUE)

## -----------------------------------------------------------------------------
sessionInfo()

