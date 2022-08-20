## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 4.5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
set.seed(1)
n=1000
mu = c(rep(0,100),rep(1,100),rep(3,100),rep(-2,100),rep(0,600))
y = mu + rnorm(n)
s = susie_trendfilter(y, 0, L=10)

## -----------------------------------------------------------------------------
plot(y,pch=".")
lines(mu,col=1,lwd=3)
lines(predict(s),col=2,lwd=2)
s$sigma2

## -----------------------------------------------------------------------------
set.seed(1)
mu = seq(0,4,length=1000)
y = mu + rnorm(n)
s = susie_trendfilter(y,0,L=10)
plot(y,pch=".")
lines(mu,col=1,lwd=3)
lines(predict(s),col=2,lwd=2)

## ---- eval=FALSE--------------------------------------------------------------
#  # install.packages("remotes")
#  # remotes::install_github("glmgen/genlasso")
#  y.tf = trendfilter(y,ord=0)
#  y.tf.cv = cv.trendfilter(y.tf)
#  plot(y,pch=".")
#  lines(mu,col=1,lwd=3)
#  lines(predict(s),col=2,lwd=2)
#  lines(y.tf$fit[,which(y.tf$lambda==y.tf.cv$lambda.min)],col=4,lwd=2)

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(1)
#  mu = seq(0,4,length=1000)
#  mu = mu + c(rep(0,500),rep(4,500))
#  y = mu + rnorm(n)
#  s = susie_trendfilter(y,0,L=10)
#  y.tf = trendfilter(y,ord=0)
#  y.tf.cv = cv.trendfilter(y.tf)
#  plot(y,pch=".")
#  lines(mu,col=1,lwd=3)
#  lines(predict(s),col=2,lwd=2)
#  lines(y.tf$fit[,which(y.tf$lambda==y.tf.cv$lambda.min)],col=4,lwd=2)

## ---- eval=FALSE--------------------------------------------------------------
#  sqrt(mean((mu-y.tf$fit[,which(y.tf$lambda==y.tf.cv$lambda.min)])^2))
#  sqrt(mean((mu-predict(s))^2))

