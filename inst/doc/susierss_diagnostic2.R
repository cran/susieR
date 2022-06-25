library(curl)
data_file <- tempfile(fileext = ".RData")
data_url <- paste0("https://raw.githubusercontent.com/stephenslab/susieR/",
                   "master/inst/datafiles/SummaryConsistency1k.RData")
curl_download(data_url,data_file)
load(data_file)
zflip = SummaryConsistency$z
ld = SummaryConsistency$ldref
b = numeric(length(zflip))
b[SummaryConsistency$signal_id] = zflip[SummaryConsistency$signal_id]

fit = susie_rss(zflip, ld)
susie_plot(fit, y='PIP', b=b)
points(SummaryConsistency$flip_id, fit$pip[SummaryConsistency$flip_id],
       col=7, pch=16)

condz = kriging_rss(zflip, ld)
condz$plot

library(ggplot2)
library(cowplot)
flipped <- rep(FALSE,1002)
flipped[SummaryConsistency$flip_id] <- TRUE
pdat <- subset(condz$conditional_dist,is.finite((logLR)))
ggplot(pdat,aes(x = z,y = -logLR,color = flipped)) +
  geom_point() +
  scale_color_manual(values = c("darkblue","darkorange")) +
  theme_cowplot()
