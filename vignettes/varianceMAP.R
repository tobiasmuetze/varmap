## ---- results = 'asis', echo = TRUE--------------------------------------
library(varmap)
data(varianceSPB)

## ---- results = 'asis', echo = TRUE--------------------------------------
knitr::kable(varianceSPB, caption = "Sample variance of historical clinical trials", row.names = TRUE)

## ---- results = 'hide', echo = TRUE--------------------------------------
out <- varmeta(sample_var = varianceHAMD$sample_var, degf = varianceHAMD$degf, mu_mean = 3, n.iter = 1000)

## ---- results = 'hide', echo = FALSE, warning=FALSE----------------------
library(ggplot2)
map_variance <- out[[1]][, "map_variance"]

map_df <- data.frame(map = as.vector(map_variance))
p <- ggplot(data = map_df, aes(x = map)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, colour="black", fill="white") +
  xlim(c(0, 100)) +
  xlab(expression(paste("Variance ", hat(sigma)[new]^{2}, " of new trial"))) +
  ylab("Density") +
  theme_bw() +
  theme(legend.key = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        text = element_text(size = 13),
        legend.text=element_text(size = 14))


## ---- fig.width=4, fig.height=4, fig.align="center", warning=FALSE, echo=FALSE, fig.cap="Histogram of MAP prior for variance"----
p

## ------------------------------------------------------------------------
fit_out <- fit_gammamix(x = 1/map_variance, k = 2)
fit_out

## ---- fig.width=4, fig.height=4, fig.align="center", warning=FALSE, echo=FALSE----
p + stat_function(fun = dmixinvgamma, args = fit_out[c("w", "shape", "rate")], size = 0.8)

