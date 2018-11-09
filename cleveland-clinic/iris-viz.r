library(ggplot2)
library(dplyr)
library(tidyr)
library(trelliscopejs)
library(rbokeh)
library(foreach)
library(nnet)

data(iris)

mc <- combn(names(iris)[1:4], 2)

iris_plot <- function(x) {
  ggplot(x, aes(x=x, y=y, group=Species, color=Species)) + geom_point() + 
    xlab(x$xlab[1]) + ylab(x$ylab[1])
#  figure(xlab=x$xlab[1], ylab=x$ylab[1]) %>% 
#    ly_points(x='x', y='y', color='Species', data=x)
}

iris_comb <- foreach(j=1:ncol(mc), .combine=rbind) %do% {
  ret <- iris[, c(mc[1, j], mc[2, j], "Species")]
  names(ret)[1:2] <- c("x", "y")
  ret$feature1 <- mc[1, j]
  ret$xlab <- mc[1, j]
  ret$feature2 <- mc[2, j]
  ret$ylab <- mc[2, j]
  ret
} %>% nest(-feature1, -feature2) 

iris_comb %>% 
  mutate(plot=map_plot(data, iris_plot)) %>%
  trelliscope(name="Iris", panel_col="plot", auto_cog=FALSE, 
              path="iris-plot")

s <- summary(multinom(Species ~ ., data=iris))
s
z <- s$coefficients/s$standard.errors
(1 - pnorm(abs(z), 0, 1)) * 2

