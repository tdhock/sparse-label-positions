library(data.table)
library(ggplot2)
ares <- readRDS("figure-isoreg-dp-atime-iterations-data.rds")
rfuns <- list(
  N=function(x)log10(x),
  "N^2"=function(x)2*log10(x),
  "N^3"=function(x)3*log10(x))
aref <- atime::references_best(ares, rfuns)

png("figure-isoreg-dp-atime-iterations.png", width=7, height=5, units="in", res=200)
plot(ares)
dev.off()

png("figure-isoreg-dp-atime-iterations-ref.png", width=12, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=0.1, kilobytes=1e5, iterations=900)
png("figure-isoreg-dp-atime-iterations-pred.png", width=9, height=5, units="in", res=200)
plot(apred)
#+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()

