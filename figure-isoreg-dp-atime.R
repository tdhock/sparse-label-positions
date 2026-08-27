library(data.table)
library(ggplot2)
atime::atime
ares <- readRDS("figure-isoreg-dp-atime-data.rds")

png("figure-isoreg-dp-atime.png", width=7, height=5, units="in", res=200)
plot(ares)+
  scale_y_log10(breaks=10^seq(-10, 10))
dev.off()

rfuns <- list(
  N=function(x)log10(x),
  "N^2"=function(x)2*log10(x),
  "N^3"=function(x)3*log10(x))
aref <- atime::references_best(ares, rfuns)
png("figure-isoreg-dp-atime-ref.png", width=12, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=0.1, kilobytes=1000)
png("figure-isoreg-dp-atime-pred.png", width=9, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()

png("figure-isoreg-dp-atime-err.png", width=5, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()
