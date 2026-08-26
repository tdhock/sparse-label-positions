library(data.table)
library(ggplot2)
atime::atime
ares <- readRDS("figure-isoreg-dp-atime-data.rds")

png("figure-isoreg-dp-atime.png", width=6, height=5, units="in", res=200)
plot(ares)
dev.off()

aref <- atime::references_best(ares)
png("figure-isoreg-dp-atime-ref.png", width=10, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=0.1, kilobytes=1000)
png("figure-isoreg-dp-atime-pred.png", width=9, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()
