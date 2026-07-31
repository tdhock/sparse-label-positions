library(data.table)
library(ggplot2)
ares <- readRDS("figure-atime-cases-data.rds")

png("figure-atime-cases.png", width=6, height=5, units="in", res=200)
plot(ares)
dev.off()

aref <- atime::references_best(ares)
png("figure-atime-cases-ref.png", width=8, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=1, kilobytes=1e4)
png("figure-atime-cases-pred.png", width=6, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()
