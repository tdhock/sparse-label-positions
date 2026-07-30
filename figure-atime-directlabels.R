library(data.table)
library(ggplot2)
ares <- readRDS("figure-atime-directlabels-data.rds")

png("figure-atime-directlabels.png", width=6, height=5, units="in", res=200)
plot(ares)
dev.off()

aref <- atime::references_best(ares)
png("figure-atime-directlabels-ref.png", width=6, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=1, kilobytes=1e4)
png("figure-atime-directlabels-pred.png", width=6, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()
