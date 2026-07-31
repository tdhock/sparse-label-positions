library(data.table)
library(ggplot2)
ares <- readRDS("figure-atime-worst-data.rds")
#ares$measurements <- ares$measurements[N<40]
#plot(ares)

png("figure-atime-worst.png", width=6, height=5, units="in", res=200)
plot(ares)
dev.off()

aref <- atime::references_best(ares)
png("figure-atime-worst-ref.png", width=6, height=5, units="in", res=200)
plot(aref)
dev.off()

apred <- predict(aref, seconds=1, kilobytes=1e4)
png("figure-atime-worst-pred.png", width=6, height=5, units="in", res=200)
plot(apred)+geom_blank(aes(10,5), data=data.table(unit="seconds"))
dev.off()
