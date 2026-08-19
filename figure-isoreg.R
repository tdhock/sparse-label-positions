N.boxes <- 10
set.seed(1)
target <- sort(runif(N.boxes, 0, 2*N.boxes))

B.lo <- 0
B.hi <- 20
h.vec <- rep(0.5, N.boxes)
l.vec <- cumsum(c(B.lo,h.vec[-N.boxes])+h.vec)
d.vec <- target-l.vec
iso.list <- isoreg(d.vec)
x.vec <- iso.list$yf+l.vec
library(data.table)
library(ggplot2)
get_update <- function(target, current, half.size)data.table(
  target, current, half.size
)[
, active := !c(TRUE, (current+half.size)[-.N] < (current-half.size)[-1])
][
, cluster := cumsum(!active)
][
, offset := cumsum(c(0,half.size[-1]+half.size[-.N])), by=cluster
][
, update := offset+mean(current-offset), by=cluster
][]

done <- FALSE
current <- target
while(!done){
  it.dt <- get_update(target, current, 0.5)
  current <- it.dt$update
  if(it.dt[, sum(abs(current-update))<1e-10])done <- TRUE
}

rect.info <- rbind(
  data.table(variable="target"),
  data.table(variable="update"),
  data.table(variable="isoreg")
)
it.dt[, isoreg := x.vec ]
it.dt[, update-isoreg]
(target.long <- melt(
  it.dt,
  measure.vars=rect.info$variable
)[rect.info, on="variable"][, Variable := factor(variable, rect.info$variable)])
gg <- ggplot()+
  geom_point(aes(
    0, target),
    shape=1,
    data=it.dt)+
  geom_rect(aes(
    xmin=1, xmax=20,
    ymin=value-half.size, ymax=value+half.size),
    color="black",
    alpha=0.5,
    data=target.long)+
  facet_grid(. ~ Variable)+
  scale_x_continuous("", breaks=c())

png("figure-isoreg.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()
