N.boxes <- 10
set.seed(1)
target <- sort(runif(N.boxes, 0, 2*N.boxes))
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
it.dt <- get_update(target, target, 0.5)

rect.info <- rbind(
  data.table(variable="current", xmin=-1, xmax=-0.1),
  data.table(variable="update", xmin=0.1, xmax=1)
)
(target.long <- melt(
  it.dt,
  measure.vars=c("current","update")
)[rect.info, on="variable"])
ggplot()+
  geom_point(aes(
    0, target),
    data=it.dt)+
  geom_rect(aes(
    xmin=xmin, xmax=xmax,
    ymin=value-half.size, ymax=value+half.size,
    fill=active),
    color="black",
    alpha=0.5,
    data=target.long)+
  geom_text(aes(
    xmin, value, label=cluster),
    hjust=0,
    data=target.long)

done <- FALSE
iteration <- 1
current <- target
iteration.dt.list <- list()
while(!done){
  it.dt <- get_update(target, current, 0.5)
  if(it.dt[, all(current==update)])done <- TRUE
  current <- it.dt$update
  iteration.dt.list[[iteration]] <- data.table(iteration, it.dt)
  iteration <- iteration+1
}
(iteration.dt <- rbindlist(iteration.dt.list))

gg <- ggplot()+
  geom_point(aes(
    iteration, target),
    data=iteration.dt)+
  geom_rect(aes(
    xmin=iteration, xmax=iteration+0.9,
    ymin=current-half.size, ymax=current+half.size,
    fill=active),
    color="black",
    alpha=0.5,
    data=iteration.dt)+
  geom_text(aes(
    iteration, current, label=paste0(" cluster ", cluster)),
    hjust=0,
    data=iteration.dt)+
  scale_x_continuous(breaks=seq(1, max(iteration.dt$iteration)))
png("figure-sim.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()
