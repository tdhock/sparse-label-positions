half.size <- 5
size <- half.size*2
m <- 11
initial <- c(9, m)
N <- 10
N.More <- N-length(initial)
(target <- c(initial, m+(size+N^-2)*seq(1, N.More)))
library(data.table)
library(ggplot2)
thresh <- 1e-10
get_update <- function(target, current, half.size)data.table(
  target, current, half.size
)[
, active := !c(TRUE, (current+half.size)[-.N] < (current-half.size)[-1]-thresh)
][
, cluster := cumsum(!active)
][
, offset := cumsum(c(0,half.size[-1]+half.size[-.N])), by=cluster
][
, update := offset+mean(current-offset), by=cluster
][]

done <- FALSE
iteration <- 1
current <- target
iteration.dt.list <- list()
while(!done){
  (it.dt <- get_update(target, current, half.size))
  (change <- it.dt[, sum(abs(current-update))])
  if(change<thresh)done <- TRUE
  current <- it.dt$update
  iteration.dt.list[[iteration]] <- data.table(iteration, it.dt)
  iteration <- iteration+1
}
(iteration.dt <- rbindlist(iteration.dt.list))

n.clust <- max(it.dt$cluster)
Zmat <- matrix(0, nrow(it.dt), n.clust)
Zmat[it.dt[, cbind(1:.N, cluster)]] <- 1
iact <- which(it.dt$active)
Amat <- matrix(0, length(iact), nrow(it.dt))
Amat[cbind(seq_along(iact), iact)] <- -1
Amat[cbind(seq_along(iact), iact-1)] <- 1
Amat %*% Zmat # should be zero
dvec <- it.dt$current-target
t(Zmat) %*% dvec # gradient, should be close to zero
fit <- lm.fit(t(Amat), -dvec)
fit$coefficients # should be all positive
t(Amat) %*% fit$coefficients + dvec # should be near zero

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
    iteration, current, label=paste0(" ", cluster)),
    hjust=0,
    data=iteration.dt)+
  scale_x_continuous(breaks=seq(1, max(iteration.dt$iteration)))
png("figure-worst-case.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()
