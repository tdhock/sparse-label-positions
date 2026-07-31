library(data.table)
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
half.size <- 5
get_targets <- function(N){
  size <- half.size*2
  m <- 11
  initial <- c(9, m)
  N.More <- N-length(initial)
  set.seed(1)
  list(
    worst=c(initial, m+(size+N^-2)*seq(1, N.More)),
    average=runif(N, 0, N*2),
    best=seq(1, N)*(size+1))
}
small.targets <- get_targets(10)
expr.list <- atime::atime_grid(
  list(case=names(small.targets)),
  proposed={
    target <- target.list[[case]]
    current <- target
    done <- FALSE
    iterations <- 0
    while(!done){
      it.dt <- get_update(target, current, half.size)
      (change <- it.dt[, sum(abs(current-update))])
      if(change<thresh)done <- TRUE
      current <- it.dt$update
      iterations <- iterations+1
    }
    data.table(iterations, solution=list(current))
  })
ares <- atime::atime(
  setup={
    target.list <- get_targets(N)
  },
  expr.list=expr.list,
  seconds.limit = 1,
  result=TRUE)
plot(ares)

saveRDS(ares, "figure-atime-cases-data.rds")


