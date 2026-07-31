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
ares <- atime::atime(
  setup={
    half.size <- 5
    size <- half.size*2
    m <- 11
    initial <- c(9, m)
    N.More <- N-length(initial)
    (target <- c(initial, m+(size+N^-2)*seq(1, N.More)))
  },
  proposed={
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
  },
  previous={
    k <- length(target)
    D <- diag(rep(1, k))
    Ik <- diag(rep(1, k - 1))
    A <- rbind(0, Ik) - rbind(Ik, 0)
    y.up <- target+half.size
    y.lo <- target-half.size
    b0 <- (y.up - target)[-k] + (target - y.lo)[-1]
    sol <- quadprog::solve.QP(D, target, A, b0)
    data.table(iterations=sol$iterations[1], solution=list(sol$solution))
  },
  seconds.limit = 1,
  result=TRUE)
plot(ares)

saveRDS(ares, "figure-atime-worst-data.rds")


