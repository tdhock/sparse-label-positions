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
ares <- atime::atime(
  setup={
    set.seed(1)
    target <- sort(runif(N, 0, 2*N))
    half.size <- 0.5
  },
  proposed={
    current <- target
    done <- FALSE
    iteration <- 0
    while(!done){
      it.dt <- get_update(target, current, half.size)
      if(it.dt[, all(current==update)])done <- TRUE
      current <- it.dt$update
      iteration <- iteration+1
    }
    data.table(iteration, solution=list(current))
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
    data.table(iteration=sol$iterations[1], solution=list(sol$solution))
  },
  seconds.limit = 1,
  result=TRUE)
saveRDS(ares, "figure-atime-directlabels-data.rds")


