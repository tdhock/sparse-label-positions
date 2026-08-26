library(data.table)
library(ggplot2)
isofuns <- list(
  stats=function(x)isoreg(x)$yf,
  directlabels=directlabels::isoreg_dp)
(expr.list <- atime::atime_grid(
  list(PKG=names(isofuns)),
  isoreg={
    l.vec <- cumsum(c(B.lo,h.vec[-N])+h.vec)
    fun <- isofuns[[PKG]]
    fun(target-l.vec)+l.vec
  }))
ares <- atime::atime(
  setup={
    set.seed(1)
    target <- sort(runif(N, 0, 2*N))
    half.size <- 0.5
    h.vec <- rep(half.size, N)
    B.lo <- -100
    B.hi <- 200
  },
  expr.list=expr.list,
  "solve.QP PKG=quadprog"={
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
plot(ares)

saveRDS(ares, "figure-isoreg-dp-atime-data.rds")

