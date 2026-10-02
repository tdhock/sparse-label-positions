library(data.table)
library(ggplot2)
if(!requireNamespace("monotone.iterations")){
  pak::pak("tdhock/monotone.iterations")
}
mit <- function(x){
  n <- length(x)
  .C(
    "isoreg_dp_iterations",
    n = as.integer(n),
    x = as.double(x),
    w = as.double(rep(1, n)),
    iterations = as.integer(rep(-1L, n)),
    PACKAGE = "monotone.iterations")
}
mit(c(1:4, 3))
isofuns <- list(
  stats=function(x){
    L <- isoreg(x)
    with(L, list(solution=yf, iterations=NA_integer_))
  },
  monotone.iterations=function(x){
    L <- mit(x)
    with(L, list(solution=x, iterations=sum(iterations)))
  },
  directlabels=function(x)list(
    solution=directlabels::isoreg_dp(x),
    iterations=NA_integer_)
)
(expr.list <- atime::atime_grid(
  list(PKG=names(isofuns)),
  isoreg={
    l.vec <- cumsum(c(B.lo,h.vec[-N])+h.vec)
    fun <- isofuns[[PKG]]
    L <- fun(target-l.vec)
    with(L, data.table(
      solution=list(solution+l.vec),
      iterations))
  }))
ares <- atime::atime(
  N=10^seq(0, 7, by=0.2),
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
    data.table(solution=list(sol$solution), iterations=sol$iterations[1])
  },
  seconds.limit = 1,
  result=TRUE)
plot(ares)
(meas.long <- ares$measurements[, {
  position <- solution[[1]]
  data.table(position, i=seq_along(position))
}, keyby=.(N, expr.name)])
diff.dt <- meas.long[, {
  ref.expr <- "solve.QP PKG=quadprog"
  ref <- position[expr.name==ref.expr]
  .SD[expr.name!=ref.expr, .(sum.abs.diff = sum(abs(position-ref))), by=expr.name]
}, by=N]
if(any(diff.dt$sum.abs.diff)>1e-5){
  print(diff.dt[order(sum.abs.diff)])
  stop("some solutions not identical")
}
ares$measurements[, let(solution = NULL, result=NULL)]
saveRDS(ares, "figure-isoreg-dp-atime-iterations-data.rds")

