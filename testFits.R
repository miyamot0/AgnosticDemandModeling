
dataSetLoad = read.csv("dataSet.csv")


dataFramePrep = data.frame(
  id       = 1:nParticipants,
  q0.HS    = numeric(length = nParticipants),
  q0.HSw   = numeric(length = nParticipants),
  q0.Koff  = numeric(length = nParticipants),
  q0.Koffw = numeric(length = nParticipants),
  a.HS     = numeric(length = nParticipants),
  a.HSw    = numeric(length = nParticipants),
  a.Koff   = numeric(length = nParticipants),
  a.Koffw  = numeric(length = nParticipants),
  K        = numeric(length = nParticipants)
)

getEXPL <- function(Q, K, A, P) {
  log(Q)/log(10) + K * (exp(-A * Q * P) - 1)
}

getEXPT <- function(Q, K, A, P) {
  Q * 10^(K*(exp(-A * Q * P) - 1))
}

min.RSS.EXPT <- function(data, par) {
  with(data, sum((getEXPT(par[1], kSet, 10^par[2], x) - y)^2))
}

min.RSS.EXPL <- function(data, par) {
  newData = data
  newAsymptote = 10^(log10(par[1]) - kSet)
  newData[newData$y == 0, "y"] <- newAsymptote
  newData$y <- log10(newData$y)

  with(newData, sum((getEXPL(par[1], kSet, 10^par[2], x) - y)^2))
}

min.RSS.EXPL.aw <- function(data, par) {
  newData = data
  newAsymptote = 10^(log10(par[1]) - kSet)
  newData[newData$y == 0, "y"] <- newAsymptote

  newData$y <- log10(newData$y)
  newData$ys   = getEXPL(par[1], kSet, 10^par[2], newData$x)
  newData$err  = (10^newData$ys - 10^newData$y)^2

  sum(newData$err)
}

minQ0 = 0.01
maxQ0 = 200

currentData = dataSet[dataSet$id == 1, ]

dataFramePrep[id, "K"] = kSet

min.RSS.EXPT.aw <- function(data, par) {
  newData = data

  newData$ys   = getEXPT(par[1], kSet, 10^par[2], newData$x)
  newData$err  = (newData$ys - newData$y)
  newData$err  = newData$err * 1/(newData$ys^2)
  newData$err  = newData$err^2

  sum(newData$err)
}

fit.HS   <- optim(par    = c(max(currentData$y), -3),
                  fn     = min.RSS.EXPL,
                  method = "BFGS",
                  lower  = c(0.01, -5),
                  upper  = c(max(currentData$y) * 1.25, 0),
                  data   = currentData)

EXPT = nls(y ~ Q0 * 10^(kSet*(exp(-(10^a) * 100 * x)-1)),
           data = currentData,
           start = c(a  = -3,
                     Q0 = c(max(currentData$y))),
           lower = c(a  = -6,
                     Q0 = 0.01),
           upper = c(a =  0,
                     Q0 = c(max(currentData$y) * 1.5)),
           algorithm = "port",
           control = list(
             maxiter = 1000,
             warnOnly = TRUE
           ))

newData = currentData
newAsymptote = 10^(log10(coef(EXPT)["Q0"]) - kSet)
newData[newData$y == 0, "y"] <- newAsymptote

EXPT2 = nls(y ~ Q0 * 10^(kSet*(exp(-(10^a) * 100 * x)-1)),
           data = currentData,
           start = c(a  = -3,
                     Q0 = c(max(currentData$y))),
           lower = c(a  = -6,
                     Q0 = 0.01),
           upper = c(a =  0,
                     Q0 = c(max(currentData$y) * 1.5)),
           weights = newData$y^2,
           algorithm = "port",
           control = list(
             maxiter = 1000,
             warnOnly = TRUE
           ))

fit.Koffw =  optim(par    = c(max(currentData$y), -3),
                   fn     = min.RSS.EXPT.aw,
                   method = "L-BFGS-B",
                   lower  = c(0.01, -6),
                   upper  = c(max(currentData$y) * 1.5, 0),
                   data   = currentData)

fit.Koffw$par
fit.HS$par
EXPT2









for (id in 1:nParticipants) {

  currentData = dataSet[dataSet$id == id, ]

  dataFramePrep[id, "K"] = kSet

  fit.Koff <- optim(par  = c(max(currentData$y), -3),
                    fn   = min.RSS.EXPT,
                    method = "L-BFGS-B",
                    lower  = c(0.01, -5),
                    upper  = c(max(currentData$y) * 1.25, 0),
                    data = currentData)

  dataFramePrep[id, c("q0.Koff", "a.Koff")] = fit.Koff$par

  fit.Koffa <- optim(par    = c(max(currentData$y), -3),
                     fn     = min.RSS.EXPT.aw,
                     method = "L-BFGS-B",
                     lower  = c(0.01, -6),
                     upper  = c(max(currentData$y) * 1.5, 0),
                     data   = currentData)

  # fit.Koffa = nls(y ~ Q0 * 10^(kSet*(exp(-a * Q0 * x)-1)),
  #          data = currentData,
  #          start = c(a  = 0.0001,
  #                    Q0 = 100),
  #          lower = c(a = -Inf,
  #                    Q0 = 1),
  #          upper = c(a = Inf,
  #                    Q0 = 125),
  #          algorithm = "port",
  #          weights = predict(fit.Koff),
  #          control = list(
  #            maxiter = 1000,
  #            warnOnly = TRUE
  #          ))
  #
  # dataFramePrep[id, c("a.Koffw", "q0.Koffw")] = c(coef(fit.Koffa)["a"],
  #                                                 coef(fit.Koffa)["Q0"])

  fit.HS   <- optim(par    = c(fit.Koff$par[1], fit.Koff$par[2]),
                    fn     = min.RSS.EXPL,
                    method = "L-BFGS-B",
                    lower  = c(0.01, -5),
                    upper  = c(max(currentData$y) * 1.25, 0),
                    data   = currentData)

  dataFramePrep[id, c("q0.HS", "a.HS")] = fit.HS$par

  fit.HWa <- optim(par    = c(fit.HS$par[1], fit.HS$par[2]),
                   fn     = min.RSS.EXPL.aw,
                   method = "L-BFGS-B",
                   lower  = c(0.01, -5),
                   upper  = c(max(currentData$y) * 1.25, 0),
                   data   = currentData)

  dataFramePrep[id, c("q0.HSw", "a.HSw")] = fit.HWa$par
}
