
preFit <- data.frame(
  P = c(0.1, 1,  2,  5,  10, 50, 100, 500, 1000, 5000),
  Q = c(99.54077086,
        95.52117752,
        91.28456421,
        79.88382452,
        64.51709973,
        16.3328254,
        5.442004322,
        0,
        0,
        0)
)

newAsymptote = 10^(log10(max(preFit$Q)) - (log10(max(preFit$Q)) - log10(min(preFit$Q[preFit$Q>0])) + 3))

kSet = log10(max(preFit$Q)) - log10(min(preFit$Q[preFit$Q>0])) + 3

preFit[preFit$Q == 0, "Q"] = newAsymptote



EXPT = nls(Q ~ Q0 * 10^(kSet*(exp(-a * 100 * P)-1)),
           data = preFit,
           start = c(a  = 0.0001,
                     Q0 = 100),
           lower = c(a = -Inf,
                     Q0 = 1),
           upper = c(a = Inf,
                     Q0 = 100),
           algorithm = "port",
           control = list(
             maxiter = 1000,
             warnOnly = TRUE
           ))

preFit$exptPred = predict(EXPT)

EXPL = nls(log(Q)/log(10) ~ log(Q0)/log(10) + kSet*(exp(-a * 100 * P)-1),
           data = preFit,
           start = c(a  = 0.0001,
                     Q0 = 100),
           lower = c(a = -Inf,
                     Q0 = 1),
           upper = c(a = Inf,
                     Q0 = 100),
           algorithm = "port",
           control = list(
             maxiter = 1000,
             warnOnly = TRUE
           ))

prePreds = predict(EXPL)

print(prePreds)

EXPL2 = nls(log(Q)/log(10) ~ log(Q0)/log(10) + kSet*(exp(-a * 100 * P)-1),
            data = preFit,
            start = c(a  = 0.0001,
                      Q0 = 100),
            lower = c(a = -Inf,
                      Q0 = 1),
            upper = c(a = Inf,
                      Q0 = 100),
            weights = (10^prePreds)^2,
            algorithm = "port",
            control = list(
              maxiter = 1000,
              warnOnly = TRUE
            ))

postPreds = predict(EXPL2)

#preFit$explPred = 10^prePreds

preFit$explPred = 10^postPreds

preFit = preFit %>%
  rename(`Exponential`   = explPred,
         `Exponentiated` = exptPred)

kable(preFit) %>%
  kable_styling(full_width = TRUE)

preFit = preFit %>%
  gather(Model, Prediction, -Q, -P)

ggplot(preFit, aes(P, Q, color = Model)) +
  geom_point() +
  geom_line(aes(P, Prediction)) +
  scale_x_log10(breaks = preFit$P,
                labels = preFit$P) +
  facet_wrap(~Model, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom")
