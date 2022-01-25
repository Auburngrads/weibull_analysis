risk_set = 150
shape = 1.27
scale = 3613

obs = sort(rweibull(risk_set, shape = shape, scale = scale))
ranks = rank(obs)

df = data.frame(event = 0,
                n_fails = 0,
                time = 0,
                at_risk = risk_set,
                median_ranks = 0,
                km_raw = 1,
                kaplan_meier = 1)

for(i in 1:length(obs)) {

    event = df$event[i] + 1
    fails = length(which(obs == obs[i]))
    rs = df$at_risk[i] - fails
    km_raw = 1 - (fails / rs)
    km = km_raw * df$kaplan_meier[i]
    mr = (ranks[i] - 0.3) / (length(obs) + 0.4)

    df = rbind(df, list(event = event,
                        n_fails = fails,
                        time = obs[i],
                        at_risk = rs,
                        median_ranks = mr,
                        km_raw = km_raw,
                        kaplan_meier = km))

}

par(mfrow = c(1,2), las = 1)
plot(df$time, 1-df$kaplan_meier, pch = 16,type = "S")
curve(pweibull(x, shape = shape, scale = scale),
      add = T,
      col = "red",
      lwd = 2)

plot(df$time, df$median_ranks, pch = 4)
curve(pweibull(x, shape = shape, scale = scale),
      add = T,
      col = "red",
      lwd = 2)

par(mfrow = c(1,1))


## Linear plots
xlines = c(seq(1,9,1),
           seq(10,90,10),
           seq(100,900,100),
           seq(1000,9000,1000),
           seq(10000,90000,10000))

xvals = c(seq(1,5,1),
          seq(10,50,10),
          seq(100,500,100),
          seq(1000,5000,1000),
          seq(10000,50000,10000))

ylines = c(seq(.0001,.0009,.0001),
         seq(.001,.009,.001),
         seq(.01,.09,.01),
         seq(.1,.9,.1),
         .95,.99,.995,.999,.9995,.9999,.632)

yvals = c(.0001,.0005,.001,.005,.01,.05,.1,
          .5,.9,.95,.99,.995,.999,.9995,.9999,.632)

old_mar = par()$mar
par(mfrow = c(1,2), las = 1, mar = c(5.1, 4.75, 4.1, 2.1))
plot(x = log(c(1,df$time)),
     y = log10(log(1/c(1,df$kaplan_meier))),
     pch = 16,
     type = "s",
     xaxt = "n",
     yaxt = "n",
     ylim = log10(log(1/(1-c(0.0001,.9999)))),
     xlab = "time",
     ylab = "Unreliability")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
abline(v = log(xlines),
       lty = 3,
       col = scales::alpha("steelblue",0.75))
axis(side = 2,
     at = log10(log(1/(1-yvals))),
     labels = yvals)
abline(h = log10(log(1/(1-ylines))),
       lty = 3,
       col = scales::alpha("steelblue",0.75))
lines(x = log(obs),
      y = log10(log(1/(1-pweibull(obs, shape = shape, scale = scale)))),
      col = "red",
      lwd = 2)

plot(x = log(df$time),
     y = log10(log(1/(1-df$median_ranks))),
     pch = 4,
     xaxt = "n",
     yaxt = "n",
     ylim = log10(log(1/(1-c(0.0001,.9999)))),
     xlab = "time",
     ylab = "Unreliability")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
abline(v = log(xlines),
       lty = 3,
       col = scales::alpha("steelblue",0.75))
axis(side = 2,
     at = log10(log(1/(1-yvals))),
     labels = yvals)
abline(h = log10(log(1/(1-ylines))),
       lty = 3,
       col = scales::alpha("steelblue",0.75))
lines(x = log(obs),
      y = log10(log(1/(1-pweibull(obs, shape = shape, scale = scale)))),
      col = "red",
      lwd = 2)

par(mfrow = c(1,1), mar = old_mar)
