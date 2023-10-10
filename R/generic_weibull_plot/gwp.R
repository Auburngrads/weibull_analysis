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

.fun = function(x,y,params){

  return(sum((y - (params[1] * (x - log(params[2]))))**2))

}

risk_set = 150
shape = 4.2
scale = 5.7
shape2 = 0.943
scale2 = 1754
N1 = 17
N2 = risk_set - N1

obs1 = rweibull(N1, shape = shape, scale = scale)
obs2 = rweibull(N2, shape = shape2, scale = scale2)


obs = sort(c(obs1, obs2))
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
    km_raw = 1 - (fails / df$at_risk[i])
    km = km_raw * df$kaplan_meier[i]
    mr = (ranks[i] - 0.3) / (length(obs) + 0.4)

    df = rbind(df, list(event = event,
                        n_fails = fails,
                        time = obs[i],
                        at_risk = df$at_risk[i] - fails,
                        median_ranks = mr,
                        km_raw = km_raw,
                        kaplan_meier = km))
}

N  = nrow(df)
x  = log(df$time)[-c(1,N)]
y1 = log(log(1/df$kaplan_meier))[-c(1,N)]
y2 = log(log(1/(1-df$median_ranks)))[-c(1,N)]

f1 = optim(par = c(1.2,3000),
      fn = .fun,
      x = x, y = y1)
f2 = optim(par = c(1.2,3000),
      fn = .fun,
      x = x, y = y2)
f3 <- fitdistrplus::fitdist(obs, distr = "weibull")

t1 = qweibull(0.2,sh = f3$e[1],   sc = f3$e[2])
t2 = qweibull(0.2,sh = f1$par[1], sc = f1$par[2])
tr = (t1 - t2)/ t1

z = qnorm(0.975)
t_e = log(obs)
mu = log(f3$estimate[2])
sigma = 1/f3$estimate[1]
zi = (t_e - mu) / sigma
var_mu = f3$vcov[2,2] / exp(mu)**2
var_sigma = f3$vcov[1,1] / (1/sigma)**4
cov_mu_sig = f3$vcov[1,2] / ( exp(mu) * -1*(1/sigma**2))
F_hat = SMRD::psev(t_e, location = mu, scale = sigma)

se_F = (SMRD::dsev(zi) / sigma) * sqrt(var_mu + 2* zi* cov_mu_sig + (zi**2)*var_sigma)
w = exp((z * se_F)/ (F_hat*(1-F_hat)))
F_up = F_hat / (F_hat + (1-F_hat) * w)
F_dn = F_hat / (F_hat + (1-F_hat) / w)


### Plot 1 points
png("points.png",600,600)
par(las = 1, mar = c(5.1, 4.75, 4.1, 2.1),mgp = c(3,0.1,0), tck = 0.025)
plot(x = log(df$time),
     y = log(log(1/df$kaplan_meier)),
     pch = 16,
     xlim = c(log(1), log(max(df$time))),
     xaxt = "n",
     yaxt = "n",
     ylim = log(log(1/(1-c(0.0001,.9999)))),
     xlab = "Failure Time",
     ylab = "Unreliability",
     col = "red")
dev.off()

### Plot 2 axes
dev.off()

## plot 3 MLE fit
png("mle.png",600,600)
par(las = 1, mar = c(5.1, 4.75, 4.1, 2.1),mgp = c(3,0.1,0), tck = 0.025)
plot(x = log(df$time),
     y = log(log(1/df$kaplan_meier)),
     pch = 16,
     xlim = c(log(1), log(max(df$time))),
     xaxt = "n",
     yaxt = "n",
     ylim = log(log(1/(1-c(0.0001,.9999)))),
     xlab = "Failure Time",
     ylab = "Unreliability")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
abline(v = log(xlines),
       col = scales::alpha("steelblue",0.25))
axis(side = 2,
     at = log(log(1/(1-yvals))),
     labels = yvals)
abline(h = log(log(1/(1-ylines))),
       col = scales::alpha("steelblue",0.25))
lines(x = log(obs),
      y = log(log(1/(1-pweibull(obs, sh = f3$e[1], sc = f3$e[2])))),
      col = "red",
      lwd = 2)
dev.off()

## Plot 4 confidence intervals
png("ci.png",600,600)
par(las = 1, mar = c(5.1, 4.75, 4.1, 2.1),mgp = c(3,0.1,0), tck = 0.025)
plot(x = log(df$time),
     y = log(log(1/df$kaplan_meier)),
     pch = 16,
     xlim = c(log(1), log(max(df$time))),
     xaxt = "n",
     yaxt = "n",
     ylim = log(log(1/(1-c(0.0001,.9999)))),
     xlab = "Failure Time",
     ylab = "Unreliability")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
abline(v = log(xlines),
       col = scales::alpha("steelblue",0.25))
axis(side = 2,
     at = log(log(1/(1-yvals))),
     labels = yvals)
abline(h = log(log(1/(1-ylines))),
       col = scales::alpha("steelblue",0.25))
lines(x = log(obs),
      y = log(log(1/(1-pweibull(obs, sh = f3$e[1], sc = f3$e[2])))),
      #col = "red",
      lwd = 2)
lines(x = t_e, y = log(log(1/(1-F_up))), col = "red", lwd = 2)
lines(x = t_e, y = log(log(1/(1-F_dn))), col = "red", lwd = 2)
dev.off()
## OLS MR fit
# lines(x = log(obs),
#       y = log(log(1/(1-pweibull(obs, sh = f1$par[1], sc = f1$par[2])))),
#       col = "red",
#       lwd = 2)
# segments(x0 = rep(0,2),
#          x1 = c(log(qweibull(0.2,sh = f3$e[1], sc = f3$e[2])),
#                 log(qweibull(0.2,sh = f1$par[1], sc = f1$par[2]))),
#          y0 = rep(log(log(1/(1-0.2))),2),
#          y1 = rep(log(log(1/(1-0.2))),2))
# segments(x0 = c(log(qweibull(0.2,sh = f3$e[1], sc = f3$e[2])),
#                 log(qweibull(0.2,sh = f1$par[1], sc = f1$par[2]))),
#          x1 = c(log(qweibull(0.2,sh = f3$e[1], sc = f3$e[2])),
#                 log(qweibull(0.2,sh = f1$par[1], sc = f1$par[2]))),
#          y0 = rep(log(log(1/(1-0.00001))),2),
#          y1 = rep(log(log(1/(1-0.2))),2),
#          col = c("blue","red"))


## plot 5 both axes 1
old_par = par()

png("both_axes_1.png",600,600)
par(las = 1,
    mar = c(4.1, 4.75, 4.1, 6),
    mgp = c(3,0.1,0),
    tck = 0.025)
plot(NA,
     xlim = c(log(1), log(max(df$time))),
     xaxt = "n",
     yaxt = "n",
     ylim = log(log(1/(1-c(0.0001,.9999)))),
     xlab = "",
     ylab = "")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
mtext(side = 1, text = "Failure Time", line = 2)
abline(v = log(xlines),
       col = scales::alpha("steelblue",0.25))
abline(h = log(log(1/(1-ylines))),
       col = scales::alpha("steelblue",0.25))
axis(side = 2,
     at = log(log(1/(1-yvals))),
     labels = yvals,
     col.axis = "red",
     font = 4)
mtext(side = 2,
      text = "Unreliability (transformed values)",
      line = 3,
      las = 0,
      col = "red",
     font = 4)
axis(side = 4,
     at = log(log(1/(1-yvals))),
     labels = round(log(log(1/(1-yvals))),digits = 3),
     hadj = -0.35)
mtext(side = 4,
      text = "Unreliability (raw values)",
      line = 3.5,
      las = 0)
dev.off()

par(old_par)


## plot 5 both axes 1

png("both_axes_2.png",600,600)
par(las = 1,
    mar = c(4.1, 4.75, 4.1, 6),
    mgp = c(3,0.1,0),
    tck = 0.025)
plot(NA,
     xlim = c(log(1), log(max(df$time))),
     xaxt = "n",
     yaxt = "n",
     ylim = log(log(1/(1-c(0.0001,.9999)))),
     xlab = "",
     ylab = "")
axis(side = 1,
     at = log(xvals),
     labels = xvals)
mtext(side = 1, text = "Failure Time", line = 2)
abline(v = log(xlines),
       col = scales::alpha("steelblue",0.25))
abline(h = log(log(1/(1-ylines))),
       col = scales::alpha("steelblue",0.25))
axis(side = 2,
     at = log(log(1/(1-yvals))),
     labels = yvals)
mtext(side = 2,
      text = "Unreliability (transformed values)",
      line = 3,
      las = 0)
axis(side = 4,
     at = log(log(1/(1-yvals))),
     labels = round(log(log(1/(1-yvals))),digits = 3),
     hadj = -0.35,
      col.axis = "red",
     font = 4)
mtext(side = 4,
      text = "Unreliability (raw values)",
      line = 3.5,
      las = 0,
      col = "red",
     font = 4)
abline(h = log(mean(obs)), col = "black",lwd = 2)
#dev.off()
