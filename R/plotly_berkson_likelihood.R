library(SMRD)
library(plotly)

b10220.ld <- frame.to.ld(berkson10220,
                         response.column = c(1,2),
                         censor.column = 3,
                         case.weight.column = 4,
                         time.units = "Kilometers")

.out2 = simple.contour(b10220.ld,
                       distribution = "weibull",
                       show.confidence = !T,
                       static = T,
                       size = 500)


p = plot_ly(z = .out2$z,
            type = "surface",
            x = .out2$x,
            y = .out2$y)

htmlwidgets::saveWidget(p,paste0("p.html"))
