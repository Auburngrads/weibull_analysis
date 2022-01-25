library(diagram)

DiffMat <- matrix(NA, nrow = 2, ncol = 2)

AA <- as.data.frame(DiffMat)
AA[[2,1]] <- expression("F[0:1]")


name <- c(expression(over("State 0","Working")),
          expression(over("State 1","Failed")))

par(family='serif', mar = c(0,0,0,0))

diagram::plotmat(A = AA,
                 pos = 2,
                 curve = .575,
                 name = name,
                 lwd = 2,
                 arr.len = 0.6,
                 arr.width = 0.25,
                 my = .15,
                 box.size = 0.08,
                 arr.type = 'triangle',
                 dtext = -1,
                 relsize = .99,
                 box.cex = 1.5,
                 cex = 1.25)
