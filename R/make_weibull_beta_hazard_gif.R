#gganimate pdf/survival plot
library(ggplot2)
library(data.table)
library(patchwork)
library(rprojroot)
library(magick)
library(purrr)

.betas <- c(seq(0.01, 0.09, 0.01),
            seq(0.10, 2.95, 0.05),
            seq(3.00, 10.0, 0.10) )

.x = rep(seq(0, 300, length = 10001), length(.betas))
.b = rep(.betas, each = 10001)
.t = rep(75, 10001 * length(.betas))
.p = rep(seq(0,1, length = 10001), length(.betas))

#.p = pweibull(.x, shape = .parms$shape, scale = .parms$scale)
.d =   dweibull(.x, shape = .b, scale = .t)
.s = 1-pweibull(.x, shape = .b, scale = .t)
.h = (.b / .t) * (.x / .t) ^ (.b - 1)
.q = .t * log(1 / (1 - .p)) ^ (1 / .b)
.df = data.table(obs = .x,
                 vals = .d,
                 shape = .b,
                 scale = .t,
                 surv = .s,
                 haz = .h,
                 quan = .q,
                 prob = .p)

theme_set(theme_bw(base_size = 16))

root = find_root(is_rstudio_project)
gif_dir = file.path(root,"images","gifs","weibull_betas_hazard")

for(i in seq_along(.betas)) {

g1 = ggplot(.df[shape == .betas[i],], aes(x = obs, y = vals)) +
  geom_line(size = 2) +
  xlim(c(0,300)) + ylim(c(0,0.06)) +
  xlab("Time") + ylab("f(t)") +
  geom_vline(xintercept = 75, col = "red", lwd = 1.5) +
  ggtitle(glue::glue("f(t|\u03B2 = {.betas[i]}, \u03B8={75})"))

g2 = ggplot(.df[shape == .betas[i],], aes(x = obs, y = surv)) +
  geom_line(size = 2) +
  xlim(c(0,300)) + ylim(c(0,1)) +
  xlab("Time") + ylab("S(t)") +
  geom_vline(xintercept = 75, col = "red", lwd = 1.5) +
  ggtitle(glue::glue("S(t|\u03B2 = {.betas[i]}, \u03B8={75})"))

g3 = ggplot(.df[shape == .betas[i],], aes(x = obs, y = haz)) +
  geom_line(size = 2) +
  xlim(c(0,300)) + ylim(c(0,0.06)) +
  xlab("Time") + ylab("h(t)") +
  geom_vline(xintercept = 75, col = "red", lwd = 1.5) +
  ggtitle(glue::glue("h(t|\u03B2 = {.betas[i]}, \u03B8={75})"))

g4 = ggplot(.df[shape == .betas[i],], aes(x = prob, y = quan)) +
  geom_line(size = 2) +
  xlim(c(0,1)) + ylim(c(0,300)) +
  xlab("Time") + ylab("t(p)") +
  geom_hline(yintercept = 75, col = "red", lwd = 1.5) +
  ggtitle(glue::glue("t(p|\u03B2 = {.betas[i]}, \u03B8={75})"))

(g1|g2)/(g3|g4)

ggsave(filename = file.path(gif_dir,paste0("image_",sprintf("%03.0f", i),".png")),
       width = 14,
       height=6,
       dpi = 150)

}

list.files(path = gif_dir, pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("weib_beta_haz.gif") # write to current dir
