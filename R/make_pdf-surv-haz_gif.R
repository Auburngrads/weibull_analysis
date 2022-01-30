#gganimate pdf/survival plot
library(ggplot2)
library(data.table)
library(patchwork)
library(rprojroot)
library(magick)
library(purrr)


.parms = list(shape = 1, scale = 75)
.x = qweibull(seq(0, 1, 0.0001), shape = .parms$shape, scale = .parms$scale)
.p = pweibull(.x, shape = .parms$shape, scale = .parms$scale)
.d = dweibull(.x, shape = .parms$shape, scale = .parms$scale)
.h = (.parms$shape / .parms$scale) * (.x / .parms$scale) ** (.parms$shape - 1)
.df = data.table(obs = rep(.x, 3),
                 vals = c(.p,.d,.h),
                 func = rep(c("CDF", "PDF", "HAZ"), each = length(.p)))

.sd = .df[func == "PDF",]
.sp = .df[func == "CDF",]
.sh = .df[func == "HAZ",]

theme_set(theme_bw(base_size = 16))

.limit = seq(0,300,3)
root = find_root(is_rstudio_project)
gif_dir = file.path(root,"images","gifs","surv_pdf_haz")

for(limit in .limit) {

g1 = ggplot(.sd[obs <= limit,], aes(x = obs, y = vals)) +
  geom_line(size = 2) +
  geom_vline(xintercept = limit, col = "red", size = 1.5) +
  #geom_polygon(data = rbind(.sd[obs > limit,],list(obs = limit, vals = 0, func = "PDF")), aes(obs, vals), alpha = 0.3, fill = "red") +
  xlim(c(0,300)) + ylim(c(0,0.015)) +
  xlab("Time") + ylab("f(t)") +
  ggtitle(glue::glue("f({limit}|\u03B2 = {.parms$shape}, \u03B8={.parms$scale}) = {.sd[obs <= limit,max(vals)]}"))

g2 = ggplot(.sp[obs <= limit,], aes(x = obs, y = 1-vals)) +
  geom_line(size = 2) +
  geom_vline(xintercept = limit, col = "red", size = 1.5) +
  xlim(c(0,300)) + ylim(c(0,1)) +
  xlab("Time") + ylab("S(t)") +
  ggtitle(glue::glue("S({limit}|\u03B2 = {.parms$shape}, \u03B8={.parms$scale}) = {.sp[obs <= limit,min(1-vals)]}"))

g3 = ggplot(.sh[obs <= limit,], aes(x = obs, y = vals)) +
  geom_line(size = 2) +
  geom_vline(xintercept = limit, col = "red", size = 1.5) +
  xlim(c(0,300)) + ylim(c(0,0.05)) +
  xlab("Time") + ylab("h(t)") +
  ggtitle(glue::glue("h({limit}|\u03B2 = {.parms$shape}, \u03B8={.parms$scale}) = {.sh[obs <= limit,max(vals)]}"))

g1 / g2 | g3

ggsave(filename = file.path(gif_dir,paste0("image_",sprintf("%03.0f", limit),".png")),
       width = 14,
       height=6,
       dpi = 150)

}

list.files(path = gif_dir, pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("surv_pdf_haz.gif") # write to current dir
