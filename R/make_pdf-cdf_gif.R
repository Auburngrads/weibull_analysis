#gganimate pdf/cdf plot
library(ggplot2)
library(data.table)
library(patchwork)
library(rprojroot)
library(magick)


.parms = list(shape = 1.6, scale = 75)
.x = qweibull(seq(0, 1, 0.0001), shape = .parms$shape, scale = .parms$scale)
.p = pweibull(.x, shape = .parms$shape, scale = .parms$scale)
.d = dweibull(.x, shape = .parms$shape, scale = .parms$scale)
.df = data.table(obs = rep(.x, 2),
                 vals = c(.p,.d),
                 func = rep(c("CDF", "PDF"), each = length(.p)))

.sd = .df[func == "PDF",]
.sp = .df[func == "CDF",]

theme_set(theme_bw(base_size = 16))

.limit = seq(3,300,3)
root = find_root(is_rstudio_project)
gif_dir = file.path(root,"reliability_course","images","gifs","pdf_cdf")

for(limit in .limit) {

g1 = ggplot(.sd, aes(x = obs, y = vals)) +
  geom_line(size = 2) +
  geom_vline(xintercept = limit, col = "red", size = 1.5) +
  geom_polygon(data = rbind(.sd[obs <= limit,],list(obs = limit, vals = 0, func = "PDF")), aes(obs, vals), alpha = 0.3, fill = "red") +
  xlab("Time") + ylab("f(t)") +
  ggtitle(glue::glue("% shaded area under f(t) = {.sp[obs <= limit,max(vals)]}"))

g2 = ggplot(.sp[obs <= limit,], aes(x = obs, y = vals)) +
  geom_line(size = 2) +
  geom_vline(xintercept = limit, col = "red", size = 1.5) +
  xlim(c(0,300)) + ylim(c(0,1)) +
  xlab("Time") + ylab("F(t)") +
  ggtitle(glue::glue("F({limit}) = {.sp[obs <= limit,max(vals)]}"))

g1 | g2

ggsave(filename = file.path(gif_dir,paste0("image_",sprintf("%03.0f", limit),".png")),
       width = 14,
       height=6,
       dpi = 150)

}

list.files(path = gif_dir, pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("cdf_pdf.gif") # write to current dir
