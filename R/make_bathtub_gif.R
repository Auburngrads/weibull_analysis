# make_bathtub_gif

tw <- function(t, shape, scale){

  .sum1 = (t/scale[1])**shape[1] + (t/scale[2])**shape[2] + (t/scale[3])**shape[3]
  .sum2 = (shape[1]/t) * (t/scale[1])**shape[1] + (shape[2]/t) * (t/scale[2])**shape[2] + (shape[3]/t) * (t/scale[3])**shape[3]

  F_tw = 1 - exp(-1 * .sum1)
  f_tw = (1 - F_tw) * .sum2

  return(list(F_tw = F_tw, f_tw = f_tw))

}

library(ggplot2)
library(rprojroot)
library(magick)
library(glue)

t = seq(0.1,290,0.1)
root = find_root(is_rstudio_project)
gif_dir = file.path(root,"reliability_course","images","gifs","bathtub")

s1 = seq(45   , 1.5  , length.out = 70)
s2 = seq(1.538, 0.338, length.out = 70)

df_lim1 <- data.frame(s1 = s1, s2 = 1.538)
df_lim2 <- data.frame(s1 = 1.5, s2 = s2)

df_lim = rbind(df_lim1, df_lim2)

df_lim$subtitle <- rep(c("Wearout failures impact the predictive removal threshold...",
                         "...But not nearly as much as premature failures"),
                       each = 70)
df_lim$alpha <- c(rep(1,35), seq(1,0,length.out = 30),rep(0,5), rep(1,70))


for(i in 1:nrow(df_lim)) {

y = tw(t = t,
     shape = c( df_lim$s1[i],6.591,df_lim$s2[i]),
     scale = c(291.0,356.0,352.0))

df = data.frame(.t = t, .f = y$f_tw, .F = y$F_tw)

ggplot(df, aes(x = t, y = .f)) +
  geom_line(lwd = 2, col = "red") +
  ylim(0,0.03) +
  ylab("f(t)") +
  geom_vline(xintercept = t[which.min(abs(y$F_tw - 0.2))],lwd = 1.2)+
  annotate("text",label = glue::glue("eTRT: 20% PoF = {t[which.min(abs(df$.F - 0.2))]}"),
           x = t[which.min(abs(df$.F - 0.2))] + 15, y = 0.0225,
           size = 20/.pt, hjust = 0) +
  theme_bw(base_size = 24) +
  labs(subtitle = df_lim$subtitle[i]) +
  theme(plot.subtitle = element_text(size = 20, face = "bold", hjust = 1, color = scales::alpha("black",df_lim$alpha[i])))

ggsave(filename = file.path(gif_dir,paste0("image_",sprintf("%03.0f", i),".png")),
       width = 14,
       height=6,
       dpi = 150)

}

list.files(path = gif_dir, pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("bathtub.gif") # write to current dir

