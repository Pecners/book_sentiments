library(av)
library(magick)
library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)
source("R/harry_potter_2/cum_moments.R")

tmp_dir <- tempdir()

t_df <- t_df |> 
  mutate(cm = cummax(cumscore) + 50)

# colors
g <- "#fab348"
r <- "#5c1510"

fo <- "RODE Noto Sans Hindi B"

walk(1:nrow(t_df), function(i) {
  cum_df <- t_df[(1:i),]
  
  pt <- t_df[i,]
  
  if (pt$cm > 200) {
    max_y <- pt$cm
  } else {
    max_y <- 300
  }
  
  p <- cum_df |> 
    ggplot(aes(ind, cumscore)) +
    geom_hline(yintercept = 0, linetype = 3, color = g) +
    geom_line(color = "white") +
    annotate(geom = "point", shape = 21, fill = g,
             size = 3, x = pt$ind, y = pt$cumscore) +
    geom_segment(aes(x = 0, xend = 0,
                     y = -100, yend = 100),
                 arrow = arrow(ends = "both", length = unit(3, "mm"), type = "closed"),
                 color = g) +
    annotate(geom = "text", label = "Happier", x = 0, y = 100,
             angle = 90, hjust = -.1, color = g, size = 7,
             family = fo, fontface = "bold") +
    annotate(geom = "text", label = "Sadder", x = 0, y = -100, size = 7,
             angle = 90, hjust = 1.1, color = g, family = fo, fontface = "bold") +
    scale_y_continuous(breaks = c(min(these_pts$cumscore), 0, max(these_pts$cumscore)),
                       labels = c("Sadder", "Neutral", "Happier"), limits = c(-250, max_y)) +
    theme_void()
  
  # p
  
  f_ind <- str_pad(pt$i, width = 5, pad = "0")
  ggsave(p, filename = glue("{tmp_dir}/{f_ind}.png"),
         w = 1650, h = 2000, units = "px", bg = r)
})

ff <- list.files(tmp_dir)
reals <- which(str_detect(ff, "png$")) |> 
  order()
ff_vec <- glue("{tmp_dir}/{ff[reals]}")


walk(1:nrow(these_pts), function(i) {
  this <- these_pts[i,]
  f_ind <- str_pad(this$i, width = 5, pad = "0")
  that <- which(str_detect(ff_vec, this$file))
  img <- image_read(ff_vec[that])

  t <- img |> 
    image_annotate(text = this$occassion |> 
                     str_wrap(30),
                   size = 90, gravity = "center",
                   location = "+0-250",
                   boxcolor = alpha("black", .9),
                   color = "white",
                   font = fo) 
  
  if (i == 1) {
    t <- t |> 
      image_annotate(text = "Emotional Arc\nof the Story",
                     size = 90, gravity = "center",
                     location = "+0+250",
                     boxcolor = alpha("black", .9),
                     color = "white",
                     font = fo) 
  }
  
  t|> 
    image_write(glue("{tmp_dir}/{f_ind}_1.png"))
})

ff <- list.files(tmp_dir)
reals <- which(str_detect(ff, "png$")) |> 
  order()
ff_vec <- glue("{tmp_dir}/{ff[reals]}")
stops <- which(str_detect(ff_vec, "_1")) |> 
  rep(70)

ff_v <- c(ff_vec, ff_vec[stops]) 

av_encode_video(input = ff_v[order(ff_v)], output = "videos/hp2.mp4")
