library(tidyverse)
library(epubr)
library(tidytext)
library(ggwordcloud)
library(wordcloud2)
library(svgparser)
library(grid)
library(colorspace)
library(magick)


orig <- epub("data/harry-potter-book-1.epub")
mask <- 

txt <- orig$data[[1]] %>%
  filter(str_detect(text, "^CHAPTER")) |> 
  mutate(id = str_remove_all(section, "[:alpha:]") |> 
           as.numeric()) |> 
  arrange(id)

g <- "#fab348"
gg <- "#ffd25f"
gg1 <- lighten(gg, .5)
r <- "#5c1510"
col <- grDevices::colorRampPalette(c("white", gg, g, r), bias = 10)(nrow(scored))
colorspace::swatchplot(col)

scored <- txt %>%
  unnest_tokens(word, text)  |> 
  anti_join(stop_words) |> 
  group_by(word) |> 
  summarise(freq = n()) |> 
  arrange(desc(freq)) |> 
  mutate(freq = log(freq))

scored$col <- grDevices::colorRampPalette(c(gg1, gg, g, r), bias = 5)(nrow(scored))
scored[1:(nrow(scored)/10),] <- scored[sample(1:(nrow(scored)/10)),]
scored[(nrow(scored)/10):nrow(scored),] <- scored[sample((nrow(scored)/10):nrow(scored)),]


png(filename = "data/hp_mask.png", width = 7, height = 7, units = "in", res = 500, bg = "transparent")
grid.draw(read_svg("data/hp_mask.svg"))
dev.off()


fp <- "data/hp_mask.png"
wordcloud2(scored, figPath = fp, size = .25, fontFamily = "Julee", 
           widgetsize = c(1200, 1200), 
           backgroundColor = r, color = scored$col, maxRotation = pi/2)





wordcloud2(tidy, figPath = "2022/2022-05-17/logo.png", size = 3,
           widgetsize = c(1200,1200), ellipticity = .9, gridSize = 10,
           fontFamily = "Julee", color = c_trim, backgroundColor = "white")

img <- image_read("plots/hp1_wc.png")
img |> 
  image_crop(geometry = "1200x1200", gravity = "northwest") |> 
  image_write("plots/hp1_wc_cropped.png")




p <- scored |> 
  filter(n > 10) |> 
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud_area(mask = "data/hp_mask.png")

ggsave(filename = "plots/hp1_wordcloud.png", plot = p)

