library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)
library(scales)
library(ggiraph)

orig <- epub("data/HP-chamber-of-secrets.epub")
hed <- read_csv("data/Hedonometer.csv", name_repair = janitor::make_clean_names)
h <- hed |> 
  mutate(happiness_score = case_when(happiness_score < 7 & happiness_score > 3 ~ NA,
                                     TRUE ~ happiness_score))


txt <- orig$data[[1]] %>%
  filter(str_detect(text, "^\\s*CHAPTER")) |> 
  mutate(id = str_remove_all(section, "[:alpha:]") |> 
           as.numeric()) |> 
  arrange(id) |> 
  filter(section != "text30")

scored <- txt %>%
  unnest_tokens(word, text) %>%
  # left_join(sentiments) %>%
  # left_join(get_sentiments("afinn")) %>%
  left_join(hed) |> 
  mutate(score = happiness_score,
         ind = row_number(), 
         x_score = score - mean(score, na.rm = TRUE),
         cumscore = cumsum(replace_na(x_score, 0)))

l <- ((nrow(scored)) / 400) |> 
  round()

these <- seq(from = 1, to = nrow(scored), by = l)

t_df <- map_df(these, function(i) {
  if (i > 25) {
    s <- scored$word[(i-25):(i+25)] |> 
      paste0(collapse = " ") 
    
    scored[i,] |> 
      mutate(i = i,
             ind = i / nrow(scored),
             tt = glue("Ind {label_comma()(i)}: {s}") |> 
               str_wrap(30))
  }
})

p <- t_df |> 
  ggplot(aes(ind, cumscore)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_line() +
  # geom_point(data = these_pts, shape = 21, size = 3,
  #            aes(x = ind, y = cumscore),
  #            fill = "red") +
  geom_segment(aes(x = 0, xend = 0,
                   y = -100, yend = 100),
               arrow = arrow(ends = "both", length = unit(3, "mm"), type = "closed")) +
  annotate(geom = "text", label = "Happier", x = 0, y = 100,
           angle = 90, hjust = -.2) +
  annotate(geom = "text", label = "Sadder", x = 0, y = -100,
           angle = 90, hjust = 1.2) +
  geom_point_interactive(aes(tooltip = tt, data_id = ind), 
                         color = "transparent") +
  # scale_y_continuous(breaks = c(min(these_pts$cumscore), 0, max(these_pts$cumscore)),
  #                    labels = c("Sadder", "Neutral", "Happier")) +
  theme_void()

p

girafe(
  ggobj = p, options = list(opts_hover_inv(css = "opacity:.9"))
)
