library(ggiraph)
library(scales)
library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)

orig <- epub("data/harry-potter-book-1.epub")
hed <- read_csv("data/Hedonometer.csv", name_repair = janitor::make_clean_names)
h <- hed |> 
  mutate(happiness_score = case_when(happiness_score < 7 & happiness_score > 3 ~ NA,
                                     TRUE ~ happiness_score))


txt <- orig$data[[1]] %>%
  filter(str_detect(text, "^CHAPTER")) |> 
  mutate(id = str_remove_all(section, "[:alpha:]") |> 
           as.numeric()) |> 
  arrange(id)

scored <- txt %>%
  unnest_tokens(word, text) %>%
  # left_join() %>%
  # left_join(get_sentiments("afinn")) %>%
  left_join(hed) |> 
  mutate(score = happiness_score,
         ind = row_number(), 
         cumscore = cumsum(replace_na(score, 0)))

w <- 5000
l <- ((nrow(scored) - (w + 1)) / 400) |> 
  round()

starts <- seq(from = w, to = nrow(scored) - w, by = l)


x <- map_df(starts, function(i) {

  w <- scored$score[(i-w):(i+w)]
  s <- scored$word[(i-25):(i+25)] |> 
    paste0(collapse = " ") 
  tibble(
    i = i,
    ind = i / nrow(scored),
    mscore = mean(w, na.rm = TRUE),
    tt = glue("Ind {label_comma()(i)}: {s}") |> 
      str_wrap(30)
  )
})


p <- x |> 
  ggplot(aes(ind, 
             mscore, 
             data_id = i)) +
  geom_line() +
  geom_point_interactive(aes(tooltip = tt, data_id = i), 
    color = "transparent") +
  theme_minimal()

girafe(
  ggobj = p, options = list(opts_hover_inv(css = "opacity:.9"))
)

