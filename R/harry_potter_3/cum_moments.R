source("R/harry_potter_3/cumulative_sentiment.R")

pts <- tribble(
  ~i, ~occassion,
  min(t_df$i), "Harry Potter and the Prisoner of Azkaban",
  7860, "Harry blows up his aunt, runs away.",
  16261, "Harry arrives safely in Diagon Alley.",
  22494, "Harry encounters his first dementor, faints.",
  29269, "Harry successfully rides Buckbeak",
  42548, "TURNING POINT: Harry's Nimbus 2000 is destroyed.",
  56640, "Harry gets a Firebolt for Christmas.",
  74526, "Hermione quits divination.",
  80488, "Buckbeak loses appeal, sentenced to die.",
  87534, "Lupin shows up in the Shreiking Shack.",
  96206, "Harry and Hermione use the time turner.",
  101084, glue("LOW POINT: Harry thinks all is lost, ",
               "realizes he saw himself produce the patronus."),
  103794, "Everyone lives, Sirius and Buckbeak get away.",


  t_df[[nrow(t_df), "i"]], "THE END."
)

these_pts <- map_df(1:nrow(pts), function(x) {
  this <- pts[[x,"i"]]
  xx <- t_df |> 
    filter(i == this) |> 
    pull(cumscore)
  
  pts[x,] |> 
    mutate(cumscore = xx,
           ind = i / nrow(scored),
           file = str_pad(i, width = 5, side = "left", pad = "0"))
  
})
