pts <- tribble(
  ~i, ~occassion,
  min(t_df$i), "Harry Potter and the Sorcerer's Stone",
  6241, "Harry gets to go to the zoo on Dudley's birthday.",
  16186, "Hagrid tells Harry he's a wizard.",
  35101, "Harry's first Potions class.",
  55576, "Harry finds out about Nicolas Flamel.",
  59476, "Norbert the dragon is born.",
  68056, "Harry decides to steal the stone himself.",
  71761, "Harry confronts Quirrel/Voldemort.",
  75076, "Harry wakes up in the hospital wing.",
  77221, "Gryffindor wins the House Cup.",
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
