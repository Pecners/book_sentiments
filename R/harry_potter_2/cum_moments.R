pts <- tribble(
  ~i, ~occassion,
  min(t_df$i), "Harry Potter and the Chamber of Secrets",
  2569, "Dobby shows up at the Dursley's, which causes trouble.",
  6421, "Fred, George, and Ron free Harry from the Dursley's, the start of a happy stretch.",
  17549, glue("Platform nine and three-quarters is blocked to Harry and Ron, ",
              "a speed bump in the upward trend of the first half of the book."),
  32957, "HIGH POINT: Nearly Headless Nick's deathday party. Shortly after it, the attacks start.",
  52859, "Christmas puts a halt to the downward trend, and things level off for a bit.",
  60349, "Harry falls into Riddle's diary, and it's pretty much all downhill from here.",
  64415, "Hermione is attacked and petrified.",
  77255, glue("LOW POINT: Harry finds Ginny and Tom Riddle in the Chamber of Secrets. ",
              "Slight upswing as Tom brags until Harry realizes he is Lord Voldemort."),
  79609, "CLIMAX: Harry battles the basilisk.",
  # 81321, "After killing the basilisk, Harry makes it back to Ron.",
  82391, "Harry, Ron, and Ginny make it out of the Chamber.",
  83675, "Harry finds out he's a true Gryffindor.",
  84959, "Harry frees Dobby.",

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
