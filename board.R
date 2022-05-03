files <- factor(letters[1:8], ordered = TRUE, levels = letters[1:8])

ranks <- factor(1:8, ordered = TRUE, levels = 1:8)

board <- data.frame(x = 0:8 - 0.5, y = 0 - 0.5, xend = 0:8 - 0.5, yend = 8 - 0.5)

squares <- unique(data.frame(expand.grid(-1:8 + 0.5, -1:8 + 0.5)) %>%
                    filter(Var1 < 7 & Var2 < 7) %>%
                    rename(xmin = Var1,
                           ymin = Var2) %>%
                    mutate(xmax = xmin + 1,
                           ymax = ymin + 1,
                           sqr = rep(c(1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1),4))) 

rank_labels <- data.frame(x = c(0:7),
                          y = rep(7.75),
                          label = c(letters[1:8]))

file_labels <- data.frame(x = rep(-0.75),
                          y = c(0:7),
                          label = c(1:8))

chessboard_nodes <- expand.grid(file = files, rank = ranks) %>%
  mutate(file_index = as.integer(file),
         rank_index = as.integer(rank),
         coordinates = paste0(file, rank))

starting_position <- data.frame(representation = c(c(4, 2, 3, 5, 6, 3, 2, 4,
                                                     1, 1, 1, 1, 1, 1, 1, 1),
                                                   rep(0, 32),
                                                   c(1, 1, 1, 1, 1, 1, 1, 1,
                                                     4, 2, 3, 5, 6, 3, 2, 4)*-1))

starting_position <- data.frame(representation = c(c(5, 3, 4, 6, 7, 4, 3, 5,
                                                     1, 1, 1, 1, 1, 1, 1, 1),
                                                   rep(0, 32),
                                                   c(2, 2, 2, 2, 2, 2, 2, 2,
                                                     5, 3, 4, 6, 7, 4, 3, 5)*-1))

starting_nodes <- cbind(chessboard_nodes, left_join(starting_position, pieces_df, by = "representation"))

