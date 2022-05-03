white_pawn_mods <- data.frame(file_mod = c(0, 0, -1, 1),
                              rank_mod = c(1, 2, 1, 1),
                              type = rep("white_pawn"),
                              action = c("movement", "special_movement", "attack", "attack"),
                              direction = NA)

black_pawn_mods <- data.frame(file_mod = c(0, 0, -1, 1),
                              rank_mod = c(-1, -2, -1, -1),
                              type = rep("black_pawn"),
                              action = c("movement", "special_movement", "attack", "attack"),
                              direction = NA)

knight_mods <- expand.grid(c(-2,-1,1,2),c(-2,-1,1,2)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) != abs(rank_mod)) %>%
  mutate(type = "knight",
         action = "movement",
         direction = NA)

rook_mods <- rbind(expand.grid(c(-7:7),c(0)), expand.grid(c(0),c(-7:7))) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) != abs(rank_mod)) %>%
  mutate(type = "rook",
         action = "movement",
         direction = rep(c("down","up","left","right"), each = 7))

bishop_mods <- expand.grid(c(-7:7),c(-7:7)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) == abs(rank_mod) & abs(file_mod) != 0) %>%
  mutate(type = "bishop",
         action = "movement",
         direction = c(rep(c("down_left","down_right"), times = 7), rep(c("up_left", "up_right"), times = 7)))

queen_mods <- rbind(rook_mods, bishop_mods) %>%
  mutate(type = "queen",
         action = "movement")

king_mods <- expand.grid(c(-1:1),c(-1:1)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) + abs(rank_mod) != 0) %>%
  mutate(type = "king",
         action = "movement",
         direction = c("down_left", "down", "down_right", "left", "right", "up_left", "up", "up_right"))

move_table <- merge(chessboard_nodes, rbind(white_pawn_mods, black_pawn_mods, knight_mods, rook_mods, bishop_mods, queen_mods, king_mods)) %>%
  mutate(target_file = file_index + file_mod,
         target_rank = rank_index + rank_mod) %>%
  filter(target_file %in% c(1:8),
         target_rank %in% c(1:8)) %>%
  rename(source_file = file_index,
         source_rank = rank_index) %>%
  mutate(source_coordinates = paste0(letters[source_file], source_rank),
         target_coordinates = paste0(letters[target_file], target_rank),
         type = factor(type)) %>%
  filter(!(type == "white_pawn" & action == "special_movement" & rank != 2)) %>%
  filter(!(type == "black_pawn" & action == "special_movement" & rank != 7)) %>%
  rowwise() %>%
  mutate(abs_file_mod = abs(file_mod),
         abs_rank_mod = abs(rank_mod)) %>%
  mutate(weight = max(c_across(abs_file_mod:abs_rank_mod)))
