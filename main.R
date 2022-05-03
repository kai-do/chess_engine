source("packages.R")
#source("data.R")
source("pieces.R")
source("board.R")
source("moves.R")
source("graph.R")
source("render.R")

`%!in%` <- Negate(`%in%`)

piece <- "knight"

grouped_move_table <- move_table %>%
  filter(type == piece)


starting_nodes_clean <- starting_nodes %>%
  mutate(coordinates = as.character(coordinates),
         type = name) %>%
  select(coordinates, representation, type, unicode, color)

nodes <- starting_nodes_clean

turn <- "white" 
not_turn <- "black"

joined_links <- inner_join(nodes %>% filter(color == turn) %>% mutate(type = as.character(type)), move_table %>% mutate(type = as.character(type))) 



nodes %>%
  filter(color == turn)

joined_links %>%
  filter(color == turn,
         type == "rook")


ultimate_links <- inner_join(nodes %>% mutate(coordinates = as.character(coordinates)), joined_links %>% mutate(target_coordinates = as.character(target_coordinates)), by = c("coordinates" = "target_coordinates")) %>%
  select(source_piece = type.y,
         source_color = color.y,
         source_coordinates = source_coordinates,
         source_file,
         source_rank,
         action,
         weight,
         direction,
         target_piece = type.x,
         target_color = color.x,
         target_coordinates = coordinates,
         target_file,
         target_rank) %>%
  filter(
    #target_color != turn | is.na(target_color), # remove targets that are not empty or opposite color
         !(action == "attack" & target_piece != not_turn), # remove pawn attacks for empty or same color ### must change for en passant
         !(action %in% c("movement", "special_movement") & source_piece %in% c("black_pawn", "white_pawn") & !(is.na(target_piece)))) # remove pawns ability to attack pieces in front of it

test <- ultimate_links %>%
  mutate(target_occupied = case_when(is.na(target_piece) ~ FALSE, TRUE ~ TRUE)) %>%
  filter(source_coordinates == "d1") %>%
  group_by(across(all_of(c("source_coordinates", "direction")))) %>%
  mutate(min_weight = min(weight))




  filter(any(target_color != turn)) 


  mutate(min_weight = min(weight))


test <- ultimate_links %>%
  mutate(target_occupied = case_when(is.na(target_piece) ~ FALSE, TRUE ~ TRUE)) %>%
  filter(source_coordinates == "d1") %>%
  group_by("direction") %>%
  filter(any(target_color != turn)) %>%
  summarize(min_weight = min(weight))
  

  #rowwise() %>%
  mutate(min_weight = min(weight))

ultimate_links %>%
  filter(source_coordinates == unique(ultimate_links$source_coordinates)[i])

i <- 1
max_i <- nrow(joined_links)

for (i in i:max_i) {
  unique(ultimate_links$source_coordinates)[i]
}


links <- ultimate_links %>%
  #filter(joined_links$target_coordinates %!in% source_coordinates) %>% # remove moves into self
  mutate(src = source_coordinates, 
         target = target_coordinates,
         group = source_piece) %>%
  select(src, target, group, weight)



         #!(action == "attack" & is.na(target_piece)))


starting_graph <- tbl_graph(nodes = nodes,
                            edges = links,
                            directed = FALSE)

render_game(starting_graph)

