create_graph <- function(piece) {
grouped_move_table <- move_table %>%
  filter(type == piece)

links <- grouped_move_table %>%
  mutate(src = source_coordinates, 
         target = target_coordinates,
         group = type,
         weight = travel) %>%
  select(src, target, group, weight)

nodes <- data.frame(name = as.character(chessboard_nodes$coordinates))

graph <- tbl_graph(nodes = nodes,
                   edges = links,
                   directed = FALSE)

return(graph)
}