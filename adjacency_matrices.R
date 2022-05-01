source("packages.R")



row <- c("a", "b", "c", "d", "e", "f", "g", "h")
row_factored <- factor(row, ordered = TRUE, levels = row)

column <- c("1", "2", "3", "4", "5", "6", "7", "8")
column_factored <- factor(column, ordered = TRUE, levels = column)



chess_board_nodes <- expand.grid(row, column)
colnames(chess_board_nodes) <- c("row", "column")

chess_board_nodes <- chess_board_nodes %>%
  mutate(coordinates = paste0(row, column)) %>%
  mutate(row_index = match(row, letters)) %>%
  mutate(column_index = as.integer(column)) %>%
  select(coordinates, row_index, row, column_index, column)

source_row <- c()
source_column <- c()
target_row <- c()
target_column <- c()

max_i <- nrow(chess_board_nodes)

level <- 0

for (level in level:7) {
  if (level == 0) {
    row_mod <- 1
    column_mod <- 2
  } else if (level == 1) {
    row_mod <- -1
    column_mod <- 2
  } else if (level == 2) {
    row_mod <- 1
    column_mod <- -2
  } else if (level == 3) {
    row_mod <- -1
    column_mod <- -2
  } else if (level == 4) {
    row_mod <- 2
    column_mod <- 1
  } else if (level == 5) {
    row_mod <- -2
    column_mod <- 1
  } else if (level == 6) {
    row_mod <- 2
    column_mod <- -1
  } else if (level == 7) {
    row_mod <- -2
    column_mod <- -1
  }
  i <- 1
  for (i in 1:max_i) {
    source_row[i + (max_i * level)] <- chess_board_nodes$row_index[i]
    source_column[i + (max_i * level)] <- chess_board_nodes$column_index[i]
    
    target_row[i + (max_i * level)] <- chess_board_nodes$row_index[i] + row_mod
    target_column[i + (max_i * level)] <- chess_board_nodes$column_index[i] + column_mod
  }
}

knights_graph_links <- data.frame(cbind(source_row,
                                        source_column,
                                        target_row,
                                        target_column))
                                  
knights_graph_links <- knights_graph_links %>%
  filter(target_row %in% c(1:8)) %>%
  filter(target_column %in% c(1:8)) %>%
  mutate(source_coordinates = paste0(letters[source_row], source_column)) %>%
  mutate(target_coordinates = paste0(letters[target_row], target_column))

links <- knights_graph_links %>%
  mutate(src = as.integer(factor(source_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1, 
         target = as.integer(factor(target_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1) %>%
  mutate(group = rep(1)) %>%
  select(src, target, group)

nodes <- data.frame(name = as.integer(factor(chess_board_nodes$coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1,
                    group = rep(1))


knight_graph <- tbl_graph(nodes = nodes,
                          edges = links,
                          directed = FALSE)


graph_plot <- ggraph(knight_graph, layout = "stress") +                                                                                                         
  geom_node_point(size = 2) +                                         
  geom_node_text(aes(label = name), nudge_y = 0.05, nudge_x = 0.2)+ 
  geom_edge_link() +
  theme_void()

show(graph_plot)

ColourScale <- 'd3.scaleOrdinal().range(["#000000", "#0000FF"]);'
# Render the network 
social_net_d3 <- forceNetwork(Links = links, Nodes = nodes, 
                              Source = "src", Target = "target", 
                              NodeID = "name",
                              Group = "group",
                              fontSize = 20, zoom = TRUE, 
                              linkColour = "black", 
                              charge = -500,
                              opacityNoHover = 1, 
                              colourScale = ColourScale, 
                              legend = TRUE) 

show(social_net_d3)
