source("packages.R")

files <- factor(letters[1:8], ordered = TRUE, levels = letters[1:8])
ranks <- factor(1:8, ordered = TRUE, levels = 1:8)

chessboard_nodes <- expand.grid(file = files, rank = ranks) %>%
  mutate(file_index = as.integer(file),
         rank_index = as.integer(rank),
         coordinates = paste0(file, rank))

knight_mods <- expand.grid(c(-2,-1,1,2),c(-2,-1,1,2)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) != abs(rank_mod)) %>%
  mutate(type = "knight")

rook_mods <- rbind(expand.grid(c(-7:7),c(0)), expand.grid(c(0),c(-7:7))) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) != abs(rank_mod)) %>%
  mutate(type = "rook")

bishop_mods <- expand.grid(c(-7:7),c(-7:7)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) == abs(rank_mod) & abs(file_mod) != 0) %>%
  mutate(type = "bishop")

queen_mods <- rbind(rook_mods, bishop_mods) %>%
  mutate(type = "queen")

king_mods <- expand.grid(c(-1:1),c(-1:1)) %>%
  rename(file_mod = Var1,
         rank_mod = Var2) %>%
  filter (abs(file_mod) + abs(rank_mod) != 0) %>%
  mutate(type = "king")

move_table <- merge(chessboard_nodes, rbind(knight_mods, rook_mods, bishop_mods, queen_mods, king_mods)) %>%
  mutate(target_file = file_index + file_mod,
         target_rank = rank_index + rank_mod) %>%
  filter(target_file %in% c(1:8),
         target_rank %in% c(1:8)) %>%
  rename(source_file = file_index,
         source_rank = rank_index) %>%
  mutate(source_coordinates = paste0(letters[source_file], source_rank),
         target_coordinates = paste0(letters[target_file], target_rank),
         type = factor(type))


generate_move_graph <- function(piece) {
  if (!(piece %in% c("pawn", "knight", "bishop", "rook", "queen", "king"))) {
   stop(paste0("Piece parameter character string '", piece, "' does not match recognized pieces"))
  } else {
    grouped_move_table <- move_table %>%
      filter(type == piece)
    
    links <- grouped_move_table %>%
      mutate(src = source_coordinates, 
             target = target_coordinates,
             group = type) %>%
      select(src, target, group)
    
    nodes <- data.frame(name = as.character(chessboard_nodes$coordinates))
    
    graph <- tbl_graph(nodes = nodes,
                       edges = links,
                       directed = FALSE)
    
    board <- data.frame(x = -1:7 + 0.5, y = 0 + 0.5, xend = -1:7 + 0.5, yend = 7 - 0.5)
    board <- data.frame(x = 0:8 - 0.5, y = 0 - 0.5, xend = 0:8 - 0.5, yend = 8 - 0.5)
    
    
    squares <- unique(data.frame(expand.grid(-1:8 + 0.5, -1:8 + 0.5)) %>%
      filter(Var1 < 7 & Var2 < 7) %>%
        rename(xmin = Var1,
               ymin = Var2) %>%
        mutate(xmax = xmin + 1,
               ymax = ymin + 1)) 
    
    graph_plot <- ggraph(graph, layout = "grid") + 
      geom_rect(xmin = squares$xmin[1],   xmax = squares$xmax[1],    ymin = squares$ymin[1],  ymax = squares$ymax[1],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[2],   xmax = squares$xmax[2],    ymin = squares$ymin[2],  ymax = squares$ymax[2],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[3],   xmax = squares$xmax[3],    ymin = squares$ymin[3],  ymax = squares$ymax[3],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[4],   xmax = squares$xmax[4],    ymin = squares$ymin[4],  ymax = squares$ymax[4],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[5],   xmax = squares$xmax[5],    ymin = squares$ymin[5],  ymax = squares$ymax[5],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[6],   xmax = squares$xmax[6],    ymin = squares$ymin[6],  ymax = squares$ymax[6],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[7],   xmax = squares$xmax[7],    ymin = squares$ymin[7],  ymax = squares$ymax[7],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[8],   xmax = squares$xmax[8],    ymin = squares$ymin[8],  ymax = squares$ymax[8],    fill = "gray30") +
      
      geom_rect(xmin = squares$xmin[9],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[10],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[11],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[12],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +      
      geom_rect(xmin = squares$xmin[15],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[16],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +
      
      geom_rect(xmin = squares$xmin[17],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[18],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[19],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[20],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[21],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[22],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +      
      geom_rect(xmin = squares$xmin[23],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[24],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +
      
      geom_rect(xmin = squares$xmin[25],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[26],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[27],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[28],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[29],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[30],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +      
      geom_rect(xmin = squares$xmin[31],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[32],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +
      
      geom_rect(xmin = squares$xmin[33],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[34],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[11],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[12],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +      
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +
      
      geom_rect(xmin = squares$xmin[9],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[10],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[11],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[12],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +      
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +
      
      geom_rect(xmin = squares$xmin[9],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray70") +
      geom_rect(xmin = squares$xmin[10],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[11],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[12],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +      
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray30") +
      
      geom_rect(xmin = squares$xmin[9],   xmax = squares$xmax[9],    ymin = squares$ymin[9],  ymax = squares$ymax[9],    fill = "gray30") +
      geom_rect(xmin = squares$xmin[10],  xmax = squares$xmax[10],   ymin = squares$ymin[10], ymax = squares$ymax[10],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[11],  xmax = squares$xmax[11],   ymin = squares$ymin[11], ymax = squares$ymax[11],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[12],  xmax = squares$xmax[12],   ymin = squares$ymin[12], ymax = squares$ymax[12],   fill = "gray70") +
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +      
      geom_rect(xmin = squares$xmin[13],  xmax = squares$xmax[13],   ymin = squares$ymin[13], ymax = squares$ymax[13],   fill = "gray30") +
      geom_rect(xmin = squares$xmin[14],  xmax = squares$xmax[14],   ymin = squares$ymin[14], ymax = squares$ymax[14],   fill = "gray70") +
      geom_edge_link(edge_color = "gray") +
      geom_node_point(aes(size = 2, color = "gray")) +                                         
      geom_node_text(aes(label = name), nudge_y = 0.05, nudge_x = 0.2)+ 
      scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
      scale_y_continuous(breaks = 1:8, labels = 1:8)  +
      geom_segment(data = board, aes(x, y, xend = xend, yend = yend), color = "gray50") +
      geom_segment(data = board, aes(y, x, xend = yend, yend = xend), color = "gray50") +
      theme_void() +
      coord_equal()
    
    show(graph_plot)
  }
}



generate_move_graph("king")


grouped_move_table <- move_table %>%
  filter(type == "knight")

links <- grouped_move_table %>%
  mutate(src = as.character(factor(source_coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates)), 
         target = as.character(factor(target_coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates))) %>%
  mutate(group = type) %>%
  select(src, target, group)

links <- grouped_move_table %>%
  mutate(src = source_coordinates, 
         target = target_coordinates,
         group = type) %>%
  select(src, target, group)

links_js <- grouped_move_table %>%
  mutate(src = as.integer(factor(source_coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates))-1, 
         target = as.integer(factor(target_coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates))-1) %>%
  mutate(group = as.integer(type)) %>%
  select(src, target, group)

nodes <- data.frame(name = as.character(factor(chessboard_nodes$coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates)))

nodes <- data.frame(name = as.character(chessboard_nodes$coordinates))

nodes_js <- data.frame(name = as.integer(factor(chessboard_nodes$coordinates, ordered = TRUE, levels = chessboard_nodes$coordinates))-1,
                       group = rep(1))

graph <- tbl_graph(nodes = nodes,
                   edges = links,
                   directed = FALSE)


graph_plot <- ggraph(graph, layout = "stress") +                                                                                                         
  geom_node_point(size = 2) +                                         
  geom_node_text(aes(label = name), nudge_y = 0.05, nudge_x = 0.2)+ 
  geom_edge_link() +
  theme_void()

show(graph_plot)

ColourScale <- 'd3.scaleOrdinal().range(["#000000", "#0000FF"]);'
# Render the network 

links_js_grouped <- links_js %>%
  filter(group == 3)

graph_js <- forceNetwork(Links = links_js_grouped, 
                         Nodes = nodes_js, 
                         Source = "src", 
                         Target = "target", 
                         NodeID = "name",
                         Group = "group",
                         fontSize = 20, 
                         zoom = TRUE, 
                         linkColour = "black", 
                         charge = -500,
                         opacityNoHover = 1, 
                         colourScale = ColourScale, 
                         legend = TRUE) 

show(graph_js)






### old style

row <- file

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


match(knight_move_table[,c(7,8)], knights_graph_links[,c(5,6)])

knight_move_table[,c(7,8)] == knights_graph_links[,c(5,6)]


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

links_js <- knights_graph_links %>%
  mutate(src = as.integer(factor(source_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1, 
         target = as.integer(factor(target_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1) %>%
  mutate(group = rep(1)) %>%
  select(src, target, group)

links <- knights_graph_links %>%
  mutate(src = as.character(factor(source_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates)), 
         target = as.character(factor(target_coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))) %>%
  mutate(group = rep(1)) %>%
  select(src, target, group)

nodes_js <- data.frame(name = as.integer(factor(chess_board_nodes$coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates))-1,
                    group = rep(1))

nodes <- data.frame(name = as.character(factor(chess_board_nodes$coordinates, ordered = TRUE, levels = chess_board_nodes$coordinates)),
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
social_net_d3 <- forceNetwork(Links = links_js, Nodes = nodes_js, 
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
