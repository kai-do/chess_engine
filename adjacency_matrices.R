source("packages.R")


piece <- "rook"

generate_move_graph <- function(piece) {
  if (!(piece %in% piece_names)) {
   stop(paste0("Piece parameter character string '", piece, "' does not match recognized pieces"))
  } else {
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
    
    
    
    board <- data.frame(x = 0:8 - 0.5, y = 0 - 0.5, xend = 0:8 - 0.5, yend = 8 - 0.5)
    
    squares <- unique(data.frame(expand.grid(-1:8 + 0.5, -1:8 + 0.5)) %>%
      filter(Var1 < 7 & Var2 < 7) %>%
        rename(xmin = Var1,
               ymin = Var2) %>%
        mutate(xmax = xmin + 1,
               ymax = ymin + 1,
               sqr = rep(c(1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1),4))) 
    
    unicode_piece <- pieces_df %>%
      filter(piece == name,
             color == "black") %>%
      select(unicode)
    
    unicode_piece <- unlist(unicode_piece)
    
    rank_labels <- data.frame(x = c(0:7),
                              y = rep(7.75),
                              label = c(1:8))
    
    file_labels <- data.frame(x = rep(-0.75),
                              y = c(0:7),
                              label = c(letters[1:8]))
    
    graph_plot <- ggraph(graph, layout = "grid") + 
      geom_rect(data = squares, 
                mapping = aes(xmin = xmin, 
                              xmax = xmax, 
                              ymin = ymin, 
                              ymax = ymax, 
                              fill = sqr,
                              ), 
                color = "gray70",
                show.legend = FALSE) +
                                               
      scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
      scale_y_continuous(breaks = 1:8, labels = 1:8)  +
      geom_segment(data = board, aes(x, y, xend = xend, yend = yend), color = "white") +
      geom_segment(data = board, aes(y, x, xend = yend, yend = xend), color = "white") +
      #geom_node_text(aes(label = name), nudge_y = 0.35, nudge_x = -0.3, color = "white") +
      geom_text(data = rank_labels, mapping = aes(x = x, y = y, label = label)) +
      geom_text(data = file_labels, mapping = aes(x = x, y = y, label = label)) +
      geom_edge_link(
        aes(
        #mapping = aes(
        #edge_width = 1
        #,
        #edge_color = weight
        #edge_alpha = 0.5
        
        #)
        width = (1/weight)*7),
        color = "gray60",
        show.legend = FALSE) +
      scale_edge_width(range = c(0.5, 4)) +
      #geom_node_point(aes(size = 0.5),
      #                color = "gray50",
      #                show.legend = FALSE) +
      geom_node_text(size = 17.5, aes(label = unicode_piece), nudge_y = 0.1, nudge_x = 0, color = "white") +
      theme_void() +
      coord_equal()
    
    #layout <- create_layout(graph, layout = "igraph", algorithm = "nicely")
    
    show(graph_plot)
  }
}

generate_move_graph("queen")


nodes <- data.frame(name = as.character(chessboard_nodes$coordinates))

nodes



























### misc ###


abs(1 - (weight/10))


render_board <- function() {
  
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
                            label = c(1:8))
  
  file_labels <- data.frame(x = rep(-0.75),
                            y = c(0:7),
                            label = c(letters[1:8]))
  
  chessboard <- ggplot() + 
    geom_rect(data = squares, 
              mapping = aes(xmin = xmin, 
                            xmax = xmax, 
                            ymin = ymin, 
                            ymax = ymax, 
                            fill = sqr,
              ), 
              color = "gray70",
              show.legend = FALSE) +
    
    scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
    scale_y_continuous(breaks = 1:8, labels = 1:8)  +
    geom_segment(data = board, aes(x, y, xend = xend, yend = yend), color = "white") +
    geom_segment(data = board, aes(y, x, xend = yend, yend = xend), color = "white") +
    geom_text(data = rank_labels, mapping = aes(x = x, y = y, label = label)) +
    geom_text(data = file_labels, mapping = aes(x = x, y = y, label = label)) +
    theme_void() +
    coord_equal()
  
  
  
  return(chessboard)
}


render_piece <- function(piece) {
  
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
  
  unicode_piece <- pieces_df %>%
    filter(piece == name,
           color == "black") %>%
    select(unicode)
  
  unicode_piece <- unlist(unicode_piece)
  
  graph_plot <- ggraph(graph, layout = "grid") + 
    geom_edge_link(
      aes(
        #mapping = aes(
        #edge_width = 1
        #,
        #edge_color = weight
        #edge_alpha = 0.5
        
        #)
        width = (1/weight)*7),
      color = "gray60",
      show.legend = FALSE) +
    scale_edge_width(range = c(0.5, 4)) +
    #geom_node_point(aes(size = 0.5),
    #                color = "gray50",
    #                show.legend = FALSE) +
    geom_node_text(size = 17.5, aes(label = unicode_piece), nudge_y = 0.1, nudge_x = 0, color = "white") +
    theme_void() +
    coord_equal()
  
  return(graph_plot)
}
  
  
  
  
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
