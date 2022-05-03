render_move_graph <- function(graph) {
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
    
    show(graph_plot)
}

render_game <- function(graph) {
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
    geom_node_text(size = 17.5, aes(label = unicode, color = color), nudge_y = 0.1, nudge_x = 0) +
    scale_color_manual(values = c("white" = "white",
                                  "black" = "black")) +
    theme_void() +
    coord_equal()
  
  show(graph_plot)
}
