source("packages.R")

pieces <- c("Pawn", "Knight", "Bishop", "Rook", "Queen", "King")
pieces_factored <- factor(pieces, ordered = TRUE, levels = pieces)

piece_representations <- data.frame(piece = pieces_factored,
                                    representation = as.integer(pieces_factored),
                                    default_value = c(1, 3, 3, 5, 9, 100))

piece_properties <- data.frame(color = c("White", "Black"),
                               bishop_limit = c("light", "dark"),
                               vulnerable_to_en_passant = c(TRUE, FALSE),
                               in_check = c(TRUE, FALSE),
                               can_castle = c(TRUE, FALSE))


row <- c("a", "b", "c", "d", "e", "f", "g", "h")
row_factored <- factor(row, ordered = TRUE, levels = row)

column <- c("1", "2", "3", "4", "5", "6", "7", "8")
column_factored <- factor(column, ordered = TRUE, levels = column)

get_index_from_coordinate <- function(coordinate) {
  if (nchar(coordinate) != 2) {
   stop("Coordinate must be length of 2") 
  } else {
      return(as.integer(match(str_sub(coordinate, 1, 1), letters)) * as.integer(str_sub(coordinate, 2, 2)))
  }
}


initial_game_state <- matrix(rep(0, 64), nrow = 8)
rownames(initial_game_state) <- row_factored
colnames(initial_game_state) <- column_factored

# initialize pawns
initial_game_state[, "2"] <- c(1, 1, 1, 1, 1, 1, 1, 1)
initial_game_state[, "7"] <- c(-1, -1, -1, -1, -1, -1, -1, -1)

#initialize backrank pieces
initial_game_state[, "1"] <- c(4, 3, 2, 5, 6, 2, 3, 4)
initial_game_state[, "8"] <- c(-4, -3, -2, -5, -6, -2, -3, -4)

game_state <- initial_game_state

coordinate <- "a4"
piece <- 1

row_coord_name <- str_sub(coordinate, 1, 1)
col_coord_name <- str_sub(coordinate, 2, 2)

row_coord <- as.integer(match(row_coord_name, letters))
col_coord <- as.integer(col_coord_name)

transition_vector <- rep(0, 64)
#transition_vector[get_index_from_coordinate(coordinate)] <- piece

transition_matrix <- matrix(transition_vector, byrow = FALSE, nrow = 8)
rownames(transition_matrix) <- row_factored
colnames(transition_matrix) <- column_factored

white_turn <- TRUE

if (piece == 1) {
  if (game_state[row_coord, col_coord] == 0 ) { ## check target square for pieces
    empty <- TRUE
    i <- 1
    if(empty) {
      if (game_state[row_coord, col_coord - i] != 0) {
        i <- i + (1)
        empty <- TRUE
      } else {
        empty <- FALSE
        i <- i + 1
        move_from_piece <- game_state[row_coord, col_coord - i]
        move_from_row <- row_coord
        move_from_col <- col_coord - i
        
        transition_matrix[row_coord, col_coord] <- piece
        transition_matrix[move_from_row, move_from_col] <- piece * -1
      }
    }
  }
}




game_state + transition_matrix

transition_matrix[row_coord, (col_coord - 1)]

transition_matrix[5,4]



as.integer(match(str_sub(coordinate, 1, 1), letters))
as.integer(str_sub(coordinate, 2, 2))

transition_matrix <- matrix(rep(1, 64), nrow = 8)
