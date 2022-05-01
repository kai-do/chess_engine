source("packages.R")

input <- "N1f3+"
input <- "Qd6xd4+"

output <- interpret_notation(input)


interpret_notation <- function(input) {

  piece <- NULL
  special <- NULL
  coordinates <- NULL
  start_coordinates <- NULL
  end_coordinates <- NULL
  
  if (input == "0-0") {
   special <- "king-side castle"
  } else if (input == "0-0-0") {
    special <- "queen-side castle"
  } else {
    if (str_detect(input, "\\+")) {
      special <- "check"
    } else if (str_detect(input, "\\#")) {
      special <- "checkmate"
    }
    
    if (str_detect(input, "x")) {
      capture <- TRUE
    } else {
      capture <- FALSE 
    }
    
    piece <- str_extract(input, "[KQRBN]{1,1}")
    coordinates <- str_extract_all(input, "[a-h][1-8]")[[1]]
    special_char <- str_extract(input, "[+#]")
    str_remove_all(input, "[+#]")
    
    if (length(coordinates) == 2) {
      start_coordinates <- coordinates[1]
      end_coordinates <- coordinates[2]
    } else if (length(coordinates) == 1) {
      start_coordinates <- NULL
      end_coordinates <- coordinates
    }
  }

  return(data.frame(input = input,
                    piece = piece,
                    start_coordinates = start_coordinates,
                    end_coordinates = end_coordinates,
                    special = special,
                    capture = capture))
}
