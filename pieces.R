pawns <- c("white_pawn", "black_pawn")
piece_names <- c("knight", "bishop", "rook", "queen", "king")
pieces <- factor(c(pawns, piece_names), ordered = TRUE, levels = c(pawns, piece_names))

pieces_df <- data.frame(name = c(pieces[1], pieces[3:7], pieces[2], pieces[3:7]),
                        representation = c(as.integer(pieces[1]), as.integer(pieces[3:7]), as.integer(pieces[2]) * -1, as.integer(pieces[3:7]) * -1),
                        color = c(rep("white", 6), rep("black", 6)),
                        value = rep(c(1, 3, 3, 5, 9, 100)),
                        unicode = c("\U2659", "\U265E", "\U265D", "\U265C", "\U265B", "\U265A",
                                    "\U2659", "\U265E", "\U265D", "\U265C", "\U265B", "\U265A"))

