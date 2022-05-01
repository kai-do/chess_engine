source("packages.R")

chess_games_df_1 <- read.pgn("chess_database_1.pgn", ignore.other.games = TRUE)
chess_games_df_2 <- read.pgn("chess_database_2.pgn", ignore.other.games = TRUE)
chess_games_df_3 <- read.pgn("chess_database_3.pgn", ignore.other.games = TRUE)
chess_games_df_4 <- read.pgn("chess_database_4.pgn", ignore.other.games = TRUE)
chess_games_df_5 <- read.pgn("chess_database_5.pgn", ignore.other.games = TRUE)
chess_games_df_6 <- read.pgn("chess_database_6.pgn", ignore.other.games = TRUE)

chess_games_df <- rbind(chess_games_df_1,
                        chess_games_df_2,
                        chess_games_df_3,
                        chess_games_df_4,
                        chess_games_df_5,
                        chess_games_df_6)

saveRDS(chess_games_df, file = "chess_games.rds")

chess_games_df_processed <- readRDS(file = "chess_games.rds") %>%
  mutate(doc_id = as.integer(gl(n(), 2, n()))) %>%
  mutate(text = stri_replace_all_regex(Movetext, "[0-9]+\\. ", "")) %>%
  mutate(dmeta1 = Result) %>%
  filter(text != "") %>%
  select(doc_id, text, dmeta1)

chess_games_df_processed$text[[25]]

string <- chess_games_df_processed$Moves[[7]]
pattern <- "\\p{Z}+"

processed <- stri_split_regex(string, pattern = pattern)

if(length(processed[[1]]) %% 2 == 1) {
  processed <- append(processed[[1]], NA)
  }

moves_matrix <- matrix(processed, byrow = TRUE, ncol = 2)
colnames(moves_matrix) <- c("White", "Black")
       

# Create corpus

chess_games_df_processed_sample <- sample_n(chess_games_df_processed, 1000000)

chess_games_corpus <- Corpus(DataframeSource(chess_games_df_processed_sample))

chess_games_tdm <- TermDocumentMatrix(chess_games_corpus)
chess_games_dtm <- DocumentTermMatrix(chess_games_corpus)


