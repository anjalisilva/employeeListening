# Aerodyne employee listening and job architecture case study
#
# Author: Anjali Silva
#
# Analysis date: 17 July 2026
#
# Purpose: ask if tables should be saved; default is no

ask_yes_no <- function(prompt_text) {
  cat(prompt_text)
  flush.console()
  if (interactive()) {
    user_input <- readline()
  } else {
    input_connection <- file("stdin", open = "r")
    user_input <- readLines(
      con = input_connection,
      n = 1L,
      warn = FALSE)
    close(input_connection)
  }
  # default is FALSE when user presses Enter or provides no input
  length(user_input) == 1L &&
    tolower(trimws(user_input)) %in% c("y", "yes")
}

# [END]