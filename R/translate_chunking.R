# Split text into chunks at word boundaries, keeping each chunk at or under
# `max_chars` characters where possible. A single word longer than `max_chars`
# becomes a chunk of its own (it is never cut). Whitespace runs are collapsed
# to single spaces, so `paste(chunks, collapse = " ")` reassembles the text.
# @noRd
split_translation_chunks <- function(text, max_chars = 1000) {
  words <- strsplit(text, "\\s+")[[1]]
  chunks <- character(0)
  current <- ""
  for (word in words) {
    candidate <- if (nchar(current) == 0) word else paste(current, word)
    if (nchar(candidate) > max_chars && nchar(current) > 0) {
      chunks <- c(chunks, current)
      current <- word
    } else {
      current <- candidate
    }
  }
  if (nchar(current) > 0) chunks <- c(chunks, current)
  chunks
}
