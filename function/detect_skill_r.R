#' Detect R skill requirement in text description
#' 
#' @param text Text character to detect.

detect_skill_r <- function(text) {
  text <- str_to_lower(text)
  if (sum(str_detect(text, "\\sr$")) > 0 |
      sum(str_detect(text, "\\sr\\.")) > 0 |
      sum(str_detect(text, "\\sr\\/")) > 0 |
      sum(str_detect(text, "\\/r[,\\.\\s]")) > 0 |
      sum(str_detect(text, "\\(r[,\\s]")) > 0 |
      sum(str_detect(text, "\\sr\\s?\\)")) > 0 |
      sum(str_detect(text, "\\sr\\s")) > 0 |
      sum(str_detect(text, "^r\\s")) > 0 |
      sum(str_detect(text, "\\sr,")) > 0 |
      sum(str_detect(text, ",r,")) > 0 |
      sum(str_detect(text, ",r\\s")) > 0 |
      sum(str_detect(text, "\\sr\\-\\d")) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
