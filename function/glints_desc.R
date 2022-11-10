#' Description formatting for Glints
#'
#' @description Using descriptionRaw column as provided by API or alternatively
#'    using get_joburl function for scraping.
#' @param url (character) Job URL(s).
#' @import dplyr
#' @import rvest
#' @import stringr
#' @export
#'
glints_desc <- function(url) {
  
  num <- length(url)
  
  for (n in num) {
    
    # get html page
    f <- read_html(url)
    desc <- html_element(f, ".DraftEditor-editorContainer")
    
    # assume prefered description tag
    desc <- desc %>%
      html_children() %>%
      html_children() %>%
      html_children() %>%
      html_children()
    
    # description tabulation
    tag <- desc %>% html_name()
    content <- desc %>% html_text()
    
    # pre-formatting
    desc <- tibble(tag, content) %>%
      mutate(content = str_squish(content),
             content = ifelse(str_detect(tag, "li"),
                              str_replace(content, "^(.+)$", "• \\1"),
                              content),
             content = ifelse(str_detect(tag, "div") &
                                str_count(content, "\\w+\\s") < 4 &
                                !str_detect(content, "^-\\s") &
                                !str_detect(content, "•\\s"),
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = str_replace(content, "^(.+)$", "\\1<br>"),
             content = str_replace(content, "^<strong>", "<br><strong>"))
    
    # post-formatting
    topost_text <- desc$content %>%
      toString() %>%
      str_replace_all(",\\s,\\s", "<br>") %>%
      str_replace_all(">,\\s", ">") %>%
      str_replace_all("<br>-\\s", "<br>• ") %>%
      str_replace_all("<br></strong>(<br>)?", "</strong><br><br>") %>%
      str_remove("^<br>") %>%
      str_remove("<br>$")
    
    if (n == num[[1]]) txt <- topost_text
    else txt <- append(txt, topost_text)
    
  }
  
  return(txt)
  
}