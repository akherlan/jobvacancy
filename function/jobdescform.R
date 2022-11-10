#' Job Description Formation
#'
#' @param urls vector of url list
#' @param markdown Logical. If TRUE it will return markdown format text.
#'
#' @return vector text explanation contained jobdesc
#'
#' @import rvest
#' @import dplyr
#' @import stringr
#'
jobdescform <- function(urls, markdown = TRUE) {
  for (n in urls) {
    # get html
    url <- url(n, "rb")
    f <- read_html(url)
    close(url)
    # assume prefered description tag
    desc <- html_element(f, ".YCeva_0")
    desc <- desc %>%
      html_children() %>%
      html_children() %>%
      html_children()
    # check indent
    if (sum(html_name(desc) == "div") == length(desc)) {
      desc <- html_children(desc)
    } else if (sum(html_name(desc) == "ul") > 0 | sum(html_name(desc) == "ol") > 0) {
      desc <- desc
    } else {
      desc <- desc
    }
    for (node in seq_along(desc)) {
      if (html_name(desc[node]) == "ul")
        d <- html_children(desc[node])
      else if (html_name(desc[node]) == "ol")
        d <- html_children(desc[node])
      else
        d <- desc[node]
      if (node == 1)
        descrip <- d
      else
        descrip <- append(descrip, d)
    }
    attributes(descrip)$class <- attributes(desc)$class
    # description tabulation
    tag <- sapply(descrip, html_name)
    content <- sapply(descrip, html_text)
    desc <- tibble(tag, content)
    # format text
    desc <- desc %>%
      mutate(content = str_squish(content),
             content = ifelse(str_detect(tag, "li"),
                              str_replace(content, "^(.+)$", "\u2022 \\1"),
                              content),
             content = ifelse((tag == "div" | tag == "p") &
                                str_count(content, "\\w+\\s") < 4 &
                                !str_detect(content, "^-\\s") &
                                #!str_detect(content, "•\\s"),
                                !str_detect(content, "\u2022\\s"),
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = ifelse((tag == "div" | tag == "p") & nchar(content) < 50,
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = ifelse(tag == "strong",
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = str_replace(content, "^(.+)$", "\\1<br>"),
             content = str_replace(content, "^<strong>", "<br><strong>"))
    # filter long paragraph
    desc <- desc %>%
      filter(!((tag == "div" | tag == "p") & nchar(content) > 200))
    # clean format
    topost_text <- desc$content %>%
      toString() %>%
      str_replace_all(",\\s,\\s", "<br>") %>%
      str_replace_all(">,\\s", ">") %>%
      str_replace_all("<br>-\\s", "<br>\u2022 ") %>%
      str_replace_all("(\\w+\\.?\\s?)<br>(\\w+)", "\\1<br><br>\\2") %>%
      str_replace_all("<br></strong>(<br>)?", "</strong><br><br>") %>%
      str_remove("^<br>") %>%
      str_remove("<br>$")
    if (n == urls[[1]]) txt <- topost_text
    else txt <- append(txt, topost_text)
  }
  if (markdown) {
    txt <- sapply(txt, function(html) {
      html %>%
        str_replace_all("<br>", "\n") %>%
        str_replace_all("<strong>", "*") %>%
        str_replace_all("</strong>", "*") %>%
        str_replace_all("\\*\\*", "*")
    }, USE.NAMES = FALSE)
  }
  return(txt)
}
