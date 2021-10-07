jstreet_descform <- function(df, num) {
  
  message("Mengambil detail deskripsi dari web")
  
  for (n in num) {
    
    url <- df$job_url[n]
    
    f <- read_html(url)
    
    desc <- html_element(f, ".vDEj0_0") %>% 
      html_element(".sx2jih0")
    
    # assuming prefered description tag
    desc <- desc %>% 
      html_children() %>% 
      html_children()
    
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
    
    desc <- desc %>% 
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
             content = ifelse(tag == "p" & nchar(content) < 50,
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = ifelse(tag == "strong",
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content),
             content = str_replace(content, "^(.+)$", "\\1<br>"),
             content = str_replace(content, "^<strong>", "<br><strong>")) %>% 
      filter(!(tag == "p" & nchar(content) > 200))
    
    jp <- desc$content %>% 
      toString() %>% 
      str_replace_all(",\\s,\\s", "<br>") %>% 
      str_replace_all(">,\\s", ">") %>% 
      str_replace_all("<br>-\\s", "<br>• ") %>% 
      str_replace_all("<br></strong>(<br>)?", "</strong><br><br>") %>% 
      str_remove("^<br>") %>% 
      str_remove("<br>$")
    
    if (n == num[[1]]) topost_text <- jp
    else topost_text <- append(topost_text, jp)
    
  }
  
  return(topost_text)
  
}
