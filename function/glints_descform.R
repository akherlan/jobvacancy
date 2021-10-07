# Description formatting
# using description column as provided by API or
# alternatively using glints_joburl() then scrape

glints_descform <- function(df, num) {
  
  # row data to be formatted
  if(sum(str_detect(names(df), "descriptionRaw")) == 1){
    topost_desc <- df[num,] %>% 
      select(descriptionRaw)
    
    # descprition tabulation
    desc <- topost_desc$descriptionRaw$blocks[[1]]
    suppressMessages(desc <- bind_cols(desc$text, desc$type, desc$depth))
    names(desc) <- c("text", "type", "depth")
    
    # html formatting markup
    
    if(sum(str_detect(desc$type, "list-item")) == 0) {
      
      # unstyled list with manual bullet or numering
      topost_text <- desc %>% 
        # pre-formatting
        mutate(
          text = str_replace_all(text, "\\n", "<br>"),
          text = str_replace_all(text, "\\u2028", "<br><br>"),
          text = str_replace(text, "^\\d{1,2}\\.?\\s?", "• "),
          text = str_replace(text, "^-\\s?", "• "),
          text = str_replace(text, "^·\\s?", "• "),
          text = str_replace_all(text, "<br>-\\s", "<br>• "),
          text = ifelse(
            str_count(text, "([A-Za-z]+\\s)") <= 2 & 
              !str_detect(text, "• "),
            paste0("<br><strong>", text, "</strong><br>"),
            text)
        ) %>%
        .[[1]] %>% 
        toString() %>% 
        # post-formatting
        str_replace_all(",\\s•\\s", "<br>• ") %>% 
        str_replace_all("<br>,\\s", "<br>") %>% 
        str_replace_all(",\\s<br>", "<br><br>") %>% 
        str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
        str_remove_all("<strong></strong>") %>% 
        str_replace_all("(<br>){3,}", "<br><br>") %>%
        str_remove("^<br>")
      
    } 
    
    else if (sum(str_detect(desc$type, "list-item")) > 0) {
      
      # styled indent list
      topost_text <- desc %>% 
        # pre-formatting
        mutate(
          text = str_replace_all(text, "\\n", "<br>"),
          text = str_replace_all(text, "\\u2028", "<br><br>"),
          text = ifelse(
            str_detect(type, "list-item"),
            paste0("• ", text),
            text),
          text = ifelse(
            str_count(text, "([A-Za-z]+\\s)") <= 2 & 
              !str_detect(type, "list-item"),
            paste0("<br><strong>", text, "</strong><br>"),
            text)
        ) %>%  
        .[[1]] %>% 
        toString() %>%
        # post-formatting
        str_replace_all(",\\s•\\s", "<br>• ") %>% 
        str_replace_all("<br>,\\s", "<br>") %>% 
        str_replace_all(",\\s<br>", "<br><br>") %>%
        str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
        str_remove_all("<strong></strong>") %>% 
        str_replace_all("(<br>){3,}", "<br><br>") %>%
        str_remove("^<br>")
      
    } 
    
    else {
      
      # others
      topost_text <- paste0("Click the link")
      
    }
    
    return(topost_text)
    
  } 
  
  else {
    
    # if there is no descriptionRaw column
    
    message("Kolom deskripsi tidak tersedia, metode scraping digunakan")
    
    url <- glints_joburl(vacancy, num)
    
    f <- read_html(url)
    desc <- html_element(f, ".DraftEditor-editorContainer")
    
    # assuming prefered description tag
    desc <- desc %>% 
      html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_children()
    
    # description tabulation
    tag <- desc %>% html_name()
    content <- desc %>% html_text()
    
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
    
    topost_text <- desc$content %>% 
      toString() %>% 
      str_replace_all(",\\s,\\s", "<br>") %>% 
      str_replace_all(">,\\s", ">") %>% 
      str_replace_all("<br>-\\s", "<br>• ") %>% 
      str_replace_all("<br></strong>(<br>)?", "</strong><br><br>") %>% 
      str_remove("^<br>") %>% 
      str_remove("<br>$")
    
    return(topost_text)
    
  }
  
}

