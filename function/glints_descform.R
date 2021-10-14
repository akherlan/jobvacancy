# Description formatting
# using descriptionRaw column as provided by API or
# alternatively using glints_joburl() for scraping

glints_descform <- function(df, num) {
  
  # condition 1:
  # if there is descriptionRaw column in df, select row (n) to be formatted
  if(sum(str_detect(names(df), "descriptionRaw")) == 1) {
    
    for (n in num) {
      topost_desc <- df[n,] %>% select(descriptionRaw)
      
      # descprition tabulation
      desc <- topost_desc$descriptionRaw$blocks[[1]]
      suppressMessages(desc <- bind_cols(desc$text, desc$type))
      names(desc) <- c("text", "type")
      
      # html formatting markup
      
      # case 1: unstyled list with manual bullet or numering
      if(sum(str_detect(desc$type, "list-item")) == 0) {
        
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
      
      # case 2: styled list
      else if (sum(str_detect(desc$type, "list-item")) > 0) {
        
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
      
      # case 3: others
      else {
        
        topost_text <- paste0("Click the link")
        
      }
      
      if (n == num[[1]]) txt <- topost_text
      else txt <- append(txt, topost_text)
      
    }
    
    return(txt)
    
  } 
  
  # condition 2:
  # if there is no descriptionRaw column, scrape from the web
  else {
    
    message("Kolom deskripsi tidak tersedia, mengambil deskripsi dari web")
    
    for (n in num) {
      
      # get html page
      url <- glints_joburl(df, n)
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
  
}

