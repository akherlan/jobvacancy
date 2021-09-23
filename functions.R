library(stringr)
suppressPackageStartupMessages(library(dplyr))

# description formatting -----
desc_formatting <- function(vacancy, rownum) {
  
  # one row data to be formatted
  topost_data <- vacancy[rownum,] %>% 
    select(id, title, CompanyId, isRemote, descriptionRaw,
           minYearsOfExperience, maxYearsOfExperience,
           expiryDate, createdAt, updatedAt)
  
  topost_desc <- topost_data %>% 
    select(descriptionRaw)
  
  # descprition tabulation
  desc <- topost_desc$descriptionRaw$blocks[[1]]
  desc <- bind_cols(desc$text, desc$type, desc$depth)
  names(desc) <- c("text", "type", "depth")
  
  # html formatting markup
  
  if(sum(str_detect(desc$type, "list-item")) == 0) {
    
    # unstyled list with manual bullet or numering
    topost_text <- desc %>% 
      mutate(
        text = str_remove_all(text, "\\n"),
        text = str_replace(text, "^\\d{1,2}\\.?\\s?", "• "),
        text = str_replace(text, "^-\\s?", "• "),
        text = str_replace(text, "^·\\s?", "• "),
        text = ifelse(
          str_count(text, "([A-Za-z]+\\s)") <= 2 & 
            !str_detect(text, "• "),
          paste0("<br><strong>", text, "</strong><br>"),
          text)
        ) %>%  
      .[[1]] %>% 
      toString() %>% 
      str_replace_all(",\\s•\\s", "<br>• ") %>% 
      str_replace_all("<br>,\\s", "<br>") %>% 
      str_replace_all(",\\s<br>", "<br><br>") %>% 
      str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
      str_remove_all("<strong></strong>") %>% 
      str_replace_all("(<br>){3,4}", "<br><br>") %>%
      str_remove("^<br>")
    
  } else if (sum(str_detect(desc$type, "list-item")) > 0) {
    
    # styled indent list
    topost_text <- desc %>% 
      mutate(
        text = str_remove_all(text, "\\n"),
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
      str_replace_all(",\\s•\\s", "<br>• ") %>% 
      str_replace_all("<br>,\\s", "<br>") %>% 
      str_replace_all(",\\s<br>", "<br><br>") %>%
      str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
      str_remove_all("<strong></strong>") %>% 
      str_replace_all("(<br>){3,4}", "<br><br>") %>%
      str_remove("^<br>")
    
  } else {
    
    # others
    topost_text <- paste0("Click the link")
    
  }
  
}

# usage example
# jobvac <- desc_formatting(vacancy, 14)
