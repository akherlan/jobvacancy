#' Get job category 

glints_category <- function() {
  
  category <- fromJSON("https://glints.com/api/jobCategories")
  category <- category$data %>% 
    select(id, name, descriptionMarkdown, createdAt, updatedAt) %>% 
    as_tibble() %>% 
    arrange(id) %>% 
    clean_names()
  return(category)
  
}
