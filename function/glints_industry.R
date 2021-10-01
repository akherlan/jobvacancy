# Get industry name

glints_industry <- function() {
  
  limit <- 1
  industry <- fromJSON(
    paste0("https://glints.com/api/industries?limit=", limit)
  )
  
  limit <- industry$count
  industry <- fromJSON(
    paste0("https://glints.com/api/industries?limit=", limit)
  )
  
  industry <- industry$data %>% 
    select(-links, -overview) %>% 
    as_tibble() %>% 
    arrange(name) %>% 
    clean_names()
  return(industry)
  
}
