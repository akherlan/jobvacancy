# Get industry name

glints_industry <- function() {
  
  limit <- 200L
  industry <- fromJSON(paste0("https://glints.com/api/industries?limit=", limit))

  if (industry$count > 500L) {
    message(sprintf("Data terlalu besar. Limit: %s", limit))
  } else if (industry$count > limit) {
    limit <- industry$count
    industry <- fromJSON(
      paste0("https://glints.com/api/industries?limit=", limit)
    )
  }
  
  industry <- industry$data %>% 
    select(-links, -overview) %>% 
    as_tibble() %>% 
    arrange(name) %>% 
    clean_names()
  return(industry)
  
}
