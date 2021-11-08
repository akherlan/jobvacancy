#' Get skill by job category id
#' 
#' @param catid numeric vector of job's category.

glints_skill <- function(catid){
  
  base_url <- "https://glints.com/api/jobCategories"
  q <- paste(base_url, catid, "skills", sep = "/")
  skill <- fromJSON(q)$data %>% 
    as_tibble() %>% 
    select(id, name, popularity, createdAt, updatedAt)
  skill$popularity <- as.integer(skill$popularity)
  skill <- arrange(skill, desc(popularity)) %>% clean_names()
  return(skill)
  
}
