# Get job url

glints_joburl <- function(df, num){
  
  joburl <- paste(
    "https://glints.com/id/opportunities/jobs",
    df$title[[num]] %>% 
      tolower() %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all("\\[") %>% 
      str_remove_all("\\]") %>% 
      str_remove_all("-") %>% 
      str_remove_all("\\.") %>% 
      str_remove_all("\\/") %>% 
      str_remove_all("&") %>% 
      str_squish() %>% 
      str_replace_all("\\s", "-"), 
    df$id[[num]],
    sep = "/")
  
  return(joburl)
  
}
