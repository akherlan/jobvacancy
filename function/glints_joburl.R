# Get job url

glints_joburl <- function(df, num) {
  
  for (n in num) {
    jurl <- paste(
      "https://glints.com/id/opportunities/jobs",
      # df$title[[num]] %>% 
      #   tolower() %>% 
      #   str_remove_all("\\(") %>% 
      #   str_remove_all("\\)") %>% 
      #   str_remove_all("\\[") %>% 
      #   str_remove_all("\\]") %>% 
      #   str_remove_all("-") %>% 
      #   str_remove_all("\\.") %>% 
      #   str_remove_all("\\/") %>% 
      #   str_remove_all("&") %>% 
      #   str_squish() %>% 
      #   str_replace_all("\\s", "-"), 
      df$id[[n]],
      sep = "/")
    
    if (n == num[[1]]) joburl <- jurl
    else joburl <- append(joburl, jurl)
    
  }
  
    return(joburl)
  
}
