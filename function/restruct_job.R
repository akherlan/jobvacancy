# Restructuring GQL result

restruct_job <- function(jobls){
  
  # restructuring
  for (i in seq_along(jobls)) {
    v <- unlist(jobls[[i]])
    v <- cbind(name = names(v), value = v)
    v <- as_tibble(v) %>% mutate(num = i)
    if(i == 1){
      vacancy <- v
    } else {
      vacancy <- bind_rows(vacancy, v)
    }
  }
  
  # reforming
  vacancy <- vacancy %>% 
    group_by(num, name) %>%
    summarise_all(~toString(value)) %>% 
    ungroup() %>% 
    pivot_wider(
      id_cols = "num", 
      names_from = "name",
      values_from = "value"
    ) %>% 
    select(-num) %>% 
    clean_names()
  
  return(vacancy)
  
}
