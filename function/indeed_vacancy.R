#' Get Job Vacancy form Indeed Indonesia
#'
#' @param query query character seperated by plus (+), e.g. "data+analyst"
#' @param page numeric indicate number of pages.

indeed_vacancy <- function(query, page = 1) {
  
  # page handle
  page <- (c(1:page)-1)*10
  
  for (p in page) {
    
    # get html page
    url <- sprintf("https://id.indeed.com/jobs?q=%s&sort=date&start=%s", query, p)
    url <- url(url, "rb")
    htmlraw <- read_html(url)
    close(url)
    
    # get vacancy items
    item <- htmlraw |> 
      html_elements(".mosaic-provider-jobcards") |> 
      html_children()
    
    # job url for full page direction
    job_url <- item[str_detect(item, "a\\sid")] |> html_attr("href")
    job_url <- paste0("https://id.indeed.com", item_url)
    
    # job title
    job_position <- htmlraw |> 
      html_elements(".jobTitle") |> 
      html_text() |> 
      str_remove("^Baru")
    
    # company
    company_name <- htmlraw |> 
      html_elements(".companyName") |> 
      html_text()
    
    company_rating <- htmlraw |> 
      html_elements(".heading6.company_location") |> 
      html_element(".ratingsDisplay") |> 
      html_text() |> 
      str_replace(",", ".") |> 
      as.numeric()
    
    # location
    location <- htmlraw |> 
      html_elements(".companyLocation") |> 
      html_text()
    company_location <- str_replace(location, "^(.+)\\s?•\\s?.+", "\\1")
    working_location <- str_extract(location, "^.+\\s?•\\s?(.+)") |> str_remove("^.+\\s?•\\s?")
    
    # salary
    salary <- htmlraw |> 
      html_elements(".resultContent") |> 
      html_element(".salary-snippet") |> 
      html_text()
    
    salary_from <- salary |> 
      str_replace("^Rp\\.?\\s?(\\d+(\\.?\\d{1,3}){1,3})\\s?-.+", "\\1") |> 
      str_remove_all("\\.") |> 
      as.numeric()
    
    salary_until <- salary |> 
      str_replace("^Rp.+-\\s?Rp\\.?\\s?(\\d+(\\.?\\d{1,3}){1,3})\\s?.+", "\\1") |> 
      str_remove_all("\\.") |> 
      as.numeric()
    
    # generate data.frame
    joblist <- data.frame(
      job_title = job_position, 
      company_name = company_name,
      company_location = company_location,
      company_rating = company_rating,
      working_location = working_location,
      salary_from = salary_from, 
      salary_until = salary_until,
      job_url = job_url
    ) |> dplyr::as_tibble()
    
    if (p == 0) vacancy <- joblist
    else vacancy <- bind_rows(vacancy, joblist)
    
  }
  
  return(vacancy)
  
}
