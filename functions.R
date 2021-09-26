library(jsonlite)
library(stringr)
suppressPackageStartupMessages(library(dplyr))

# get vacancy
glints_vacancy <- function(limit = 50L, cty = "ID", type = "FULL_TIME") {
  
  base_url <- "https://glints.com/api/jobs"
  
  countryCode   <- cty
  jobTypes      <- type
  # INTERNSHIP, FULL_TIME, PART_TIME, PROJECT_BASED
  JobCategoryId <- "{%22$in%22:[1,2,5,7,10]}"
  # when looking for vacancies with keyword "data analyst" manually on the web
  # five categories below are appear in the filter column
  # 1  = software enggineering
  # 2  = data science
  # 5  = marketing
  # 7  = bussiness development / sales
  # 10 = finance
  # for more, see: glints_category()
  startDate     <- as.character.Date(Sys.Date()-1)
  status        <- "OPEN"
  limit         <- limit
  
  # %22 is doublequote and 
  # %2C is comma
  # %7B is left curly bracket
  # %7D is left curly bracket
  # ref: https://www.unicodepedia.com/unicode/basic-latin/2c/comma/
  
  query <- paste0(
    base_url, "?where={",
    "%22CountryCode%22:%22", countryCode, "%22%2C",
    "%22type%22:%22", jobTypes, "%22%2C",
    "%22JobCategoryId%22:", JobCategoryId, "%2C",
    # '%22startDate%22:%22', startDate, '%22%2C',
    "%22status%22:%22", status, "%22}&limit=", limit
  )
  
  
  vacancy <- fromJSON(query)
  
  if(vacancy$count > 0){
    message(
      paste("Mengambil", limit, "dari", vacancy$count, "data lowongan kerja", 
            sep = " ")
    )
  } else if (vacancy$count == 0){
    # if there is no data in the startDate, stop the process
    stop(
      paste0(
        "\nTidak ada data lowongan pada ", startDate,
        "\nProses dihentikan"
      )
    )
  } else {
    # another condition
    stop(
      cat("Terjadi kesalahan")
    )
  }
  
  vacancy <- vacancy$data %>% 
    as_tibble() %>% 
    select(id, title, JobCategoryId, CompanyId, type, descriptionRaw, 
           minYearsOfExperience, maxYearsOfExperience, numberOfHires,
           deadline, startDate, endDate, durationLegacy, expiryDate,
           status, isPublic, isRemote, closedAt, createdAt, updatedAt) %>% 
    arrange(desc(updatedAt))
  
  return(vacancy)
  
}

# get job url
glints_joburl <- function(vacancy, num){
  
  joburl <- paste(
    "https://glints.com/id/opportunities/jobs",
    vacancy$title[[num]] %>% 
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
    vacancy$id[[num]],
    sep = "/")
  
  return(joburl)
  
}


# post formatting
glints_post <- function(vacancy, num = 1L) {
  
  # using generic function: glints_descform()
  
  # position
  
  jobtitle <- vacancy$title[[num]]
  
  # company
  # link: https://glints.com/id/companies/{CompanyName}/{CompanyId}
  
  jobhire_baseurl <- "https://glints.com/api/companies/"
  company_id <- vacancy$CompanyId[[num]]
  
  jobhire <- fromJSON(
    paste0(jobhire_baseurl, company_id)
  )$data$name
  
  # desctiption
  
  jobdesc <- glints_descform(vacancy, num)
  
  # link
  
  joburl <- glints_joburl(vacancy, num)
  
  # body
  
  jobpost <- paste(
    paste0("<strong>", str_to_upper(jobtitle), "</strong>"),
    paste0("at ", jobhire, "<br>"),
    paste0(jobdesc, "<br>"),
    joburl,
    sep = "<br>"
  )
  
  return(jobpost)
  
}


# get category

glints_category <- function() {
  
  category <- fromJSON("https://glints.com/api/jobCategories/")
  category <- category$data %>% as_tibble()
  category <- category %>% 
    select(id, name, descriptionMarkdown, createdAt, updatedAt) %>% 
    as_tibble(category) %>% 
    arrange(id)
  return(category)
  
}


# get company
glints_company <- function(limit = 50L, cty = "ID") {
  
  limit <- limit
  cty <- cty
  company <- fromJSON(
    paste0(
      "https://glints.com/api/companies?where={%22CountryCode%22:%22", cty, "%22}",
      "&limit=", limit
    )
  )
  
  count <- company$count
  print(paste("Mengambil", limit, 
              "dari", count, "rekaman data perusahaan",
              sep = " "))
  
  company <- as_tibble(company$data)
  
  return(company)
  
}


# description formatting
glints_descform <- function(df, num) {
  
  # one row data to be formatted
  if(sum(str_detect(names(df), "descriptionRaw")) == 1){
    topost_desc <- df[num,] %>% 
      select(descriptionRaw)
  } else {
    stop("Kolom deskripsi tidak tersedia")
  }

  # descprition tabulation
  desc <- topost_desc$descriptionRaw$blocks[[1]]
  suppressMessages(desc <- bind_cols(desc$text, desc$type, desc$depth))
  names(desc) <- c("text", "type", "depth")
  
  # html formatting markup
  
  if(sum(str_detect(desc$type, "list-item")) == 0) {
    
    # unstyled list with manual bullet or numering
    topost_text <- desc %>% 
      mutate(
        text = str_replace_all(text, "\\n", "<br>"),
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
        text = str_replace_all(text, "\\n", "<br>"),
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
  
  return(topost_text)
  
}

# detect R skill requirement in text description
detect_r <- function(text){
  if(
    sum(str_detect(text, "\\sr$")) > 0 |
    sum(str_detect(text, "\\sr\\.")) > 0 |
    sum(str_detect(text, "\\sr\\s")) > 0 |
    sum(str_detect(text, "^r\\s")) > 0 |
    sum(str_detect(text, "\\sr,")) > 0 |
    sum(str_detect(text, ",r,")) > 0 |
    sum(str_detect(text, ",r\\s")) > 0 |
    sum(str_detect(text, "\\sr\\-\\d")) > 0 
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

