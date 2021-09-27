library(jsonlite)
library(stringr)
library(rvest)
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
    # if there is no data pulled, stop the process
    stop("Tidak ada data lowongan tersedia")
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
  
  names(vacancy) <- c(
    "id", "title", "JobCategoryId", "company_id", "type", "descriptionRaw", 
    "min_experience", "max_experience", "num_hire",
    "deadline", "startDate", "endDate", "durationLegacy", "expiryDate",
    "status", "is_public", "is_remote", "closedAt", "createdAt", "updatedAt"
  )
  
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

  if(!is.null(vacancy$company_name)){
    
    # using company name from table
    jobhire <- vacancy$company_name[[num]]
    message("Menggunakan kolom company_name")

  } else if(!is.null(vacancy$company_id)){
    
    # pull company data from glints
    jobhire_baseurl <- "https://glints.com/api/companies/"
    jobhire <- fromJSON(
      paste0(jobhire_baseurl, vacancy$company_id[[num]])
    )$data$name 
    message("Mencari data nama perusahaan dari web")
    
  } else {
    stop("Gagal mendapatkan nama perusahaan")
  }
  
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


# get industry
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
    arrange(name)
  return(industry)
  
}


# get category

glints_category <- function() {
  
  category <- fromJSON("https://glints.com/api/jobCategories")
  category <- category$data %>% 
    select(id, name, descriptionMarkdown, createdAt, updatedAt) %>% 
    as_tibble() %>% 
    arrange(id)
  return(category)
  
}

# get skill by job category id

glints_skill <- function(catid){
  
  base_url <- "https://glints.com/api/jobCategories"
  q <- paste(base_url, catid, "skills", sep = "/")
  skill <- fromJSON(q)$data %>% 
    as_tibble() %>% 
    select(id, name, popularity, createdAt, updatedAt)
  skill$popularity <- as.integer(skill$popularity)
  skill <- arrange(skill, desc(popularity))
  
  return(skill)
  
}


# get company
glints_company <- function(limit = 50L, cty = "ID") {
  
  company <- fromJSON(
    paste0(
      "https://glints.com/api/companies?where={%22CountryCode%22:%22", cty, "%22}",
      "&limit=", limit
    )
  )
  
  count <- company$count
  message(
    paste(
      "Mengambil", limit, 
      "dari", count, "rekaman data perusahaan",
      sep = " "
    )
  )
  
  company <- company$data %>% 
    select(id, name, IndustryId, address, website, logo, tagline,
           isVerified, isHiring, CityId, CountryCode, 
           createdAt, updatedAt) %>% 
    as_tibble()
  
  return(company)
  
}


# Description formatting -----
glints_descform <- function(df, num) {
  # using glints_joburl() then scrape for alternative
  
  # row data to be formatted
  if(sum(str_detect(names(df), "descriptionRaw")) == 1){
    topost_desc <- df[num,] %>% 
      select(descriptionRaw)
    
    # descprition tabulation
    desc <- topost_desc$descriptionRaw$blocks[[1]]
    suppressMessages(desc <- bind_cols(desc$text, desc$type, desc$depth))
    names(desc) <- c("text", "type", "depth")
    
    # html formatting markup
    
    if(sum(str_detect(desc$type, "list-item")) == 0) {
      
      # unstyled list with manual bullet or numering
      topost_text <- desc %>% 
        # pre-formatting
        mutate(
          text = str_replace_all(text, "\\n", "<br>"),
          text = str_replace_all(text, "\\u2028", "<br><br>"),
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
        # post-formatting
        str_replace_all(",\\s•\\s", "<br>• ") %>% 
        str_replace_all("<br>,\\s", "<br>") %>% 
        str_replace_all(",\\s<br>", "<br><br>") %>% 
        str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
        str_remove_all("<strong></strong>") %>% 
        str_replace_all("(<br>){3,}", "") %>%
        str_remove("^<br>")
      
    } else if (sum(str_detect(desc$type, "list-item")) > 0) {
      
      # styled indent list
      topost_text <- desc %>% 
        # pre-formatting
        mutate(
          text = str_replace_all(text, "\\n", "<br>"),
          text = str_replace_all(text, "\\u2028", "<br><br>"),
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
        # post-formatting
        str_replace_all(",\\s•\\s", "<br>• ") %>% 
        str_replace_all("<br>,\\s", "<br>") %>% 
        str_replace_all(",\\s<br>", "<br><br>") %>%
        str_replace_all("\\.,\\s", "\\.<br><br>") %>% 
        str_remove_all("<strong></strong>") %>% 
        str_replace_all("(<br>){3,}", "") %>%
        str_remove("^<br>")
      
    } else {
      
      # others
      topost_text <- paste0("Click the link")
      
    }
    
    return(topost_text)
    
  } else {
    
    message("Kolom deskripsi tidak tersedia, metode scraping digunakan")
    
    url <- glints_joburl(vacancy, num)
    
    f <- read_html(url)
    desc <- html_element(f, ".DraftEditor-editorContainer")
    
    # assuming prefered description tag
    desc <- desc %>% 
      html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_children()
    
    # description tabulation
    tag <- desc %>% html_name()
    content <- desc %>% html_text()
    
    desc <- tibble(tag, content) %>% 
      mutate(content = str_squish(str_replace(content, "^(.+)$", "\\1<br>")),
             content = ifelse(str_detect(tag, "li"), 
                              str_replace(content, "^(.+)$", "• \\1"),
                              content),
             content = ifelse(str_detect(tag, "div") & 
                                str_count(content, "\\w+\\s") < 3 &
                                !str_detect(content, "^-\\s"),
                              str_replace(content, "^(.+)$", "<strong>\\1</strong>"),
                              content))
    
    topost_text <- desc$content %>% 
      toString() %>% 
      str_replace_all(",\\s,\\s", "<br>") %>% 
      str_replace_all(">,\\s", ">") %>% 
      str_replace_all("<br>-\\s", "<br>• ") %>% 
      str_replace_all("<br></strong>(<br>)?", "</strong><br><br>") %>% 
      str_remove("<br>$")
    
    return(topost_text)
    
  }
  
}

# Detect R skill requirement in text description -----
detect_skill_r <- function(text){
  
  text <- str_to_lower(text)
  
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

