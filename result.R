library(dplyr)
library(stringr)
library(jsonlite)

source("functions.R")

rownum = 28

# Position

jobtitle <- vacancy$title[[rownum]]

# Company
# link: https://glints.com/id/companies/{CompanyName}/{CompanyId}

jobhire_baseurl <- "https://glints.com/api/companies/"
company_id <- vacancy$CompanyId[[rownum]]

jobhire <- fromJSON(
  paste0(jobhire_baseurl, company_id)
)$data$name

# Desctiption

jobdesc <- desc_formatting(vacancy, rownum)

# Link

joburl <- paste(
  "https://glints.com/id/opportunities/jobs",
  jobtitle %>% 
    tolower() %>% 
    str_remove_all("\\(") %>% 
    str_remove_all("\\)") %>% 
    str_remove_all("-") %>% 
    str_squish() %>% 
    str_replace_all("\\s", "-"), 
  vacancy$id[[rownum]],
  sep = "/")

# Text

jobpost <- paste(
  paste0("<strong>", str_to_upper(jobtitle), "</strong>"),
  paste0("at ", jobhire, "<br>"),
  paste0(jobdesc, "<br>"),
  joburl,
  sep = "<br>"
)
