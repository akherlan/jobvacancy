library(httr)
library(jsonlite)
library(stringr)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(rvest)

source("functions.R")
source("function_graphql.R")

# available job category
glints_category()
glints_skill(2)

# available job industry
glints_industry()

# list of company
company <- glints_company(20)

# query
vacancy <- glints_vacancy(100)

# num = 20
# v <- vacancy
# glints_post(v, num)
# glints_joburl(v, num)
# glints_descform(v, num)
# detect_skill_r(glints_descform(v, num))

# graphql query

url <- "https://glints.com/api/graphql"
opnam <- "searchJobs"
# key <- "data analyst"
jobid <- 2
limit <- 100
var <- sprintf(
  '{
    "data": {
      "JobCategoryId": %s,
      "CountryCode": "ID",
      "limit": %s,
      "offset": 30,
      "prioritiseHotJobs": true,
      "includeExternalJobs": true,
      "variant": "A"
    }
  }', jobid, limit
)

query <- 
'query searchJobs($data: JobSearchConditionInput!) {
  searchJobs(data: $data) {
    jobsInPage {
      id
      title
      isRemote
      status
      createdAt
      isHot
      salaryEstimate {
        minAmount
        maxAmount
        CurrencyCode
      }
      company {
        name
        id
      }
      citySubDivision {
        name
      }
      city {
        name
      }
      country {
        name
      }
      category {
        name
      }
      salaries {
        salaryType
        salaryMode
        maxAmount
        minAmount
        CurrencyCode
      }
      applicantCount
      minYearsOfExperience
      maxYearsOfExperience
    }
    totalJobs
  }
}'


jobs <- GQL(
  query = query,
  .variables = var,
  .operationName = opnam,
  .url = url
)

job_total <- jobs$searchJobs$totalJobs
job <- jobs$searchJobs$jobsInPage

for (i in seq_along(job)) {
  s <- unlist(job[[i]])
  s <- cbind(name = names(s), value = s)
  s <- as_tibble(s) %>% mutate(num = i)
  if(i == 1){
    vacancy <- s
  } else {
    vacancy <- rbind(vacancy, s)
  }
}

vacancy <- vacancy %>% 
  pivot_wider(
    id_cols = "num", 
    names_from = "name",
    values_from = "value"
  ) %>% 
  select(-num)

# dput(names(vacancy))

names(vacancy) <- c(
  "id", "title", "is_remote", "status", "created_at", "is_hot",
  "company_name", "company_id", "city", "country", "job_category", 
  "salary_type","salary_mode", "salary_max", "salary_min", "salary_currency",
  "applicant_count", "min_experience", "max_experience",
  "city_subdivision", "salary_estim_min",
  "salary_estim_max", "salary_estim_currency"
)

# R programming requirement detection
for (s in 1:nrow(vacancy)) {
  p <- suppressMessages(glints_post(vacancy[s,]))
  d <- detect_skill_r(p)
  if(d) {
    message(sprintf("Data ke %s memenuhi kualifikasi", s))
    if("r" %in% ls()){
      r <- append(r, s)
    } else {
      r <- c()
      r <- append(r, s)
    }
  } else {
    message(sprintf("Melewati data ke %s", s))
  }
  Sys.sleep(1)
}

