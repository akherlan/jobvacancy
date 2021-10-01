# Glints
# <https://glints.com/id/>

source("requirement.R")
lapply(list.files("function", ".R", full.names = TRUE), source)

# available job category
glints_category()
glints_skill(2)

# available job industry
glints_industry()

# list of company
glints_company(10)

# query
vacancy <- glints_vacancy(70)

# num = 20
# v <- vacancy
# glints_post(v, num)
# glints_joburl(v, num)
# glints_descform(v, num)
# detect_skill_r(glints_descform(v, num))

# parameter
url <- "https://glints.com/api/graphql"
opnam <- "searchJobs"
jobid <- 2
limit <- 50
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

# pulling data
jobs <- GQL(
  query = query,
  .variables = var,
  .operationName = opnam,
  .url = url
)

job <- jobs$searchJobs$jobsInPage

vacancy <- restruct_job(job)

# arranging
vacancy <- vacancy
  select(
    id, title, matches("category"), matches("city"),
    country_name, applicant_count, matches("company"),
    matches("experience"), matches("salaries"), matches("salary_estimate"),
    status, is_remote, is_hot, created_at
  )

# R programming requirement detection
for (s in 1:nrow(vacancy)) {
  p <- suppressMessages(glints_post(vacancy, s))
  d <- detect_skill_r(p)
  if(d) {
    message(sprintf("Data ke %s memenuhi kualifikasi", s))
    if("r" %in% ls()){
      r <- append(r, p)
    } else {
      r <- c()
      r <- append(r, p)
    }
  } else {
    message(sprintf("Melewati data ke %s", s))
  }
  Sys.sleep(1)
}

