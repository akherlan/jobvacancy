# Glints
# <https://glints.com/id/>

source("requirement.R")
for (f in list.files("function", ".R", full.names = TRUE)) source(f)

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

# pull data
jobs <- GQL(
  query = query,
  .variables = var,
  .operationName = opnam,
  .url = url
)

job <- jobs$searchJobs$jobsInPage

vacancy <- restruct_job(job)

# R programming requirement detection
for (s in 1:nrow(vacancy)) {
  p <- suppressMessages(glints_descform(vacancy, s))
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


topost <- glints_post(vacancy, r)

