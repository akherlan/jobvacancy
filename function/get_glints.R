#' Glints
# <https://glints.com/id/>

get_glints <- function(jobid, limit) {
  # parameter
  url <- "https://glints.com/api/graphql"
  opnam <- "searchJobs"
  if (missing(jobid)) { jobid <- 2 }
  if (missing(limit)) { limit <- 30 }
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
  return(vacancy)
}