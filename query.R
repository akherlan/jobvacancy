library(httr)
library(jsonlite)
library(stringr)
suppressPackageStartupMessages(library(dplyr))

source("functions.R")
source("function_graphql.R")

# available job category
glints_category()
glints_skill(2)

# available job industry
glints_industry()

# list of company
company <- glints_company(20L)

# query
vacancy <- glints_vacancy(150)
View(vacancy)

num = 4
v <- vacancy %>% select(-updatedAt)
v <- vacancy
glints_post(v, num)
glints_joburl(v, num)
glints_descform(v, num)
detect_skill_r(glints_descform(v, num))

# graphql query
url <- "https://glints.com/api/graphql"
keyword <- "data analyst"
limit <- 30

var <- sprintf(
  '{
    "data": {
      "SearchTerm": "%s",
      "CountryCode": "ID",
      "limit": %s,
      "offset": 30,
      "prioritiseHotJobs": true,
      "includeExternalJobs": true,
      "variant": "A"
    }
  }', 
  keyword, limit
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
      isActivelyHiring
      isHot
      salaryEstimate {
        minAmount
        maxAmount
        CurrencyCode
        __typename
      }
      company {
        ...CompanyFields
        __typename
      }
      citySubDivision {
        id
        name
        __typename
      }
      city {
        ...CityFields
        __typename
      }
      country {
        ...CountryFields
        __typename
      }
      category {
        id
        name
        __typename
      }
      salaries {
        ...SalaryFields
        __typename
      }
      applicantCount
      minYearsOfExperience
      maxYearsOfExperience
      __typename
    }
    totalJobs
    __typename
  }
}

fragment CompanyFields on Company {
  id
  name
  logo
  __typename
}

fragment CityFields on City {
  id
  name
  __typename
}

fragment CountryFields on Country {
  code
  name
  __typename
}

fragment SalaryFields on JobSalary {
  id
  salaryType
  salaryMode
  maxAmount
  minAmount
  CurrencyCode
  __typename
}'


test <- GQL(
  query = query,
  .variables = var,
  .operationName = "searchJobs",
  .url = url
)

glimpse(test$searchJobs)
