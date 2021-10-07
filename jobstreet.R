# Jobstreet
# <https://www.jobstreet.co.id/>

source("requirement.R")
for (f in list.files("function", ".R", full.names = TRUE)) source(f)

# parameter
url <- "https://xapi.supercharge-srp.co/job-search/graphql?country=id&isSmartSearch=true"
key <- "data analyst"
page <- 1
var <- sprintf(
  '{
    "keyword": "%s",
    "jobFunctions": [
      501,
      503,
      508,
      512
    ],
    "locations": [],
    "salaryType": 1,
    "jobTypes": [],
    "createdAt": null,
    "careerLevels": [],
    "page": %s,
    "country": "id",
    "sVi": "",
    "categories": [
      "501",
      "503",
      "508",
      "512"
    ],
    "workTypes": [],
    "industries": [],
    "locale": "id"
  }', key, page
)

query <- 
'query getJobs($country: String, $locale: String, $keyword: String, $createdAt: String, $jobFunctions: [Int], $categories: [String], $locations: [Int], $careerLevels: [Int], $minSalary: Int, $maxSalary: Int, $salaryType: Int, $candidateSalary: Int, $candidateSalaryCurrency: String, $datePosted: Int, $jobTypes: [Int], $workTypes: [String], $industries: [Int], $page: Int, $pageSize: Int, $companyId: String, $advertiserId: String, $userAgent: String, $accNums: Int, $subAccount: Int, $minEdu: Int, $maxEdu: Int, $edus: [Int], $minExp: Int, $maxExp: Int, $seo: String, $searchFields: String, $candidateId: ID, $isDesktop: Boolean, $isCompanySearch: Boolean, $sort: String, $sVi: String, $duplicates: String, $flight: String, $solVisitorId: String) {
  jobs(country: $country, locale: $locale, keyword: $keyword, createdAt: $createdAt, jobFunctions: $jobFunctions, categories: $categories, locations: $locations, careerLevels: $careerLevels, minSalary: $minSalary, maxSalary: $maxSalary, salaryType: $salaryType, candidateSalary: $candidateSalary, candidateSalaryCurrency: $candidateSalaryCurrency, datePosted: $datePosted, jobTypes: $jobTypes, workTypes: $workTypes, industries: $industries, page: $page, pageSize: $pageSize, companyId: $companyId, advertiserId: $advertiserId, userAgent: $userAgent, accNums: $accNums, subAccount: $subAccount, minEdu: $minEdu, edus: $edus, maxEdu: $maxEdu, minExp: $minExp, maxExp: $maxExp, seo: $seo, searchFields: $searchFields, candidateId: $candidateId, isDesktop: $isDesktop, isCompanySearch: $isCompanySearch, sort: $sort, sVi: $sVi, duplicates: $duplicates, flight: $flight, solVisitorId: $solVisitorId) {
    ...LegacyCompat_SearchResult
  }
}

fragment LegacyCompat_SearchResult on SearchResult {
  solMetadata
  jobs {
    id
    sourceCountryCode
    isStandout
    companyMeta {
      id
      isPrivate
      name
    }
    jobTitle
    description
    employmentTypes {
      name
    }
    sellingPoints
    locations {
      code
      name
      children {
        code
        name
      }
    }
    categories {
      code
      name
      children {
        code
        name
      }
    }
    postedAt
    salaryRange {
      currency
      max
      min
      period
      term
    }
    isClassified
  }
}'

# pull data
jobs <- GQL(
  query = query,
  .variables = var,
  .url = url
)

# (query_meta <- jobs$jobs$solMetadata)
job <- jobs$jobs$jobs

vacancy <- restruct_job(job)

# R programming requirement detection
for (s in 1:nrow(vacancy)) {
  p <- suppressMessages(jstreet_descform(vacancy, s))
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


desc <- jstreet_descform(vacancy, r)
vpost <- vacancy[r,]

topost <- sapply(1:nrow(vpost), function(j) {
  sprintf(
    "<strong>%s</strong><br>at %s<br><br>%s<br><br>%s",
    str_to_upper(vpost$job_title[j]),
    vpost$company_meta_name[j],
    desc[j],
    vpost$job_url[j]
  )
})

