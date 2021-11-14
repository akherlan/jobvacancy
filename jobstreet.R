# Jobstreet
# <https://www.jobstreet.co.id/>

source("requirement.R")

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

# check scraped page
scraped <- read.csv("output/scraped.csv")
vacancy <- filter(vacancy, is.na(match(vacancy$id, scraped$id)) == TRUE)
if (nrow(vacancy) == 0) {
  message("Tidak ada postingan baru.")
  # if there are new post
} else {
  
  # detect R programming requirement
  for (s in 1:nrow(vacancy)) {
    p <- suppressMessages(jstreet_descform(vacancy, s))
    d <- detect_skill_r(p)
    if(d) {
      message(sprintf("Posting ke-%s memenuhi kualifikasi", s))
      if("r" %in% ls()){
        r <- append(r, s)
      } else {
        r <- c()
        r <- append(r, s)
      }
    } else {
      message(sprintf("Melewati posting ke-%s", s))
    }
    Sys.sleep(0.5)
  }
  
  # save what was scraped
  scraped <- data.frame(vacancy[,c("id", "job_title", "source")], get_time = Sys.time())
  write.table(x = scraped,
              file = "output/scraped.csv", 
              sep = ",", 
              append = TRUE, 
              row.names = FALSE, 
              col.names = FALSE)
  
  # check if there are duplicate post
  if (r != 0) {
    posted_job <- read.csv("output/posted.csv")
    posting_job <- vacancy[r, c("id", "job_title", "source")]
    posting_job <- filter(posting_job, 
                          is.na(match(posting_job$id, posted_job$id)) == TRUE)
  } else {
    posting_job <- data.frame()
  }
  
  # post and save for tracking
  if (nrow(posting_job) == 0) {
    message("Tidak ada postingan baru.")
  } else {
    
    # filter data
    vacancy <- filter(vacancy, vacancy$id %in% posting_job$id)
    
    # generate post
    desc <- jstreet_descform(vacancy, 1:nrow(vacancy))
    topost <- sapply(1:nrow(vacancy), function(j) {
      sprintf(
        "<strong>%s</strong><br>at %s<br><br>%s<br><br>%s",
        str_to_upper(vacancy$job_title[j]),
        vacancy$company_meta_name[j],
        desc[j],
        vacancy$job_url[j]
      )
    })
    
    # send post
    md <- sapply(topost, function(html) {
      html %>% 
        str_replace_all("<br>", "\n") %>%
        str_replace_all("<strong>", "*") %>% 
        str_replace_all("</strong>", "*") %>% 
        str_replace_all("\\*\\*", "*")
    })
    # bot: sending message script
    bot <- Bot(token = bot_token("idnrbot"))
    ab <- md
    message("Sending message to Telegram")
    while ("ab" %in% ls()) {
      
      for (post in seq_along(md)) {
        tryCatch(
          {
            bot$sendMessage(
              chat_id = as.integer(Sys.getenv("ADMIN_ID")),
              text = md[[post]],
              parse_mode = "Markdown"
            )
            Sys.sleep(2)
          }, 
          error = function(e){
            cat("ERROR in", post, ":", conditionMessage(e), "\n")
          }
        )
      }
      
      rm(ab) # for stop looping
      
      # summary
      bot$sendMessage(
        chat_id = as.integer(Sys.getenv("ADMIN_ID")),
        text = sprintf(
          "*Ikhtisar*\nDijalankan pada %s waktu %s. Jumlah %s postingan.",
          Sys.time(), Sys.timezone(), length(md)
        ),
        parse_mode = "Markdown"
      )
      
    }
    # end of sending message script
    
    # save posted data
    posting_job <- data.frame(posting_job, get_time = Sys.time())
    write.table(x = posting_job,
                file = "output/posted.csv", 
                sep = ",", 
                append = TRUE, 
                row.names = FALSE, 
                col.names = FALSE)
    
  }
  # end if posting_job != 0
}
