# Glints
# <https://glints.com/id/>

source("requirement.R")

# parameter
url <- "https://glints.com/api/graphql"
opnam <- "searchJobs"
jobid <- 2
limit <- 30
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

# check scraped page
scraped <- read.csv("output/scraped.csv")
vacancy <- filter(vacancy, is.na(match(vacancy$id, scraped$id)) == TRUE)
if (nrow(vacancy) == 0) {
  message("Tidak ada postingan baru.")
  # if there are new post
} else {
  
  # detect R programming requirement
  for (s in 1:nrow(vacancy)) {
    p <- suppressMessages(glints_descform(vacancy, s))
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
    topost <- glints_post(vacancy, 1:nrow(vacancy))
    
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
