# Get vacancy

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
    arrange(desc(updatedAt)) %>% 
    clean_names()
  
  return(vacancy)
  
}