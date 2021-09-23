library(jsonlite)
suppressPackageStartupMessages(library(dplyr))

main_url <- "https://glints.com/api/"
path <- "jobs"

# parameter
countryCode = "ID"
jobTypes = "FULL_TIME" # INTERNSHIP, FULL_TIME, PART_TIME, PROJECT_BASED
JobCategoryId = 2
startDate = as.character.Date(Sys.Date()-1)
status = "OPEN"

# %22 is doublequote and %2C is comma
# ref: https://www.unicodepedia.com/unicode/basic-latin/2c/comma/

query <- paste0(
  main_url, path, "?where={",
  '"CountryCode":"', countryCode, '"%2C',
  '"type":"', jobTypes, '"%2C',
  '"JobCategoryId":"', JobCategoryId, '"%2C',
  # '"startDate":"', startDate, '"%2C',
  '"status":"', status, '"}'
)

vacancy <- fromJSON(query)

# data conversion
if(vacancy$count > 0){
  vacancy_link <- vacancy$links
  vacancy_count <- vacancy$count
  vacancy <- vacancy$data %>% as_tibble()
  print(paste0("Jumlah total ", vacancy_count, " lowongan, limit 30"))
} else {
  print(paste0("Tidak ada lowongan pada ", startDate))
}
