# Get company detail

glints_company <- function(limit = 50L, cty = "ID") {
  
  company <- fromJSON(
    paste0(
      "https://glints.com/api/companies?where={%22CountryCode%22:%22", cty, "%22}",
      "&limit=", limit
    )
  )
  
  count <- company$count
  message(
    paste(
      "Mengambil", limit, 
      "dari", count, "rekaman data perusahaan",
      sep = " "
    )
  )
  
  company <- company$data %>% 
    select(id, name, IndustryId, address, website, logo, tagline,
           isVerified, isHiring, CityId, CountryCode, 
           createdAt, updatedAt) %>% 
    as_tibble() %>% 
    clean_names()
  
  return(company)
  
}
