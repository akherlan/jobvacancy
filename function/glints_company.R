#' Get company detail
#' 
#' @param limit integer of amount of limit company details to gather.
#' @param cty character of country code. Default "ID".

glints_company <- function(limit = 50L, cty = "ID") {
  
  company <- fromJSON(
    paste0(
      "https://glints.com/api/companies?where={%22CountryCode%22:%22", cty, "%22}",
      "&limit=", limit
    )
  )
  
  count <- company$count
  
  message(sprintf("Mengambil %s dari %s rekaman data perusahaan", limit, count))
  
  company <- company$data %>% 
    select(id, name, IndustryId, address, website, logo, tagline,
           isVerified, isHiring, CityId, CountryCode, 
           createdAt, updatedAt) %>% 
    as_tibble() %>% 
    clean_names()
  
  return(company)
  
}
