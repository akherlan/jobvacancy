# Post formatting
# using function: 
#     glints_descform()
#     glints_joburl()

glints_post <- function(df, num = 1L) {
  
  # position
  
  jobtitle <- df$title[[num]]
  
  # company
  
  if(!is.null(df$company_name)){
    
    # using company name from table
    jobhire <- df$company_name[[num]]
    message("Menggunakan kolom 'company_name'")
    
  } else if(!is.null(df$company_id)){
    
    # pull company data from glints
    jobhire_baseurl <- "https://glints.com/api/companies/"
    jobhire <- fromJSON(
      paste0(jobhire_baseurl, df$company_id[[num]])
    )$data$name 
    message("Mencari data nama perusahaan dari web")
    
  } else {
    
    stop("Gagal mendapatkan nama perusahaan")
    
  }
  
  # desctiption
  
  jobdesc <- glints_descform(df, num)
  
  # link
  
  joburl <- glints_joburl(df, num)
  
  # body
  
  jobpost <- paste(
    paste0("<strong>", str_to_upper(jobtitle), "</strong>"),
    paste0("at ", jobhire, "<br>"),
    paste0(jobdesc, "<br>"),
    joburl,
    sep = "<br>"
  )
  
  return(jobpost)
  
}
