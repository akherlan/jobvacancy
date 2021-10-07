# Post formatting
# using function: 
#     glints_descform()
#     glints_joburl()

glints_post <- function(df, num) {
  
  for (n in num) {
    # position
    
    jobtitle <- df$title[[n]]
    
    # company
    
    if(!is.null(df$company_name)){
      
      # using company name from table
      jobhire <- df$company_name[[n]]
      message("Menggunakan kolom 'company_name'")
      
    } else if(!is.null(df$company_id)){
      
      # pull company data from glints
      jobhire_baseurl <- "https://glints.com/api/companies/"
      jobhire <- fromJSON(
        paste0(jobhire_baseurl, df$company_id[[n]])
      )$data$name 
      message("Mencari data nama perusahaan dari web")
      
    } else {
      
      stop("Gagal mendapatkan nama perusahaan")
      
    }
    
    # desctiption
    
    jobdesc <- glints_descform(df, n)
    
    # link
    
    joburl <- glints_joburl(df, n)
    
    # body
    
    jp <- paste(
      paste0("<strong>", str_to_upper(jobtitle), "</strong>"),
      paste0("at ", jobhire, "<br>"),
      paste0(jobdesc, "<br>"),
      joburl,
      sep = "<br>"
    )
    
    if (n == num[[1]]) jobpost <- jp
    else jobpost <- append(jobpost, jp)
    
  }
  
  return(jobpost)
  
}
