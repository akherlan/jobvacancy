#' Post formatting
#'
 
# using function: 
#     glints_descform()
#     get_joburl()

glints_post <- function(df, num) {
  
  for (n in num) {
    
    # position
    # jobtitle <- df$title[[n]]
    jobtitle <- df$job_title[[n]]
    
    # company
    # case 1: using company name from table
    if (!is.null(df$company_name)) {
      
      jobhire <- df$company_name[[n]]
      
    }
    
    # case 2: pull company data from glints's web
    else if (!is.null(df$company_id)) {
      
      jobhire_baseurl <- "https://glints.com/api/companies/"
      jobhire <- fromJSON(
        paste0(jobhire_baseurl, df$company_id[[n]])
      )$data$name 
      message("Mencari data nama perusahaan dari web")
      
    } 
    
    # case 3: unknown
    else {
      
      stop("Gagal mendapatkan nama perusahaan")
      
    }
    
    # desctiption
    jobdesc <- glints_descform(df, n)
    
    # link
    joburl <- get_joburl(df, n)
    
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
