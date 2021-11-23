#' Get job url
#' 
#' @param df vacancy data.frame
#' @param num numeric vector of row from data.frame
#' @param id character job id

get_joburl <- function(df, num, id) {
  
  # using id
  if (missing(df)) {
    for (n in seq_along(id)) {
      if (nchar(id[n]) == 36) {
        # glints
        jurl <- "https://glints.com/id/opportunities/jobs/"
      } else if (nchar(id[n]) == 7 & str_detect(id[n], "^[0-9]{7}$")) {
        # jobstreet
        jurl <- "https://www.jobstreet.co.id/id/job/"
      }
      jurl <- paste0(jurl, id[n])
      if (id[n] == id[1]) joburl <- jurl
      else joburl <- append(joburl, jurl)
    }
  }
  
  # using vacancy data.frame
  if (!missing(df)) {
    if (missing(num)) {
      message('argument "num" must be supplyed')
    } else {
      for (n in num) {
        if (nchar(df$id[[n]]) == 36) {
          # glints
          jurl <- "https://glints.com/id/opportunities/jobs/"
        } else if (nchar(df$id[[n]]) == 7 & str_detect(df$id[[n]], "^[0-9]{7}$")) {
          # jobstreet
          jurl <- "https://www.jobstreet.co.id/id/job/"
        }
        jurl <- paste0(jurl, df$id[[n]])
        if (n == num[[1]]) joburl <- jurl
        else joburl <- append(joburl, jurl)
      }
    }
  }
  
  return(joburl)
  
}
