#' Get vacancy data v2

get_vacancy2 <- function(key, src, post_mode = TRUE) {

  if (missing(key)) {
    key <- c(
      "data analyst",
      "business analyst",
      "data engineer"
    )
  }
  
  if (missing(src)) {
    stop("Missing argument src")
  }
  
  else if (!(src %in% c("glints", "kalibrr", "jobstreet"))) {
    stop('Argument src not in c("glints", "kalibrr", "jobstreet")')
  }
  
  else if (src == "glints") {
    message("Get job post from Glints...")
    vacancy <- map_df(key, ~suppressMessages(unmplymnt::glints(.x, 15))) %>% distinct()
  }
  
  else if (src == "kalibrr") {
    stop("Coming soon!")
  }
  
  else if (src == "jobstreet") {
    message("Get job post from Jobstreet...")
    vacancy <- map_df(key, ~suppressMessages(unmplymnt::jobstreet(.x, 15))) %>% distinct()
  }
  
  if (!post_mode) { return(vacancy) } else {
    
    # check scraped page
    scraped <- read.csv("output/scraped.csv")
    vacancy <- filter(vacancy, is.na(match(vacancy$job_id, scraped$id)) == TRUE)
    if (nrow(vacancy) == 0) {
      message("Nothing to post.")
    # if there are new post
    } else {
      # detect R programming requirement
      for (s in 1:nrow(vacancy)) {
        if (src == "jobstreet") p <- suppressMessages(jstreet_descform(vacancy, s))
        if (src == "glints") p <- suppressMessages(glints_descform(vacancy, s))
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
      scraped <- data.frame(vacancy[,c("job_id", "job_title", "source")], get_time = Sys.time()) %>% 
        rename(id = job_id)
      write.table(x = scraped,
                  file = "output/scraped.csv", 
                  sep = ",", 
                  append = TRUE, 
                  row.names = FALSE, 
                  col.names = FALSE)
      
      # check if there are duplicate post
      if (exists("r")) {
        posted_job <- read.csv("output/posted.csv")
        posting_job <- vacancy[r, c("job_id", "job_title", "source")]
        posting_job <- filter(posting_job, 
                              is.na(match(posting_job$job_id, posted_job$id)) == TRUE)
      } else {
        posting_job <- data.frame()
      }
      
      # post and save for tracking
      if (nrow(posting_job) == 0) {
        message("Nothing to post.")
      } else {
        
        # filter data
        vacancy <- filter(vacancy, vacancy$job_id %in% posting_job$job_id)
        
        # generate post
        if (src == "jobstreet") {
          desc <- jstreet_descform(vacancy, 1:nrow(vacancy))
          topost <- sapply(1:nrow(vacancy), function(j) {
            sprintf(
              "<strong>%s</strong><br>at %s<br><br>%s<br><br>%s",
              str_to_upper(vacancy$job_title[j]),
              vacancy$company[j],
              desc[j],
              vacancy$job_url[j]
            )
          })
        } else if (src == "glints") {
          topost <- glints_post(vacancy, 1:nrow(vacancy))
        } else {
          message(sprintf('Cannot generate post due to argument "src"'))
        }
        
        # send post
        md <- sapply(topost, function(html) {
          html %>% 
            str_replace_all("<br>", "\n") %>%
            str_replace_all("<strong>", "*") %>% 
            str_replace_all("</strong>", "*") %>% 
            str_replace_all("\\*\\*", "*")
        })
        # bot: sending message script
        bot <- Bot(token = bot_token("IDNRBOT"))
        ab <- md
        message("Sending message to Telegram")
        while ("ab" %in% ls()) {
        # while (exists("ab")) {
          
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
        posting_job <- data.frame(posting_job %>% rename(id = job_id), get_time = Sys.time())
        write.table(x = posting_job,
                    file = "output/posted.csv", 
                    sep = ",", 
                    append = TRUE, 
                    row.names = FALSE, 
                    col.names = FALSE)
        
      }
      # end if posting_job != 0
      
    }
    # end if there are a new post after comparing with database
    
    
  }
  # end if mode
  
}
