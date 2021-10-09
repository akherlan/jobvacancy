# SEND MESSAGES (TELEGRAM BOT)

source("requirement.R")
for (f in list.files("function", ".R", full.names = TRUE)) source(f)
# source("jobstreet.R")
source("glints.R")

md <- sapply(topost, function(html) {
  html %>% 
    str_replace_all("<br>", "\n") %>%
    str_replace_all("<strong>", "*") %>% 
    str_replace_all("</strong>", "*") %>% 
    str_replace_all("\\*\\*", "*")
})

bot <- Bot(token = bot_token("idnrbot"))

ab <- md

while ("ab" %in% ls()) {
  
  # session info
  bot$sendMessage(
    chat_id = as.integer(Sys.getenv("ADMIN_ID")),
    text = sprintf(
      "*Session info*\n```\nWaktu  : %s\nTZ     : %s\nJumlah : %s postingan\n```", 
      Sys.time(), Sys.timezone(), length(md)
    ),
    parse_mode = "Markdown"
  )
  
  # bot$sendMessage(
  #   chat_id = as.integer(Sys.getenv("ADMIN_ID")),
  #   text = md[[1]],
  #   parse_mode = "Markdown"
  # )
  
  for (post in seq_along(md)) {
    bot$sendMessage(
      chat_id = as.integer(Sys.getenv("ADMIN_ID")),
      text = md[[post]],
      parse_mode = "Markdown"
    )
   Sys.sleep(2)
  }
  
  # for stop looping
  rm(ab)
  
}

# always error on forwarding: https://www.jobstreet.co.id/id/job/3650658
# Error in private$request(url, data) : Bad Request (HTTP 400).
