# SEND MESSAGES (TELEGRAM BOT)

source("requirement.R")
for (f in list.files("function", ".R", full.names = TRUE)) source(f)
# source("jobstreet.R")
source("glints.R")

# markdown format
md <- sapply(topost, function(html) {
  html %>% 
    str_replace_all("<br>", "\n") %>%
    str_replace_all("<strong>", "*") %>% 
    str_replace_all("</strong>", "*") %>% 
    str_replace_all("\\*\\*", "*")
})

# bot
bot <- Bot(token = bot_token("idnrbot"))

ab <- md

while ("ab" %in% ls()) {
  
  # bot$sendMessage(
  #   chat_id = as.integer(Sys.getenv("ADMIN_ID")),
  #   text = md[[1]],
  #   parse_mode = "Markdown"
  # )
  
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
      error=function(e){
        cat("ERROR in ", post, ":", conditionMessage(e), "\n")
      }
    )
  }
  
  # for stop looping
  rm(ab)
  
  # summary
  bot$sendMessage(
    chat_id = as.integer(Sys.getenv("ADMIN_ID")),
    text = sprintf(
      "*Summary*\n```\nWaktu  : %s\nTZ     : %s\nJumlah : %s postingan\n```", 
      Sys.time(), Sys.timezone(), length(md)
    ),
    parse_mode = "Markdown"
  )
  
}

