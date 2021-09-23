# COMMAND AS FUNCTIONS

# say hello -----
start <- function(bot, update){
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = sprintf("Hello %s!", update$message$from$first_name)
  )
}

# text capitalization -----
caps <- function(bot, update, arg){
  if(length(arg > 0L)){
    text_caps <- toupper(paste(arg, collapse = " "))
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps
    )
  }
}

# send the messages back for non-command -----
echo <- function(bot, update){
  bot$sendMessage(
    chat_id = update$message$chat_id, 
    text = update$message$text
  )
}

# killer for admin -----
kill <- function(bot, update){
  # goodbye message
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = "Bot telah dimatikan, sampai jumpa..."
  )
  # clean kill update
  bot$getUpdates(offset = update$update_id + 1L)
  # stop the updater polling
  update$stop_polling()
}

# other commands define as unknown -----
unknown <- function(bot, update){
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = "Perintah tidak dimengerti."
  )
}

# cont. to handler
