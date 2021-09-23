library(telegram.bot)

# setup -----
# bot <- Bot(token = bot_token("idnrbot"))
# target <- 1427719843L

# daftar user
# user2 <- 142771983L
# andi <- 150398752L # forbidden 403 by default if you are stranger, nice!
# group <- -1001083834780

# function -----
# send message
# kirim_pesan("kamu lagi apa?")
# kirim_pesan <- function(teks){
#   bot$sendMessage(
#     chat_id = target,
#     text = teks,
#     parse_mode = "Markdown"
#   )
# }

# update pesan
# history <- bot$getUpdates()

updater <- Updater(token = bot_token("idnrbot"))

answer_cb <- function(bot, update){
  
  storage <- update$callback_query$data  # defining data storage
  
  # send custom keyboard
  bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                  text = paste0("Hello"))
  
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id,
                          text = paste("Answer recorded:", storage))
}

updater <- updater + CommandHandler("answer", answer_cb)
updater$start_polling()
