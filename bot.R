# BOT EXECUTION

source("bot/setup.R")     # 1. bot configuration
source("bot/function.R")  # 2. define functions
source("bot/handler.R")   # 3. update handlers from functions
updater$start_polling()   # 4. starting bot
