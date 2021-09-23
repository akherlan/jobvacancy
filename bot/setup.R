# BOT CONFIGURATION

library(telegram.bot)

# replace "idnrbot" suitable with bot setup in .Renviron
updater <- Updater(token = bot_token("idnrbot"))

# admin_id could be changed in project's .Rprofile
admin <- as.BaseFilter(function(message) message$from_user == admin_id)

# cont. to function
