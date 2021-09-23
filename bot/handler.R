# COMMAND HANDLER

# TIPS: Add commands on Telegram bot via BotFather:
#       /mybots > select your bot > Edit bots > Edit commands
#
# Here are the commands: 
# start - Say hello
# caps - Capitalization (followed by message)
# kill - Stop bot operation (admin)

updater <- updater + 
  CommandHandler("start", start) +                    # say hello
  CommandHandler("caps", caps, pass_args = TRUE) +    # text capitalization
  MessageHandler(echo, MessageFilters$text)           # return text

# killer for admin
updater <<- updater + CommandHandler("kill", kill, admin)

# other commands not mentioned above
updater <- updater + MessageHandler(unknown, MessageFilters$command)

# cont. to exec
