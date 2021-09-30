lapply(
  c(
    "httr",
    "jsonlite",
    "rvest",
    "stringr",
    "dplyr",
    "tidyr",
    "janitor",
    "telegram.bot"
  ), function(x) if(!require(x, character.only = TRUE)) install.packages(x)
)
