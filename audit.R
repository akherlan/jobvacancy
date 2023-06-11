cat("--------- Audit ----------\n")
suppressPackageStartupMessages(source("requirement.R"))
source("function/jobdescform.R")
source("function/glints_desc.R")
library(unmplymnt)
library(purrr)
library(telegram.bot)
library(rvest)
library(dplyr)
library(stringr)

# Keywords
key_search <- c("audit kepatuhan", "compliance audit")
location <- list("Jakarta", "Bogor", "Bekasi")
keyword <- list("compliance", "kepatuhan")
exclude_title <- list("tax", "manager", "senior")

# Bot settings
bot <- Bot(token = bot_token("IDNRBOT"))

send_msg <- function(bot, msg) {
  map(msg, ~{
    tryCatch(
      {
        bot$sendMessage(
          chat_id = Sys.getenv("AUDIT_ID"),
          text = .x,
          parse_mode = "Markdown"
        )
        Sys.sleep(2)
      },
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
      }
    )
  })
}

post_job <- function(j, scraped) {
  if (nrow(j) == 0) {
    cat("No job to post today\n")
  } else {
    cat(
      paste(
        "Total", nrow(j), "job(s). Keywords:",
        paste0(map_chr(keyword, ~.x), collapse = ", "), "\n"
      )
    )
    # create message
    msg <- map(seq_len(nrow(j)), ~{
      paste0(
        "*", toupper(j$job_title[.x]), "*\nat ", j$company[.x], "\n\n",
        j$job_url[.x], "\n\nCategory: ", j$category,
        "\nKeyword: #", keyword[[1]], " #", keyword[[2]]
      )
    })
    # send message
    cat("Sending message to Telegram...\n")
    send_msg(bot, msg)
    # save scraped details
    j_scrape <- j[c("job_title", "company", "source", "job_id")]
    names(j_scrape) <- c("job_title", "company_name", "source", "id")
    j_scrape$timestamp <- format(Sys.time()) # "%Y-%m-%d %T%z"
    j_scrape$timezone <- Sys.timezone()
    scraped <- rbind(scraped, j_scrape)
  }
  write.csv(scraped, "output/audit.csv", row.names = FALSE)
}

# Jobstreet
scraped <- read.csv("output/audit.csv")
cat("Get data from Jobstreet...\n")
j <- tryCatch({
  map_df(key_search, ~jobstreet(.x))
},
error = function(e) {
  cat("ERROR: ", conditionMessage(e), "\n")
})

j <- unique(j)
j <- map_df(location, ~{
  j[grepl(.x, j$city), ]
})

# filtering with previous scraped data
j <- j[!(j$job_id %in% scraped$id), ]

# filtering title
j_title <- map(exclude_title, ~{
  grepl(.x, tolower(j$job_title))
})
j_title <- data.frame(j_title[[1]], j_title[[2]])
j_title[[(ncol(j_title) + 1)]] <- as.logical(rowSums(j_title))
j_title <- !j_title[[ncol(j_title)]]
j <- j[j_title, ]

cat("Reading description info...\n")
j_desc <- map_chr(j$job_url, ~try(jobdescform(.x)))
# matching keyword
j_keys <- map(keyword, ~grepl(.x, j_desc))
j_keys <- data.frame(j_keys[[1]], j_keys[[2]])
j_keys[[(ncol(j_keys) + 1)]] <- as.logical(rowSums(j_keys))
j_keys <- j_keys[[ncol(j_keys)]]
j <- j[j_keys, ]

post_job(j, scraped)

# Glints
cat("Get data from Glints...\n")
scraped <- read.csv("output/audit.csv")
g <- tryCatch({
  suppressMessages(map_df(key_search, ~glints(.x)))
},
error = function(e) {
  cat("ERROR: ", conditionMessage(e), "\n")
})
g <- unique(g)
g <- map_df(location, ~{
  g[grepl(.x, g$city), ]
})

# filtering with previous scraped data
g <- g[!(g$job_id %in% scraped$id), ]

# filtering title
g_title <- map(exclude_title, ~{
  grepl(.x, tolower(g$job_title))
})
g_title <- data.frame(g_title[[1]], g_title[[2]])
g_title[[(ncol(g_title) + 1)]] <- as.logical(rowSums(g_title))
g_title <- !g_title[[ncol(g_title)]]
g <- g[g_title, ]

cat("Reading description info...\n")
g_desc <- map_chr(g$job_url, ~try(glints_desc(.x)))
# matching keyword
g_keys <- map(keyword, ~grepl(.x, g_desc))
g_keys <- data.frame(g_keys[[1]], g_keys[[2]])
g_keys[[(ncol(g_keys) + 1)]] <- as.logical(rowSums(g_keys))
g_keys <- g_keys[[ncol(g_keys)]]
g <- g[g_keys, ]

post_job(g, scraped)
