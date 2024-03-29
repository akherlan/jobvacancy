#' GraphQL

# <https://gist.github.com/rentrop/83cb1d8fc8593726a808032e55314019>
# url <- "https://glints.com/api/graphql"
# url <- "https://xapi.supercharge-srp.co/job-search/graphql?country=id&isSmartSearch=true"

GQL <- function(query, 
                ..., 
                .token = NULL,
                .variables = NULL, 
                .operationName = NULL, 
                .url = url){
  pbody <- list(query = query, variables = .variables, operationName = .operationName)
  if(is.null(.token)){
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <- POST(.url, body = pbody, encode = "json", add_headers(Authorization=auth_header), ...)
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if(!is.null(res$errors)){
    warning(toJSON(res$errors))
  }
  res$data
}
