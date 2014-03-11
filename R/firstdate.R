getATND <- function(username=NULL){
  require(jsonlite)
  require(RCurl)
  if(is.null(username)){
    stop("username is required")
  }
  url <- "http://api.atnd.org/events/users/?twitter_id=%s&format=json&count=100"
  s <- sprintf(url, username)
  res <- getURL(s)
  res <- fromJSON(res)$events
  return(res)
}

getTwilog <- function(username=NULL){
  require(jsonlite)
  require(RCurl)
  if(is.null(username)){
    stop("username is required")
  }
  i <- 140
  url <- "twilog.org/%s/%s"
  res <- NULL
  repeat{
    s <- sprintf(url, username,i)
    res0 <- getURL(s)
    Sys.sleep(1)
    if(grepl("box-pd15 mt35", res0)){
      break
    }
    res <- rbind(res, data.frame(html=res0, stringsAsFactors=FALSE))
    i <- i+1
  }
  invisible(res)
}