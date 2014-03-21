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

getEventUsers <- function(eventid=NULL){
  require(jsonlite)
  require(RCurl)
  if(is.null(eventid)){
    stop("eventid is required")
  }
  url <- "http://api.atnd.org/events/users/?event_id=%s&format=json"
  s <- sprintf(url, eventid)
  res <- getURL(s)
  res <- fromJSON(res)$event$users[[1]]
  return(res)
}

getTwilog <- function(username=NULL){
  require(XML)
  require(RCurl)
  if(is.null(username)){
    stop("username is required")
  }
  i <- 0
  url <- "twilog.org/%s/%s"
  res <- NULL
  repeat{
    s <- sprintf(url, username,i)
    res0 <- getURL(s)
    Sys.sleep(1)
    if(grepl("box-pd15 mt35", res0)){
      break
    }
    res <- c(res, res0)
    i <- i+1
  }
  parsed <- htmlParse(res, encoding="utf-8")
  result <- data.frame(
    posted = xpathSApply(parsed, "//p[@class='tl-posted']",xmlValue),
    name = xpathSApply(parsed, "//p[@class='tl-name']",xmlValue),
    text = xpathSApply(parsed, "//p[@class='tl-text']",xmlValue),
    stringsAsFactors=FALSE
  )
  invisible(result)
}

getFacebook <- function(user){
  return(user)
}

getTwitter <- function(user, key, secret){
  require(httr)
  require(rjson)
  require(plyr)
  myapp <- oauth_app("twitter", 
                     key = key, 
                     secret = secret)
  twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)
  url <- sprintf("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=%s&count=180&include_rts=1",user)
  req <- GET(url,config(token = twitter_token))
  resjson <- fromJSON(content(req, as="text"))
  tmp <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","C")
  result <- ldply(resjson, function(x){data.frame(
    name=x$user$name,
    screen_name=x$user$screen_name,
    location=x$user$location,
    time=as.POSIXlt(paste0(substr(x$created_at,5,19),substr(x$created_at,27,30)),
                 format="%B %d %H:%M:%S%Y"),
    tweetid=x$id_str,
    text=x$text,
    stringsAsFactors=FALSE
  )})
  Sys.setlocale("LC_TIME",tmp)
  invisible(result)
}

sayTweet <- function(tweetdata, var="text", voice="Kyoko", interval=3){
  if(Sys.info()["sysname"]!="Darwin"){
    stop("This function is Mac only...")
  }
  for(i in seq_len(nrow(tweetdata))){
    system(paste("say -v", voice, tweetdata[i,var]))
    Sys.sleep(interval)
  }
}

visTL <- function(tweetdata, group="name", path="sample.html"){
  require(rCharts)
  tweetdata$tmp <- as.numeric(as.factor(tweetdata[,group]))
  n1 <- nPlot(data=tweetdata, y="tmp", x="time", group=group, type="lineWithFocusChart")
  n1$chart(tooltipContent="#! function(key,x,y,e){ return e.point.text} !#",
           size="#!150!#",
           forceY=paste0("#![",min(tweetdata$tmp)-1, ",", max(tweetdata$tmp)+1, "]!#")
           )
  n1$xAxis(tickFormat = "#!function(d) {return d3.time.format('%m-%d %H')(new Date( d * 1000 ));}!#")
  n1$save("sample.html", cdn=TRUE)
  tmp <- paste(readLines(path, warn=FALSE), collapse="\n")
  tmp <- gsub("<style>.+</style>","<style>
    .rChart {
      display: block;
      margin-left: auto; 
      margin-right: auto;
      width: 800px;
      height: 400px;
    }  
    .nvd3.nv-line .nvd3.nv-scatter .nv-groups .nv-point {
        fill-opacity: 1;
      }

    </style>", tmp)
  writeLines(tmp, path)
  browseURL(path)
}