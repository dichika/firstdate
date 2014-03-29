getATNDUsers <- function(username=NULL){
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

getATNDEventUsers <- function(eventid=NULL){
  require(jsonlite)
  require(RCurl)
  if(is.null(eventid)){
    stop("eventid is required")
  }
  url <- "http://api.atnd.org/events/users/?event_id=%s&format=json"
  s <- sprintf(url, eventid)
  res <- getURL(s)
  res <- jsonlite::fromJSON(res)$event$users[[1]]
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
  resjson <- rjson::fromJSON(content(req, as="text"))
  tmp <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","C")
  result <- ldply(resjson, function(x){data.frame(
    name=x$user$name,
    screen_name=x$user$screen_name,
    location=x$user$location,
    time=as.POSIXlt(paste0(substr(x$created_at,5,19),substr(x$created_at,27,30)),
                 format="%B %d %H:%M:%S%Y",tz="Europe/London"),
    tweetid=x$id_str,
    text=x$text,
    media=ifelse(!is.null(x$entities$media[[1]]$media_url),
                 x$entities$media[[1]]$media_url,
                 NA),
    stringsAsFactors=FALSE
  )})
  attributes(result$time)$tzone <- "Japan"
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
  n1$save("xxxxxxxxxxxxxxx.html", cdn=TRUE)
  tmp <- paste(readLines("xxxxxxxxxxxxxxx.html", warn=FALSE), collapse="\n")
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
}

slideTL <- function(res, covertitle="title", covertext="text", covertime="2014-01-01 00:00:00",path="output.html"){
  require(rCharts)
  releaseList <- plyr::alply(res, 1, function(x) {
    list(
      #       startDate = format(as.Date(x$time), "%Y,%m,%d"), 
      startDate = format(x$time, "%Y,%m,%d,%H,%M,%S"), 
      headline = x$screen_name, 
      text = x$text,
      asset=if(!is.na(x$media)){list(media=x$media)}else{list()}
    )
  })
  
  m = Timeline$new()
  m$main(headline = covertitle,
         type = "default", 
         text = covertext,
         startDate = format(as.POSIXlt(covertime), "%Y,%m,%d,%H,%M,%S"), 
#          startDate = format(as.Date(min(res$time)), "%Y,%m,%d"), 
         asset = list()
  )
# m$config(start_zoom_adjust="6")
  names(releaseList) <- NULL
  m$event(releaseList)
  tmp <- tempfile()
  m$save(tmp)
  
  x <- paste(readLines(tmp, warn = F), collapse = "\n")
  x <- gsub(paste0(system.file(package="rCharts"),"/libraries/timeline/js/storyjs-embed.js"), 
            "http://cdn.knightlab.com/libs/timeline/2.26.1/js/storyjs-embed.js", 
            x)
  writeLines(x, con = path)
}

getExif <- function(path){
  require(XML)
  tmp <- tempfile()
  cond <- try(url(path),silent=TRUE)
  if(!any(class(cond) %in% "try-error")){
    download.file(path, tmp)
    path <- tmp
  }
  path2 <- sprintf('exiftool -h "%s"', path)
  info <- system(path2, inter=TRUE)
  info2 <- gsub("<!.+-->","",paste(collapse="", info))
  result <- readHTMLTable(info2, stringsAsFactors=FALSE)[[1]]
  result <- cbind(path=path, result)
  colnames(result) <- c("path", "variable", "value")
  invisible(result)
}

getExif2Map <- function(path){
  require(XML)
  tmp <- tempfile()
  cond <- try(url(path),silent=TRUE)
  if(!any(class(cond) %in% "try-error")){
    download.file(path, tmp)
    path <- tmp
  }
  path2 <- sprintf('exiftool -h "%s"', path)
  info <- system(path2, inter=TRUE)
  info2 <- gsub("<!.+-->","",paste(collapse="", info))
  result <- readHTMLTable(info2, stringsAsFactors=FALSE)[[1]]
  colnames(result) <- c("variable","value")
  if(!any(result$variable %in% "GPS Position")){
    stop("\nThis photo seems to have no GPS information.")
  }
  convLatLon <- function(latlon){
    res <- strsplit(latlon, split=" ")[[1]]
    res <- res[grepl("[[:digit:]]",res)]
    res <- as.numeric(gsub("[[:punct:]]", "",res))
    res <- res[1] + res[2]/60 + res[3]/360000
  }
  lat <- convLatLon(result[result$variable=="GPS Latitude","value"])
  lon <- convLatLon(result[result$variable=="GPS Longitude","value"])
  u <- sprintf("https://www.google.com/maps/place/%s,%s", lat, lon)
  browseURL(u)
}


getHealthGraph <- function(key, secret){
  require(httr)
  runkeeper <- oauth_endpoint(base_url="https://runkeeper.com/apps",
                              authorize="authorize",
                              access="token")
  myapp <- oauth_app("runkeeper", key, secret)
  runkeeper_token <- oauth2.0_token(runkeeper, myapp)
  req <- GET("http://api.runkeeper.com/user", 
             config(token = runkeeper_token))
  resjson <- rjson::fromJSON(content(req, as="text"))
  resjson
}

visCloud <- function(dat, var="text", fontfamily="HiraKakuProN-W3", minF=3, trg=c("名詞","動詞"), ...){
  require(wordcloud)
  require(RMeCab)
  pieces <- unlist(lapply(dat[,var], function(x)RMeCabC(x, ...)))
  pieces <- pieces[((names(pieces) %in% trg) & (nchar(pieces>1))) & !(grepl("[[:alnum:]]|[[:punct:]]",pieces))]
  piecesDF <- data.frame(table(pieces))
  par(family=fontfamily)
  wordcloud(words=piecesDF$pieces, freq=piecesDF$Freq, min.freq=minF,rot.per=0,
            random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  
}