library("bitops")
library("RCurl")
library("rjson")
library("rvest")


setwd("D:\\BaiduYunDownload\\data")

# ??ȡ??ҳ??ҳ??
link <- "http://qing.bao.hu.wei.cheng.nian.i7p.work/v.php?category=mf&viewtype=detailed&page=330"

temp <- html("http://qing.bao.hu.wei.cheng.nian.i7p.work/v.php?category=mf&viewtype=detailed&page=330"
             ,encoding="utf-8")

temp_ind <- temp %>% html_nodes("#paging") %>% html_nodes("a") %>% html_text()
ind <- temp_ind[c(2, length(temp_ind)-1)]


#??ȡ????/??ʱ/????/UID/?ղ?/?鿴/????/????/��??
get_values <- function(k=330){

 # k <- 330
 page <- paste("http://qing.bao.hu.wei.cheng.nian.i7p.work/v.php?category=mf&viewtype=detailed&page=",
              k, sep="")
 temp <- html(page)

 #??ȡ????/??ʱ/????/UID/?ղ?/?鿴/????/????
 title <- temp %>% html_nodes(".imagechannelinfo .title") %>% html_text()
 info <- temp %>% html_nodes(".imagechannelinfo .info") %>% html_text()
 times <- gsub("Runtime:", "", info[grepl("Runtime", info)])
 auther <- temp %>% html_nodes(".imagechannelinfo a") %>% html_text()
 auther <- auther[which(c(1:length(auther))%%2 ==0)]
 info <- temp %>% html_nodes(".imagechannelinfo ") %>% html_text()

 views <- sapply(info, function(x){ t <- gsub("\n|\t", "", x)
                                   temp <- gsub(" ", "", unlist(strsplit(t, "Views: |Favorites:|Comments:|Point:"))[-1])
                                  data <- temp[1]})
 favorites <- sapply(info, function(x){ t <- gsub("\n|\t", "", x)
                                   temp <- gsub(" ", "", unlist(strsplit(t, "Views: |Favorites:|Comments:|Point:"))[-1])
                                   data <- temp[2]})
 comments <- sapply(info, function(x){ t <- gsub("\n|\t", "", x)
                                   temp <- gsub(" ", "", unlist(strsplit(t, "Views: |Favorites:|Comments:|Point:"))[-1])
                                   data <- temp[3]})
 points <- sapply(info, function(x){ t <- gsub("\n|\t", "", x)
                                   temp <- gsub(" ", "", unlist(strsplit(t, "Views: |Favorites:|Comments:|Point:"))[-1])
                                   data <- temp[4]})
 names(views) <- NULL
 names(favorites) <- NULL
 names(comments) <- NULL
 names(points) <- NULL

 info <-  temp %>% html_nodes(".imagechannelinfo a") %>% html_attr("href")

#??ȡ??Ƶ??��??
link2 <- info[!grepl("UID", info)]

#??ȡ?û?uid
 uid <- sapply(info[grepl("UID", info)], function(x){ unlist(strsplit(x, "UID="))[2]})
 names(uid) <- NULL

 result <- data.frame(title = title,
                     auther = auther,
                     uid = uid,
                     times = times,
                     views = views,
                     favorites = favorites,
                     comments = comments,
                     points = points,
                     link = link2,
                     page = rep(page, length(title)),
                     stringsAsFactors = F
                     )

 
}


##??ȡ????ҳ????Ƶ?ĵ?ַ????
get_source <- function(y){
  temp <- html("C:\\Users\\liufengyan\\Desktop\\1.html") %>% html_nodes("#mediaspace script") %>% html_text()
  temp1 <- unlist(strsplit(temp, "\r\n"))
  vid <- unlist(strsplit(temp1[grepl("file", temp1)], "\\',\\'|\\'\\)"))[2]
  max_vid <- unlist(strsplit(temp1[grepl("max_vid", temp1)], "\\',\\'|\\'\\)"))[2]
  mp4 <- unlist(strsplit(temp1[grepl("mp4", temp1)], "\\',\\'|\\'\\)"))[2]
  seccode <- unlist(strsplit(temp1[grepl("seccode", temp1)], "\\',\\'|\\'\\)"))[2]
  #ͨ???ӿڻ?ȡ?ļ???ַ
  getfile <- paste("http://qing.bao.hu.wei.cheng.nian.i7p.work/getfile.php?VID=",
              vid, 
              "&mp4=", mp4,
              "&seccode=", seccode,
              "&max_vid=", max_vid, sep="")
  file <- getURL(getfile)
  file <- unlist(strsplit(file, "file=|&domainUrl"))[2]
  file <- gsub("%26", "&",
               gsub("%3D", "=", 
               gsub("%3F", "?", 
                    gsub("%2F", "/", 
                         gsub("%3A", ":", file)))))
  temp <- getBinaryURL(file)
  note <- file("1.mp4", open="wb")
  writeBin(temp, note)
  close(note)
  
}


result <- data.frame(NULL)
for(k in 1:10){
  temp <- get_values(k)
  result <- rbind(result, temp)
  cat(paste(k, "\n"))
  
}


path <- paste(format(Sys.Date(), "%Y%m%d"), ".csv", sep="")
write.csv(file=path, result, row.names = F)
