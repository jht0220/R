library("bitops")
library("RCurl")
library("rjson")
library("rvest")

Sys.setenv("http_proxy"="127.0.0.1:8787")
curl<-getCurlHandle(proxy="127.0.0.1:16823")
curl <- getCurlHandle()
web <- "http://t66y.com/index.php"
temp <- read_html(web,curl)


read_html("http://t66y.com/htm_data/16/1610/2102215.html", curl) %>% html_nodes("title") %>% html_text()
check_proxy <- function(server="127.0.0.1", port=16823, type="HTTP"){
  
  temp <- getCurlHandle()
  t2 <- tryCatch({getURL("http://t66y.com/index.php", proxy=server, proxytype=type, proxyport=port, verbose=T)},
                 error= function(e){"??ʱ&?޷?????"})
  if(grepl("??", t2)){
    tag <- TRUE
  }else{
    tag <- FALSE
  }
  tag
}

getURL("http://t66y.com/index.php")
