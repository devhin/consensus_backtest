setwd("~/Projets_R/13_consensus_backtest")
# for each ticker in cac 40 index download boursorama html page and save price + consensus.
library(readr)
library(rvest)
library(stringr)
library(xlsx)
library(readxl)
libelles <- read_delim('libelles.csv', ';')

scraped <- data.frame(matrix(0, ncol = 2 * nrow(libelles), nrow = 1))
colnames(scraped) <- paste0(rep(libelles$ticker_boursorama, 2), rep(c('_p', '_c'), each=nrow(libelles)))
rownames(scraped) <- Sys.Date()
dir.create(paste0('html/html_', Sys.Date()))

for (t in libelles$ticker_boursorama) {
  url <- paste0('https://www.boursorama.com/cours/1r', t)
  print(url)
  page <- read_html(url)
  write_xml(page, file=paste0('html/html_', Sys.Date(), '/', Sys.Date(), '_', t, '.html'))
  
  prix <- as.character(html_nodes(page, '.c-instrument--last')[1])
  start <- gregexpr('data-ist-last>', prix)[[1]][1] + nchar('data-ist-last>')
  stop <- gregexpr('</span>', prix)[[1]][1] - 1
  prix <- substr(prix, start, stop)
  scraped[1, paste0(t, '_p')] <- prix
  
  consensus <- as.character(html_nodes(page, '.c-median-gauge__tooltip')[1])
  start<-gregexpr('tooltip\">', consensus)[[1]][1] + nchar('tooltip\">')
  stop<-gregexpr('</div>\n', consensus)[[1]][1] - 1
  consensus <- substr(consensus, start, stop)
  scraped[1, paste0(t, '_c')] <- consensus
  
  Sys.sleep(runif(1, 0, 3))
}

# pour le jour 1
# write.xlsx(scraped, 'data.xlsx', sheetName = 'data', col.names = T, row.names = T, append = F)
# pour les jours 2 et plus
data <- as.data.frame(read_excel("data.xlsx"))
rownames(data) <- data[,1]
data <- data[,2:(2 * nrow(libelles) + 1)]
data <- rbind(data, scraped) 
write.xlsx(data, 'data.xlsx', sheetName = 'data', col.names = T, row.names = T, append = F)
