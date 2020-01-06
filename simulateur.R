setwd("~/Projets_R/13_consensus_backtest")
# for each ticker in cac 40 index download boursorama html page and save price + consensus.
library(readr)
library(rvest)
library(stringr)
library(xlsx)
library(readxl)
library(zoom)
libelles <- read_delim('libelles.csv', ';')
data <- as.data.frame(read_excel("data.xlsx"))
rownames(data) <- data[,1]
data <- data[,2:(2 * nrow(libelles) + 1)]
# alpha between -pi/2 and pi/2.
# alpha > 0 -o-> trust in the consensus.
alpha <- -pi/4

simul <- data.frame(matrix(0, ncol = nrow(libelles)+1, nrow = nrow(data)))
colnames(simul) <- c(paste0(libelles$ticker_boursorama, '_n'), 'valo')
data <- cbind(data, simul)

c_prime_fctn <- function(c) {
  y <- -c/4+5/4
  return(y)
}

c_alpha_fctn <- function(c_p, a) {
  y = 1:length(c_p)
  for (k in 1:length(c_p)) {
    if (a>=0) {
      if (c_p[k]<(tan(a)-1)/(2*tan(a))) {y[k] <- 0}
      if (c_p[k]>(tan(a)+1)/(2*tan(a))) {y[k] <- 1}
      else {y[k] <- c_p[k]*tan(a)+(1-tan(a))/2}
    }
    if (a<0) {
      if (c_p[k]<(tan(a)+1)/(2*tan(a))) {y[k] <- 1}
      if (c_p[k]>(tan(a)-1)/(2*tan(a))) {y[k] <- 0}
      else {y[k] <- c_p[k]*tan(a)+(1-tan(a))/2}
    }
  }
  return(y)
}

# initialisation
for (i in libelles$ticker_boursorama) {
  data[1, paste0(i, '_n')] <- sample(1:100,1)
}
data[1, "valo"] <- sum(as.numeric(data[1, 1:nrow(libelles)])*as.numeric(data[1, (2*nrow(libelles)+1):(3*nrow(libelles))]))

alpha <- seq(-pi/2+0.01, pi/2-0.01, 0.05)
pm_value <- rep(0,length(alpha))
res_alpha <- as.data.frame(cbind(alpha, pm_value))
for (k in 1:length(alpha)) {
  # simulation
  for (j in 2:nrow(data)) {
    c_alpha_tot <- 0
    for (i in libelles$ticker_boursorama) {
      c_prime <- c_prime_fctn(as.numeric(data[(j-1), paste0(i, '_c')]))
      c_alpha <- c_alpha_fctn(c_prime, res_alpha$alpha[k])
      c_alpha_tot <- c_alpha_tot + c_alpha
    }
    for (i in libelles$ticker_boursorama) {
      c_prime <- c_prime_fctn(as.numeric(data[(j-1), paste0(i, '_c')]))
      c_alpha <- c_alpha_fctn(c_prime, res_alpha$alpha[k])
      data[j, paste0(i, '_n')] <- c_alpha/c_alpha_tot*as.numeric(data$valo[(j-1)])/as.numeric(data[(j-1), paste0(i, '_p')])
    }
    data[j, 'valo'] <- sum(as.numeric(data[j, 1:nrow(libelles)])*as.numeric(data[j, (2*nrow(libelles)+1):(3*nrow(libelles))]))
  }
  res_alpha$pm_value[k] <- (data$valo[nrow(data)]/data$valo[1]-1)*100
}

plot(res_alpha$alpha, res_alpha$pm_value, 'l')


# best and worst cases
# valo_bst <- rep(0, nrow(data))
# valo_wst <- rep(0, nrow(data))
# valo_bst[1] <- data[1, "valo"]
# valo_wst[1] <- data[1, "valo"]
# for (j in 2:nrow(data)) {
#   var <- data[j,1:40]/data[j-1,1:40]
#   valo_bst[j] <- valo_bst[j-1]*var[var == max(var)]
#   valo_wst[j] <- valo_wst[j-1]*var[var == min(var)]
# }
# 
# plot(1:nrow(data), data$valo, 'l')
# lines(1:nrow(data), valo_bst, 'l')
# lines(1:nrow(data), valo_wst, 'l')
# zm()

print((data$valo[nrow(data)]/data$valo[1]-1)*100)

write.xlsx(data, 'data_res.xlsx', sheetName = 'data', col.names = T, row.names = T, append = F)
