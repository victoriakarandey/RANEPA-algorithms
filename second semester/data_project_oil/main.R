#upload data
library(here)

dfq <- read.csv(here('brentquarter.csv'))
dfd <- read.csv(here('brentdaily.csv'))

dfq$DATE <- as.integer(format(as.Date(dfq$DATE), "%Y%m%d"))

#в дневных данных второй столбец форматируем в числа
dfd$DCOILBRENTEU <- as.numeric(dfd$DCOILBRENTEU)
#fight against NA
dfd <- na.omit(dfd)

mean_variance <- function(df, col){
  
  
  colnum <- which(colnames(df) == col)
  
  #finding the mean
  summa <- 0
  for (i in 1:nrow(df)){
    summa <- summa + df[i, colnum]
  }
  mean_counted <- summa / nrow(df)
  
  numerator <- 0
  for (j in 1:nrow(df)){
    numerator <- numerator + (df[j, colnum] - mean_counted)^2
  }
  variance <- numerator / nrow(df)
  
  return(c(mean_counted, variance))
}

monthlydfs <- function(df){
  
  df$DATE <- as.Date(df$DATE)
  df$MONTH <- format(df$DATE, "%Y-%m")
  
  monthly_mean <- data.frame(MONTH = character(), DCOILBRENTEU = numeric())
  
  for (i in unique(df$MONTH)) {
    month_data <- subset(df, MONTH == i)
    month_mean <- mean(month_data$DCOILBRENTEU)
    monthly_mean <- rbind(monthly_mean, data.frame(MONTH = i, DCOILBRENTEU = month_mean))
    }
  return(monthly_mean)
}


monthlydfs(dfd)

mean_variance(dfq, 'POILBREUSDQ')
mean_variance(dfd, 'DCOILBRENTEU')