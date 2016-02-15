library(data.table)
library(bit64)

add_changes <- function(d) {
  d[,Prev.Close :=  c(NA,head(Adj.Close,n=-1))]
  d[,Pct.Change := Adj.Close/Prev.Close - 1]
  d[,Return := log(Adj.Close/Prev.Close)]
}

portfolio.year <- function(date) {
  dt <- as.POSIXlt(date)
  factor( dt$year 
          #make the year run jan 29th to jan 29th
          #and include the few days of feb 2016 into 2015 as well
          - ifelse((dt$mon == 0 & dt$mday < 29) | 
                     dt$year == 116,1,0)
          #posixlt years are since 1900
          + 1900
  )
}
#http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&d=1&e=15&f=2016&g=d&a=0&b=3&c=1950&ignore=.csv
read_ticker <- function(ticker,qry=NA) {
  if( is.na(qry) ) {
    qry <- ticker
  }
  d <- fread(paste0('http://real-chart.finance.yahoo.com/table.csv?s=',qry,'&d=1&e=15&f=2016&g=d&a=0&b=29&c=2010&ignore=.csv'))
  setnames(d,make.names(names(d)))
  #delete some things we're not interested in
  d[,`:=`(Open = NULL, High = NULL, Low = NULL, Volume = NULL)]
  d[,Date := as.POSIXct(Date)]
  add_changes(d)
  d[,Symbol := ticker]
  d[,Quarter := factor(trunc(as.POSIXlt(Date)$mon / 3)+1,c(1,2,3,4,5),
                       labels = c("Q1","Q2","Q3","Q4",'All'))]
  d[,Year := portfolio.year(Date)]
}

beta <- function(market,sec) {
  keep <- ! is.na(market) & ! is.na(sec)
  cov(market[keep],sec[keep])/var(market[keep])
}

gain <- function(prev,price) {
  start <- head(prev[!is.na(prev)],n=1)
  end <- tail(price,n=1)
  (end - start)/start
}

calc_stats <- function(mkt,d) {
  if( is.data.table(mkt)) {
    d[,Market.Pct.Change := mkt$Pct.Change]
    d[,Market.Close := mkt$Adj.Close]
    d[,Market.Prev := mkt$Prev.Close]
  } else {
    d[,`:=`(Marget.Pct.Change = NA, Market.Close = NA, Market.Prev = NA)]
  }
  ret <- data.table()
  bycols <- c()
  for( bycol in c('Symbol','Year','Quarter')) {
    bycols <- c(bycols,bycol)
    ret <- rbind(ret,
              d[,.(Gain = gain(Prev.Close,Adj.Close),
                   Relative.Gain = gain(Prev.Close,Adj.Close) - gain(Market.Prev,Market.Close),
                   Beta = beta(Market.Pct.Change,Pct.Change),
                   Volatility = sd(Return,na.rm=T)*sqrt(252)
                   ),
                by = bycols],
              fill=TRUE)
  }
  ret[is.na(Quarter),Quarter := 'All']
  ret[is.na(Year),Year := 'Overall']
  ret[order(Year,Quarter,Symbol)]
}