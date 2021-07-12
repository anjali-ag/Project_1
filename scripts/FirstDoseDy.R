urlfile="https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/doses/doses-by-day.csv"
nycdata<-read.csv(url(urlfile))
dates=as.Date(nycdata[,1])
dates[length(dates)-1]


library(dygraphs)
library(xts)

dataTS <- data.frame(
  time=seq(from=dates[1], to=dates[length(dates)], by=1 ), 
  value=nycdata[,2]
)

dataTS <- xts(x = dataTS$value, order.by = dataTS$time)

p <- dygraph(dataTS, main="First Doses Administered in NYC") %>%
  dyAxis("x",label="Dates") %>%
  dyAxis("y",label="No. of doses administered") %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )%>%
  dyLegend(show="onmouseover", hideOnMouseOut = TRUE)%>%
  dyRangeSelector();
p


