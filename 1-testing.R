### Load packages
library(xts)
library(tidyverse)
library(ggthemes)
library(quantmod)
library(ggfortify)
library(plotly)
library(magrittr)
library(lubridate)
library(PerformanceAnalytics)

### ETF Data
icolcap <- Ad(getSymbols("ICOL", src = "yahoo", from = "2018-01-29", to = today(), 
                      auto.assign = FALSE))

### Calculate Data
icolcap_returns <- Return.calculate(icolcap, method = "discrete")
initial_investment <-  5339692 ##  $5,339,692

icolcap_returns_plus <- icolcap_returns + 1
icolcap_returns_plus[1] <- initial_investment

time_value <- cumprod(icolcap_returns_plus)

### 4% EA

other <- icolcap_returns 
coredata(other) <- rep((1+0.04)^(1/252), length(icolcap_returns))
other[1] <- initial_investment
other_cumulative <- cumprod(other)

### Plot result

total <- merge.xts(time_value, other_cumulative)
total_tbl <- timetk::tk_tbl(total)
colnames(total_tbl) <- c("Fecha","iCOLCAP", "Renta Fija 4%")
total_tbl_new <- total_tbl %>%  mutate(diff = abs(iCOLCAP - `Renta Fija 4%`))


total_tbl_gather <- total_tbl_new %>%  
  gather(key = "variable", value = "valor", -c(Fecha, diff))

dollar_d <- scales::dollar_format()

plot <-  ggplot(total_tbl_gather, aes(x = Fecha, y = valor, color = variable,
                               text = paste("Fecha: ", as.Date(Fecha),
                                            '<br>Valor : ', dollar_d(valor),
                                            "<br>Diferencia :", dollar_d(diff)
                                            ), group = 1)) + ## trick from: https://stackoverflow.com/questions/47507186/tooltip-removes-regression-line-ggplotly
    geom_line() +
  hrbrthemes::theme_ipsum() +
  ggthemes::scale_color_gdocs(guide = FALSE) +
  labs(x = "",
       y = "Inversión a través del tiempo",
       title = "Inversión en iCOLCAP") +
  scale_y_continuous(labels= scales::dollar_format()) 
ggplotly(plot, tooltip = c("text"))


Return.cumulative(icolcap_returns)
total_tbl_gather[nrow(total_tbl_gather), 2]
