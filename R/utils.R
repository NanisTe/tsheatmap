firstDayMonth <- function(x) {
  # https://www.r-bloggers.com/dates-in-r-and-the-first-day-of-the-month/
  x = lubridate::date(as.character(x))
  day = base::format(x,format = "%d")
  monthYr = base::format(x,format = "%Y-%m")
  y = base::tapply(day,monthYr, min)
  first = zoo::as.Date(base::paste(base::row.names(y),y,sep = "-"))
  base::as.factor(first)
}
