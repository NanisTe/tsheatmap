if(getRversion() >= "2.15.1")  utils::globalVariables(c("Datetime"
                                                        ,"plotyear"
                                                        ,"weeknum"
                                                        ,"min_day_datetime"
                                                        ,"daylightsaving"
                                                        ,"daylightsavinghours_per_day"
                                                        ,"is_day_of_timeshift"
                                                        ,"datapoints"
                                                        ,"."
                                                        ,"weekday"
                                                        ,"Data"
                                                        ,"test"
                                                        ,"aes"))
#' Heatmap Plot of Timeseries
#'
#' This function creates a plotly object with the heatmap of the timeseries data.frame.
#'
#' @importFrom xts xts
#' @importFrom zoo index
#' @importFrom lubridate with_tz
#' @importFrom lubridate date
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom dplyr inner_join
#' @importFrom dplyr '%>%'
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom viridis scale_fill_viridis
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#'
#' @export
#' @param data a data.frame with at least one column of POSIXct values.
#' @param tzone give the timezone format to be used. default = "UTC"
#' @param datetime_colname column name of the column with POSIXct values of the data.frame \code{data}
#' @param data_colname column name of the column with values to be plotted of the data.frame \code{data}
#' @param xlab x axis label of the plot
#' @param ylab y axis label of the plot
#' @param LegendTitle legend title of the plot
#' @return a heatmap of the timeseries data as plotly object
tsheatmap <- function(data,
                             # timesteps = lubridate::as.difftime(1, units = "hours"), # to be used if aggregates and disaggregates are going to be implemented
                             tzone = "UTC",
                             datetime_colname = "Datetime",
                             data_colname = NULL,
                             xlab = "ISO week",
                             ylab = "",
                             LegendTitle = ""){

  # browser()
  #TODO: check number of columns of data.frame
  #TODO: check if datetime column is available & if it is POSIXct
  #
  # Check if data_colname is given:
  if(is.null(data_colname)){
    c_names <-
      data %>%
      dplyr::select(-(datetime_colname)) %>%
      base::colnames()
    data_colname <- c_names[1] # choose first column of data frame if data_colname is.null
    }


  #' <!--######################################################################%##
  #                                                                              #
  ####                             1 Input Checks                             ####
  #                                                                              #
  ##%##########################################################################%##
  #' -->  # 1 Input Checks
  #' <!--  98311be45ae642d3a52660856199c561 -->

  # heatmap_data = data %>% select((datetime_colname),(data_colname))

  dt_orig = lubridate::as.difftime(1, units = "hours")
  dt_aggr = 1/24
  # factors
  dt_factor = dt_aggr/as.numeric(dt_orig,"days")


  # get number of timesteps per day of timedifferences in the data
  # testthat
  # testdiff <-
  #   seq.POSIXt(as.POSIXct("2015-01-01 00:15:00"),as.POSIXct("2015-01-04 00:15:00"), by = "15 mins") %>%
  #   .[c(1,3:length(.))]

  # TODO: make it a function
  dt_per_day <-
    diff(data[,datetime_colname]) %>%
    unique.POSIXlt() %>%
    min() %>%
    (function(x){units(x) <- "days";return(x)}) %>%
    as.numeric() %>%
    `^`(.,-1)


  # create weekday_hour_levels
  weekdays_abbr <-
    data$Datetime %>%
    lubridate::wday(label = T, abbr = T, week_start = 1) %>%
    levels()
  # weekdays_abbr <- c("Mo.", "Di.", "Mi.", "Do.", "Fr.", "Sa.", "So.") # TODO: make it multi lang
  hourdays = as.character(seq(0, dt_per_day - 1 )) # TODO: is that needed?


  # tzone = "Europe/Zurich"
  # tzone = "UTC"

  # dt_per_day = 24
  # dt_per_day = 96
  #

  ref_difftime <- as.difftime(1,units = "hours")

  timediff <- as.difftime(15,units = "mins") # testthat
  timediff = diff(data[,datetime_colname])
  timediff_unit = timediff %>% units()


  #TODO: check if unique is of length 1 and which time difference to take if there are multiple
  tdiff <- unique(timediff) %>% as.difftime(units = timediff_unit)
  units(tdiff) <- units(ref_difftime)

  fac = as.numeric(tdiff)


  dayfactor = c(d1 = 1
                ,d7 = 2
                ,d6 = 3
                ,d5 = 4
                ,d4 = 5
                ,d3 = 6
                ,d2 = 7)

  # browser()

  heatmap_data <-
    data %>%
    dplyr::select((datetime_colname),
                  Data = (data_colname)) %>%
    # #for testing purposes
    # SBB_Consumption_2015 %>%
    # select(Datetime
    #        ,Data = Energie_SBB
    # ) %>%
    # ElDat_ohneKKW_mitPV %>%
    # select(Datetime,Data = Cons_Total_Enduse_ControlBlock) %>%
    # filter(year(Datetime) == 2016) %>%
    dplyr::mutate(Datetime = lubridate::with_tz(Datetime,tzone = tzone)) %>%
    dplyr::mutate(weeknum = lubridate::isoweek(Datetime)
                  ,date = lubridate::date(Datetime)
                  ,year = lubridate::year(Datetime)
                  ,month = lubridate::month(Datetime)
                  ,weeknum = as.numeric(strftime(date, format = "%V"))
                  ,daylightsaving = lubridate::dst(Datetime)
    ) %>%
    dplyr::group_by(weeknum) %>%
    dplyr::mutate(weekly_datapoints = n()) %>%
    dplyr::ungroup()


  # Check for Datetime issues.
  #
  # get_expected_timeseries(heatmap_data
  #                         ,tz = "Europe/Zurich"
  #                         ,timesteps = lubridate::as.difftime(1,units = "hours")
  #                         ,format = "%Y-%m-%d %H:%M:%S")
  #
  # get_unexpected_datetimes(heatmap_data
  #                          ,expected_timezone = "Europe/Zurich"
  #                          ,expected_timesteps = lubridate::as.difftime(1,units = "hours")
  #                          ,format = "%Y-%m-%d %H:%M:%S")
  # show_unexpected_datetimes(heatmap_data
  #                           ,expected_timezone = "Europe/Zurich"
  #                           ,expected_timesteps = lubridate::as.difftime(1,units="hours")
  #                           ,format = "%Y-%m-%d %H:%M:%S")

  # # correct for January dates belong to KW 52 oder 53
  # heatmap_data$weeknum[which((heatmap_data$month == 1) & (heatmap_data$weeknum == 52 | heatmap_data$weeknum == 53))] = 0
  #
  # # correct for December dates belong to KW 1
  # heatmap_data$weeknum[which((heatmap_data$month == 12) & (heatmap_data$weeknum == 1))] = 53
  # #
  #



  (
    heatmap_data <-
      heatmap_data %>%
      dplyr::group_by(weeknum,date) %>%
      dplyr::mutate(
        min_day_datetime = min(Datetime)
        ,tz = lubridate::tz(Datetime)
        # ,test = dayfactor[paste0("d",wday(date))]*24+revcumsum(c(1,as.numeric(diff(Datetime))))) %>%
        # dt_per_day = 24 hours per day or 48 half hours per day etc.
        ,test = dayfactor[paste0("d",lubridate::wday(date))]*dt_per_day +
          as.numeric(min_day_datetime - Datetime,
                     units = units(ref_difftime))/fac + 1/fac) %>%
      dplyr::mutate(datapoints = dplyr::n()
                    ,daylightsavinghours_per_day = sum(daylightsaving)
                    ,is_day_of_timeshift = ifelse(!(daylightsavinghours_per_day %in% c(0,dt_per_day)),T,F)) %>%
      # filter(is_day_of_timeshift & datapoints > 24) %>%
      # cerrecting timeshifts of day light saving
      dplyr::mutate(test = dplyr::if_else(datapoints <= dt_per_day, test + 1/fac, test)
                    ,test = dplyr::if_else(is_day_of_timeshift & datapoints > dt_per_day, test + 1/fac, test)
                    ,test = dplyr::if_else(is_day_of_timeshift & daylightsaving & datapoints <= dt_per_day , test - 1/fac, test)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        weekday = weekdays_abbr[lubridate::wday(x = date,week_start = getOption("lubridate.week.start",1))]
        ,plotyear = dplyr::if_else(weeknum == 53 & month == 1, year - 1, year)
        ,plotyear = dplyr::if_else(weeknum == 1 & month == 12, plotyear + 1, plotyear)
      )
    # filter(plotyear == 2016) # TODO: Week 53 in january days of next year
  )
  # %>%
  #   filterXts("2015",tzone="Europe/Zurich")

  # Check if sundays can have 25 hours
  has_timeshift = lubridate::dst(heatmap_data$Datetime) %>% any


  z_limits = c(min(heatmap_data$Data),max(heatmap_data$Data))

  # find id of first day in month
  first_days <-
    heatmap_data$Datetime %>%
    firstDayMonth %>%
    as.POSIXct(tz = lubridate::tz(heatmap_data$Datetime))

  id_first_day = which(heatmap_data$Datetime %in% first_days)
  data_first_day = data.frame(x = heatmap_data$weeknum[id_first_day],
                              y = heatmap_data$test[id_first_day],
                              year = heatmap_data$year[id_first_day])



  # heatmap
  heatmap_plotly <-
    (
      gg.obj <-
        ggplot2::ggplot()  +
        ggplot2::geom_tile(data = heatmap_data
                  , aes(x = weeknum,
                        y = test,
                        fill = Data,
                        text = sprintf("Datetime: %s<br>Day: %s<br>Value: %f<br>Week: %f<br>ID: %f"
                                       ,Datetime, weekday, Data, weeknum, test)
                  )
        ) +
        viridis::scale_fill_viridis(
          limits = z_limits,
          name = LegendTitle,# TODO: Legend name from function argument
          option = 'D', # plasma "D" = viridis
          direction = 1,
          na.value = "red") +
        # added if else to consider daylight shifting where one sunday has 25 instead of 24 hours
        ggplot2::geom_hline(yintercept = seq(0.5,
                                             7*dt_per_day + 0.5,
                                             by = dt_per_day) + if (T) {c(0,rep(2/fac,7))}else{0},
                            color = "white") +
        ggplot2::geom_point(data = subset(heatmap_data, Datetime %in% first_days),
                            aes(x = weeknum,y = test), pch = 3, color = "white") +
        ggplot2::facet_wrap('plotyear', ncol = 1) +
        ggplot2::scale_x_continuous(name = "Kalenderwoche (KW)",
                                    expand = c(0,0),
                                    breaks = seq(min(heatmap_data$weeknum,na.rm = T),
                                                 max(heatmap_data$weeknum,na.rm = T),
                                                 length = 13),
                                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")) +
        ggplot2::scale_y_discrete(name = "Wochentag",
                                  expand = c(0, 0),
                                  limits = seq(dt_per_day/2 + 0.5 ,
                                               7*dt_per_day + 0.5,
                                               by = dt_per_day ),
                                  labels = rev(c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")))
    ) %>%
    plotly::ggplotly()

  return(heatmap_plotly)
}
