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
                                                        ,"tile_ID"
                                                        ,"aes"
                                                        ,"diff_num"
                                                        ,"tz"))
#' Heatmap Plot of Timeseries
#'
#' This function creates a plotly object with the heatmap of the timeseries data.frame.
#'
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
#' @importFrom ggplot2 theme
#'
#' @param data a data.frame with at least one column of POSIXct values.
#' @param tzone give the timezone format to be used. default = "UTC"
#' @param datetime_colname column name of the column with POSIXct values of the data.frame \code{data}
#' @param data_colname column name of the column with values to be plotted of the data.frame \code{data}
#' @param z_limits a vector of two numeric \code{c(min,max)} e.g \code{c(0,100)} to scale the heatmap colors.
#' @param x_label x axis label of the plot
#' @param y_label y axis label of the plot
#' @param LegendTitle legend title of the plot
#' @param na.value.col a color value for all na values in the heatmap
#' @return a heatmap of the timeseries data as plotly object
#' @export
tsheatmap <- function(data,data_colname = NULL,tzone = "UTC",datetime_colname = "Datetime",z_limits = NULL,x_label = "ISO week",y_label = "Day of the week",LegendTitle = "",na.value.col = "red",ggplot = F){
  # data = data.frame with an unduplicated timeseries column in POSIXct format
  # tzone = string of the timezone the plot shall be
  #
  #
  #TODO:  add arguments to aggregate data within the tsheatmap function
  #       timesteps = lubridate::as.difftime(1, units = "hours"),

  # browser()
  #TODO:  check number of columns of data.frame
  #TODO:  check if datetime column is available & if it is POSIXct
  #
  #TODO: Check input data
  #
  # Check if data_colname is given:
  if(is.null(data_colname)){
    c_names <-
      data %>%
      dplyr::select(-(datetime_colname)) %>%
      dplyr::select_if(is.numeric) %>%
      base::colnames()
    data_colname <- c_names[1] # choose first column of data frame if data_colname is.null
    warning("data_colname argument empty: Choosing first numeric column named \"",data_colname,"\"")
    }


  # TODO: make it a function
  dt_per_day <-
    diff(data[,datetime_colname]) %>%
    unique.POSIXlt() %>%
    abs() %>%
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


  # set reference time difference ----
  ref_difftime <- as.difftime(1,units = "hours")

  # get the major time difference of the data frames datetime column ----
  timediff <- diff(data[,datetime_colname])
  timediff_unit <- timediff %>% units()
  #TODO: check if unique is of length 1 and which time difference to take if there are multiple
  tdiff <- unique(timediff) %>% as.difftime(units = timediff_unit) %>% abs() %>% min()

  # set reference time difference unit to major time difference ----
  units(tdiff) <- units(ref_difftime)

  # factor which will be used to count the numbers of tiles in one day
  fac = as.numeric(tdiff)


  # parameter to loop through indexes to be used in geom_tile
  dayfactor = c(d1 = 1
                ,d7 = 2
                ,d6 = 3
                ,d5 = 4
                ,d4 = 5
                ,d3 = 6
                ,d2 = 7)

  # preparing Heatmap Data ----
  #TODO: datetime column is renamed here to Datetime and Datetime is
  #      used allover the rest of the code. Consider using the user supllied name
  #      of the datetime column or rename Datetime column after all calculations.

  heatmap_data <-
    data %>%
    dplyr::select((datetime_colname),
                  Data = (data_colname)) %>%
    dplyr::mutate(Datetime = lubridate::with_tz(Datetime,tzone = tzone)) %>%
    dplyr::mutate(weeknum = lubridate::isoweek(Datetime)
                  ,date = paste0(date(Datetime)," 00:00:00") %>%
                    as.POSIXct(tz=tzone)
                  ,year = lubridate::year(Datetime)
                  ,month = lubridate::month(Datetime)
                  ,weeknum = as.numeric(strftime(date, format = "%V"))
                  ,daylightsaving = lubridate::dst(Datetime)
    ) %>%
    dplyr::group_by(weeknum) %>%
    dplyr::mutate(weekly_datapoints = n()) %>%
    dplyr::ungroup()


  # Check for Datetime issues. ----
  # future feature ?
  #
  # get_expected_timeseries(data
  #                         ,tz = "UTC"
  #                         ,timestep = lubridate::as.difftime(1,units = "hours")
  #                         ,format = "%Y-%m-%d %H:%M:%S")
  #
  # get_unexpected_datetimes(data
  #                          ,expected_timezone = "UTC"
  #                          ,expected_timesteps = lubridate::as.difftime(1,units = "hours")
  #                          ,format = "%Y-%m-%d %H:%M:%S"
  #                          ,show_offset = c(0))
  # show_unexpected_datetimes(heatmap_data
  #                           ,expected_timezone = "Europe/Zurich"
  #                           ,expected_timesteps = lubridate::as.difftime(1,units="hours")
  #                           ,format = "%Y-%m-%d %H:%M:%S")

  #TODO: Check if this code is still needed
  # # correct for January dates belong to KW 52 oder 53
  # heatmap_data$weeknum[which((heatmap_data$month == 1) & (heatmap_data$weeknum == 52 | heatmap_data$weeknum == 53))] = 0
  #
  # # correct for December dates belong to KW 1
  # heatmap_data$weeknum[which((heatmap_data$month == 12) & (heatmap_data$weeknum == 1))] = 53
  # #
  #


  # Calculate everything on daily basis by grouping by weeks and days -----
  (
    heatmap_data <-
      heatmap_data %>%
      dplyr::group_by(weeknum,date) %>%
      dplyr::mutate(
        min_day_datetime = min(Datetime),
        tz = lubridate::tz(Datetime),
        diff_num = dt_per_day - (
          difftime(Datetime, date, units = units(tdiff)) %>% as.numeric(units = units(tdiff)) /
            fac
        )
        ,
        tile_ID = dayfactor[paste0("d", lubridate::wday(date))] * dt_per_day + diff_num + 1 /
          fac
      ) %>%
      dplyr::mutate(
        datapoints = dplyr::n(),
        daylightsavinghours_per_day = sum(daylightsaving),
        is_day_of_timeshift = ifelse(
          !(daylightsavinghours_per_day %in% c(0, dt_per_day)),
          T,
          F)
      ) %>%
      # filter(is_day_of_timeshift & datapoints > 24) %>%
      # cerrecting timeshifts of day light saving
      #
      #TODO: Daylight saving in october position of hours (see: Ternadata)
      dplyr::mutate(
        tile_ID = dplyr::if_else(datapoints <= dt_per_day, tile_ID + 1 / fac, tile_ID)
        ,
        tile_ID = dplyr::if_else(
          is_day_of_timeshift &
            datapoints > dt_per_day,
          tile_ID + 1 / fac,
          tile_ID
        )
        ,
        tile_ID = dplyr::if_else(
          is_day_of_timeshift &
            daylightsaving &
            datapoints <= dt_per_day ,
          tile_ID - 1 / fac,
          tile_ID
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        weekday = weekdays_abbr[lubridate::wday(x = date,
                                                week_start = getOption("lubridate.week.start", 1))],
        plotyear = dplyr::if_else(weeknum == 53 &
                                    month == 1, year - 1, year),
        plotyear = dplyr::if_else(weeknum == 1 &
                                    month == 12, plotyear + 1, plotyear)
      )
  )

  # Check if there is any daylight saving involved in the applied timezone ----
  has_timeshift = lubridate::dst(heatmap_data$Datetime) %>% any


  if( is.null( z_limits ) | length(z_limits) != 2 & class(z_limits) == "numeric"){
    z_limits = c(min(heatmap_data$Data,na.rm = T),max(heatmap_data$Data,na.rm = T))
  }

  # find id of first day in month
  first_days <-
    heatmap_data$Datetime %>%
    firstDayMonth %>%
    as.POSIXct(tz = lubridate::tz(heatmap_data$Datetime))

  id_first_day = which(heatmap_data$Datetime %in% first_days)
  data_first_day = data.frame(x = heatmap_data$weeknum[id_first_day],
                              y = heatmap_data$tile_ID[id_first_day],
                              year = heatmap_data$year[id_first_day])



  # heatmap
  ret <-
    ggplot2::ggplot()  +
    ggplot2::geom_tile(data = heatmap_data
                       , aes(
                         x = weeknum,
                         y = tile_ID,
                         fill = Data,
                         text = sprintf(
                           "Datetime: %s<br>tz: %s<br>Day: %s<br>Value: %f<br>Week: %f<br>ID: %f<br>Diffnum: %f"
                           ,
                           Datetime,
                           tz,
                           weekday,
                           Data,
                           weeknum,
                           tile_ID,
                           diff_num
                         )
                       )) +
    # ggplot2::theme(legend.position = "bottom") +
    viridis::scale_fill_viridis(
      limits = z_limits,
      name = LegendTitle,
      # TODO: Legend name from function argument
      option = 'D',
      # plasma "D" = viridis
      direction = 1,
      na.value = na.value.col,
      # guide = guide_colorbar(
      #   direction = "horizontal",
      #   barheight = unit(2, units = "mm"),
      #   barwidth = unit(50, units = "mm"),
      #   draw.ulim = F,
      #   title.position = 'top',
      #   # some shifting around
      #   title.hjust = 0.5,
      #   label.hjust = 0.5
      # )
    ) +
    # added if else to consider daylight shifting where one sunday has 25 instead of 24 hours
    ggplot2::geom_hline(yintercept = (dayfactor + 1) * dt_per_day + 2 * 1 / fac + 0.5,
                        color = "white") +
    ggplot2::geom_point(
      data = subset(heatmap_data, Datetime %in% first_days),
      aes(x = weeknum, y = tile_ID),
      pch = 3,
      color = "white"
    ) +
    ggplot2::facet_wrap('plotyear', ncol = 1) +
    ggplot2::scale_x_continuous(
      name = x_label,
      expand = c(0, 0),
      breaks = seq(
        min(heatmap_data$weeknum, na.rm = T),
        max(heatmap_data$weeknum, na.rm = T),
        length = 13
      ),
      # minor_breaks = unique(heatmap_data$weeknum) %>% sort() , # TODO: add minor labels with weeknum see https://stackoverflow.com/questions/39717545/add-secondary-x-axis-labels-to-ggplot-with-one-x-axis
      labels = c(
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec",
        "Jan"
      )
    ) +
    ggplot2::scale_y_discrete(
      name = y_label,
      expand = c(0, 0),
      limits = (dayfactor + 1) * dt_per_day - dt_per_day /
        2 + 1 / fac + 0.5,
      labels = rev(c(
        "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
      ))
    )


  if (ggplot) {
    ret <- plotly::ggplotly(ret)
  }

  return(ret)
}
