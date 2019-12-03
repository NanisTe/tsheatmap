# tsheatmap
Creating heatmaps of yearly timeseries data for pattern visualisation of calendar related data.

Use a data.frame with a column with POSIXct datetime series and some numerical data.

## Output

Standard Output of tsheatmap is a plotly plot and can be changed to ggplot with the argument ggplot = T.

## Defaults:
data,
data_colname = NULL,
tzone = "UTC",
datetime_colname = "Datetime",
z_limits = NULL,
x_label = "ISO week",
y_label = "Day of the week",
LegendTitle = "",
na.value.col = "red",
ggplot = F

## Arguments description:
data
  Data.frame with at least a column of POSIXct values and a numeric data column

data_colname = NULL,
  The column name of the data.frame which holds the data.

tzone = "UTC",
  Set the timezone you would the data like to be plotted.
  
datetime_colname = "Datetime",
  Set the column name of the data.frame which holds the datetime information.
  
z_limits = NULL,
  Set upper and lower limits of the data column which will be used in scaling the colorbar / legend.
  
x_label = "ISO week",
  Set label of the x-axis
  
y_label = "Day of the week",
  Set label of the y-Axis
  
LegendTitle = "",
  Set a legend title.
  
na.value.col = "red",
  Color of tiles which have NA as value.
  
ggplot = FALSE
  Change function output from plotly plot to ggplot.

More detailed introduction coming soon.
