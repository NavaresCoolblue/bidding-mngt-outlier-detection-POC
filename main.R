require(reshape2)
require(mclust)
require(RAD)
source('anomality_detection.R')
library(ggplot2)

data = read.csv('data.csv')

# aux function
create_posixct = function(date, hour){
  first_part = paste(as.character(date), as.character(hour), sep = " ")
  as.POSIXct(paste(first_part, "00", "00", sep = ":"),
             format = "%Y-%m-%d %H:%M:%S")
}

# format data
ts = data[names(data) %in% c('datetime', 'external_customer_id', 
                             'hour_of_day', 'device', 'estimated_conversion_rate_index')]
ts$serie_id = paste(ts$external_customer_id, ts$device, sep = '.')
ts$datetime = create_posixct(ts$datetime, ts$hour_of_day)
ts = ts[!(names(ts) %in% c('external_customer_id', 'hour_of_day', 'device'))]
ts = reshape2::dcast(data = ts, formula = datetime~serie_id, 
                     value.var = 'estimated_conversion_rate_index')

# just for POC get only the ts without NAs
ts = ts[colSums(is.na(ts)) == 0]

# save datetime and remove it from ts for calculations
datetime = ts$datetime
ts = ts[, -c(1)] 

# model based clustering to group ts and determine how many clusters
clusters = mclust::Mclust(t(ts))
n_clusters = unique(clusters$classification) 

for (i in n_clusters){
  # get series which belong to cluster i
  indices = names(ts)[clusters$classification == i]
  cluster_ts = ts[, names(ts) %in% indices]
  if (i == 1) {
    outliers = AnomalyDetection.rpca(cluster_ts)  
  } else {
    outliers = rbind(outliers, AnomalyDetection.rpca(cluster_ts))
  }
}

# format df
regex = '(.*)\\.(.*)\\.(.*)'
outliers$id = gsub(regex,'\\1',  rownames(outliers), perl = TRUE)
outliers$device = gsub(regex,'\\2',  rownames(outliers), perl = TRUE)

id_select = "4652505709"
device_select = "Desktop"

filter_rows = (outliers$id == id_select) & (outliers$device == device_select)
data_chart = outliers[filter_rows, ]

# recover datetime
data_chart$time = datetime
dates_full = data.frame(time = seq(from = data_chart$time[1],
                                   to = tail(data_chart$time, 1),
                                   by = 'hour'))
# complete ts, dont want fancy lines in it
plot_data = merge(dates_full, data_chart, by = 'time', all.x = TRUE)

p = ggplot2::ggplot(data = plot_data, ggplot2::aes(x = time, y = X_original)) + 
  ggplot2::geom_line() + 
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = subset(plot_data, abs(S_transform) > 0), color = "red",
                      ggplot2::aes(size = S_transform^2))
  

p




