## clear workspace
rm(list=ls())

### load libraries
library(dplyr)
library(ggplot2)
library(data.table)
library(ggplot2)

library(pryr) # used for exploring memory usage
library(lineprof) # used for line profiling
library(microbenchmark) # used for comparing times
library(zoo) # used to fill in NA's with linear interpolation
library(gridExtra) # used to show plots next to each other
library(leaflet) # used for maps
library(treemapify)



setwd("C:/Users/joaka/Desktop/Machine Learning Projects/grocery_sales_forecasting")

# fread is by far the fastest!
#microbenchmark(
#train_n1 = read.csv("train.csv", nrows=300000, colClasses=c("integer","character","integer","integer","numeric","character"), stringsAsFactors = FALSE),
#train_n2 = read.csv("train.csv", nrows=300000, stringsAsFactors = FALSE),
#train_n3 = read.table("train.csv", nrows=300000, header=TRUE, sep=",", stringsAsFactors = FALSE),
#train_n4 = fread("train.csv", nrows=300000, stringsAsFactors = FALSE),
#times = 5L
#)

### Read train set

# we know the total number of rows in the dataset
n_total = 125497040

# in order to read a smaller part of the dataset adjust n_read
n_read = 40000000
n_skip = n_total - n_read

headers = fread("train.csv", nrows=1, header=FALSE, stringsAsFactors = FALSE)
train_n = fread("train.csv", nrows=n_read, skip=n_skip , colClasses=c("integer","date","integer","integer","numeric","logical"), stringsAsFactors = FALSE)
colnames(train_n) = unlist(headers)

# as.Date is very time consuming BUT if you specify the format it becomes much faster!
#train_n$date = as.Date(train_n$date)
train_n$date = as.Date(train_n$date, format="%Y-%m-%d")

train_n_flt = train_n %>% filter(date >= "2016-08-14" & date <= "2017-08-14") 

# with substr(), R ran out of memory. 
# as.numeric(format(...)) works but still consumes a lot of memory (went from 2.81GB to 4.02GB after running the following 3 lines)
train_n_flt$year = as.numeric(format(train_n_flt$date, "%Y"))
train_n_flt$month = as.numeric(format(train_n_flt$date, "%m"))
train_n_flt$day = as.numeric(format(train_n_flt$date, "%d"))

# Returns are shown as negative sales. Besause we care about predicting sales we create an adj_sales column where 0 is assigned if sales<0.
train_n_flt$adj_sales = ifelse(train_n_flt$unit_sales < 0, 0 , train_n_flt$unit_sales)

# create summary data frames
train_n_month = train_n_flt %>% group_by(month) %>% summarize(tot_sales = sum(unit_sales),
                                                              tot_adj_sales = sum(adj_sales))
train_n_day = train_n_flt %>% group_by(day) %>% summarize(tot_sales = sum(unit_sales),
                                                        avg_sales = mean(unit_sales))

train_n_ts = train_n_flt %>% group_by(date) %>% summarize(tot_sales = sum(unit_sales),
                                                          tot_adj_sales = sum(adj_sales))
train_n_store = train_n_flt %>% group_by(store_nbr) %>% summarize(tot_sales = sum(unit_sales),
                                                                        tot_adj_sales = sum(adj_sales))
train_n_store_month = train_n_flt %>% group_by(month, store_nbr) %>% summarize(tot_sales = sum(unit_sales),
                                                                               tot_adj_sales = sum(adj_sales))
train_n_prom = train_n_flt %>% group_by(date, onpromotion) %>% summarize(avg_sales = mean(unit_sales),
                                                                         avg_adj_sales = mean(adj_sales))


# test = fread("test.csv", stringsAsFactors = FALSE)

#object_size(train_n_flt)
#object_size(test)
#mem_used()

### Load the rest of the files

## holidays_events file
holidays_events = read.csv("holidays_events.csv", stringsAsFactors = FALSE)
holidays_events$date = as.Date(holidays_events$date)

# check for duplicates
length(unique(holidays_events$date))

# since there are some duplicates check if the dates that are duplicate are actual holidays (not transferred) in all instances
holidays_events$date_transf = paste(holidays_events$date, holidays_events$transferred)
length(unique(holidays_events$date_transf))


# only keep holidays that were not transferred
holidays_events_actual = holidays_events[holidays_events$transferred == "False",]
# unique(holidays_events_actual$type)

# There are duplicate dates. This happens in the cases where a holiday is both local and national.
# None of those is transferred in the current train dataset so there shouldn't be an issue with the "match" below.
train_n_ts$holiday = holidays_events_actual$type[match(train_n_ts$date, holidays_events_actual$date)]
train_n_ts$holiday = ifelse(is.na(train_n_ts$holiday), FALSE, TRUE)

## items file
items = read.csv("items.csv", stringsAsFactors = FALSE)
train_n_flt$family = items$family[match(train_n_flt$item_nbr, items$item_nbr)]
train_n_flt$class = items$class[match(train_n_flt$item_nbr, items$item_nbr)]
train_n_flt$perishable = items$perishable[match(train_n_flt$item_nbr, items$item_nbr)]

# check for duplicates
length(unique(train_n_flt$item_nbr))
train_n_flt$item_nbr_family = paste(train_n_flt$item_nbr, train_n_flt$family)
length(unique(train_n_flt$item_nbr_family))

train_n_fam = train_n_flt %>% group_by(family) %>% summarize(tot_sales = sum(unit_sales),
                                                             tot_adj_sales = sum(adj_sales))

## oil file
oil = read.csv("oil.csv", stringsAsFactors = FALSE)
oil$date = as.Date(oil$date)

oil$month = as.numeric(format(oil$date, "%m"))
oil_month = oil %>% group_by(month) %>% summarize(avg_price = mean(price))

# fill in missing values using linear interpolation of elements before and after
# first fill in the very first value in the time series which is missing
oil[1,2] = oil[2,2]
oil$price = na.approx(oil$dcoilwtico)

# match is the fastest way to merge two data frames but it can be used only if there is a unique key in the second data frame for each value of the first
train_n_ts$oil_price = oil$price[match(train_n_ts$date, oil$date)]

## stores file
stores = read.csv("stores.csv", stringsAsFactors = FALSE)
train_n_store$store_cluster = stores$cluster[match(train_n_store$store_nbr, stores$store_nbr)]
train_n_store_month$store_cluster = stores$cluster[match(train_n_store_month$store_nbr, stores$store_nbr)]

## transactions file
transactions = read.csv("transactions.csv", stringsAsFactors = FALSE)
transactions$date = as.Date(transactions$date)

#sample_submission = read.csv("sample_submission.csv", stringsAsFactors = FALSE)

# Note: there is an important input missing from this dataset due to confidentiality issues and this is price.

summary(train_n)
summary(holidays_events)
summary(items)
summary(oil)
summary(stores)
summary(transactions)


### Create graphs

# sales are highest in December and lowest in September
ggplot(train_n_month, aes(month, tot_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12)
# ignoring returns
ggplot(train_n_month, aes(month, tot_adj_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12)
# returns do not make a big difference
grid.arrange(ggplot(train_n_month, aes(month, tot_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12), ggplot(train_n_month, aes(month, tot_adj_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12), ncol=2)

# average sales and total sales are highest on the first 5 days of the month and lowest towards the end of the month
ggplot(train_n_day, aes(day, tot_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:31)
ggplot(train_n_day, aes(day, avg_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:31)

ggplot(train_n_ts, aes(date)) + geom_line(aes(y=tot_sales)) + scale_x_date(date_breaks="months", date_labels="%b")

# total sales and oil price do not seem to be correlated
plot1 = ggplot(train_n_ts, aes(date, tot_sales)) + geom_line() + geom_smooth(method = "loess", color = "red", span = 1/5) + scale_x_date(date_breaks="months", date_labels="%b") + labs(y="Total Sales")
plot2 = ggplot(oil_flt, aes(date, price)) + geom_line() + geom_smooth(method = "loess", color = "red", span = 1/5) + scale_x_date(date_breaks="months", date_labels="%b") + labs(y="Oil Price")
grid.arrange(plot1, plot2, ncol=2)

# similar conclusion if we plot monthly bar charts
plot3 = ggplot(train_n_month, aes(month, tot_sales)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12)
plot4 = ggplot(oil_month, aes(month, avg_price)) + geom_bar(stat="identity") + scale_x_continuous(breaks=1:12)
grid.arrange(plot3, plot4, ncol=2)

# total sales are higher on holidays
ggplot(train_n_ts, aes(x=date, y=tot_sales, color=holiday)) + geom_point() + scale_x_date(date_breaks="months", date_labels="%b") + geom_smooth(method = "lm", se=FALSE)

# total sales by store cluster
# sales are higher for clusters 14, 6, 11, 8
ggplot(train_n_store, aes(x=reorder(store_cluster, -tot_sales), y=tot_sales)) + geom_bar(stat="identity", fill="steelblue")
ggplot(train_n_store_month, aes(x=month, y=tot_sales, color=store_cluster)) + geom_point() + scale_color_gradient(low="blue", high="red")

# sales time series by store cluster. Different clusters indeed show different behavior.
ggplot(train_n_store, aes(x = date, y = tot_sales, color = store_cluster)) + geom_line() + 
  facet_wrap(~store_cluster, ncol = 4, scale = "free_y") + 
  labs(x = '', y = 'Total unit sales', title = 'Sales by Store Cluster') + 
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust=1))


# on average sales are higher when there is promotion
ggplot(train_n_prom, aes(x=date, y=avg_sales, color=onpromotion)) + geom_point()

# top product categories sold
ggplot(train_n_fam, aes(family, tot_sales)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90))
# create ordered bar chart
# the top categories sold are Grocery I, Beverages, Produce, Cleaning and Dairy
train_n_fam_ord = train_n_fam[order(train_n_fam$tot_sales, decreasing = TRUE), ]  # sort
train_n_fam_ord$family = factor(train_n_fam_ord$family, levels = train_n_fam_ord$family)  # to retain the order in plot.
ggplot(train_n_fam_ord, aes(family, tot_sales)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90))

# another way to do the ordered bar chart
train_n_fam %>%
  ggplot(aes(x = reorder(as.factor(family), tot_sales), y = tot_sales)) +
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  labs(y = 'Total unit sales', x = 'Product Family', title = 'Total Sales by Product Family') +
  coord_flip()


# Experimenting with Leaflet (plot stores by city). City coordinates need to be rearraned. Cities are sorted alphabetically.

lat =  c(-0.2389, 0.0367141, -0.9316, -1.6636, 0.3516889, -1.5905, -1.4924, -1.2543, -2.1710, -2.227827,
          -1.8622, -1.8019, -1.0225, -2.6285, -2.2347644, -2.9001, -4.0079, -3.2581, -0.1807, 0.98333, -0.9677, -0.2714) 

lng = c(-79.1774, -78.1507, -78.6058, -78.6546, -78.1222, -78.9995, -78.0024, -78.6229, -79.9224, -80.9585,   
         -79.9777, -79.5346, -79.4604, -80.3896, -80.9002, -79.0059, -79.2113, -79.9554, -78.4678, -79.65, -80.7089, -79.4648)

stores %>% group_by(city) %>% summarize(num_stores = n()) %>%
  leaflet() %>% 
  setView(lat = -0.900653, lng = -78.467834, zoom = 7) %>% 
  addTiles() %>%
  addCircleMarkers(
    ~lng,
    ~lat,
    radius = ~ num_stores,
    label = ~ city
  )

# Experimenting with treemaps. Treemap of store types by state.

stores %>%
  group_by(state, type) %>%
  count() %>%
  ungroup() %>%
  mutate(n = log10(n+1)) %>%
  ggplot(aes(area = n, fill = type, label = type, subgroup = state)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  theme(legend.position = "null") +
  ggtitle("Store types grouped by states - tree map (logarithmic scaling)")