# =====================================================
# Stat133: Final Project
# Description: Visualization
# =====================================================

# Name: Paul Park
# SID : 23046317

# =====================================================
#  Pre processing of data
# =====================================================
# call the package "maps" and "ggplot2"
library("maps")
library("ggplot2")
library("dplyr")

# assign data of East Pacific basins to "raw2" and those of North Atlantic basins to "raw3"
raw2 <- read.csv("rawdata/rawdata2.csv", header = FALSE, stringsAsFactors = FALSE)
raw3 <- read.csv("rawdata/rawdata3.csv", header = FALSE, stringsAsFactors = FALSE)

# manipulate the files so that they are easier to work with
raw2 <- raw2[, c(1, 7, 9, 10, 11)]
raw3 <- raw3[, c(1, 7, 9, 10, 11)]

colnames(raw2) <- c("id", "date", "lat", "long", "wind")
colnames(raw3) <- c("id", "date", "lat", "long", "wind")

raw2 <- raw2[-(1:3), ]
raw3 <- raw3[-(1:3), ]

raw2$long <- as.numeric(raw2$long)
raw2$lat <- as.numeric(raw2$lat)
raw2$wind <- as.numeric(raw2$wind)
raw3$long <- as.numeric(raw3$long)
raw3$lat <- as.numeric(raw3$lat)
raw3$wind <- as.numeric(raw3$wind)

# =====================================================
# Plotting part

# note that in all of the below graphs, warning messages
# will show up saying some rows containing missing values
# were removed. This is because the trajectories outside
# the window limits were cut off from the graphs.
# =====================================================
# extract the rows only from 1980 to 2010
extracted2_1 <- raw2[(head(which(substr(raw2$date, 1, 4) == 1980), 1):
                      tail(which(substr(raw2$date, 1, 4) == 2010), 1)), ]
extracted2_2 <- raw3[(head(which(substr(raw3$date, 1, 4) == 1980), 1):
                      tail(which(substr(raw3$date, 1, 4) == 2010), 1)), ]
extracted2 <- full_join(extracted2_1, extracted2_2)

# make sure that the trajectories are all connected like they are meant to be
extracted2$long[which(extracted2$long > 100)] <- extracted2$long[which(extracted2$long > 100)] - 360

# produce a plot showing the trajectory of all storms(1980-2010)
world_map <- map_data("world")

ggplot(extracted2, aes(x = long, y = lat, group = id)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_path(aes(col = wind)) +
  scale_x_continuous(name = "longitude", limits = c(-180, 0), breaks = seq(-180, 0, by = 20)) +
  scale_y_continuous(name = "latitude", limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  theme(panel.background = element_rect(fill = '#FFFAFA')) +
  ggtitle("The trajectory of all storms from 1980 to 2010") +
  coord_cartesian(xlim = c(-180, 0), ylim = c(0, 90)) +
  ggsave("images/trajectory_all.png") +
  ggsave("images/trajectory_all.pdf")

# produce a plot showing the trajectory of all storms per month[1980-2010] (one facet per month)
idcount2 <- numeric(0)
for (i in 1:length(unique(extracted2$id))) {
  idcount2[i] <- length(which(extracted2$id == unique(extracted2$id)[i]))
}

extracted2$month <- numeric(nrow(extracted2))
for (i in 1:length(unique(extracted2$id))) {
  extracted2$month[head(which(extracted2$id == unique(extracted2$id)[i]), 1)] <- 
    as.numeric(substr(head(extracted2$date[extracted2$id == unique(extracted2$id)[i]], 1), 6, 7))
}
month <- extracted2$month[-(which(extracted2$month == 0))]
month <- rep(month, times = idcount2)
extracted2$month <- month

ggplot(extracted2, aes(x = long, y = lat, group = id)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_path(aes(col = wind)) +
  scale_x_continuous(name = "longitude", limits = c(-180, 0), breaks = seq(-180, 0, by = 45)) +
  scale_y_continuous(name = "latitude", limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  theme(panel.background = element_rect(fill = '#FFFAFA')) +
  ggtitle("The trajectory of all storms per month from 1980 to 2010") +
  coord_cartesian(xlim = c(-180, 0), ylim = c(0, 90)) +
  facet_wrap(~month) +
  ggsave("images/trajectory_month.png") +
  ggsave("images/trajectory_month.pdf")

# produce a plot showing the trajectory of storms in decade 1980s (one facet per year)
extracted3_1 <- raw2[(head(which(substr(raw2$date, 1, 4) == 1980), 1):
                      tail(which(substr(raw2$date, 1, 4) == 1989), 1)), ]
extracted3_2 <- raw3[(head(which(substr(raw3$date, 1, 4) == 1980), 1):
                      tail(which(substr(raw3$date, 1, 4) == 1989), 1)), ]
extracted3 <- full_join(extracted3_1, extracted3_2)

extracted3$long[which(extracted3$long > 100)] <- extracted3$long[which(extracted3$long > 100)] - 360

extracted3$year <- substr(extracted3$date, 1, 4)

# you can check if there are occasions where storms started and ended in other years by using
extracted3[substr(extracted3$date, 6, 7) == '01',]

ggplot(extracted3, aes(x = long, y = lat, group = id)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_path(aes(col = wind)) +
  theme(panel.background = element_rect(fill = '#EEE5DE')) +
  scale_x_continuous(name = "longitude", limits = c(-180, 0), breaks = seq(-180, 0, by = 45)) +
  scale_y_continuous(name = "latitude", limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  ggtitle("The trajectory of all storms from 1980 to 1989") +
  coord_cartesian(xlim = c(-180, 0), ylim = c(0, 90)) +
  facet_wrap(~year) +
  ggsave("images/trajectory_1980s.png") +
  ggsave("images/trajectory_1980s.pdf")

# produce a plot showing the trajectory of storms in decade 1990s (one facet per year)
extracted4_1 <- raw2[(head(which(substr(raw2$date, 1, 4) == 1990), 1):
                      tail(which(substr(raw2$date, 1, 4) == 1999), 1)), ]
extracted4_2 <- raw3[(head(which(substr(raw3$date, 1, 4) == 1990), 1):
                      tail(which(substr(raw3$date, 1, 4) == 1999), 1)), ]
extracted4 <- full_join(extracted4_1, extracted4_2)

extracted4$long[which(extracted4$long > 100)] <- extracted4$long[which(extracted4$long > 100)] - 360

extracted4$year <- substr(extracted4$date, 1, 4)

# you can check if there are occasions where storms started and ended in other years by using
extracted4[substr(extracted4$date, 6, 7) == '01',]

ggplot(extracted4, aes(x = long, y = lat, group = id)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_path(aes(col = wind)) +
  theme(panel.background = element_rect(fill = '#EEE5DE')) +
  scale_x_continuous(name = "longitude", limits = c(-180, 0), breaks = seq(-180, 0, by = 45)) +
  scale_y_continuous(name = "latitude", limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  ggtitle("The trajectory of all storms from 1990 to 1999") +
  coord_cartesian(xlim = c(-180, 0), ylim = c(0, 90)) +
  facet_wrap(~year) +
  ggsave("images/trajectory_1990s.png") +
  ggsave("images/trajectory_1990s.pdf")

# produce a plot showing the trajectory of storms in decade 2000s (one facet per year)
extracted5_1 <- raw2[(head(which(substr(raw2$date, 1, 4) == 2000), 1):
                      tail(which(substr(raw2$date, 1, 4) == 2009), 1)), ]
extracted5_2 <- raw3[(head(which(substr(raw3$date, 1, 4) == 2000), 1):
                      tail(which(substr(raw3$date, 1, 4) == 2009), 1)), ]
extracted5 <- full_join(extracted5_1, extracted5_2)

extracted5$long[which(extracted5$long > 100)] <- extracted5$long[which(extracted5$long > 100)] - 360

extracted5$year <- substr(extracted5$date, 1, 4)

# you can check if there are occasions where storms started and ended in other years by using
extracted5[substr(extracted5$date, 6, 7) == '01',]

ggplot(extracted5, aes(x = long, y = lat, group = id)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_path(aes(col = wind)) +
  theme(panel.background = element_rect(fill = '#EEE5DE')) +
  scale_x_continuous(name = "longitude", limits = c(-180, 0), breaks = seq(-180, 0, by = 45)) +
  scale_y_continuous(name = "latitude", limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  ggtitle("The trajectory of all storms from 2000 to 2009") +
  coord_cartesian(xlim = c(-180, 0), ylim = c(0, 90)) +
  facet_wrap(~year) +
  ggsave("images/trajectory_2000s.png") +
  ggsave("images/trajectory_2000s.pdf")