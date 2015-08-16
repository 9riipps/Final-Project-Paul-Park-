# =====================================================
# Stat133: Final Project
# Description: Data analysis
# =====================================================

# Name: Paul Park
# SID : 23046317

# =====================================================
# Bring clean data frames from the first part and modify them
# =====================================================

# call useful packages and read csv files obtained from the data processing part
library("stringr")
library("dplyr")

storms <- read.csv("data/storms.csv", header = TRUE, stringsAsFactors = FALSE)
storms$date <- as.Date(storms$date, "%m/%d/%Y")
tracks <- read.csv("data/tracks.csv", header = TRUE, stringsAsFactors = FALSE)
tracks$date <- as.Date(tracks$date, "%m/%d/%Y")

combined <- left_join(tracks, storms, by = 'id')
combined <- combined[-9]

extracted <- storms[(head(which(substr(storms$date, 1, 4) == 1980), 1):
                       tail(which(substr(storms$date, 1, 4) == 2010), 1)), ]

# =====================================================
# Plotting part
# =====================================================

# obtain frequencies and barplots for number of storms per year

num_storms_per_year <- as.numeric(substr(extracted$date, 1, 4))
write.csv(data.frame(table(num_storms_per_year)), "data/num_storms_per_year.csv", row.names = FALSE)
barplot(table(num_storms_per_year), main = "Number of storms per year", ylim = c(0, 35))
dev.copy(png, file = "images/num_storms_per_year.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_year.pdf")
dev.off()

# obtain frequencies and barplots for number of storms per year with winds >= 35 knots

max_wind <- numeric(0)
for (i in (head(which(substr(storms$date, 1, 4) == 1980), 1):
             tail(which(substr(storms$date, 1, 4) == 2010), 1))) {
  max_wind[i] <- max(combined$wind[combined$id == i])
}
max_wind <- na.omit(max_wind)

num_storms_per_year_35 <- as.numeric(substr(extracted$date[which(max_wind >= 35)], 1, 4))
write.csv(data.frame(table(num_storms_per_year_35)), "data/num_storms_per_year_35.csv", row.names = FALSE)
barplot(table(num_storms_per_year_35), main = "Numer of storms per year(>= 35 knots)", ylim = c(0, 35))
dev.copy(png, file = "images/num_storms_per_year_35.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_year_35.pdf")
dev.off()

# obtain frequencies and barplots for number of stomrs per year with winds >= 64 knots

num_storms_per_year_64 <- as.numeric(substr(extracted$date[which(max_wind >= 64)], 1, 4))
write.csv(data.frame(table(num_storms_per_year_64)), "data/num_storms_per_year_64.csv", row.names = FALSE)
barplot(table(num_storms_per_year_64), main = "Number of storms per year(>= 64 knots)", ylim = c(0, 20))
dev.copy(png, file = "images/num_storms_per_year_64.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_year_64.pdf")
dev.off()

# obtain frequencies and barplots for number of storms per year with winds >= 96 knots

num_storms_per_year_96 <- as.numeric(substr(extracted$date[which(max_wind >= 96)], 1, 4))
write.csv(data.frame(table(num_storms_per_year_96)), "data/num_storms_per_year_96.csv", row.names = FALSE)
barplot(table(num_storms_per_year_96), main = "Number of storms per year(>= 96 knots)", ylim = c(0, 10))
dev.copy(png, file = "images/num_storms_per_year_96.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_year_96.pdf")
dev.off()

# obtain frequencies and barplots for number of storms per month

num_storms_per_month <- as.numeric(substr(extracted$date, 6, 7))
write.csv(data.frame(table(num_storms_per_month)), "data/num_storms_per_month.csv", row.names = FALSE)
barplot(table(num_storms_per_month), main = "Number of storms per month", ylim = c(0, 200))
dev.copy(png, file = "images/num_storms_per_month.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_month.pdf")
dev.off()

# obtain frequencies and barlots for number of storms per month with winds >= 35 knots

num_storms_per_month_35 <- as.numeric(substr(extracted$date[which(max_wind >= 35)], 6, 7))
write.csv(data.frame(table(num_storms_per_month_35)), "data/num_storms_per_month_35.csv", row.names = FALSE)
barplot(table(num_storms_per_month_35), main = "Number of storms per month(>= 35 knots)", ylim = c(0, 200))
dev.copy(png, file = "images/num_storms_per_month_35.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_month_35.pdf")
dev.off()

# obtain frequencies and barplots for number of storms per month with winds >= 64 knots

num_storms_per_month_64 <- as.numeric(substr(extracted$date[which(max_wind >= 64)], 6, 7))
write.csv(data.frame(table(num_storms_per_month_64)), "data/num_storms_per_month_64.csv", row.names = FALSE)
barplot(table(num_storms_per_month_64), main = "Number of storms per month(>= 64 knots)", ylim = c(0, 100))
dev.copy(png, file = "images/num_storms_per_month_64.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_month_64.pdf")
dev.off()

# obtain frequencies and barplots for number of storms per month with winds >= 96 knots

num_storms_per_month_96 <- as.numeric(substr(extracted$date[which(max_wind >= 96)], 6, 7))
write.csv(data.frame(table(num_storms_per_month_96)), "data/num_storms_per_month_96.csv", row.names = FALSE)
barplot(table(num_storms_per_month_96), main = "Number of storms per month(>= 96 knots)", ylim = c(0, 50))
dev.copy(png, file = "images/num_storms_per_month_96.png")
dev.off()
dev.copy2pdf(file = "images/num_storms_per_month_96.pdf")
dev.off()

# obtain annual average numbers of storms with maximum wind speed of at least 35 knots
# also find the 1st, 2nd, and 3rd quartiles and the standard deviation

mean_35 <- mean(table(num_storms_per_year_35))
sd_35 <- sd(table(num_storms_per_year_35))
q1_35 <- as.numeric(quantile(table(num_storms_per_year_35))[2])
q2_35 <- as.numeric(quantile(table(num_storms_per_year_35))[3])
q3_35 <- as.numeric(quantile(table(num_storms_per_year_35))[4])

summary_35 <- data.frame(mean = mean_35,
                         sd = sd_35,
                         q1 = q1_35,
                         q2 = q2_35,
                         q3 = q3_35)
rownames(summary_35) <- "35 knots"

# obtain annual average numbers of storms with maximum wind speed of at least 64 knots
# also find the 1st, 2nd, and 3rd quartiles and the standard deviation

mean_64 <- mean(table(num_storms_per_year_64))
sd_64 <- sd(table(num_storms_per_year_64))
q1_64 <- quantile(table(num_storms_per_year_64))[2]
q2_64 <- quantile(table(num_storms_per_year_64))[3]
q3_64 <- quantile(table(num_storms_per_year_64))[4]

summary_64 <- data.frame(mean = mean_64,
                         sd = sd_64,
                         q1 = q1_64,
                         q2 = q2_64,
                         q3 = q3_64)
rownames(summary_64) <- '64 knots'

# obtain annual average numbers of storms with maximum wind speed of at least 96 knots
# also find the 1st, 2nd, and 3rd quartiles and the standard deviation

mean_96 <- mean(table(num_storms_per_year_96))
sd_96 <- sd(table(num_storms_per_year_96))
q1_96 <- quantile(table(num_storms_per_year_96))[2]
q2_96 <- quantile(table(num_storms_per_year_96))[3]
q3_96 <- quantile(table(num_storms_per_year_96))[4]

summary_96 <- data.frame(mean = mean_96,
                         sd = sd_96,
                         q1 = q1_96,
                         q2 = q2_96,
                         q3 = q3_96)
rownames(summary_96) <- '96 knots'

summary_all <- full_join(summary_35, summary_64)
summary_all <- full_join(summary_all, summary_96)

rownames(summary_all) <- c('35 knots', '64 knots', '96 knots')
write.csv(summary_all, "data/summary_all.csv")

# produce a plot for the regression line of mean pressure and mean wind speed for each storm
mean_press <- numeric(0)
for (i in (head(which(substr(storms$date, 1, 4) == 1980), 1):tail(which(substr(storms$date, 1, 4) == 2010), 1))) {
  mean_press[i] <- mean(combined$press[combined$id == i])
}
mean_press <- na.omit(mean_press)

indices_mean_press <- which(mean_press == 0)

mean_press <- mean_press[-indices_mean_press]

mean_wind <- numeric(0)
for (i in (head(which(substr(storms$date, 1, 4) == 1980), 1):
             tail(which(substr(storms$date, 1, 4) == 2010), 1))) {
  mean_wind[i] <- mean(combined$wind[combined$id == i])
}
mean_wind <- na.omit(mean_wind)

mean_wind <- mean_wind[-indices_mean_press]

# save it as a png file and a pdf file in the directory
plot.new()
plot.window(xlim = c(600, 1100), xaxs = "i",
            ylim = c(20, 100), yaxs = "i")
z <- lm(mean_wind~mean_press)
abline(h = seq(20, 100, 10), v = seq(600, 1100, 20), col = "#d3d3d3FF")
abline(a = coef(z)[1], b = coef(z)[2], lwd = 2, col = "#FF0000")
points(mean_press, mean_wind)
axis(1)
axis(2, las = 1)
box()
title("Regression Line(pressure/wind speed for mean)",
      xlab = "mean pressure", ylab = "mean wind speed")
dev.copy(png, file = "images/regression_mean.png")
dev.off()
dev.copy2pdf(file = "images/regression_mean.pdf")
dev.off()

# produce a plot for the regression line of median pressure and median wind speed for each storm
median_press <- numeric(0)
for (i in (head(which(substr(storms$date, 1, 4) == 1980), 1):
             tail(which(substr(storms$date, 1, 4) == 2010), 1))) {
  median_press[i] <- median(combined$press[combined$id == i])
}
median_press <- na.omit(median_press)

indices_median_press <- which(median_press == 0)

median_press <- median_press[-indices_median_press]

median_wind <- numeric(0)
for (i in (head(which(substr(storms$date, 1, 4) == 1980), 1):
             tail(which(substr(storms$date, 1, 4) == 2010), 1))) {
  median_wind[i] <- median(combined$wind[combined$id == i])
}
median_wind <- na.omit(median_wind)

median_wind <- median_wind[-indices_median_press]


# save it as a png file and a pdf file in the directory
plot.new()
plot.window(xlim = c(900, 1100), xaxs = "i",
     ylim = c(10, 120), yaxs = "i")
z2 <- lm(median_wind~median_press)
abline(h = seq(10, 120, 10), v = seq(900, 1100, 10), col = "#d3d3d3FF")
abline(a = coef(z2)[1], b = coef(z2)[2], lwd = 2, col = "#FF0000")
points(median_press, median_wind)
axis(1)
axis(2, las = 1)
box()
title("Regression Line for median(pressure/wind speed)",
      xlab = "median pressure", ylab = "median wind speed")
dev.copy(png, file = "images/regression_median.png")
dev.off()
dev.copy2pdf(file = "images/regression_median.pdf")
dev.off()
