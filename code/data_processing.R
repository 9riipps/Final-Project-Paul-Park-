# =====================================================
# Stat133: Final Project
# Description: Data processing
# =====================================================

# Name: Paul Park
# SID : 23046317

# =====================================================
# Preprocessing of data to construct a new data frame "storms.csv"
# =====================================================

# call the package "stringr"
library("stringr")
library("dplyr")

# call the rawdata by using one of the read.table functions
raw <- read.csv("rawdata/rawdata.hdat", sep = "", header = FALSE)

# extract those rows that are in "header" format from "raw"
file1 <- raw[grep("SNBR", (raw$V5:raw$V6)),]

# get id's
id <- 1:nrow(file1)

# get dates
date <- file1$V2 # I won't run as.Date now since the plot you have on the instruction slides have character values under date column

# get days
merged <- paste(file1[, 3], file1[, 4])
days <- as.numeric(gsub("M=", "", str_extract(merged, "M=[[:space:]]?\\d?\\d?\\d")))

# get names
raw1_2 <- readLines("rawdata/rawdata.hdat")
name <- substr(raw1_2[grep("SNBR", raw1_2)], 36, 47)
name <- gsub("\\s", "", name)

# combine all these columns into a data.frame and save it as "storms.csv"
data1 <- data_frame(id, date, days, name)
write.csv(data1, "data/storms.csv", row.names = FALSE)

# =====================================================
# Preprocessing to construct a new data frame "tracks.csv"
# =====================================================

raw1_3 <- read.csv("rawdata/rawdata.hdat", header = FALSE)

# get id counts
idcount <- as.numeric(gsub("M=", "", str_extract(raw1_3[,1], "M=\\s?\\d?\\d?\\d")))
idcount <- na.omit(idcount)
idcount <- as.numeric(idcount)

# get id
id2 <- rep(id, times = idcount*4)

# get dates
date2 <- str_extract(raw1_3[, 1], "\\d\\d/\\d\\d(/\\d\\d\\d\\d)?")

# get rid of the rows that are in "trailer" format
date2 <- na.omit(date2)
date2 <- as.character(date2)

indices_date <- grep("/\\d\\d\\d\\d", date2) # these indices represent those rows that are in "header" format

fdate <- character(0)
for (i in 1:(length(indices_date) - 1)) {
  fdate <- c(fdate, paste((date2[(indices_date[i] + 1):(indices_date[i + 1] - 1)]),
                          gsub("\\d\\d/\\d\\d", "", date2[indices_date[i]]), sep =""))
}

fdate <- c(fdate, paste((date2[(tail(indices_date, 1) + 1):length(date2)]),
                        gsub("\\d\\d/\\d\\d", "", date2[tail(indices_date, 1)]), sep = ""))

date2 <- fdate

grep("01/01", date2)
grep("12/31", date2) # note that in two occasions, storms lasted two consecutive years.
date2[6192:6197] <- gsub("1954", "1955", date2[6192:6197])
date2[11842:11848] <- gsub("2005", "2006", date2[11842:11848])

date2 <- rep(date2, each = 4) # Again I won't run as.Date now since the plot you have on the instruction slides have character values under date column

# get periods
period2 <- character(0)

for (i in 1:length(idcount)) {
  period2 <- c(period2, rep(c('00h', '06h', '12h', '18h'), times = idcount[i]))
}

# get stages
stage2 <- str_extract_all(gsub("\\*$", "", raw1_2), "([[:digit:]][*SWEL][[:digit:]]?)")
stage2 <- unlist(stage2)
stage2 <- gsub("[[:digit:]]", "", stage2)
stage2 <- gsub("\\*", "cyclone", stage2)
stage2 <- gsub("S", "subtropical", stage2)
stage2 <- gsub("E", "extratropical", stage2)
stage2 <- gsub("W", "wave", stage2)
stage2 <- gsub("L", "remanent low", stage2)

# get latitudes
latitude2 <- str_extract_all(raw1_2, "(\\d[*SEWL](\\d|\\s)(\\d|\\s)\\d(\\d|\\s)(\\d|\\s)(\\d|\\s)\\d)")
latitude2 <- unlist(latitude2)
latitude2 <- gsub("\\d[*SEWL]", "", latitude2)

# =====================================================
# meanwhile save the indices wih all zero values so we can subset them out later
indices_all_zero <- grep("0000000", latitude2)
indices_all_zero2 <- grep("0   0", latitude2)
indices_final <- c(indices_all_zero, indices_all_zero2)
# =====================================================

latitude2 <- substr(latitude2, 1, 3)
latitude2 <- as.numeric(latitude2) / 10

# get longitudes
longitude2 <- str_extract_all(raw1_2, "(\\d[*SEWL](\\d|\\s)(\\d|\\s)\\d(\\d|\\s)(\\d|\\s)(\\d|\\s)\\d)")
longitude2 <- unlist(longitude2)
longitude2 <- gsub("(\\d)[*SEWL]", "", longitude2)
longitude2 <- substr(longitude2, 4, 7)
longitude2 <- as.numeric(longitude2) / 10
longitude2[which(longitude2 > 180)] <- longitude2[which(longitude2 > 180)] - 360

# get wind speeds

wind2 <- str_extract_all(raw1_2[-grep("SNBR|SRC", raw1_2)], "[*SEWL].{11}")
wind2 <- unlist(wind2)
wind2 <- gsub("[*SEWL].{7}", "", wind2)
wind2 <- as.numeric(wind2)

# get pressures

press2 <- str_extract_all(raw1_2[-grep("SNBR", raw1_2)], "( \\d?\\d?\\d?\\d[*SEWL])")
press2 <- unlist(press2)
press2 <- gsub("[*SEWL]", "", press2)
press2 <- as.numeric(press2)

# combine all these columns into a data.frame and save it as "tracks.csv"
data2 <- data_frame(id2, date2, period2, stage2, latitude2, longitude2, wind2, press2)
colnames(data2) <- c("id", "date", "period", "stage", "lat", "long", "wind", "press")

data2 <- data2[-indices_final, ]

write.csv(data2, "data/tracks.csv", row.names = FALSE)