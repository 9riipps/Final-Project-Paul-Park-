# =====================================================
# Stat133: Final Project
# Description: Skeleton
# =====================================================

# Name: Paul Park
# SID : 23046317

# =====================================================

# inspect the current working directory
getwd()

# now create all the subdirectories as designed in the instructions
dir.create("code")
dir.create("rawdata")
dir.create("data")
dir.create("resources")
dir.create("report")
dir.create("images")
file.create("readme.md")

# download the raw data files to the corresponding directory
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat",
              "rawdata/rawdata.hdat")
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv",
              "rawdata/rawdata2.csv")
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv",
              "rawdata/rawdata3.csv")
