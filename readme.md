Hello, welcome to Paul Park's STAT 133 final project


I am a junior statistics major who is currently taking the summer session of the class "computing with data"


I am working with the data from all the storms from 1851 up to this date. I am specifically intersted in the storms from 1980 to 2010 and how their trajectories would look like in the world map.

This project consists of four important parts.

The first part is extracting data from the raw data and cleaning the data to make a couple of nice data frames.
The second part is basically analysing the storms using the clean data I obtained from the first part.
The third part is plotting and visualizing using the data from the previous parts.
And last but not least, the fourth part is putting up everything you learned from this project into one report file.

Under the R project folder there are six subdirectories each including different types of data.

The "code" folder includes all the r scripts, which are primarily from part one to three.
The "rawdata" folder includes all the raw data that were used in this project.
The "data" folder includes the clean data that were extracted and modified from the raw data.
The "imgaes" folder includes all the plots and imgaes produced in this project.
The "report" folder, obviously, has one report file that summarizes the entire project.
And the "resources" folder, for me, is an empty folder where one might be putting all the other resources used in this project.

Thank you so much for reading this and enjoy. In case you hae questions, please send me an email to crazynine@berkeley.edu

Things to note,
1) If you source "visualization.R", you will get warnings saying some rows were removed in geom_path(). That is because the plot window(which is set so that only certain part of the world is shown) cut off some of the connected trajectories that lie outside the window.
2) In the regression analysis of mean press and mean wind speed, there are some sifnificant outliers that weren't removed since the mean pressure wans't zero but probably contained some zeros values, hence bringing down the mean.