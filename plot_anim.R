# ----
# Plot animals
#
# by Luis Miguel Senzano Castro
# São Paulo State University - UNESP,
# Rio Claro, SP, Brazil.
# e-mail: luismiguelsenzanocastro@gmail.com
# Last modification: 04/19/2020
# ----



# set working directory

setwd("")


# packages

install_github("sckott/rphylopic") # install 'rphylopic' package
library(rphylopic)
library(dplyr)


# create a dummy data that represents the hourly body temperature selected by 15 free-ranging toads

set.seed(123)
Tb <- runif(n = 15, min = 23, max = 33)
hour <- runif(n = 15, min = 7, max = 18)
ID <- c("toad1","toad2","toad3","toad4","toad5",
          "toad6","toad7","toad8","toad9","toad10",
          "toad11","toad12","toad13","toad14","toad15")

data <- data.frame(ID, Tb, hour)


# plot

at = seq(0,24,1)
lab = c("00:00","","","03:00","","","06:00","","","09:00","","","12:00","","","15:00",
        "","","18:00","","","21:00","","","23:59")

plot(data$hour, data$Tb, xlab = "Hour", ylab= "Body Temperature (°C)",
     xlim = c(0,24), ylim = c(20, 35), pch=19, col="dodgerblue",cex = 1.1,
     axes = FALSE, xpd=TRUE,)
axis(side=1, at= at, labels=lab, las=2, cex.axis=0.9,lwd=0.2)
axis(side=2, at=seq(20, 35, by= 3), cex.axis=0.9)
text(4,33.9,bquote(' Animal Tb '), cex = 0.8, col="black")
points(1, 33.8, type = "p", cex = 1.2, pch=19, col="dodgerblue")


# Create a new plot replacing points with small toads

## load toad image

# Many life forms are available in (http://phylopic.org/). After selecting a silhouette, copy the
# code image from the URL-link bar

toad <- image_data("e1a404c9-4edc-45c3-a694-0817157832fc", # image code
                   size = 64)[[1]] # image size (see details in http://phylopic.org/)

plot_phylopic_base(toad,  x=0.5, y=0.5, ysize=0.4, color= "black", alpha = 0.7) # check the image


# get axis coordinates to add image

xrang <- 24 # x-axis range = 24 (i.e. 0-24)
yrang <- 15 # y-axis range = 25 (i.e. 20-35)
ymin <- 20 # lower y-axis value
ycorrec <- ymin/yrang

Tb.toad <- data %>%
  group_by(ID) %>%
  summarise(coord_x = hour/xrang,
            coord_y = (Tb/yrang)-ycorrec) #'ycorrec' corrects y-axis position, due to it doesn't start from 0

hour.vect <- Tb.toad[["coord_x"]]   # estract column as vector
hour.vect <- as.numeric(hour.vect)  # convert to a numeric vector
tb.vect <- Tb.toad[["coord_y"]]
tb.vect <- as.numeric(tb.vect)

# Match coordinate points to that of images

posx <- hour.vect  # x-axis position (it goes from  0 (left) to 1 (right))
posy <- tb.vect    # y-axis position (it goes from  0 (buttom) to 1 (top))
size <- rep(0.05, 15) # due to N=15 individuals

# plot 

plot(data$hour, data$Tb, xlab = "Hour", ylab= "Body Temperature (°C)",
     xlim = c(0,24), ylim = c(20, 35), pch=19, col=NA,
     axes = FALSE, xpd=TRUE,)
axis(side=1, at= at, labels=lab, las=2, cex.axis=0.9,lwd=0.2)
axis(side=2, at=seq(20, 35, by= 3), cex.axis=0.9)
# legend
text(4,33.9,bquote(' Animal Tb '), cex = 0.8, col="black")
add_phylopic_base(toad, x=0.07, y=0.89, ysize=0.05, color= "dodgerblue", alpha = 1)

add_phylopic_base(toad, # image
                  x=posx, # x-axis vector position must be between 0 (left limit)-1 (right limit) 
                  y=posy, # y-axis vector position must be between 0 (buttom limit)-1 (top limit)
                  ysize=size, 
                  color= "dodgerblue", alpha = 0.9)


# Note: There is a 'add_phylopic()' function to create a ggplot2 layer to add to an existing plot
# See the 'rphylopic' manual for details (https://cran.fiocruz.br/web/packages/rphylopic/rphylopic.pdf)

### end---