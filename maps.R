setwd("./data")

library(maptools)
library(spdep)

# Read European shapefile
euro.poly <- readShapePoly("ue.shp")
euro.nb <- poly2nb(euro.poly)
euro.listw <- nb2listw(euro.nb, style = "W", zero.policy = TRUE)

# Read and process data for different years
process_data <- function(name,year) {
  euro.dane <- read.csv(paste(name,year, ".csv", sep = ""), header = TRUE, sep = ";", dec = ",")
  coords <- coordinates(euro.poly)
  colnames(coords) <- c("x", "y")
  euro.dane <- cbind(euro.dane, coords)
  
  if(name=="TME"){
    medianTME <- median(euro.dane$TME)
    q1_tme <- quantile(euro.dane$TME, probs = 0.25)
    brks <- quantile(euro.dane$TME, probs = c(0, medianTME-q1_tme, medianTME, medianTME+q1_tme,1))
    colors <- c("red", "lightpink", "deeppink", "magenta4")
    plot(euro.poly, col = colors[findInterval(euro.dane$TME, brks)], forcefill = FALSE)
  }else{
    brks <- c(1, 2, 3, 4)
    colors <- c("azure", "green", "red", "blueviolet")
    plot(euro.poly, col = colors[findInterval(euro.dane$G1, brks)], forcefill = FALSE)
  }
  legend("bottomleft", fill = colors,
         legend = c("less than Median-Q", "from Median-Q to Median", "from Median to Median+Q", "more than Median+Q"),
         leglabs(brks1), bty = "n", cex = 0.8)
}

# Process data for different years EU
process_data("UE",2011)
process_data("UE",2015)
process_data("UE",2019)
# Process data for different years TME
process_data("TME",2011)
process_data("TME",2015)
process_data("TME",2019)

