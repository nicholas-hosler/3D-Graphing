# 3D Plot script importing dta data 

tryCatch(library(haven),error=function(cond){install.packages('haven');library(haven)}) # for reading DTA files
tryCatch(library(plot3D),error=function(cond){install.packages('plot3D');library(plot3D)}) # 3D Plotting lib

# reading in dta data and converting to csv format and reimporting (helps with standardization)
data <- read_dta("Compustat_Max.dta")
write.csv(data,"data.csv", row.names = FALSE)
data <- read.csv("data.csv")

x <- data$OPEXMKUP_PT_1d
y <- data$prod_opex

z <- data$ab90_ROIC_wogw3

# Test numbers
# n = 5000
# x = runif(n)
# y = runif(n)
# z = x + 2*y + sin(x*2*pi)

# Divide into bins
x_c = cut(x, 20) 
y_c = cut(y, 20) 
x_l = levels(x_c)
y_l = levels(y_c)

# Compute the mean of z within each x,y bin 
#  the below computation was shared to me on the R message board
# very strict self-regulating community, no reason to to expect it to be incorrect 
z_p = matrix(0, 20, 20) 
for (i in 1:length(x_l)){
  for (j in 1:length(y_l)){
    z_p[i,j] = mean(z[x_c %in% x_l[i] & y_c %in% y_l[j]])
  }   
}   

# Get the middle of each bin
x_p = sapply(strsplit(gsub('\\(|]', '', x_l), ','), function(x) mean(as.numeric(x)))
y_p = sapply(strsplit(gsub('\\(|]', '', y_l), ','), function(x) mean(as.numeric(x)))

# Ploting using the provided bin as the z variable
hist3D(x_p, y_p, z_p, bty = "g", border = "black", 
       ticktype = "detailed", pch = 19, cex = 0.5,
       main = "Data", xlab = "Markup",
       ylab ="Omega", zlab = "Star")
