#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# README ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Program R code supplement to Carlisle et al. 2023
# Beyond overlap: considering habitat preference and fitness outcomes in the 
# umbrella species concept
# Animal Conservation
# https://doi.org/10.1111/acv.12899

# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit
# Wyoming Game and Fish Department (current)
# jason.carlisle@wyo.gov or jason.d.carlisle@gmail.com
# 9/11/2023

# This is script 1 of 3 in an analysis workflow that compares nest-site
# selection and nest survival of Greater Sage-Grouse to sympatric sagebrush-
# associated songbirds.

# Script 1
# Fits two Random Forests models to sage-grouse nest data,
# an RSF model of nest-site selection
# an SPF model of nest survival

# Note that this script is for illustrative purposes only.
# Due to sensitive sage-grouse nest location data being omitted from the 
# publicly available dataset, the analysis reported in Carlisle et al. 2023
# cannot be directly reproduced here.
# Also note the number of trees, iterations, permutations has been reduced
# here to make examples run faster.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Packages and working directory ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Package versions used in the published analysis
# Later versions (espcially rfUtilities) will likely require code modification
require(randomForest) # v 4.7-1.1
require(rfUtilities)  # v 2.0-0

# Set working directory (if needed)
# setwd("D:/Carlisle_2023_ACV_DataPackage")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Input data ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Grouse locations and pseudo-absence points
grouse <- read.csv("InputData/Data_SageGrouse_Nests.csv")

dim(grouse)  # 254 66

# 16 covariates to remove a priori that have less biological justification
drops <- c("Bare", "Litter", "Shrub",
           "HLI", "Sage.Bin", "Herb.Bin", "ShrubHt.Bin",
           "Sage.Bin.SD.1", "Sage.Bin.SD.2", "Sage.Bin.SD.3",
           "Herb.Bin.SD.1", "Herb.Bin.SD.2", "Herb.Bin.SD.3",
           "ShrubHt.Bin.SD.1", "ShrubHt.Bin.SD.2", "ShrubHt.Bin.SD.3")

# And remove X and Y coordinates since they were not made publicly available
drops <- c(drops, "Xcoord", "Ycoord")

# Remove
grouse <- grouse[, !(names(grouse) %in% drops)]
rm(drops)

# 127 nests (Nest = 1) and 127 pseudo-absence points (Nest = 0)
table(grouse$Nest)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Remove redundant variables ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Iterative test for multicollinearity (multivariate redundancy)
p <- 0.06  # not a p-value, see help doc for ?multi.collinear

# Covariate data
xdata <- grouse[, 3:ncol(grouse)]

cl <- multi.collinear(xdata, p = p)

# This function recommends which to remove, doesn't actually remove them
# Checks for hinge-pin variables (if you remove one, are the rest okay?)
for (l in cl) {
  cl.test <- xdata[, -which(names(xdata) == l)]
  print(paste("REMOVE VARIABLE", l, sep = ": "))
  multi.collinear(cl.test, p = p)    
}

# Remove multi-collinear variables
for (l in cl) {grouse <- grouse[,-which(names(grouse) == l)]}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Prep to fit Random Forests models ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Number of trees in each forest (odd will break ties)
b <- 101

# X (covariate) and Y (response) data
# r. prefix is for RSF model for nest-site selection
# s. prefix is for SPF model for nest survival
r.y <- as.factor(grouse[, "Nest"])  # factor with two levels
s.y <- as.factor(grouse[!is.na(grouse$Surv), "Surv"])  # factor with two levels

r.x <- grouse[, 3:ncol(grouse)]
s.x <- grouse[!is.na(grouse$Surv), 3:ncol(grouse)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Model selection ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
r.mod <- rf.modelSel(x = r.x,
                     y = r.y,
                     imp.scale = "mir",
                     ntree = b,
                     r = c(seq(0.1, 0.9, 0.1)),
                     seed = 333)
(r.mod.tab <- r.mod$test)  # which model is best?

s.mod <- rf.modelSel(x = s.x,
                     y = s.y,
                     imp.scale = "mir",
                     ntree = b,
                     r = c(seq(0.1, 0.9, 0.1)),
                     seed = 333)
(s.mod.tab <- s.mod$test)  # which model is best?

# Formatted tables
r.mod.tab$mod <- as.numeric(row.names(r.mod.tab))
r.mod.tab <- round(r.mod.tab, 2)
r.mod.tab <- r.mod.tab[, c(5, 1:4)]
names(r.mod.tab) <- c("Model", "MIR Threshold", "OOB Error", "Class Error", "K")
r.mod.tab

s.mod.tab$mod <- as.numeric(row.names(s.mod.tab))
s.mod.tab <- round(s.mod.tab, 2)
s.mod.tab <- s.mod.tab[, c(5, 1:4)]
names(s.mod.tab) <- c("Model", "MIR Threshold", "OOB Error", "Class Error", "K")
s.mod.tab

# Subset data to selected covariates
r.data <- data.frame(Nest = r.y, r.x[, r.mod$selvars])
s.data <- data.frame(Surv = s.y, s.x[, s.mod$selvars])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit final Random Forests models ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

(r.final <- randomForest(y = r.data[, "Nest"],
                         x = r.data[, 2:ncol(r.data)],
                         ntree = b,
                         importance = TRUE,
                         norm.votes = TRUE,
                         proximity = TRUE))

(s.final <- randomForest(y = s.data[, "Surv"],
                         x = s.data[, 2:ncol(s.data)],
                         ntree = b,
                         importance = TRUE,
                         norm.votes = TRUE,
                         proximity = TRUE))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Bootstrap error convergence plots ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
plot(r.final,
     main = "RSF Bootstrap Error Convergence",
     ylim = c(0, 1),
     col = c("black", "blue", "red"))
legend("topright",
       legend = c("OOB", "Random Points", "Nest Points"),
       fill = c("black", "blue", "red"))

plot(s.final,
     main = "SPF Bootstrap Error Convergence",
     ylim = c(0,1),
     col = c("black", "blue", "red"))
legend("topright",
       legend = c("OOB", "Failed Nests", "Survived Nests"),
       fill = c("black", "blue", "red"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Partial effects plots ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# First, reset plot margins to default values
# starts at bottom and goes clockwise
par(mar = c(5.1, 4.1, 4.1, 2.1))

for (i in names(r.data[, 2:ncol(r.data)])) {
  rf.partial.prob(x = r.final,
                  pred.data = r.data,
                  xname = i,
                  which.class = "1",
                  smooth = TRUE,
                  main = "RSF Partial Plot",
                  ylab = "Probability")
}

for (i in names(s.data[, 2:ncol(s.data)])) {
  rf.partial.prob(x = s.final,
                  pred.data = s.data,
                  xname = i,
                  which.class = "1",
                  smooth = TRUE, 
                  main = "SPF Partial Plot", 
                  ylab = "Probability")
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Variable importance plots ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
r.p <- as.matrix(r.final$importance[, 3])
ord <- rev(order(r.p[, 1],
                 decreasing = TRUE)[1:dim(r.p)[1]])  
dotchart(r.p[ord, 1],
         pch = 19,
         main = "RSF Variable Importance",
         xlab = "Mean Decrease in Accuracy")

s.p <- as.matrix(s.final$importance[, 3])
ord <- rev(order(s.p[, 1],
                 decreasing = TRUE)[1:dim(s.p)[1]])  
dotchart(s.p[ord, 1],
         pch = 19,
         main = "SPF Variable Importance",
         xlab = "Mean Decrease in Accuracy")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Cross-validation to assess model fit ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
(r.cv <- rf.crossValidation(x = r.final,
                            xdata = r.data[, 2:ncol(r.data)],
                            p = 0.10,
                            n = 25,
                            seed = 333))

(s.cv <- rf.crossValidation(x = s.final,
                            xdata = s.data[, 2:ncol(s.data)],
                            p = 0.10,
                            n = 25,
                            seed = 333))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Significance test ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
(r.perm <- rf.significance(x = r.final,
                           xdata = r.data[, 2:ncol(r.data)],
                           nperm = 100,
                           ntree = b))

(s.perm <- rf.significance(x = s.final,
                           xdata = s.data[, 2:ncol(s.data)],
                           nperm = 100,
                           ntree = b))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#