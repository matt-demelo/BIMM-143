#' ---
#' title: "Lecture 05 Data Visualization Practice, BIMM 143"
#' author: "Matt Demelo"
#' date: "April 18, 2019"
#' ---

# <> Lecture 5 R Graphics <>

# 2A. Line Plot - Scatterplot of weight dataframe
weight <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)

plot(weight$Age, weight$Weight, pch = 15, cex = 1.5, lwd = 2, ylim = c(2,10), 
     xlab = "Age (months)", ylab = "Weight (kg", 
     main = "Scatterplot of Weight v. Age in Babies")

# 2B. Barplot - Barplot of frequency of genetic features
features <- read.table("bimm143_05_rstats/feature_counts.txt", 
                       sep = "\t", header = TRUE)

barplot(features$Count)
# A very bland, unlabeled plot is generated. This isn't going to work for us.

# Need to argue with the plot to make it more useful for our purposes

par(mar = c(3.1,11.1,4.1,2))
barplot(features$Count, horiz = TRUE, names.arg = features$Feature, 
        xlab = "Number of Occurences", main = "Frequency of Genetic Features", 
        las = 1)

# Section 2C
par(mar=c(5,10,5,5))
hist(c(rnorm(10000),rnorm(10000) + 4), main = "Histogram test")

# Section 3 -- More bar charts: Male vs Female atheletes, changing bar colors
counts <- read.table("bimm143_05_rstats/male_female_counts.txt", sep = "\t",
                     header = TRUE)
  # Alternatively, the following does the same with less specificities
counts <- read.delim("bimm143_05_rstats/male_female_counts.txt")

par(mar=c(6,5,5,2))

barplot(counts$Count, names.arg = counts$Sample, las = 2, 
        col = c("blue2","red2"), main = "RedBlue MF")

barplot(counts$Count, names.arg = counts$Sample, las = 2, 
        col = c(1,2), main = "BlackRed MF") #black and red

barplot(counts$Count, names.arg = counts$Sample, las = 2, 
        col = rainbow(2), main = "Rainbow MF") #cyan and red

