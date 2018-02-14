#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

sum.year <- sapply(split(NEI$Emissions, NEI$year), sum)

ytick <- as.integer(c(3e6, 4e6, 5e6, 6e6, 7e6, 8e6))
par(cex = 0.75)
plot(sum.year, type = "l", xlab = "Year", ylab = "Emission(tons)", xaxt = "n", yaxt = "n")
axis(1, at=1:4, labels = names(sum.year))
axis(2, at=ytick,  labels = prettyNum(ytick, big.mark = ","))
title(main = "Total US. Emission from 1999 to 2008")
text(sum.year, labels = prettyNum(sum.year, big.mark = ","))
dev.copy(png, "plot1.png", width = 480, height = 480)
dev.off()
