#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

NEI.Baltimore <- NEI[NEI$fips == "24510",]

sum.year.Baltimore <- sapply(split(NEI.Baltimore$Emissions, NEI.Baltimore$year), sum)

plot(sum.year.Baltimore, type = "l", xlab = "Year", ylab = "Emission(tons)", xaxt = "n")
axis(1, at=1:4, labels=names(sum.year))
title(main = "Total Baltimore Emission from 1999 to 2008")
text(sum.year.Baltimore, labels = prettyNum(sum.year.Baltimore, big.mark = ","))
dev.copy(png, "plot2.png", width = 480, height = 480)
dev.off()