#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

nei <- tbl_df(NEI)
nei.bal <- filter(nei, fips == '24510')
nei.bal.gr <- group_by(nei.bal, type, year)
nei.bal.gr.sum <- summarise(nei.bal.gr, sumEmi = sum(Emissions))

ggplot(data = nei.bal.gr.sum, aes(x = year, y = sumEmi, color = type)) + 
  geom_line() +
  ggtitle("Emission by type 1999 - 2008") +
  ylab("Emission(ton)")
dev.copy(png, "plot3.png", width = 600, height = 480)
dev.off()