#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

nei <- tbl_df(NEI)
scc <- tbl_df(SCC)

# Filter for Coal related row in sCC
scc.coal <- filter(scc, grepl("[Cc]oal", EI.Sector))

# Use semi_join for all nei with SCC in scc.coal 
nei.coal <- semi_join(nei, scc.coal, by = "SCC")

nei.coal.year <- summarise(group_by(nei.coal, year), sumEmi = sum(Emissions))

ggplot(data = nei.coal.year, aes(x = year, y = sumEmi)) + 
  geom_line() +
  ggtitle("Emission from Coal Combustion from 1999 - 2008") +
  ylab("Emission(ton)")
dev.copy(png, "plot4.png", width = 600, height = 480)
dev.off()
