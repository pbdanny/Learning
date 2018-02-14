#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

nei <- tbl_df(NEI)
scc <- tbl_df(SCC)

# Filter for "Motor Vehicles" in Short.Name
nei.los <- filter(nei, fips == '06037')
scc.mot <- filter(scc, grepl("Motor Vehicles", Short.Name))

# Use semi_join for all nei with SCC in scc.coal 
nei.los.mot <- semi_join(nei.los, scc.mot, by = "SCC")

nei.los.mot.year <- summarise(group_by(nei.los.mot, year), sumEmi = sum(Emissions))

ggplot(data = nei.los.mot.year, aes(x = year, y = sumEmi)) + 
  geom_line() +
  ggtitle("Emission from Motor Vehicles Combustion in LA from 1999 - 2008") +
  ylab("Emission(ton)")
dev.copy(png, "plot6.png", width = 600, height = 480)
dev.off()