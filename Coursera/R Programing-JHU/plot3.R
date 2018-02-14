df <- read.table("household_power_consumption.txt", header = TRUE, sep =";", na.strings = "?", stringsAsFactors = FALSE)

df2 <- subset(df, Date == "1/2/2007" | Date == "2/2/2007")

df3 <- df2

df3$DT <- paste(df3$Date,df3$Time, sep = " ")

df3$DTLT <- strptime(df3$DT, format = "%d/%m/%Y %H:%M:%S")

#Plot 3
with(df3, plot(DTLT, Sub_metering_1, type = "n"))
with(df3, lines(DTLT, Sub_metering_1, type = "line"))
with(df3, lines(DTLT, Sub_metering_2, type = "line", col = "red"))
with(df3, lines(DTLT, Sub_metering_3, type = "line", col = "blue"))
legend("topright", lty = 1, col = c("black", "blue", "red"), legend = c(names(df3)[7],names(df3)[8],names(df3)[9]))

dev.copy(png, file = "plot3.png")  ## Copy my plot to a PNG file
dev.off() ## Close device