df <- read.table("household_power_consumption.txt", header = TRUE, sep =";", na.strings = "?", stringsAsFactors = FALSE)

df2 <- subset(df, Date == "1/2/2007" | Date == "2/2/2007")

df3 <- df2

df3$DT <- paste(df3$Date,df3$Time, sep = " ")

df3$DTLT <- strptime(df3$DT, format = "%d/%m/%Y %H:%M:%S")

#Plot 2
plot(df3$DTLT, df3$Global_active_power, type = "line", xlab = "", ylab = "Global Active Power (kilowatt)")

dev.copy(png, file = "plot2.png")  ## Copy my plot to a PNG file
dev.off() ## Close device