df <- read.table("household_power_consumption.txt", header = TRUE, sep =";", na.strings = "?", stringsAsFactors = FALSE)

df2 <- subset(df, Date == "1/2/2007" | Date == "2/2/2007")

df3 <- df2

df3$DT <- paste(df3$Date,df3$Time, sep = " ")

df3$DTLT <- strptime(df3$DT, format = "%d/%m/%Y %H:%M:%S")

#plot4
par(mfrow = c(2,2))

with(df3, {
  plot(DTLT, Global_active_power, type = "line", ylab = "Global Active Power")
  plot(DTLT, Voltage, type = "line", xlab = "datetime", ylab = "Voltage")
  {
  plot(DTLT, Sub_metering_1, type = "n", ylab = "Energy sub metering")
  lines(DTLT, Sub_metering_1, type = "line")
  lines(DTLT, Sub_metering_2, type = "line", col = "red")
  lines(DTLT, Sub_metering_3, type = "line", col = "blue")
  legend("topright", lty = 1, col = c("black", "blue", "red"), legend = c(names(df3)[7],names(df3)[8],names(df3)[9]))
  }
  plot(DTLT, Global_reactive_power, type = "line", xlab = "datetime")
})

dev.copy(png, file = "plot4.png")  ## Copy my plot to a PNG file
dev.off() ## Close device