read_data <- function(file_name="household_power_consumption.txt") {
	
	# read in power consumption data
	power_consumption <- read.table(file="household_power_consumption.txt", sep=";", header=TRUE,
	stringsAsFactors=FALSE)
	
	# convert Date to more readable format
	power_consumption$Date <- as.Date(strptime(power_consumption$Date, "%d/%m/%Y"))
	
	# filter out dates we do not want (looking for Feb 1 & Feb 2, 2007)
	feb_1_2_2007 <- power_consumption[power_consumption$Date=="2007-02-01" | 				power_consumption$Date=="2007-02-02",]
	
	# convert measured values characters/strings to numeric
	feb_1_2_2007$Global_active_power <- as.numeric(feb_1_2_2007$Global_active_power)
	feb_1_2_2007$Global_reactive_power <- as.numeric(feb_1_2_2007$Global_reactive_power)
	feb_1_2_2007$Voltage <- as.numeric(feb_1_2_2007$Voltage)
	feb_1_2_2007$Global_intensity <- as.numeric(feb_1_2_2007$Global_intensity)
	feb_1_2_2007$Sub_metering_1 <- as.numeric(feb_1_2_2007$Sub_metering_1)
	feb_1_2_2007$Sub_metering_2 <- as.numeric(feb_1_2_2007$Sub_metering_2)
	feb_1_2_2007$Sub_metering_3 <- as.numeric(feb_1_2_2007$Sub_metering_3)
	
	# create new column containing combined datetime
	# first past Date & time columns together
	date_time <- paste(feb_1_2_2007$Date, feb_1_2_2007$Time)
	# use strptime to convert string to posix object
	feb_1_2_2007$datetime <- strptime(date_time, "%Y-%m-%d %H:%M:%S")

	# return the data frame containing data
	return(feb_1_2_2007)
}

create_plot_1 <- function(data) {
	png(filename="plot1.png")
	hist(data$Global_active_power, col="Red", xlab="Global Active Power (kilowatts)",
		main="Global Active Power")
	dev.off()
}

create_plot_2 <- function(data) {
	png(filename="plot2.png")
	plot(data$datetime, data$Global_active_power,type="l", ylab="Global Active Power (kilowatts)", xlab="")
	dev.off()
}

create_plot_3 <- function(data) {
	png(filename="plot3.png")
	plot(data$datetime, data$Sub_metering_1, col="Black", type="l", xlab="", ylab="Energy sub metering")
	points(data$datetime, data$Sub_metering_2, col="Red", type="l")
	points(data$datetime, data$Sub_metering_3, col="Blue", type="l")
	legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col=c("Black","Red","Blue"))
	dev.off()
}