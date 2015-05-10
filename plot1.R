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

	# return the data frame containing data
	return(feb_1_2_2007)
}

create_plot_1 <- function(data) {
	png(filename="plot1.png")
	hist(data$Global_active_power, col="Red", xlab="Global Active Power (kilowatts)",
		main="Global Active Power")
	dev.off()
}