compare_utility_meters <- function() {
    # Draw a line graph for DateTime and sub meter 1's utilization
    with(final, plot(Date, m1, type="l", xlab = "", ylab = "Energy sub metering"))
    
    # Add lines for sub meter 2 and 3
    with(final, lines(Date, m2, type="l", col="red"))
    with(final, lines(Date, m3, type="l", col="blue"))
    
    # Add a legend @ top right corner.
#   legend("topright", lwd=1, lty=1, col = c("black", "blue", "red"), box.lwd = 1, 
#       legend = c("SM_1", "SM_2", "SM_3"))
}

# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

data <- filter(data, Date >= as.Date("2007-01-01") & 
    Date <= as.Date("2007-01-31")) 

new_data <- ddply(
	data,
	.(Date),
	function(x) {
		sum(x$Sub_metering_1)
	}
)


final <- ddply(
    data, 
    .(Date), 
    function(x) {  
        sum(x$Sub_metering_1,x$Sub_metering_2, x$Sub_metering_3)
    }
)

meter_1 <- ddply(
    data, 
    .(Date), 
    function(x) {  
        sum(x$Sub_metering_1)
    }
)
final$m1 <- meter_1$V1

meter_2 <- ddply(
    data, 
    .(Date), 
    function(x) {  
        sum(x$Sub_metering_2)
    }
)
final$m2 <- meter_2$V1

meter_3 <- ddply(
    data, 
    .(Date), 
    function(x) {  
        sum(x$Sub_metering_3)
    }
)
final$m3 <- meter_3$V1

# Draw once on the current device for visual feedback then plot in the png device
compare_utility_meters()
png(file="test.png", width=480, height=480, units="px")
compare_utility_meters()
dev.off()