compare_utility_meters <- function() {
    # Draw a line graph for DateTime and sub meter 1's utilization
    with(data, plot(DateTime, Sub_metering_1, type="l", xlab = "", ylab = "Energy sub metering"))
    
    # Add lines for sub meter 2 and 3
    with(data, lines(DateTime, Sub_metering_2, type="l", col="red"))
    with(data, lines(DateTime, Sub_metering_3, type="l", col="blue"))
    
    # Add a legend @ top right corner.
    legend("topright", lwd=1, lty=1, col = c("black", "blue", "red"), box.lwd = 1, 
        legend = c("SM_1", "SM_2", "SM_3"))
}

print("Enter the start and end date for analysis.")
print("Data is collected for a period of 4 years between 2007-01-01 and 2010-12-30")

start_date <- readline("Enter START date in yyyy-mm-dd format: ")
end_date <- readline("Enter END date in yyyy-mm-dd format: ")

# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

data <- filter(data, Date >= as.Date(start_date) & 
    Date <= as.Date(end_date)) 


# Draw once on the current device for visual feedback then plot in the png device
compare_utility_meters()
png(file="compare_utility_meters.png", width=480, height=480, units="px")
compare_utility_meters()
dev.off()