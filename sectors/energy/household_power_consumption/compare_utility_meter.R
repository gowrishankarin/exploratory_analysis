compare_utility_meters <- function(meter, sub, period) {
    # Draw a line graph for DateTime and sub meter 1's utilization
    p <- qplot(
        Date, meter, data = final,
        geom = c("point", "smooth"), method = "loess",
        formula = y~x, color = Date, size  = 2,
        xlab = paste("Analysis Period ", period), 
        ylab = "Active Energy Consumption in Watt-Hour",
        main = "Individual household energy utilization"
        
    )
    
    main_title <- "Individual household energy utilization"
    
    title <- ggtitle(bquote(atop(.(main_title), atop(italic(.(sub))), "")))
    
    print(p + title)

}

# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

print("Analysing Individual Household Energy Utilizaton")
print("Data is collected for a period of 4 years between 2007-01-01 and 2010-12-30")

start_date <- readline("Enter START date in yyyy-mm-dd format(data format not validated): ")
end_date <- readline("Enter END date in yyyy-mm-dd format(data format not validated): ")

start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

data <- filter(data, Date >= start_date & Date <= end_date) 

period <- paste(start_date, " to ", end_date, " ", format(end_date - start_date))
print(period)

start <- gsub("-", x=start_date, "")
end <- gsub("-", x=end_date, "")

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

final <- mutate(final, Day = weekdays(Date))

# Draw once on the current device for visual feedback then plot in the png device
compare_utility_meters(final$m1, "Kitchen Appliances(Dish Washer, Oven & Microwave)", period)
file = paste("./pics-cum/", start, "-", end, "-", format(end_date - start_date), "-kitchen.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$m1, "Kitchen Appliances(Dish Washer, Oven & Microwave)", period)
dev.off()

compare_utility_meters(final$m2, "Laundry Room(Washing Machine, Tumble Drier, Fridge & Light)", period)
file = paste("./pics-cum/", start, "-", end, "-", format(end_date - start_date), "-", "laundry.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$m2, "Laundry Room(Washing Machine, Tumble Drier, Fridge & Light)", period)
dev.off()

compare_utility_meters(final$m3, "Water Heater & Air Conditioner", period)
file = paste("./pics-cum/", start, "-", end, "-", format(end_date - start_date), "-wh_ac.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$m3, "Water Heater & Air Conditioner", period)
dev.off()

compare_utility_meters(final$V1, "All Appliances", period)
file = paste("./pics-cum/", start, "-", end, "-", format(end_date - start_date), "-all.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$m3, "All Appliances", period)
dev.off()
