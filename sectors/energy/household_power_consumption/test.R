compare_utility_meters <- function(meter, sub, period, ylab) {
    # Draw a line graph for DateTime and sub meter 1's utilization
    p <- qplot(
        Date, meter, data = final,
        geom = c("point", "smooth"), method = "loess",
        formula = y~x, color = Date, size  = 2,
        xlab = paste("Analysis Period ", period), 
        ylab = ylab,
        main = "Individual household energy utilization"
        
    )
    
    main_title <- "Individual household energy utilization"
    
    title <- ggtitle(bquote(atop(.(main_title), atop(italic(.(sub))), "")))
    
    facet <-  facet_wrap(~Day)
    
    print(p + title )
    
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

final <- ddply(
    data, 
    .(Date), 
    function(x) {  
        mean(x$Voltage)
    }
)

active_power <- ddply(
    data, 
    .(Date), 
    function(x) {  
        mean(x$Global_active_power)
    }
)
final$ap <- active_power$V1

reactive_power <- ddply(
    data, 
    .(Date), 
    function(x) {  
        mean(x$Global_reactive_power)
    }
)
final$rp <- reactive_power$V1

intensity <- ddply(
    data, 
    .(Date), 
    function(x) {  
        mean(x$Global_intensity)
    }
)
final$int <- intensity$V1

final <- mutate(final, Day = weekdays(Date))

# Draw once on the current device for visual feedback then plot in the png device
compare_utility_meters(final$V1, "Flunctuation", 
    period, "Voltage Fluctuation")
file = paste("./pics-vi/", start, "-", end, "-", format(end_date - start_date), "-kitchen.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$V1, "Kitchen Appliances(Dish Washer, Oven & Microwave)", 
    period, "Voltage Fluctuation")
dev.off()

compare_utility_meters(final$ap, "Power Consumption", 
    period, "Global Active Power")
file = paste("./pics-vi/", start, "-", end, "-", format(end_date - start_date), "-", "laundry.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$ap, "Laundry Room(Washing Machine, Tumble Drier, Fridge & Light)", 
    period, "Global Active Power")
dev.off()

compare_utility_meters(final$rp, "Power Variation", 
    period, "Global Reactive Power")
file = paste("./pics-vi/", start, "-", end, "-", format(end_date - start_date), "-wh_ac.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$rp, "Water Heater & Air Conditioner", 
    period, "Global Reactive Power")
dev.off()

compare_utility_meters(final$int, "Intensity", period, "Intensity")
file = paste("./pics-vi/", start, "-", end, "-", format(end_date - start_date), "-all.png", sep="")
png(file=file, width=960, height=480, units="px")
compare_utility_meters(final$int, "All Appliances", period, "Intensity")
dev.off()
