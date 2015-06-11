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

print("Analysing Individual Household Energy Utilizaton(Voltage Fluctuation)")
print("Data is collected for a period of 4 years between 2007-01-01 and 2010-12-30")

start_date <- readline("Enter START date in yyyy-mm-dd format(data format not validated): ")
end_date <- readline("Enter END date in yyyy-mm-dd format(data format not validated): ")

start_date <- "2007-01-01"
end_date <- "2007-12-31"

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

require(gridExtra)

x.min <- min(final$V1, na.rm = T)
x.p5 <- quantile(final$V1, 0.05, na.rm = T)
x.p25 <- quantile(final$V1, 0.25, na.rm = T)
x.median <- median(final$V1, na.rm = T)
x.mean <- mean(final$V1, na.rm = T)
x.p75 <- quantile(final$V1, 0.75, na.rm = T)
x.p95 <- quantile(final$V1, 0.95, na.rm = T)
x.max <- max(final$V1, na.rm = T)

p1 <- qplot(x = V1, data = final) + 
    geom_histogram(fill = "peachpuff", color = "grey", binwidth = 0.5) +
    coord_cartesian(xlim = c(230, 244)) + 
    ggtitle("Voltage Fluctuation (rounded to 0.5 volt)") +
    geom_vline(xintercept = c(x.p5), color = "brown", alpha = 1) +
    geom_text(x = x.p5, y = 44, label = paste("5%", round(x.p5, digits = 1), 
        sep = '\n'), color = "brown") +
    geom_vline(xintercept = c(x.p95), color = "brown", alpha = 1) +
    geom_text(x = x.p95, y = 44, label = paste("95%", round(x.p95, digits = 1),
        sep = '\n'), color = "brown") +
    geom_text(x = 231, y = 44, label = paste("Min", round(x.min, digits=1), 
        sep='\n'), color = "blue") +
    geom_text(x = x.max, y = 44, label = paste("Max", round(x.max, digits=1), 
              sep='\n'), color = "blue") + 
    geom_vline(xintercept = c(x.p25), color = "chocolate3", alpha = 1) +
    geom_text(x = x.p25, y = 44, label = paste("25%", round(x.p25, digits = 1), 
        sep = '\n'), color = "chocolate3") +
    geom_vline(xintercept = c(x.p75), color = "chocolate3", alpha = 1) +
    geom_text(x = x.p75, y = 44, label = paste("75%", round(x.p75, digits = 1),
        sep = '\n'), color = "chocolate3") + 
    geom_vline(xintercept = c(x.mean), color = "red", alpha = 1) +
    geom_text(x = x.mean, y = 22, label = paste("Ave", round(x.mean, digits = 1),
    	sep = '\n'), color = "red") +
    geom_vline(xintercept = c(x.median), color = "red", alpha = 1) + 
    geom_text(x = x.median, y = 22, label = paste("Med", round(x.median, digits = 1),
    	sep = '\n'), color = "red")

p2 <- qplot(x = V1, data = final) + coord_cartesian(xlim = c(237.5, 244)) +
    geom_histogram(fill = "peachpuff", color = "grey", binwidth = 0.5) +
    ggtitle("Primary Distribution")
    
p3 <- qplot(x = V1, data = final) + coord_cartesian(xlim = c(230.5, 237)) +
    geom_histogram(fill = "pink", color = "grey", binwidth = 0.5) +
    ggtitle("Faulty Distribution")

grid.arrange(p1, p2, p3, nrow = 3)

plot_all <- function() {
    
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
    
}
