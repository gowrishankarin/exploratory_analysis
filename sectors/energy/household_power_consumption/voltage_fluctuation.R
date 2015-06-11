analyse_voltage_fluctuation <- function() {
    
    p1 <- qplot(x = V1, data = final, xlab = "Voltage", ylab = "Frequency") + 
        geom_histogram(fill = "peachpuff", color = "grey", binwidth = 0.5) +
        coord_cartesian(xlim = c(x.min-1, x.max+1)) + 
        ggtitle(paste("Voltage Fluctuation (rounded to 0.5 volt)", period)) +
        geom_vline(xintercept = c(x.p5), color = "brown", alpha = 1) +
        geom_text(x = x.p5, y = 44, label = paste("5%", round(x.p5, digits = 1), 
            sep = '\n'), color = "brown") +
        geom_vline(xintercept = c(x.p95), color = "brown", alpha = 1) +
        geom_text(x = x.p95, y = 44, label = paste("95%", round(x.p95, digits = 1),
            sep = '\n'), color = "brown") +
        geom_text(x = x.min, y = 44, label = paste("Min", round(x.min, digits=1), 
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
        geom_text(x = x.mean, y = 44, label = paste("Ave", round(x.mean, digits = 1),
            sep = '\n'), color = "red") +
        geom_vline(xintercept = c(x.median), color = "green3", alpha = 1) + 
        geom_text(x = x.median, y = 22, label = paste("Med", round(x.median, digits = 1),
            sep = '\n'), color = "green3")
    
    p2 <- qplot(x = V1, data = final, xlab = "Voltage", ylab = "Frequency") + 
        coord_cartesian(xlim = c(237.5, 244)) +
        geom_histogram(fill = "peachpuff", color = "grey", binwidth = 0.5) +
        ggtitle("Primary Distribution")
    
    p3 <- qplot(x = V1, data = final, xlab = "Voltage", ylab = "Frequency") + 
        coord_cartesian(xlim = c(230.5, 237)) +
        geom_histogram(fill = "pink", color = "grey", binwidth = 0.5) +
        ggtitle("Faulty Distribution")
    
    grid.arrange(p1, p2, p3, nrow = 3)
    
}


# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

print("Analysing Individual Household Energy Utilizaton(Voltage Fluctuation)")
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

analyse_voltage_fluctuation()
file = paste("./pics-vi/", start, "-", end, "-", format(end_date - start_date), "-fluctuation.png", sep="")
png(file=file, width=960, height=720, units="px")
analyse_voltage_fluctuation()
dev.off() 
