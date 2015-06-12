plot1 <- function() {
    p <- ggplot(data = final, aes(y = V1, x = ap)) +
        geom_smooth(method = "lm", se = F, fullrange = T, colour = "steelblue", size = 1) +
        geom_smooth(method = "loess", formula = y ~ x, se = F, colour = "pink", size = 3) +
        geom_point(aes(color=Month)) + 
        ggtitle(paste("Voltage Fluctuation impact on Reactive Power ", period)) +
        xlab("Reactive Power") + ylab("Voltage")
    
    print(p)
    
    return(p)
}

plot2 <- function(p) {
    q = p + facet_wrap(~Month, nrow = 4) + 
        ggtitle(paste("Monthly Voltage Fluctuation impact on Reactive Power ", period)) +
        xlab("Reactive Power") + ylab("Voltage")
    print(q) 
}

plot3 <- function() {
    p <- ggplot(data = final, aes(y = V1, x = Day)) +
        geom_smooth(method = "lm", se = F, fullrange = T, colour = "steelblue", size = 1) +
        geom_smooth(method = "loess", formula = y ~ x, se = F, colour = "pink", size = 3) +
        geom_point(aes(color=Month)) + 
        ggtitle(paste("Monthly Voltage Fluctuation ", period)) +
        xlab("Days") + ylab("Voltage") + facet_wrap(~Month, nrow = 4)
    
    print(p)
    
    return(p)
}



# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

print("Analysing Individual Household Energy Utilizaton(Voltage Fluctuation)")
print("Data is collected for a period of 4 years between 2007-2010")

year <- readline("Enter the year for which analysis has to be done: ")


data <- filter(data, year(Date) == year) 

period <- paste("Year ",  toString(year))
print(period)

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

final <- mutate(final, 
    Month = months(Date), 
    Weekday = weekdays(Date), 
    Day = day(Date))

final <- tbl_df(final)

p <- plot1() 
file = paste("./pics-monthly/", period, "-voltage_vs_rp.png", sep="")
png(file=file, width=720, height=720, units="px")
plot1() 
dev.off() 

plot2(p) 
file = paste("./pics-monthly/", period, "-voltage_vs_rp_monthly.png", sep="")
png(file=file, width=960, height=960, units="px")
plot2(p) 
dev.off() 

plot3() 
file = paste("./pics-monthly/", period, "-voltage_monthly.png", sep="")
png(file=file, width=960, height=960, units="px")
plot3() 
dev.off() 
