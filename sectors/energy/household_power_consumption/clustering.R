myplclust <- function(hclust, lab = hclust$labels, main, sub,
    lab.col = rep(1, length(hclust$labels)),
    hang = 0.1, ...) {
    
    # This function is taken from JHU course on R Programming course
    # material by Roger Peng et al.
    
    
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x < 0)]
    x <- x[which(x < 0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels = FALSE, main = main, sub = sub, hang = hang, ...)
    
    
    text(x = x, y = y[hclust$order] - (max(hclust$height)*hang), 
         labels = lab[hclust$order], col = lab.col[hclust$order], srt = 90,
         adj = c(1, 0.5), xpd = NA, ...)
    
}

# Get the cached data laoded through load.R
data <- cacheHPCData(pFunc)

print("Analyse & Predict household energy consumption data, Analysis period should be between 30 and 90 days")
print("Data is collected for a period of 4 years between 2007-01-01 and 2010-12-30")

start_date <- readline("Enter START date in yyyy-mm-dd format(data format not validated): ")
analysis_duration <- readline("Enter duration of analysis, 30 - 90 days: ")

#start_date <- "2007-01-01"
#analysis_duration <- 10

start_date <- as.Date(start_date)
end_date <- as.Date(start_date) + as.numeric(analysis_duration) 

data <- filter(data, Date >= start_date & Date <= end_date) 

period <- paste(start_date, " to ", end_date, " ", format(end_date - start_date))

start <- gsub("-", x=start_date, "")
end <- gsub("-", x=end_date, "")

volt <- ddply(
    data, 
    .(Date), 
    function(x) {  
        median(x$Voltage, na.rm = T)
    }
)



final <- tbl_df(volt)

active_power <- ddply(
    data, 
    .(Date), 
    function(x) {  
        median(x$Global_active_power, na.rm = T)
    }
)
final$ap <- active_power$V1

reactive_power <- ddply(
    data, 
    .(Date), 
    function(x) {  
        median(x$Global_reactive_power, na.rm = T)
    }
)
final$rp <- reactive_power$V1

intensity <- ddply(
    data, 
    .(Date), 
    function(x) {  
        median(x$Global_intensity, na.rm = T)
    }
)

final$intensity <- intensity$V1


final <- mutate(final, V1 = round(V1, digits = 0))


final$Date = NULL

clustering_hierarchical <- function() {
    # HIERARCHICAL CLUSTERING
    par(mar = rep(2, 4))
    distance <- dist(final$V1)
    hClustering <- hclust(distance)

    myplclust(hClustering, lab = final$V1, main = "Cluster by Voltage", sub = period)
    file = paste("./clustering/", start, "-", end, "-", format(end_date - start_date), "-cluster_dendro.png", sep="")
    png(file=file, width=600, height=400, units="px")
    myplclust(hClustering, lab = final$V1, main = "Cluster by Voltage", sub = period)
    dev.off()
    
    library(gplots)
    
    scaled <- data.matrix(final)
    rownames(scaled) <- scaled[, 1]
    
    scaled <- scale(scaled[, -1])
    par(mar = rep(1, 4))
    heatmap.2(scaled, cexRow=0.5, cexCol=0.95, scale="none", trace="none", main = period)
    file = paste("./clustering/", start, "-", end, "-", format(end_date - start_date), "-heatmap.png", sep="")
    png(file=file, width=600, height=400, units="px")
    heatmap.2(scaled, cexRow=0.5, cexCol=0.95, scale="none", trace="none", main = period)
    dev.off()
    
    return(hClustering)
}

svd_help <- function(new_f) {
    
    par(mfrow = c(4, 2))
    #image(t(new_f)[, nrow(new_f):1])
    plot(rowMeans(new_f), nrow(new_f):1, pch = 19, xlab = "Row Means", ylab = "", main = period)
    plot(colMeans(new_f), pch = 19, ylab = "Column Means")
    
    svd1 <- svd(scale(new_f))
    
    plot(svd1$u[, 1], nrow(new_f):1, pch=19, xlab = "SVD(u)")
    plot(svd1$v[, 1], pch=19, ylab = "SVD(v)")
    
    plot(svd1$d, pch=19, xlab = "SVD(d)")
    plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "SVD(d) Variation")
    
    pca1 <- prcomp(new_f, scale=T)
    plot(pca1$rotation[,1], svd1$v[,1], pch=19, xlab = "PCA & SVD Alignment")
    abline(c(0, 1))
    
    par(mfrow = c(1, 1))
}

analyse_svd_pca <- function() {
    hClustering <- clustering_hierarchical()
    new_f <- final[hClustering$order,]
    
    
    svd_help(new_f)
    file = paste("./clustering/", start, "-", end, "-", format(end_date - start_date), "-svd.png", sep="")
    png(file=file, width=900, height=600, units="px")
    svd_help(new_f)
    dev.off()
}

cor_help <- function(method) {
    par(mfrow = c(3, 2))
    main <- paste(toupper(method), "Correlation Analysis")
    print(main)
    
    c1 =  with(final, cor(rp, ap, method = method))^2
    #with(final, )
    
    c2 =  with(final, cor(V1, ap, method = method))^2
    #with(final, )
    
    c3 =  with(final, cor(intensity, ap, method = method))^2
    #with(final, )
    
    c4 =  with(final, cor(V1, intensity, method = method))^2
    #with(final, )
    
    c5 =  with(final, cor(rp, intensity, method = method))^2
    #with(final, )
    
    with(final, {
        plot(rp ~ ap, xlab = "RP", ylab = "AP", pch = 19, col = "blue", sub = period, main = main)
        plot(V1 ~ ap, pch = 19, col = "blue")
        plot(intensity ~ ap, pch = 19, col = "blue")
        plot(V1 ~ intensity, pch = 19, col = "blue")
        plot(rp ~ intensity, pch = 19, col = "blue")
    })
    
    
    cor_mat <- data.frame(c(c1, c2, c3, c4, c5))
    
    rownames(cor_mat) <- c("RP vs AP", "Voltage vs AP", "Intensity vs AP", 
        "Voltage vs Intensity", "RP vs Intensity")
    colnames(cor_mat) <- c("Correlation")
    
    print(cor_mat)
    
    par(mfrow = c(1, 1))    
}

cor_ap_intensity <- function() {
    
    c1 <-  with(final, cor(intensity, ap, method = "pearson"))^2
    c2 <-  with(final, cor(intensity, ap, method = "kendall"))^2
    c3 <-  with(final, cor(intensity, ap, method = "spearman"))^2
    cor_mat <- data.frame(c(c1, c2, c3), row.names = c("Pearson", "Kendall", "Spearman"))
    rownames(cor_mat) <- c("Pearson", "Kendall", "Spearman")
    colnames(cor_mat) <- c("Correlation")
    print(cor_mat)
    
}

analyse_correlation <- function(method) {
    cor_help(method)
    file = paste("./clustering/", start, "-", end, "-", format(end_date - start_date), "-correlation.png", sep="")
    png(file=file, width=600, height=600, units="px")
    cor_help(method)
    dev.off()
    
    cor_ap_intensity()
}

predict_help <- function(no_of_days) {
    
    # Get user input for prediction days...
    # Prediction starts from the day where analysis ends...
    # Acquire data for end_date + x days 
    # Extract intensity and rp columns in order
    
    
    prediction_end <- end_date + as.numeric(no_of_days)
    predict_data <- cacheHPCData(pFunc)
    predict_data <- filter(predict_data, Date > end_date & Date <= prediction_end) 
    
    p_active_power <- ddply(
        predict_data, 
        .(Date), 
        function(x) {  
            median(x$Global_active_power, na.rm = T)
        }
    )
    p_data <- tbl_df(p_active_power)
    
    p_intensity <- ddply(
        predict_data, 
        .(Date), 
        function(x) {  
            median(x$Global_intensity, na.rm = T)
        }
    )
    p_data$intensity <- p_intensity$V1
    
    p_data$Date = NULL
    
    predict_data <- with(final, lm( ap ~ intensity))
    predicted <- predict(predict_data, data.frame(intensity = p_data$intensity), interval = "confidence")
    print(predicted)
    
    
    plot(x = rownames(predicted), y = predicted[,"upr"], 
        pch = 4, col = "red", bg = "yellow", cex = 1.2, xlab = "Days", sub = period,
        ylab = "Active Power", main = "Compare Predicted vs Measured Active Power")
    points(x = rownames(predicted), y = predicted[, "lwr"], 
        pch = 4, col = "green", bg = "yellow", cex = 1.2)
    points(x = rownames(predicted), y = predicted[, "fit"], 
        pch = 18, col = "blue", bg = "yellow", cex = 0.5)
    
    points(x = rownames(predicted), y = p_data$V1, pch = 10, col = "blue", cex = 1.2)
    
}

prediction_linear_regression <- function() {
    no_of_days <- readline("For how many days you want to predict?: ")
    
    predict_help(no_of_days)
    file = paste("./clustering/", start, "-", end, "-", no_of_days, "-days-", 
        "predict.png", sep="")
    png(file=file, width=900, height=600, units="px")
    predict_help(no_of_days)
    dev.off()
}

prediction_pca <- function() {
    
    prin_comp_data <- with(final, princomp(~ ap + intensity))
    new_intensity <- data.frame(intensity=c(3.6))
    predicted <- predict(prin_comp_data, data.frame(intensity = new_intensity))
    
    print(predicted) 
}

print_choices <- function() {
    writeLines("\n")
    print("-----------Choose what you want to do-----------")
    print('1. Hierarchical Clustering')
    print("2. Hierarchical Clustering & SVD")
    print("3. Correlation Analysis(Pearson Product-Moment)")
    print("4. Correlation Analysis(Not Implemented)")
    print("5. Correlation Analysis(Not Implemented)")
    print("6. Prediction using Linear Regression")
    print("0. Quit Analysis")
    
    writeLines("\n")
    
    choice <- readline("Enter an option(0-6): ")
    
    writeLines("\n")

    return(as.numeric(choice))
}


choice <- print_choices()

while(choice != 0) {
    
    if(choice == 1) {
        clustering_hierarchical()
    }
    if(choice == 2) {
        analyse_svd_pca()
    }
    if(choice == 3) {
        analyse_correlation("pearson")
    }
    if(choice == 4) {
        
    }
    if(choice == 5) {
        
    }
    if(choice == 6) {
        prediction_linear_regression()
    }
    if(choice == 0) {
        return(0)
    }
    choice <- print_choices()
}
