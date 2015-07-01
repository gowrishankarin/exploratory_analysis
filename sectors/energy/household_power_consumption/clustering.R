myplclust <- function(hclust, lab = hclust$labels, 
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
    plot(hclust, labels = FALSE, hang = hang, ...)
    
    
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
#end_date <- "2007-04-01"

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
    
    myplclust(hClustering, lab = final$V1)
    
    library(gplots)
    
    scaled <- data.matrix(final)
    rownames(scaled) <- scaled[, 1]
    
    scaled <- scale(scaled[, -1])
    
    heatmap.2(scaled, cexRow=0.5, cexCol=0.95, scale="none", trace="none")
    
    return(hClustering)
}

analyse_svd_pca <- function() {
    hClustering <- clustering_hierarchical()
    new_f <- final[hClustering$order,]
    
    par(mfrow = c(4, 2))
    image(t(new_f)[, nrow(new_f):1])
    plot(rowMeans(new_f), nrow(new_f):1, pch = 19, xlab = "Row Means")
    plot(colMeans(new_f), pch = 19, ylab = "Column Means")
    
    svd1 <- svd(scale(new_f))
    
    plot(svd1$u[, 1], nrow(new_f):1, pch=19, xlab = "SVD(u)")
    plot(svd1$v[, 1], pch=19, ylab = "SVD(v)")
    
    plot(svd1$d, pch=19, xlab = "SVD(d)")
    plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "SVD(d) Variation")
    
    pca1 <- prcomp(new_f, scale=T, xlab = "Principle Component")
    plot(pca1$rotation[,1], svd1$v[,1], pch=19, xlab = "PCA & SVD Alignment")
    abline(c(0, 1))
}

analyse_correlation <- function(method) {
    
    print(paste(toupper(method), "Correlation Analysis"))
    
    cor_val =  with(final, cor(rp, ap, method = method))^2
    print(paste("RP vs AP", cor_val))
    with(final, plot(rp ~ ap))
    
    cor_val =  with(final, cor(V1, ap, method = method))^2
    with(final, plot(V1 ~ ap))
    print(paste("Voltage vs AP", cor_val))
    
    cor_val =  with(final, cor(intensity, ap, method = method))^2
    with(final, plot(intensity ~ ap))
    print(paste("Intensity vs AP", cor_val))
    
    cor_val =  with(final, cor(V1, intensity, method = method))^2
    with(final, plot(V1 ~ intensity))
    print(paste("Voltage vs Intensity", cor_val))
    
    cor_val =  with(final, cor(rp, intensity, method = method))^2
    with(final, plot(rp ~ intensity))
    print(paste("RP vs Intensity", cor_val))
}

prediction_linear_regression <- function() {
    par(mfrow = c(3, 2))
    predict_data <- with(final, lm(ap ~ intensity))
    new_intensity <- data.frame(intensity=c(1.7, 2.4, 3.6))
    predicted <- predict(predict_data, data.frame(intensity = new_intensity), interval = "confidence")
    print(predicted)
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
    print("4. Correlation Analysis(Kendall's Rank)")
    print("5. Correlation Analysis(Spearman's Rank)")
    print("6. Prediction using ")
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
        analyse_correlation("kendall")
    }
    if(choice == 5) {
        analyse_correlation("spearman")
    }
    if(choice == 6) {
        prediction_linear_regression()
    }
    if(choice == 0) {
        return(0)
    }
    choice <- print_choices()
}



# HIERARCHICAL CLUSTERING
#hClustering <- clustering_hierarchical()

# SINGULAR VALUE DECOMPOSITION
# PRINCIPLE COMPONENT ANALYSIS
#analyse_svd_pca()

# CORRELATION ANALYSIS
#analyse_correlation()

# PREDICTION USING LINEAR REGRESSION
#prediction_linear_regression()

# PREDICTION USING PCA
# prediction_pca()

