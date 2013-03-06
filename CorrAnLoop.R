correlation <- function(directory = "MJ Data Extract.csv", metric1 = "LTD.Unit.Rec", metric2 = "LTD.Unit.Sold") {
#Set Variables
  j <-1
  framelist = list()
#Read data and extract column names  
  x <- read.csv(directory)
  headers <- colnames(x)
  headers.Rowcount <- NROW(headers)
  dimensions <- headers[1:10]
  dimensions.Rowcount <- NROW(dimensions)
  expressions <- headers [11:24]
  expressions.Rowcount <- NROW(headers)
#Loop through fields
  for (j in 1:dimensions.Rowcount) {
#Set Variables that need to be reset within first loop
    i <- 1
    corr <- numeric(0)
    field <- headers[[j]]
    column <- x[,field]  
    levels <- levels(column)
    levels.Rowcount <- NROW(levels)
#Loop through metrics
    for (i in 1:levels.Rowcount) {
      level <- levels[[i]]
      reduced <- x[x[,field] == level,]
     met1 <- reduced[,metric1]
     met2 <- reduced[,metric2]
      corr[[i]] <- cor(met1, met2)
     i <- i+1
    }
    framelist[[j]] <- data.frame(levels,corr)
    j<- j+1
    print(framelist)
  }  

}