#NOTE: .CSV must be formatted as follows:
# 1) All metric values must be number formatted (i.e. no $'s)
# 2) Dimensions must be on the left, expressions on right

correlation <- function(directory = "MJ Data Extract.csv", dimensionCount = 10) {
#Set Variables
  j <-1
  final = list()
#Read data and extract column names  
  x <- read.csv(directory)
  headers <- colnames(x)
  headers.Rowcount <- NROW(headers)
  dimensions <- headers[1:dimensionCount]
#  dimensions.Rowcount <- NROW(dimensions) UNUSED
#  expressions <- headers[11:24] UNUSED
  expressions <- headers [(1+dimensionCount):headers.Rowcount]
  expressions.Rowcount <- NROW(expressions)
#Loop through fields
  for (j in 1:dimensionCount) {
#Set Variables that need to be reset within first loop
    i <- 1
    corr <- numeric(0)
    framelist = list()
    field <- headers[[j]]
    column <- x[,field]  
    levels <- levels(column)
    levels.Rowcount <- NROW(levels)
#Loop through metrics
    for (i in 1:levels.Rowcount) {
#Reset k for loop and reset corrmatrix for new run through loop
      k <-1
      corrmatrix <- rep(NA,14)         
      level <- levels[[i]]
      reduced <- x[x[,field] == level,]
#Loop through 1st metrics      
        for (k in 1:expressions.Rowcount) {
          met1 <- reduced[,expressions[[k]]]
          l <- 1
#Loop through 2nd metrics          
          for (l in 1:expressions.Rowcount) {
            met2 <- reduced[,expressions[[l]]]
            corr[[l]] <- cor(met1, met2)
            l <- l+1
          }
#Create matrix for one entry from one field with 14 X 14 correlations (number of metrics = 14)          
          corrmatrix <- rbind(corrmatrix,corr)
          k <- k+1
      }
#Rename column headers to expression headers
      corrDataframe <- data.frame(corrmatrix)
      names(corrDataframe) <- expressions     
#Rename row names and remove first NA row          
      corrDataframe = corrDataframe[-1,]
      rownames(corrDataframe) <- expressions
      framelist[[level]] <- corrDataframe     
      i <- i+1
    }
    print(framelist)
    final[[field]] <- framelist
    j<- j+1
  }  

}