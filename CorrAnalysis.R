correlation <- function(field = "Chain.Name", directory = "MJ Data Extract.csv", metric1 = "WTD.Rec.U", metric2 = "WTD.Unit.Sold") {
  i <- 1
  corr <- numeric(0)
  x <- read.csv(directory)
  column <- x[,field]  
  levels <- levels(column)
  levels.Rowcount <- NROW(levels)
  for (i in levels.Rowcount) {
    level <- levels[[i]]
#    reduced <- x[x$field == level,]
    reduced <- x[x$Chain.Name == level,]
    preconvert1 <- reduced$WTD.Rec.U
    preconvert2 <- reduced$WTD.Unit.Sold
    met1 <- as.numeric(levels(preconvert1))[preconvert1]
    met2 <- as.numeric(levels(preconvert2))[preconvert2]
#    corr[[i]] <- cor(paste0("reduced$",metric1),paste0("reduced$",metric2))
     corr[[i]] <- cor(met1, met2)
    i = i+1
  }
  corr
}