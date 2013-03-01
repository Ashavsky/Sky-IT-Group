correlation <- function(field = "Chain.Name", directory = "MJ Data Extract.csv", metric1 = "LTD.Unit.Rec", metric2 = "LTD.Unit.Sold") {
  i <- 1
  corr <- numeric(0)
  x <- read.csv(directory)
  column <- x[,field]  
  levels <- levels(column)
  levels.Rowcount <- NROW(levels)
  for (i in 1:levels.Rowcount) {
    level <- levels[[i]]
    reduced <- x[x[,field] == level,]
    met1 <- reduced[,metric1]
    met2 <- reduced[,metric2]
     corr[[i]] <- cor(met1, met2)
    i <- i+1
  }
  data.frame(levels,corr)
}