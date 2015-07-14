mahDis <- function(data, group) {
  gps <- unique(group)
  
  if (length(gps) != 2){
    stop('Provide only two groups for comparations')
  }
  
  A <- data[group == gps[1], ]
  B <- data[group == gps[2], ]
  
  medianA <- colMedians(A)
  medianB <- colMedians(B)
  
  na <- nrow(A)
  nb <- nrow(B)
  
  vmahalA <- (cov(A) * pi) / (2 * na)
  vmahalB <- (cov(B) * pi) / (2 * nb)
  
  totalVar <- ((na - 1) * vmahalA + (nb - 1) * vmahalB) / (na + nb - 2)
  
  v <- medianA - medianB
  
  as.numeric(t(v) %*% solve(totalVar) %*% v)
}

permutationPvalue <- function(replication, data, group) {
  distVector <- replicate(replication, {mahDis (data, sample(group))})
  realDist <- mahDis (data, group)
  pValue <- mean(distVector > realDist)
  pValue
}