# idea, hierarchical color assignment

#' map n parts into the cyclic range of [-.5 , .5]
divide <- function(n, space = 1){
  i <- seq_len(n)
  (i - 0.5)/(n)
}

# 
# 
divide(2)
divide(3)
divide(2, space=2)

# x should be a data.frame of factors
recursiveDivide <- function(x, spacing=1){
  parts <- droplevels(x[[1]])
  x <- x[,-1,drop=FALSE]
  
  n <- nlevels(parts)
  res <- divide(n)[parts]
  names(res) <- rownames(x)
  
  if (length(x)){
    F <- n+spacing
    
    sp <- unname(split(x, parts))
    s <- (unlist(lapply(sp, recursiveDivide)) - 0.5) / F
    s <- s[names(res)]
    res <- res + s
  }  
  res
}

showHier <- function(d){
  d <- sort(d)
  barplot(matrix(1, nrow=length(d), ncol=1), col=hsv(d), border=NA, axes=FALSE)  
}

# reg <- data.frame( LD=c("N","N","N","W","W","W", "E", "E", "E", "S", "S", "S")
#                  , PV=c("Gr","F","D","NH","SH","F", "U", "O", "G", "Z", "NB", "L")
#                  )
# 
# rd <- recursiveDivide(reg, spacing=2)
# showHier(rd)
# 
