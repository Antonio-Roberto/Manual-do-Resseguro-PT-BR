estatisticas <- function(x){
  x <- x[!is.na(x)]
  me <- mean(x)
  med <- median(x)
  n <- length(x)
  s <- sd(x)
  cofv<- sd(x)/mean(x)
  mi <- min(x)
  ma <- max(x)
  ampl <- ma-mi
  q25 <- quantile(x, probs = 0.25)
  q75 <- quantile(x, probs = 0.75)
  Estatisticas<- c("N", "Média", "Mediana", "Desvio", "CV", "Quantil 25%", "Quantil 75%", "Min", "Max", "Amplitude")
  Descritivas<- c(n, me, med, s,cofv, q25, q75, mi, ma, ampl)
  return(data.frame(Estatisticas, Descritivas, stringsAsFactors = FALSE))
}
print(estatisticas(sin))