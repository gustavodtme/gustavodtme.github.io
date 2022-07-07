x <- rnorm(100, 50, 2)
y <- rnorm(100, 50, 2)
m1 <- mean(x)
m2 <- mean(y)
sd1 <- sd(x)
sd2 <- sd(y)

a <- function(x, y, alpha = 0.05) {
  sd1 <- sd(x)
  sd2 <- sd(y)
  variance <- sd1/sd2
  if (variance > 0.5 & variance < 2){
    m1 <- mean(x)
    m2 <- mean(y)
    n1 <-length(x)
    n2 <-length(y)
    gl <- n1 + n2 - 2
    sp <- sqrt(((n1-1)*(sd1^2)+(n2-2)*(sd2^2))/(n1+n2-2))
    t <- (m1-m2)/(sp*sqrt((1/n1)+(1/n2)))
    ttab1 <- qt(1-(alpha/2), df = gl)
    p <- 2*pt(abs(t), gl, lower.tail = FALSE)
    cat("T-teste de duas amostras com variâncias semelhantes \n \n")
    resultado <- list("T_teste" = t, "Ponto_Critico (+-)" = ttab1)
    print(resultado)
    cat("\n ============================\n \n")
    cat(paste("Valor-p: ", p), "\n")
    cat("\n ============================\n")
    if (abs(t) >= ttab1) {
      cat("\n Rejeita-se H0!")
    } else {
      cat("\n Não se rejeita H0!")
    }
  } else {
    m1 <- mean(x)
    m2 <- mean(y)
    n1 <-length(x)
    n2 <-length(y)
    r1 <- (sd1^2)/n1
    r2 <- (sd2^2)/n2
    sdel <- sqrt(r1+r2)
    t <- (m1-m2)/sdel
    gl <- ((r1+r2)^2)/((r1^2)/(n1-1) + (r2^2)/(n2-1))
    ttab1 <- qt(1-(alpha/2), df = gl)
    p <- 2*pt(abs(t), gl, lower.tail = FALSE)
    cat("T-teste de duas amostras com variâncias diferentes \n \n")
    resultado <- list("T_teste" = t, "Ponto_Critico (+-)" = ttab1)
    print(resultado)
    cat("\n ============================\n \n")
    cat(paste("Valor-p: ", p), "\n")
    cat("\n ============================\n")
    if (abs(t) >= ttab1) {
      cat("\n Rejeita-se H0!")
    } else {
      cat("\n Não se rejeita H0!")
    }
  }
}
a(x, y)
