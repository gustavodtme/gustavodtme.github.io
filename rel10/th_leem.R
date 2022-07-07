th <- function(x, y = NULL, test = "ttest", plot = FALSE, alpha = 0.05, decision = "cp", h0 = NULL, alternative = "two-sided") {
  plotcurve <- function(t, gl, alpha, ttab1, rounding =2, a = 1){
    if (a == 1) {
      curve(dt(x, df = gl), -5, 5, ylab = expression(f[G](g)), xlab="G")
      aux <- seq(-ttab1, ttab1, by=0.01)
      y1<- seq(-5, -ttab1, by=0.01)
      y2 <- seq(ttab1, 5, by=0.01)
      fx <- dt(aux, gl)
      fy1 <- dt(y1, gl)
      fy2 <- dt(y2, gl)
      polygon(c(aux, rev(aux)),
              c(fx, rep(0, length(fx))),
              col="gray90")
      polygon(c(y1, rev(y1)),
              c(fy1, rep(0, length(fy1))),
              col="red")
      polygon(c(y2, rev(y2)),
              c(fy2, rep(0, length(fy2))),
              col="red")
      abline(v = t, lty=2, col = "black")
      qq <- round(t, digits=2)
      t2 <- round(t ,digits = 2)
      qqaux <-round(ttab1, digits=2)
      Pr <- round(pt(qq, gl), digits = rounding)
      Pr <- gsub("\\.", ",", Pr)
      qq <- gsub("\\.", ",", qq)
      axis(side=1, at=t2, labels=t2, col="black", font = 2)
      abline(v = -qqaux, lty=2, col = "red")
      abline(v = qqaux, lty=2, col = "red")
      legend("topleft", bty="n", fill="red",
             legend=substitute(Alfa == P, list(P=alpha)))
    }
    if (a == 2) {
      curve(dt(x, df = gl), -5, 5, ylab = expression(f[G](g)), xlab="G")
      aux <- seq(-5, ttab1, by=0.01)
      y2 <- seq(ttab1, 5, by=0.01)
      fx <- dt(aux, gl)
      fy2 <- dt(y2, gl)
      polygon(c(aux, rev(aux)),
              c(fx, rep(0, length(fx))),
              col="gray90")
      polygon(c(y2, rev(y2)),
              c(fy2, rep(0, length(fy2))),
              col="red")
      abline(v = t, lty=2, col = "black")
      qq <- round(t, digits=2)
      t2 <- round(t, digits=2)
      qqaux <-round(ttab1, digits=2)
      Pr <- round(pt(qq, gl), digits = rounding)
      Pr <- gsub("\\.", ",", Pr)
      qq <- gsub("\\.", ",", qq)
      axis(side=1, at=t2, labels=t2, col="black", font = 2)
      abline(v = qqaux, lty=2, col = "red")
      legend("topleft", bty="n", fill="red",
             legend=substitute(Alfa == P, list(P=alpha)))
    }
    if (a == 3){
      curve(dt(x, df = gl), -5, 5, ylab = expression(f[G](g)), xlab="G")
      aux <- seq(ttab1, 5, by=0.01)
      y1<- seq(-5, ttab1, by=0.01)
      fx <- dt(aux, gl)
      fy1 <- dt(y1, gl)
      polygon(c(aux, rev(aux)),
              c(fx, rep(0, length(fx))),
              col="gray90")
      polygon(c(y1, rev(y1)),
              c(fy1, rep(0, length(fy1))),
              col="red")
      abline(v = t, lty=2, col = "black")
      qq <- round(t, digits=2)
      t2 <- round(t, digits=2)
      qqaux <-round(ttab1, digits=2)
      Pr <- round(pt(qq, gl), digits = rounding)
      Pr <- gsub("\\.", ",", Pr)
      qq <- gsub("\\.", ",", qq)
      axis(side=1, at=t2, labels=t2, col="black", font = 2)
      abline(v = qqaux, lty=2, col = "red")
      legend("topleft", bty="n", fill="red",
             legend=substitute(Alfa == P, list(P=alpha)))
    }
  }
  if(alpha <= 0 || alpha >= 1 ) stop("'alpha' deve ser estar compreendido entre 0 e 1!", call. = FALSE)
  if (is.logical(plot) != TRUE) stop ("'plot' deve ser uma variável do tipo lógico", call. = FALSE)
  if(test == "ttest"){
    if(is.null(y) == FALSE) {
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
        if (plot == TRUE) {
          plotcurve(t, gl, alpha, ttab1, a = 1)
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
        if (plot == TRUE) {
          plotcurve(t, gl, alpha, ttab1, a = 1)
        }
      }
    } else {
      if(all(alternative != c("two.sided", "t", "T", "greater", "g", "G", "less", "l", "L"))) stop("'alternative' deve ser 'two.sided', 'greater' ou 'less'", call. = FALSE)
      if(is.nan(h0) == TRUE) stop("insira o argumento 'h0!'", call. = FALSE)
      if(is.numeric(h0) != TRUE) stop("A média deve ser um valor numérico!", call. = FALSE)
      if(any(alternative == c("two.sided", "t", "T"))){
        n <- length(x)
        xbar <- mean(x)
        sdev <- sd(x)
        t <- (xbar - h0)/(sdev/sqrt(n))
        gl <- n -1
        ttab1 <- qt(1-(alpha/2), df = gl)
        p <- 2*pt(abs(t), gl, lower.tail = FALSE)
        cat("T-teste de uma amostra com h0 sendo média igual a", h0, "\n \n")
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
        if (plot == TRUE) {
          plotcurve(t, gl, alpha, ttab1, a = 1)
        }
      }
      if (any(alternative == c("greater", "g", "G"))){
        n <- length(x)
        xbar <- mean(x)
        sdev <- sd(x)
        t <- (xbar - h0)/(sdev/sqrt(n))
        gl <- n -1
        ttab1 <- qt(1-alpha, df = gl)
        p <- pt(t, gl, lower.tail = FALSE)
        cat("T-teste de uma amostra com h1 sendo média maior que", h0, "\n \n")
        resultado <- list("T_teste" = t, "Ponto_Critico (+-)" = ttab1)
        print(resultado)
        cat("\n ============================\n \n")
        cat(paste("Valor-p: ", p), "\n")
        cat("\n ============================\n")
        if (t >= ttab1) {
          cat("\n Rejeita-se H0!")
        } else {
          cat("\n Não se rejeita H0!")
        }
        if (plot == TRUE) {
          plotcurve(t, gl, alpha, ttab1, a = 2)
        }
      }
      if (any(alternative == c("less", "l", "L"))) {
        n <- length(x)
        print(n)
        xbar <- mean(x)
        sdev <- sd(x)
        t <- (xbar - h0)/(sdev/sqrt(n))
        gl <- n -1
        ttab1 <- qt(alpha, df = gl)
        p <- pt(t, gl, lower.tail = TRUE)
        cat("T-teste de uma amostra com h1 sendo média menor que", h0, "\n \n")
        resultado <- list("T_teste" = t, "Ponto_Critico (+-)" = ttab1)
        print(resultado)
        cat("\n ============================\n \n")
        cat(paste("Valor-p: ", p), "\n")
        cat("\n ============================\n")
        if (t <= ttab1) {
          cat("\n Rejeita-se H0!")
        } else {
          cat("\n Não se rejeita H0!")
        }
        if (plot == TRUE) {
          plotcurve(t, gl, alpha, ttab1, a = 3)
        }
      }
    }
    
  }
  else stop("ainda de implementar o resto", call. = FALSE)
}