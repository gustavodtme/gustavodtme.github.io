dist_prob <- function(x, dist, location = 0, scale = 1, lower.tail = TRUE){
  switch (dist, #switch para as outras funcoes
    "gumbel" = {
      gumbel_prob <- function(x, location, scale, tail.lower = TRUE) { # cálculo da probabilidade
        z <- (x - location)/scale
        p <- exp(-(z+exp(-z)))/scale
        prob <- exp(-exp(-z))
        if(tail.lower == FALSE){ #check do lower.tail
          prob <- 1 - prob
        }
      }
      gumbel_density <- function(x, location, scale){# função usada no gráfica
        z <- (x - location)/scale
        density <- (exp(-(z + exp(-z))))/scale
      }
      q <- aux
      curve(gumbel_density(x, 0, 1), -5, 10, ylab = expression(f[Z](z)), xlab="Z")
      aux <- seq(x, 10, by=0.01)
      y <- seq(-5, x, by=0.01)
      fx <- gumbel_density(aux, 0, 1)
      fy <- gumbel_density(y, 0, 1)
      if(lower.tail == TRUE){
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        qq <- round(q, digits=2)
        Pr <- round(gumbel_prob(qq, 0, 1, tail.lower = TRUE), digits=3)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        legend("topleft", bty="n", fill="gray90",
               legend=substitute(P(Z~`>`~q)==Pr, list(q=qq, Pr=Pr)))
        
      } else {
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="gray90")
        qq <- round(q, digits=2)
        Pr <- round(gumbel_prob(qq, 0, 1, tail.lower = FALSE), digits=3)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        legend("topleft", bty="n", fill="gray90",
               legend=substitute(P(Z~`>`~q)==Pr, list(q=qq, Pr=Pr)))
      }
      stop("fim do programa", call. = FALSE, domain = "R-leem")
    },
    {
      stop("You must enter with a valid distribution!", call. = FALSE, domain = "R-leem")
    }
  )
}
dist_prob(1, dist = "gumbel", location = 0, scale = 1)

