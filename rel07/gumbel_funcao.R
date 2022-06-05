gumbel_prob <- function(x, location = 0, scale = 1, lower.tail = TRUE) { # cálculo da probabilidade
  z <- (x - location)/scale
  p <- exp(-(z+exp(-z)))/scale
  prob <- exp(-exp(-z))
  if(lower.tail == FALSE){ #check do lower.tail
    prob <- 1 - prob
  }
  return(prob)
}

gumbel_density <- function(x, location, scale){# função usada no gráfica
  z <- (x - location)/scale
  density <- (exp(-(z + exp(-z))))/scale
  return(density)
}

gumbel_prob(-1, 0, 1)

curve(gumbel_density(x, 0, 2), -10, 20)

a <- c(-50:200)/10 # corrigindo a função que estava dando errado, ignorar
z <- (a - 0)/1
p <- exp(-((a - 0)/1+exp(-((a - 0)/1))))/1
plot(p, type = "l")

integrate(gumbel_prop, -100, -1)
