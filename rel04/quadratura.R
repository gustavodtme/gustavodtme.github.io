xk <- SMR:::GaussLegendre(20) # Calculando os pesos da quadratura pos Legendre

lim <- -0.5

fxgauss <- function(x, b, media, dp) {
  aux <- (b/dp)+ ((1 + x)/(x-1)*dp) + (media/dp) 
  aux2 <- 2*exp(-0.5*aux^2)  
  aux2/((2.506628)*(x-1)^2)
}
sum(xk$weights * fxgauss(xk$nodes, lim, 0, 1) )

pnorm(-0.5, 0, 1)
