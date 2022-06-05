# Probability
#' @importFrom manipulate manipulate slider
#' @export
p <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "t-student") {
    if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
    if (lower.tail) {
      plotcurve <- function(q, nu) {
        curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(-6, q, by=0.01)
        y <- seq(q, 6, by=0.01)
        fx <- dt(x, df = nu)
        fy <- dt(y, df = nu)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pt(qq, df = nu, lower.tail = T), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T<=q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
      }
      if (gui == "plot" ) {
        # Probability
        nu <- argaddit$df
        prob <- pt(q = q, df = nu)
        # Plot
        plotcurve(q, nu)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, nuaux),
                               qaux = manipulate::slider(-6, 6, q),
                               nuaux = manipulate::slider(nu, nu + 200, nu))
      }
    } else {
      plotcurve <- function(q, nu) {
        curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(q, 6, by=0.01)
        y <- seq(-6, q, by=0.01)
        fx <- dt(x, df = nu)
        fy <- dt(y, df = nu)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pt(qq, df = nu, lower.tail = F), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T~`>`~q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
      }
      if (gui == "plot") {
        # Probability
        nu <- argaddit$df
        prob <- pt(q = q, df = nu)
        # Plot
        plotcurve(q, nu)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, nuaux),
                               qaux = manipulate::slider(-6, 6, q),
                               nuaux = manipulate::slider(nu, nu + 200, nu))
      }
      
    }
  }
  if (dist == "gumbel"){
    if (!any(names(argaddit) == "location")) stop("Insira o argumento 'location'!", call. = FALSE)
    if (!any(names(argaddit) == "scale")) stop("Insira o argumento 'scale'!", call. = FALSE)
    if (argaddit$scale < 0 ) stop("o argumento 'scale' deve ser maior que zero!", call. = FALSE)
    if (lower.tail) {
      plotcurve <- function(q, location, scale){
        curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
        aux <- seq(q, 10, by=0.01)
        y <- seq(-5, q, by=0.01)
        fx <- dgumbel(aux, location, scale)
        fy <- dgumbel(y, location, scale)
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="gray90")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="red")
        abline(v = location, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pgumbel(qq, location, scale), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(G<=q)==Pr, list(q=qq, Pr=Pr)))
      }
      if (gui == "plot" ) {
        # Probability
        location <- argaddit$location
        scale <- argaddit$scale
        prob <- pgumbel(q = q, location, scale)
        # Plot
        plotcurve(q, location, scale)
      }
      if (gui == "rstudio") {
        location <- argaddit$location
        scale <- argaddit$scale
        manipulate::manipulate(plotcurve(q, location, scale),
                               q = manipulate::slider(-5, 10, q),
                               location = manipulate::slider(location - 10, location + 10, location ),
                               scale = manipulate::slider(1, scale + 100, scale))
        prob <- pgumbel(q = q, location, scale)
      }
    } else {
      plotcurve <- function(q, location, scale){
        curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
        aux <- seq(q, 10, by=0.01)
        y <- seq(-5, q, by=0.01)
        fx <- dgumbel(aux, location, scale)
        fy <- dgumbel(y, location, scale)
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v = location, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pgumbel(qq, location, scale, lower.tail = FALSE), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(G>q)==Pr, list(q=qq, Pr=Pr)))
      }
      if (gui == "plot" ) {
        # Probability
        location <- argaddit$location
        scale <- argaddit$scale
        prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
        # Plot
        plotcurve(q, location, scale)
      }
      if (gui == "rstudio") {
        location <- argaddit$location
        scale <- argaddit$scale
        manipulate::manipulate(plotcurve(q, location, scale),
                               q = manipulate::slider(-5, 10, q),
                               location = manipulate::slider(location - 10, location + 10, location ),
                               scale = manipulate::slider(1, scale + 100, scale))
        prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
      }
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}

pgumbel <- function(q, location = 0, scale = 1, lower.tail = TRUE) {
  z <- (q - location)/scale
  p <- exp(-(z+exp(-z)))/scale
  prob <- exp(-exp(-z))
  if(lower.tail == FALSE){
    prob <- 1 - prob
  }
  return(prob)
}

dgumbel <- function(x, location, scale){
  z <- (x - location)/scale
  density <- (exp(-(z + exp(-z))))/scale
  return(density)
}
p(q =1, dist = "gumbel", location = 2, scale = 1, gui = "rstudio", lower.tail = FALSE)
