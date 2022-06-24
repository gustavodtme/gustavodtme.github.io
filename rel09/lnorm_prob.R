p2 <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "lognormal"){
    if (!any(names(argaddit) == "meanlog")) stop("Insira o argumento 'meanlog'!", call. = FALSE)
    if (!any(names(argaddit) == "sdlog")) stop("Insira o argumento 'sdlog'!", call. = FALSE)
    if (argaddit$sdlog < 0 ) stop("o argumento 'sdlog' deve ser maior que zero!", call. = FALSE)
    if (lower.tail) {
      plotcurve <- function(q, meanlog, sdlog){
        curve(dlnorm(x, meanlog, sdlog), 0, 50, ylab = expression(f[G](g)), xlab="G")
        aux <- seq(q, 50, by=0.01)
        y <- seq(0, q, by=0.01)
        fx <- dlnorm(aux, meanlog, sdlog)
        fy <- dlnorm(y, meanlog, sdlog)
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="gray90")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="red")
        abline(v = (exp(meanlog + ((sdlog)^2)/2)), lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(plnorm(qq, meanlog, sdlog), digits = rounding)
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
        meanlog <- argaddit$meanlog
        sdlog <- argaddit$sdlog
        prob <- plnorm(q = q, meanlog, sdlog)
        # Plot
        plotcurve(q, meanlog, sdlog)
      }
      if (gui == "rstudio") {
        meanlog <- argaddit$meanlog
        sdlog <- argaddit$sdlog
        manipulate::manipulate(plotcurve(q, meanlog, sdlog),
                               q = manipulate::slider(0, 50, q),
                               meanlog = manipulate::slider(meanlog - 10, meanlog + 10, meanlog ),
                               sdlog = manipulate::slider(1, sdlog + 10, sdlog))
        prob <- plnorm(q = q, meanlog, sdlog)
      }
    } else {
      plotcurve <- function(q, meanlog, sdlog){
        curve(dlnorm(x, meanlog, sdlog), 0, 50, ylab = expression(f[G](g)), xlab="G")
        aux <- seq(q, 50, by=0.01)
        y <- seq(0, q, by=0.01)
        fx <- dlnorm(aux, meanlog, sdlog)
        fy <- dlnorm(y, meanlog, sdlog)
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v = (exp(meanlog + ((sdlog)^2)/2)), lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(plnorm(qq, meanlog, sdlog, lower.tail = FALSE), digits = rounding)
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
        prob <- plnorm(q = q, meanlog, sdlog, lower.tail = FALSE)
        # Plot
        plotcurve(q, meanlog, sdlog)
      }
      if (gui == "rstudio") {
        meanlog <- argaddit$meanlog
        sdlog <- argaddit$sdlog
        manipulate::manipulate(plotcurve(q, meanlog, sdlog),
                               q = manipulate::slider(0, 50, q),
                               meanlog = manipulate::slider(meanlog - 10, meanlog + 10, meanlog ),
                               sdlog = manipulate::slider(1, sdlog + 10, sdlog))
        prob <- plnorm(q = q, meanlog, sdlog, lower.tail = FALSE)
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

p2(2,dist = "lognormal", meanlog = 0, sdlog = 1, gui = "rstudio", lower.tail = FALSE)

