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