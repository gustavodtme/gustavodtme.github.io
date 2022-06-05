q <- x <- 0

curve(dnorm(x, 0, 1), -5, 5, ylab = expression(f[Z](z)), xlab="Z")
x <- seq(q, 4, by=0.01)
y <- seq(-4, -q, by=0.01)
fx <- dnorm(x, 0, 1)
fy <- dnorm(y, 0, 1)
polygon(c(x, rev(x)),
        c(fx, rep(0, length(fx))),
        col="gray90")
polygon(c(y, rev(y)),
        c(fy, rep(0, length(fy))),
        col="gray90")
abline(v=0, lty=2)
qq <- round(q, digits=2)
Pr <- round(pnorm(qq, 0, 1, lower.tail = F), digits=5)
Pr <- gsub("\\.", ",", Pr)
qq <- gsub("\\.", ",", qq)
legend("topleft", bty="n", fill="gray90",
       legend=substitute(P(-q~`<`~Z~`>`~q)==Pr, list(q=qq, Pr=Pr)))

