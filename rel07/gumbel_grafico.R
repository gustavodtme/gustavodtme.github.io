q <- aux <- 1

placeholder <- TRUE

curve(gumbel_density(x, 0, 1), -5, 10, ylab = expression(f[Z](z)), xlab="Z")
aux <- seq(q, 10, by=0.01)
y <- seq(-5, q, by=0.01)
fx <- gumbel_density(aux, 0, 1)
fy <- gumbel_density(y, 0, 1)

if(placeholder == TRUE){
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        qq <- round(q, digits=2)
        Pr <- round(gumbel_prob(qq, 0, 1,), digits=3)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        legend("topleft", bty="n", fill="gray90",
        legend=substitute(P(Z~`>`~q)==Pr, list(q=qq, Pr=Pr)))

} else {
        polygon(c(aux, rev(aux)),
                c(fx, rep(0, length(fx))),
                col="gray90")
        qq <- round(q, digits=2)
        Pr <- round(gumbel_prob(qq, 0, 1, lower.tail = FALSE), digits=3)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        legend("topleft", bty="n", fill="gray90",
               legend=substitute(P(Z~`>`~q)==Pr, list(q=qq, Pr=Pr)))
}







