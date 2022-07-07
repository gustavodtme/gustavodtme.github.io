#' Test of hypothesis
#'
#' Performs hypothesis testing for various parameters of one or more populations
#'
#' @param x R object. See in details.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param test character value. The options are: \code{"ttest"}, \code{"ztest"}, \code{"ptest"}, \code{"chitest"}, \code{"ftest"}, \code{"anova"}, \code{"friedman"}, \code{"kruskal"}.
#' @param h0 numeric value. The hypothesized parameter.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param alpha significance level of the test
#' @param exact a logical indicating whether you want to use the exact test or not. Default is \code{exact=TRUE}.
#' @param correct a logical indicating whether Yates' continuity correction should be applied where possible. This argument must be used when \code{exact = FALSE}.
#' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' @param plot a logical indicating whether you want a graph indicating the regions of rejection or not of the null hypothesis, as well as the test decision.
#'
#' @export
th <- function(x, y = NULL, test = "ztest", h0, alternative = "two.sided", alpha = 0.05,
               exact = TRUE, correct = FALSE, paired = FALSE, plot = FALSE, ...) {
  argaddit <- list(...)
  if (missing(x)) {
    xfile <- file.choose(new = TRUE)
    x <- read.table(xfile, h = TRUE)
  }
  # Z test:
  if (test == "ztest") {
    if (!any(names(argaddit) == "sd")) {
      sdev <- readline("Insert the value of standard deviation population? ")
      sdev <- as.numeric(sdev)
    } else sdev <- argaddit$sd
    if (missing(h0)) {
      h0 <- readline("Insert the value of null hypothesis? ")
      h0 <- as.numeric(h0)
    }
    if (any(alternative == c("two.sided", "t", "T"))) {
      if (is.null(y)) {
        n <- length(x)
        ztest <- (mean(x) - h0) / (sdev /sqrt(n))
        ztab <- qnorm(1 - alpha)
        ztab <- c(-ztab, ztab)
        cat("    Z teste bilateral   \n")
        cat("==============\n")
        cat(paste("O valor do teste eh: ", ztest), "\n")
        cat("\n ==============\n")

        resultado <- list("Z_teste" = ztest, "Ponto_Critico (+-)" = ztab)
        print(resultado)

        cat(paste("Valor-p: ", 2 * pnorm(abs(ztest), lower.tail = FALSE)), "\n")

        cat("\n ==============\n")

        if (abs(ztest) >= ztab[2]) {
          cat("\n ==============")
          cat("\n Rejeita-se H0!")
        } else {
          cat("\n ==============")
          cat("\n Não se rejeita H0!")
        }

      }
    }

    if (any(alternative == c("less", "l", "L"))) {
      print("less")
    }

    if (any(alternative == c("greater", "g", "G"))) {
      print("greater")
    }


    #ztest <- (mean(x) - h0) / (sdev)

  }
}



