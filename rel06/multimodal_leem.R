mfreq <- function (x, details = FALSE, na.rm = FALSE, rounding = 2, grouped = TRUE) 
{
  if (!is.numeric(rounding) | rounding < 0) {
    stop("The 'rounding' argument must be numeric and positive!", 
         call. = FALSE, domain = "R-leem")
  }
  rounding <- trunc(rounding)
  if (!is.logical(details)) {
    stop("The 'details' argument must be logical!", 
         call. = FALSE, domain = "R-leem")
  }
    
  if (!is.logical(grouped)) {
    stop("The 'grouped' argument must be logical!", 
         call. = FALSE, domain = "R-leem")
  }
    
  if (!is.logical(na.rm)) {
    stop("The 'na.rm' argument must be logical!", call. = FALSE, 
         domain = "R-leem")
  }
    
  if (class(x) != "leem") {
    stop("Use the 'new_leem()' function to create an object of class leem!", 
         call. = FALSE)
  }
    
  if (class(x) == "leem" & is.null(attr(x, "table"))) 
    x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar == 0) {
      if (all(x$tabela$Fi == x$tabela$Fi[1])) {
        mo <- "The data set has no mode!"
      }
      else {
        pos <- which(x$tabela$Fi == max(x$tabela$Fi))
        mo <- x$tabela$Groups[pos]
      }
    }
    else {
      if (all(x$tabela$Fi == x$tabela$Fi[1])) {
        mo <- "The data set has no mode!"
      }
      else {
        pos <- which(x$tabela$Fi == max(x$tabela$Fi))
        mo <- round(as.numeric(x$tabela$Groups[pos]), rounding)
      }
    }
    resume <- list(mode = mo, table = x$tabela, rawdata = x$estat$raw_data)
    if (details) {
      return(resume)
    }
    else {
      return(mo)
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped) {
      pos <- which(x$tabela$Fi == max(x$tabela$Fi))
      compos <- length(pos)
      mo <- vector(mode = "integer", length = compos)
      j <- 1
      for(i in pos) {
        if (i == 1) {
          aux1 <- 0
        }
        else {
          aux1 <- x$tabela$Fi[i - 1]
        }
        if (i == x$estat$Numero_de_classes) {
          aux2 <- 0
        }
        else {
          aux2 <- x$tabela$Fi[i + 1]
        }
        del1 <- x$tabela$Fi[i] - aux1
        del2 <- x$tabela$Fi[i] - aux2
        mo[j] <- x$estat$LI_classes[i] + (del1/(del1 + del2)) * 
          x$estat$Ampl_clas
        j <- j + 1
        
      }
      mo <- round(mo, rounding)
      resume <- list(mode = mo, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(mo)
      }
    } else {
      x <- x$estat$raw_data
      x <- new_leem(x, 1)
      x <- tabfreq(x)
      if (all(x$tabela$Fi == x$tabela$Fi[1])) {
        mo <- "The data set has no mode!"
      }
      else {
        pos <- which(x$tabela$Fi == max(x$tabela$Fi))
        mo <- round(x$tabela$Groups[pos], rounding)
      }
      resume <- list(mode = mo, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(mo)
      }
      }
   
  }
}

