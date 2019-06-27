uncor = function(dataFrame, yName = NULL, yIndex = 1, cutoff = 0.5) {
  uncorrelated = matrix(nrow = 0, ncol = 2)
  if (!is.null(yName)) {
    x = get(paste0(deparse(substitute(dataFrame)),
                   "$",
                   yName))
  } else {
    x = dataFrame[, c(1:length(dataFrame[1,])) != yIndex]
  }
  for(i in 1:(length(names(x)) -1)) {
    for(j in (i+1):length(names(x))) {
      if(abs(cor(x[, i], x[, j])) < cutoff) {
        uncorrelated = rbind(uncorrelated,
                             matrix(cbind(names(x)[i], names(x)[j]), ncol=2))
      }
    }
  }
  return(uncorrelated)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

findBestLM = function(data, pValue = 0.05, yName = NULL, yIndex = 1, xNames = NULL, xIndices = c(2,3)) {
  if (!is.null(yName)) {
    yIndex = which(colnames(data) == yName)
  }
  
  if (!is.null(xNames)) {
    xIndices = which(colnames(data) %in% xNames)
  }
  
  xWithCor = cbind(xIndices, rep(7, length(xIndices)))
  for (i in 1:length(xIndices)) {
    xWithCor[i, 2] = abs(cor(data[, xIndices[i]], data[, yIndex]))
  }
  
  xIndices = xWithCor[order(xWithCor[,2], decreasing = TRUE),][,1]
  
  xTerms = c()
  for (i in xIndices) {
    xTerms = c(xTerms, paste0(deparse(substitute(data)), "[, ", i, "]"))
  }
  # p-Wert klein -> komplexeres Modell besser
  
  for (i in 1:(length(xIndices) -1)) {
    modelBig = lm(reformulate(response = "data[, yIndex]", termlabels = xTerms))
    
    xTerms = xTerms[1:(length(xTerms) -1)]
    xIndices = xIndices[1:(length(xIndices) -1)]
    
    modelSmall = lm(reformulate(response = "data[, yIndex]", termlabels = xTerms))
    
    pValCurrent = anova(modelBig, modelSmall)$`Pr(>F)`[2]
    
    if (pValCurrent <= pValue) {
      return(modelBig)
    }
  }
  
  return(modelSmall)
}





