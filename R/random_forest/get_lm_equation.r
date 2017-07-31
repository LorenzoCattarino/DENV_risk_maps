lm_eqn <- function(df, y, x){
  frmla <- as.formula(paste0(y, " ~ ", x, " - 1"))
  m <- lm(frmla, df)
  #browser()
  n <- length(df[,y])
  sse <- sum((df[,y] - m$fitted.values)^2) 
  sst <- sum(df[,y]^2) - ((sum(df[,y]))^2 / n)
  r2 <- 1 - (sse / sst) # as in excel 2016
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(b = format(coef(m), digits = 4), 
                        r2 = format(r2, digits = 4)))
  as.character(as.expression(eq))                 
}
