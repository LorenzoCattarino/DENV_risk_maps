lm_eqn <- function(df, y, x){
  
  # fit linear model - no intercept 
  frmla <- as.formula(paste0(y, " ~ ", x, " - 1"))
  m <- lm(frmla, df)
  
  #browser()
  
  # calculates R2
  r2 <- 1 - crossprod(residuals(m)) / crossprod(df[,y] - mean(df[,y]))
  #r2 <- summary(m)$r.squared # R 
  
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(b = format(coef(m), digits = 4), 
                        r2 = format(r2, digits = 4)))
  as.character(as.expression(eq))                 
}
