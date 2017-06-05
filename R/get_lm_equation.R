lm_eqn <- function(df, y, x){
  frmla <- as.formula(paste(y, x, sep = " ~ "))
  m <- lm(frmla, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))                 
}
