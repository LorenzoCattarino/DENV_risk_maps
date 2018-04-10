calculate_wgt_cor <- function(d.sub){
  round(wtd.cor(d.sub[,x], d.sub[,y], weight = d.sub[,"new_weight"]), 3)
}
