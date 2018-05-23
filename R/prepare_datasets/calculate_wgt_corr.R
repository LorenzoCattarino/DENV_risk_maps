calculate_wgt_cor <- function(d.sub, x, y){
  round(wtd.cor(d.sub[,x], d.sub[,y], weight = d.sub[,"new_weight"])[,"correlation"], 3)
}
