ROCStatFunc <- function(dat, group, var,retype = c("threshold", "specificity", "sensitivity","npv","ppv"),
                        auc = T,youden = T, digit = 3){
  subgroup <- levels(as.factor(dat[[group]]))
  subgroup1 <- paste0(subgroup[2], " vs ", subgroup[1])
  rocmodel <- roc(dat[[group]], dat[[var]])
  other <- coords(rocmodel, "b", ret = retype)
  other <- round(other, digit)
  if(auc == T){
    auc <- round(ci.auc(rocmodel),digit)
    auc <- paste0(auc[2],"(",auc[1],"-",auc[3],")")
    if(youden == T){
      abc <- coords(rocmodel, "b", ret = c("specificity", "sensitivity"))
      youdenres <- abc[1] + abc[2] - 1
      youdenres <- round(youdenres, digit)
      result <- c(group, subgroup1, auc, other, youdenres)
      names(result) <- c("group", "subgroup","auc(95%CI)", retype, "youden")
    }else{
      result <- c(group, subgroup1, auc, other)
      names(result) <- c("group", "subgroup", "auc(95%CI)", retype)
    }
  }else{
    if(youden == T){
      abc <- coords(rocmodel, "b", ret = c("specificity", "sensitivity"))
      youdenres <- abc[1] + abc[2] - 1
      youdenres <- round(youdenres, digit)
      result <- c(group, subgroup1, other, youdenres)
      names(result) <- c("group","subgroup", retype, "youden")
    }else{
      result <- c(group, subgroup1,other)
      names(result) <- c("group", "subgroup",retype)
    }
  }
  return(result)
}