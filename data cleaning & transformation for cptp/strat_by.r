# Rscript
#
# Par Youssef de Madeen Amadou
#
# Stratifier un dataset d'apres les valeurs d'une colonne donnee
# et appliquer des transformations a partir d'un ensemble de conditions
#


rm(list = ls(all.names = TRUE))
cat("\014")
source('~/cptp/opal-login.r')
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

strat_by = function(x,y,z){
  
  # Paremetres : 
  # - x : la table a stratifier
  # - y : la variable de stratification
  # - z : la variable a observer
  
  t0 = list();
  ds = eval(parse(text=x)) %>% sapply(.,as.character) %>% data.table
  tmp = Map(function(i) eval(parse(text=paste0('ds[',y,'==\'',i,'\',]'))),unique(ds[,y,with=F]) %>% unlist)
  t0$na = Filter(function(j) anyNA(j[,z,with=FALSE]),tmp)
  t0$nna = Filter(function(j) !anyNA(j[,z,with=FALSE]),tmp)
  
  return (t0)
}

do.call(strat_by,list('dss',c('CPACID'),c('Disp_1_SndTime')))

#END