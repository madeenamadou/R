# completer des donnees manquantes a partir d'un ensemble de conditions sur les donnees

Map(function(x){
  
  sst1 = na.exclude(x[grepl('SST1',CPAC_Source_ID),Disp_1_SndTime])[1]
  if (!sst1 %>% is.na) {x[grepl('SST1',CPAC_Source_ID),
                          Disp_1_SndTime:=ifelse(is.na(Disp_1_SndTime),sst1,Disp_1_SndTime)]}
  
  sst2 = na.exclude(x[grepl('SST2',CPAC_Source_ID),Disp_1_SndTime])[1]
  if (!sst2 %>% is.na) {x[grepl('SST2',CPAC_Source_ID),
                          Disp_1_SndTime:=ifelse(is.na(Disp_1_SndTime),sst2,Disp_1_SndTime)]}
  
  edt1 = na.exclude(x[grepl('EDT1',CPAC_Source_ID),Disp_1_SndTime])[1]
  if (!edt1 %>% is.na) {x[grepl('EDT1',CPAC_Source_ID),
                          Disp_1_SndTime:=ifelse(is.na(Disp_1_SndTime),edt1,Disp_1_SndTime)]}
  
  edt2 = na.exclude(x[grepl('EDT2',CPAC_Source_ID),Disp_1_SndTime])[1]
  if (!edt2 %>% is.na) {x[grepl('EDT2',CPAC_Source_ID),
                          Disp_1_SndTime:=ifelse(is.na(Disp_1_SndTime),edt2,Disp_1_SndTime)]}
  
  uri1 = na.exclude(x[grepl('URI1',CPAC_Source_ID),Disp_1_SndTime])[1]
  if (!uri1 %>% is.na) {x[grepl('URI1',CPAC_Source_ID),
                          Disp_1_SndTime:=ifelse(is.na(Disp_1_SndTime),uri1,Disp_1_SndTime)]}
  
  return (x)
},t_l$na)

Reduce(rbind,append(t_l_na_c,t_l$nna))
