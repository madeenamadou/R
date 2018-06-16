# tableau croise de variables d'un dataset
dcast(atp_bs[,list(ADM_PART_ID,SAMPLE_TYPE)], ADM_PART_ID ~ SAMPLE_TYPE, value.var = c("SAMPLE_TYPE"))
dcast(merge[,list(Sample_TYPE,Type_source)],Sample_TYPE~Type_source,value.var = c("Type_source"))

# ajouter les totaux au tableau croise de la fonction table
addmargins(with(merge2,table(Sample_TYPE,Type_source,useNA = 'always')))

# importer un fichier d'Opal a R
opal.file_download(o,"/home/wrosner/LocationMoreInfo.xlsx",
                   "~/LocationMoreInfo.xlsx")

# charger un fichier de R a Opal
opal.file_upload(o,"~/ATP_Measurements_Paper.html","/home/yamadou/pm/report/")

# importer un dataset de Opal a R
opal.assign(o,'cptp','Atlantic_R2_SAMPLE.AtlanticPATHBiosamples_October3_2016_noNAs',
            id.name = 'entity_id',identifiers = 'entity_id') + opal.execute(o,'cptp') %>% data.table

opal.assign(o,'cptp','BCGP-OPAL.OPAL_4CPTP_UPDATED2',
            id.name = 'entity_id',variables = list('BARCODE_FULL','Participant')) + opal.execute(o,'cptp') %>% data.table




# lire un fichier de donnees d'Opal a R
ohs_source_new <- '/home/chenwei/Release 2/Biosample/Data up to May 31, 2016/CPTP_OHS_Biosample_Source_By_SampleID_20161116.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table

# Convertir Format UNIX -> Format HUMAIN
as.POSIXct.numeric(x,origin = "1582-10-14",tz="GMT")
as.POSIXct.numeric(x,origin = "1582-10-14 00:00:00",tz="GMT")

#calculer la difference de temps entre deux dates
difftime(x,y,units = 'days')

# Explorer le contenu de donnees textuelles
Filter(function(x) length(x)!=0,sapply(X,function(x){
  grep("[^0-9.NANULL-]",x,ignore.case = T,value = T)}))

# Nettoyer des donnees textuelles
gsub("[[:punct:]]", "", txt)
gsub("[[:digit:]]", "", txt)
gsub("http\\w+", "", txt)
gsub("[ \t]{2,}", "", txt)
gsub("^\\s+|\\s+$", "", txt)
gsub("\\n", "", txt)

# comparer 02 datasets de donnees continues d'apres les quantiles de distributions
qqnorm(cag_dnaH$SAMPLE_DNA_CONC_PICOGREEN,
       ylab="Theoretical Quantiles", 
       xlab="Samples Quantiles", 
       main="Normal Q-Q Plot for SAMPLE_DNA_CONC_PICOGREEN") ; qqline(cag_dnaH$SAMPLE_DNA_CONC_PICOGREEN)

# boxplot
boxplot(x,y,col='cyan')

# barplot
barchart(Freq~Code,data=bb,groups=Study, 
         scales=list(x=list(rot=70,cex=0.8,labels=ab$Category)),
         xlab=var_label[var,Label], ylab = 'Freq.',
         col=rainbow(2))

# barplot
ggplot(bb, aes(Code, Freq, fill = Study)) + 
  geom_bar(stat="identity", position = "dodge",na.rm = TRUE) + 
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.justification = "center") +
  scale_x_discrete(breaks=y$Code,
                   labels=ab$Category) + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# barplot
bp <- barplot(y$atlH,main=var_label[x,Label],xlab=var_label[x,Label], ylab = 'Freq.',
              col=rainbow(dim(y)),names.arg = ab$Category,
              legend.text = ab$Category,axisnames = F,
              args.legend = list("topright", bty = "n"),xlim=c(0,20),width=2)
text(bp, 0, round("a", 1),cex=1,pos=3)

# definir un repertoire de travail
setwd("/home/yamadou") 

# importer d'opal d'apres une condition sur les nomes de variables
opal.assign(o,'cptp','TTP_R2.atp_onyx_ref',
            variables = "name().matches(/(TIME)|(DRINK)|(MEAL)|(DAT)|(CAFFEINE)|(ALCOHOL)|(TOBACCO)/gi)")

# lire un fichier SPSS
foreign::read.spss("",to.data.frame = T,reencode = 'utf-8') %>% data.table
Hmisc::spss.get("",lowernames = T,to.data.frame = T,force.single = F,charfactor = F,allow = c('-','_')) %>% data.table
file = "" %>% as.data.set(memisc::spss.system.file(.))

# decompresser un fichier
unzip("",exdir = "")


##########


##classification for PM dataset
#sub20 <- "PM_FRACTURE_RISK"
#cont <- cont[which(cont!="PM_FRACTURE_RISK")]
##classification for raw datasset
#sup20 <- Filter(function(x) {
#  tmp0 = eval(parse(text=paste0(stud,"$",x)))
#  return (length(levels(factor(tmp0))) > 20)
#},names(eval(parse(text = stud))))
#sub20 <- Filter(function(x) {
#  tmp0 = eval(parse(text=paste0(stud,"$",x)))
#  return (length(levels(factor(tmp0))) <= 20)
#},names(eval(parse(text = stud))))