source("functions.R", encoding = "UTF-8", local=TRUE)

library(Hmisc)

list_spe <- c('Dig','Gyn','Uro')

calcul_indicateur_organe <- function(data,list_organe,gyneco = FALSE){
  indic <- c()
  data_nvx <- nvx_patients(data)
  indic["Nouveaux Patients"] <- nrow(data_nvx)
  for(i in names(list_organe)){
    ind <- c()
    for(j in list_organe[[i]]){
      #indic[i] <- as.integer(indic[i]) + length(grep(j,data_nvx[,'Organe'])) #nrow(subset(data_nvx, Organe == j))
      ind <- c(ind,grep(j,data_nvx[,'Localisation..CIM.10.']))
    }
    indic[i] <- length(unique(ind))
  }
  if(!gyneco){
    indic["Non renseigné"] <- nrow(subset(data_nvx, Organe == ""))
    indic["Autres"] <- indic[1] - sum(indic[2:length(indic)])
  }
  return(indic)
}

calcul_col <- function(spe_name,fich,list_uro,list_gyn,list_dig){
  col_val = switch(
    spe_name,
    CHMS_Uro = list("CHMS Urologie",calcul_indicateur_organe(fich,list_uro)),
    CHMS_Dig = list("CHMS Digestif",calcul_indicateur_organe(fich,list_dig)),
    CHMS_Gyn = list("CHMS Gynécologie",calcul_indicateur_organe(fich,list_gyn,TRUE)),
    Med_Uro = list("Médipole Urologie",calcul_indicateur_organe(fich,list_uro)),
    Med_Dig = list("Médipole Digestif",calcul_indicateur_organe(fich,list_dig)),
    Med_Gyn = list("Médipole Gynécologie",calcul_indicateur_organe(fich,list_gyn,TRUE))
  )
  
  names(col_val) = c("colName","colVal")
  
  return(col_val)
}

data_Organe <- function(liste_fichier){
  tIndic <- list()
  
  k <- 1
  for(i in liste_fichier){
    if(str_detect(names(liste_fichier)[k],'Uro') || str_detect(names(liste_fichier)[k],'Dig') || str_detect(names(liste_fichier)[k],'Gyn')){
      vals <- calcul_col(names(liste_fichier)[k],i,list_organe_Uro,list_organe_Gyn,list_organe_Dig)
      
      col <- c()
      l <- 1
      for(j in names(vals$colVal)){
        if(l < length(names(vals$colVal))){
          col <- paste(col,j,":",vals$colVal[j],"\n")
        }
        else{
          col <- paste(col,j,":",vals$colVal[j])
        }
        l <- l+1 
      }
      tIndic[[vals$colName]] <- col
      
    }
    k <- k+1
  }
  return(data.frame(tIndic,row.names = 'Detail par organe'))
}

# ######################################### Test ##########################################
# 
# file_chms_uro <- read.csv("C:/Users/quentin.malnatti/Documents/Data/CHMS/2019/ExportIndicMedicaux_CHMS_2019_Urologie.csv",sep=";", quote="",colClasses = "character")
# file_med_uro <- read.csv("C:/Users/quentin.malnatti/Documents/Data/Medipole/2019/ExportIndicMedicaux_Medipole_2019_Urologie.csv",sep=";", quote="",colClasses = "character")
# file_chms_dig <- read.csv("C:/Users/quentin.malnatti/Documents/Data/CHMS/2020/ExportIndicMedicaux_CHMS_2020_Digestif.csv",sep=";", quote="",colClasses = "character")
# file_med_dig <- read.csv("C:/Users/quentin.malnatti/Documents/Data/Medipole/2020/ExportIndicMedicaux_Medipole_2020_Digestif.csv",sep=";", quote="",colClasses = "character")
# file_chms_gyn <- read.csv("C:/Users/quentin.malnatti/Documents/Data/CHMS/2019/ExportIndicMedicaux_CHMS_2019_Gyneco.csv",sep=";", quote="",colClasses = "character")
# file_med_gyn <- read.csv("C:/Users/quentin.malnatti/Documents/Data/Medipole/2019/ExportIndicMedicaux_Medipole_2019_Gynecologie.csv",sep=";", quote="",colClasses = "character")
# 
# files <- list(file_med_dig) #file_chms_uro,file_med_uro,file_chms_dig,file_med_dig,file_chms_gyn,file_med_gyn)
# names(files) <- c("Med_Dig") #"CHMS_Uro","Med_Uro","CHMS_Dig","Med_Dig","CHMS_Gyn","Med_Gyn")
# 
# data_Organe(files)
# calcul_indicateur_organe(file_chms,list_organe_Dig)
# write.csv2(t(gsub("<br>","\n",data_Organe(files))),"ex.csv",col.names = TRUE)
# write.csv2(data_Organe(files),"ex.csv",row.names = TRUE)
