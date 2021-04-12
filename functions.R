library(xlsx)
library(tidyverse)
library(reshape2)

# List of organs lists

#### Digestif
list_colRect <- c("C18","C19","C20") #c("GRELE","DUODEN","JEJUNUM","ILEON","CAECUM","COLON","APPENDICE","RECTUM","CANAL","SIGMOID")
list_grele <- c("C17")
list_esto <- c("C16") #c("ESTOMAC","ANTRE","CANAL DU PYLORE","BULBE")
list_pancr <- c("C23","C24","C25") #c("PANCREAS","VESICULE BILIAIRE","CHOLEDOQUE","ARBRE BILIAIRE","AMPULLOME VATERIEN","AMPOULE DE VATER","CANAL WIRSUNG","VOIES BILIAIRES")
list_foie <- c("C22") #c("FOIE")
list_oeso <- c("C15") #c("OESOPHAGE","CARDIA")
list_anus <- c("C21") #c("ANUS")
list_organe_Dig <- list(list_colRect,list_grele,list_esto,list_pancr,list_foie,list_oeso,list_anus)
names(list_organe_Dig) <- c("Côlon / Rectum","Grêle","Estomac","Bilio pancréatique","Foie","OEsophage","Anus")

#### Gynecologie
list_sein <- c("C50") #c("SEIN")
list_ovaire <- c("C56") #c("OVAIRE","TROMPE")
list_uterus <- c("C53","C54","C55") #c("UTERUS","UTERIN","ENDOMETRE")
list_vuvle <- c("C51")
list_vagin <- c("C52")
list_gyn_autres <- c("C57")
list_inSitu <- c("D05","D06","D07")
list_gyn_nr <- c("C80.+0")
list_organe_Gyn <- list(list_sein,list_ovaire,list_uterus,list_vuvle,list_vagin,list_gyn_autres,list_inSitu,list_gyn_nr)
names(list_organe_Gyn) <- c("Sein","Ovaires","Utérus","Vulve","Vagin","Autres","In Situ","Non renseigné")

#### Urologie
list_prost <- c("C61") #c("PROSTATE")
list_vessie <- c("C66","C67","C68") #c("VESSIE","BASSINET","PYELON","URETRE","URETERE","EXCRETRICE")
list_vergTest <- c("C60","C62","C63") #c("VERGE","TESTICULE","PENIS","PREPUCE","TESTIS")
list_rein <- c("C64") #c("REIN")
list_organe_Uro <- list(list_prost,list_vessie,list_vergTest,list_rein)
names(list_organe_Uro) <- c("Prostate","Vessie/Voies excrétrices","Verge/Testicule","Rein")

# List of onco gériatre
oncoGeriatre1 <- "MARION Annyck"
oncoGeriatre2 <- "PAILLARD Claire"


# List of functions
#### Sommer dans un dataframe
somme <- function(vect){
  return(sum(as.integer(vect)))
}

#### Calculer moyenne pondéréé
moyenne <- function(val,coef){
  ind_na <- which(is.na(val))
  if(length(ind_na) > 0){
    val[,ind_na] <- 0
    coef[,ind_na] <- 0
  }
  num <- sum(as.integer(val)*as.integer(coef))
  if(is.na(num)){
    print(val)
    print(coef)
  }
  denum <- sum(as.integer(coef))
  return(round(num/denum,0))
}

#### Extraction de valeur dans un vecteur de chaines de caractères
extract_val <- function(vect){
  val1 <- c()
  val2 <- c()
  
  for(i in vect){
    ind_par <- gregexpr("\\(",i)[[1]][1]
    v1 <- as.integer(substr(i,1,ind_par-2))
    val1 <- c(val1,v1)
    v2 <- as.integer(substr(i,ind_par+2,nchar(i)-2))
    val2 <- c(val2,v2)
  }
  return(list("v1" = val1,"v2" = val2))
}

#### lire dans un fichier CSV
open_CSVfile <- function(path,file_name=NULL,quote = TRUE){
  if(quote){
    q = ""
  }
  else{
    q = "\"'"
  }
  if(is.null(file_name)){
    data <- read.csv(path,sep=";", quote=q,colClasses = "character")
  }
  else{
    data <- read.csv(paste(path,file_name,sep=""),sep=";", quote=q,colClasses = "character")
  }
  
  ind... <- which(names(data)=='...')
  if(length(ind...) > 0){
    names(data)[ind...] <- 'newCol'
  }
  
  return(data)
}

#### ecrire dans un fichier Excel
write_XLfile <- function(data,file_name,sheet_name="sheet1",col_name=FALSE,row_name=FALSE,append=FALSE){
  write.xlsx(data,file=file_name,sheetName=sheet_name,col.names=col_name,row.names=row_name,append=append)
}

#### ecrire dans un fichier CSV
write_CSVfile <- function(data,file_name,row_name=FALSE){
  write.csv2(data,file_name,row.names=row_name)
}

calcul_delais <- function(col1,col2){
  ind_1 <- which(col1 != "")
  ind_2 <- which(col2 != "")
  ind <- intersect(ind_1,ind_2)
  col1 <- col1[ind,]
  col2 <- col2[ind,]
  delais <- as.Date(col1,"%d/%m/%Y") - as.Date(col2,"%d/%m/%Y")
  return(delais[which(!is.na(delais))])
}

#### Calcul des delais (sans les week-end)
calcul_delais_noWe <- function(date1,date2){
  ind <- intersect(which(date1 != ""),which(date2 != ""))
  date1 <- as.Date(date1[ind,],"%d/%m/%Y")
  date2 <- as.Date(date2[ind,],"%d/%m/%Y")

  n_NoWe <- c()
  for(i in 1:length(date1)){
    if(!is.na(date2[i] <= date1[i])){
      if(date2[i] <= date1[i]){
        enum <- as.POSIXlt(seq(date2[i],date1[i],'days'))$wday
        n_NoWe <- c(n_NoWe,sum(enum != 0 & enum != 6)-1)
      }
      else{
        enum <- as.POSIXlt(seq(date1[i],date2[i],'days'))$wday
        n_NoWe <- c(n_NoWe,-(sum(enum != 0 & enum != 6)-1))
      }
    }
  }
  return(n_NoWe)
}

#### Calcul délais moyen et médian
calcul_delais_moy_med <- function(date1,date2,noWe = FALSE){
  if(noWe){
    delais <- calcul_delais_noWe(date1,date2)
  }
  else{
    delais <- calcul_delais(date1,date2)
  }
  return(list("delaisMoy" = round(mean(delais),0),"delaisMed" = floor(median(delais))))
}

#### Calcul délais entre le diagnostic et la RCP moyen et médian 
calcul_delais_moy_med_diagRCP <- function(date1,date2,noWe = FALSE){
  if(noWe){
    delais <- calcul_delais_noWe(date1,date2)
  }
  else{
    delais <- calcul_delais(date1,date2)
  }
  q1 <- quantile(delais)[2]
  q3 <- quantile(delais)[4]
  v_sup <- min(max(delais),q3 + 1.5*(q3-q1))
  v_inf <- max(min(delais),q1 - 1.5*(q3-q1))
  delais <- delais[which(delais>=v_inf & delais<=v_sup)]
  return(list("delaisMoy" = round(mean(delais),0),"delaisMed" = floor(median(delais))))
}

#### Cacul nombre de dossiers
calcul_dossiers <- function(data){
  return(nrow(data))
}

#### Calcul nombre de dossiers avec une condition
calcul_dossiers_cond <- function(data,col){
  return(nrow(subset(data, data[[col]] != "")))
}

#### Calcul nombre de patients distincts
calcul_patients <- function(data){
  return(nrow(distinct(data,IPPR.Patient)))
}

#### Data nouveaux patients
nvx_patients <- function(data){
  nvxPatients <- subset(data, Type.Patient == "Nouveau patient" | Type.Patient == "Autre")
  return(distinct(nvxPatients,IPPR.Patient,.keep_all=TRUE))
}

#### Calcul nombre de nouveaux patients
calcul_nvx_patients <- function(data){
  return(nrow(nvx_patients(data)))
}

#### Liste des RCP distinctes
list_RCP <- function(data){
  return(distinct(data,Date.Séance))
}

#### Calcul nombre RCP
calcul_RCP <- function(listRCP){
  return(nrow(listRCP))
}

#### Calcul nombre dossier moyen par RCP
calcul_dossier_RCP <- function(data,listRCP){
  return(round(nrow(data)/nrow(listRCP),0))
}

#### Calcul nombre participants par RCP
calcul_part_RCP <- function(data){
  participants <- data['Participants']
  nb_part <- apply(participants,1,function(participants) str_count(participants,",")) + 1
  return(list("Moy" = round(mean(nb_part),0), "Med" = floor(median(nb_part))))
}

#### Calcul taux
calcul_taux <- function(data,col,cond=NULL){
  
  if(is.null(cond)){
    taux <- round((length(which(data[col] != "" & !is.na(data[col])))/nrow(data))*100,0)
  }
  else{
    taux <- round((length(which(data[col] != "" & !is.na(data[col]) & data[col] != cond))/nrow(data))*100,0)
  }
  
  #if(col == 'Etat.général.du.patient..OMS.'){
  #  cat('nrow : ',nrow(data),',ntaux : ',length(which(data[col] != "" & !is.na(data[col]))),',taux : ',taux,'\n')
  #}
  return(taux)
}

#### Calcul taux items t,n,m
calcul_taux_item <- function(data,list_item){
  return(round((length(Reduce(union,list_item))/nrow(data))*100,0))
}

#### Calcul age des patients
calcul_age <- function(data,annee){
  date_naissance <- data["Date.Naissance.Patient"]
  annee_naissance <- data.frame(apply(date_naissance,1,function(date_naissance) substring(date_naissance,7,10)))
  annee_naissance <- apply(annee_naissance,1,function(annee_naissance) strtoi(annee_naissance))
  return(annee - annee_naissance)
}

#### Calcul data en fonction de l'age
subdata_age <- function(data,annee,ageInf=NULL,ageSup=NULL){
  
  age <- calcul_age(data,annee)
    
  if(!is.null(ageInf) & !is.null(ageSup)){
    ind_age <- which((age >= ageInf) & (age <= ageSup))
    return(data[ind_age,])
  }
  else if(!is.null(ageInf)){
    ind_age <- which((age >= ageInf))
    return(data[ind_age,])
  }
  else if(!is.null(ageSup)){
    ind_age <- which((age <= ageSup))
    return(data[ind_age,])
  }
  else{
    return(data)
  }
}

#### Calcul présence onco gériatre
taux_pres_oncoGeriatre <- function(data_70){
  p_70 <- data_70["Participants"]
  ind_1 <- grep(oncoGeriatre1,p_70[,1])
  ind_2 <- grep(oncoGeriatre2,p_70[,1])
  ind <- union(ind_1,ind_2)
  return(round((length(ind) / nrow(data_70))*100,0))
}

#### Plot multiple sur le meme graphe
mult_plot <- function(data,xVar,yVar){
  data_plot <- rownames_to_column(data, var = xVar)
  data_plot <- melt(data = data_plot, id.vars = c(xVar), variable.name = yVar)
  return(data_plot)
}