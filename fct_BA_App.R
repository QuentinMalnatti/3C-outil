source("X:/ERMIOS/3C TERRITOIRE/OUTIL BILAN/functions.R", encoding = "UTF-8", local=TRUE)

library(openxlsx)
library(matlab)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)

######################################################### BILAN ANNUEL ##########################################################

#### Calcul indicateurs 
calcul_indicateur <- function(data,annee,hemato=FALSE,medipole=FALSE){
  indicateurs <- list()
  
  indicateurs['nb_dossier'] <- calcul_dossiers(data)
  
  indicateurs['nb_patients'] <- calcul_patients(data)
  
  indicateurs['nb_nvx_patients'] <- calcul_nvx_patients(data)
  
  indicateurs['nb_patients_rePresente'] <- as.integer(indicateurs['nb_patients']) - as.integer(indicateurs['nb_nvx_patients'])
  
  RCP <- list_RCP(data)
  indicateurs['nb_RCP'] <- calcul_RCP(RCP)
  
  indicateurs['nb_dossier_RCP_moy'] <- calcul_dossier_RCP(data,RCP)
  
  nb_part_RCP <- calcul_part_RCP(data)
  indicateurs['nb_part_RCP'] <- paste(toString(nb_part_RCP$Moy),' (',toString(nb_part_RCP$Med),')')
  
  delais_diff <- calcul_delais_moy_med(data["Date.Validation.CR"],data["Date.Séance"],noWe=TRUE)
  indicateurs['delais_diff_moy_med'] <- paste(toString(delais_diff$delaisMoy),' (',toString(delais_diff$delaisMed),')')
  
  delais_incr_RCP <- calcul_delais_moy_med(data["Date.Séance"],data["Date.Inscription.Dossier"])
  indicateurs['delais_incr_RCP_moy_med'] <- paste(toString(delais_incr_RCP$delaisMoy),' (',toString(delais_incr_RCP$delaisMed),')')
  
  data_nvx <- subset(data, Type.Patient == "Nouveau patient")
  if(annee <= 2018){
    if(annee == 2017){
      if(hemato){
        date_diag <- data_nvx["Date.de.diagnostic"]
      }
      else{
        date_diag <- data_nvx["Date.de.diagnostic.de.certitude"]
      }
    }
    else if(annee == 2018){
      if(hemato){
        date_diag <- data_nvx["Date.de.diagnostic"]
        ind_miss <- which(date_diag =="")
        date_diag[ind_miss,] <- data_nvx[ind_miss,"Date.de.diagnostic.initial"]
      }
      else{
        date_diag <- data_nvx["Date.de.diagnostic.de.certitude"]
        ind_miss <- which(date_diag =="")
        date_diag[ind_miss,] <- data_nvx[ind_miss,"Date.de.diagnostic.initial"]
      }
    }
  }
  else{
    date_diag = data_nvx["Date.de.diagnostic.initial"]
  }
  
  delais_diag_RCP <- calcul_delais_moy_med_diagRCP(data_nvx["Date.Séance"],date_diag)
  indicateurs['delais_diag_RCP_moy_med'] <- paste(toString(delais_diag_RCP$delaisMoy),' (',toString(delais_diag_RCP$delaisMed),')')
  
  indicateurs['taux_date_diag'] <- calcul_taux(data.frame(date_diag),1)
  
  indicateurs['taux_CIM10'] <- calcul_taux(data,'Localisation..CIM.10.')
  
  if(!hemato){
    
    indicateurs['taux_circ_dec'] <- calcul_taux(data,'Circonstances.de.découverte')
    
    indT <- which(data['Clinique...T'] != "")
    indpT <- which(data['Post.opératoire...pT'] != "")
    indrT <- which(data['rT'] != "")
    indicateurs['taux_itemT'] <- calcul_taux_item(data,list(indT,indpT,indrT))
    
    indN <- which(data['N'] != "")
    indpN <- which(data['pN'] != "")
    indrN <- which(data['rN'] != "")
    indicateurs['taux_itemN'] <- calcul_taux_item(data,list(indN,indpN,indrN))
    
    indM <- which(data['M'] != "")
    indpM <- which(data['pM'] != "")
    indrM <- which(data['rM'] != "")
    indicateurs['taux_itemM'] <- calcul_taux_item(data,list(indM,indpM,indrM))
  }
  else{
    indicateurs['taux_circ_dec'] <- NA
    indicateurs['taux_itemT'] <- NA
    indicateurs['taux_itemN'] <- NA
    indicateurs['taux_itemM'] <- NA
  }
  
  indicateurs['taux_stade_OMS'] <- calcul_taux(data,'Etat.général.du.patient..OMS.')
  
  if(!hemato){ 
    indicateurs['taux_tabac'] <- calcul_taux(data,'Tabac')
    indicateurs['taux_expo_pro'] <- calcul_taux(data,'Exposition.Professionnelle')
  }
  else{
    indicateurs['taux_tabac'] <- NA
    indicateurs['taux_expo_pro'] <- NA
  }
  
  indicateurs['taux_question_pose'] <- calcul_taux(data,'Précisions.sur.la.question.posée')
  
  age_patient <- calcul_age(data,annee)
  
  indicateurs['age_patient_moy_med'] <- paste(toString(round(mean(age_patient),0)),' (',toString(median(age_patient)),')')
  
  data_70 <- data[which(age_patient >= 70),]
  data_75 <- data[which(age_patient >= 75),]
  indicateurs['nb_dossier_patientAge'] <- paste(toString(nrow(data_70)),' (',toString(nrow(data_75)),')')
  
  indicateurs['taux_depistage_geriatrique'] <- calcul_taux(data_70,'Score.du.G8')
  
  if(annee > 2018){
    indicateurs['taux_prop_consult_onco_geria'] <- calcul_taux(data_70,'Proposition.de.consultation.onco.gériatrique..','Non')
  }
  else{
    indicateurs['taux_prop_consult_onco_geria'] <- NA
  }
    
  indicateurs['taux_prop_essai'] <- calcul_taux(data,'Proposition.d.inclusion.dans.un.essai..','Non')
  
  if(!hemato){
    indicateurs['taux_prop_consult_onco_gene'] <- calcul_taux(data,'Proposition.de.consultation.d.oncogénétique..','Non')
  }
  else{
    indicateurs['taux_prop_consult_onco_gene'] <- 0
  }
  
  if(annee == 2017 & hemato){
    indicateurs['taux_conform_ref'] <- calcul_taux(data,'Conformité.au.référentiel.de.prise.en.charge.déclaré.par.la.RCP..')
  }
  else{
    indicateurs['taux_conform_ref'] <- calcul_taux(data,'Conformité.au.réferentiel..')
  }
  
  if(!hemato){
    indicateurs['taux_pres_radio'] <- calcul_taux(data,'Quorum...Radiologue')
  }
  else{
    indicateurs['taux_pres_radio'] <- 0
  }
  
  if(annee == 2017 & hemato){
    indicateurs['taux_pres_anapath'] <- NA
  }
  else{
    indicateurs['taux_pres_anapath'] <- calcul_taux(data,'Quorum...Anapath')
  }
  
  if(!medipole){
    if(!hemato){
      indicateurs['taux_pres_nucleaire'] <- calcul_taux(data,'Quorum...Médecin.nucléaire')
    }
    else{
      indicateurs['taux_pres_nucleaire'] <- 0
    }
  }
  else{
    indicateurs['taux_pres_nucleaire'] <- NA
  }
  
  if(!medipole){
    indicateurs['taux_pres_oncogeriatre'] <- taux_pres_oncoGeriatre(data_70)
  }
  else{
    indicateurs['taux_pres_oncogeriatre'] <- NA
  }
  
  indicateurs['taux_pres_prescripteur'] <- calcul_taux(data,'Etat.présence.Prescripteur',"Absent")
  
  return(indicateurs)
}

#### Calcul indicateur territoire
calcul_indic_territoire <- function(df,fich_glob,colName,annee){
  indic_Terr <- data.frame((-1)*ones(nrow(df),1),row.names = rownames(df))
  
  indic_Terr['Nombre de dossiers',1] <- somme(df['Nombre de dossiers',])
  indic_Terr['Nombre de patients différents',1] <- calcul_patients(fich_glob)
  indic_Terr['Nombre de nouveaux patients',1] <- calcul_nvx_patients(fich_glob)
  indic_Terr['Nombre de patients suivis',1] <- indic_Terr['Nombre de patients différents',1] - indic_Terr['Nombre de nouveaux patients',1]
  indic_Terr['Nombre de réunions',1] <- somme(df['Nombre de réunions',])
  indic_Terr['Nombre moyen de dossiers enregistrés par RCP',1] <- moyenne(df['Nombre moyen de dossiers enregistrés par RCP',],df['Nombre de réunions',])
  
  nb_part_RCP <- calcul_part_RCP(fich_glob)
  indic_Terr['Nombre de participants moyen (médian) par RCP',1] <- paste(toString(nb_part_RCP$Moy),' (',toString(nb_part_RCP$Med),')') 
  
  delais_diff <- calcul_delais_moy_med(fich_glob["Date.Validation.CR"],fich_glob["Date.Séance"],noWe=TRUE)
  indic_Terr['Délai de diffusion moyen (median) hors week-end (jours)',1] <- paste(toString(delais_diff$delaisMoy),' (',toString(delais_diff$delaisMed),')') 
  
  delais_incr_RCP <- calcul_delais_moy_med(fich_glob["Date.Séance"],fich_glob["Date.Inscription.Dossier"])
  indic_Terr['Délai moyen (median) entre inscription et RCP (jours)',1] <- paste(toString(delais_incr_RCP$delaisMoy),' (',toString(delais_incr_RCP$delaisMed),')') 
  
  if(annee > 2018){
    data_nvx <- subset(fich_glob, Type.Patient == "Nouveau patient")
    delais_diag_RCP <- calcul_delais_moy_med_diagRCP(data_nvx["Date.Séance"],data_nvx["Date.de.diagnostic.initial"])
    indic_Terr['Délai moyen (median) entre diagnostic et RCP (jours)',1] <- paste(toString(delais_diag_RCP$delaisMoy),' (',toString(delais_diag_RCP$delaisMed),')') 
  }
  else{
    vect_moy_delais_diag_RCP <- data.frame(t(extract_val(df['Délai diagnostic-RCP moyen (median) (jours)',])$v1))
    indic_Terr['Délai moyen (median) entre diagnostic et RCP (jours)',1] <- moyenne(vect_moy_delais_diag_RCP,df['Nombre de nouveaux patients',])
  }
    
  age_patient <- calcul_age(fich_glob,annee)
  indic_Terr['Age patient moyen (median)',1] <- paste(toString(round(mean(age_patient),0)),' (',toString(median(age_patient)),')')
  
  data_70 <- subdata_age(fich_glob,annee,ageInf = 70) 
  data_75 <- subdata_age(fich_glob,annee,ageInf = 75) 
  indic_Terr['Nombre de dossiers de patients de 70 ans et plus (75 ans et plus)',1] <- paste(toString(nrow(data_70)),' (',toString(nrow(data_75)),')') 
  
  vect_nb_pat_70 <- data.frame(t(extract_val(df['Nombre de dossiers de patients de 70 ans et plus (75 ans et plus)',])$v1))
  indic_Terr['Dépistage gériatrique (%)',1] <- moyenne(df['Dépistage gériatrique (%)',],vect_nb_pat_70)
  indic_Terr['Proposition consultation oncogériatrique (%)',1] <- moyenne(df['Proposition consultation oncogériatrique (%)',],vect_nb_pat_70)
  
  for(i in rownames(indic_Terr)){
    if(indic_Terr[i,1] == "-1"){
      indic_Terr[i,1] <- moyenne(df[i,],df['Nombre de dossiers',])
    }
  }
  colnames(indic_Terr) <- colName
  return(indic_Terr)
}

#### Calcul indicateurs dans le server
data_BA <- function(liste_fichier,input){
  col <- c('CHMS Thoracique','CHMS Urologie','CHMS Gynécologie','CHMS ORL','CHMS Générale','CHMS Digestif','CHMS Endocrines','CHMS Hématologie','Medipole Digestif','Medipole Gynécologie','Medipole Urologie')
  row <- c('Nombre de dossiers','Nombre de patients différents','Nombre de nouveaux patients','Nombre de patients suivis','Nombre de réunions','Nombre moyen de dossiers enregistrés par RCP','Nombre de participants moyen (médian) par RCP','Délai de diffusion moyen (median) hors week-end (jours)','Délai moyen (median) entre inscription et RCP (jours)','Délai moyen (median) entre diagnostic et RCP (jours)','Date de diagnostic renseignée (%)','Code CIM 10 rensigné (%)','Circonstance de découverte renseignée (%)','item T renseigné (%)','item N renseigné (%)','item M renseigné (%)','item stade OMS renseigné (%)','Tabac renseigné (%)','Exposition professionnelle renseignée (%)','Question posée renseignée (%)','Age patient moyen (median)','Nombre de dossiers de patients de 70 ans et plus (75 ans et plus)','Dépistage gériatrique (%)','Proposition consultation oncogériatrique (%)','Proposition inclusion dans essai thérapeutique (%)', 'Proposition consultation oncogénétique (%)', 'Conformité au référentiel renseigné (%)',"Dossiers discutés en présence d'un radiologue (%)","Dossiers discutés en présence d'un médecin anatomopathologiste (%)","Dossiers discutés en présence d'un médecin nucléaire (%)","Dossiers discutés en présence d'un médecin oncogériatre (%)","Dossiers discutés en présence du prescipteur (%)")
  tIndic <- data.frame(row.names=row)
  
  k <- 1
  for(i in liste_fichier){
    print(names(liste_fichier)[k])
    if(!str_detect(names(liste_fichier)[k],'Med')){
      if(names(liste_fichier)[k] != 'CHMS_Hem'){
        tIndic[,col[k]] <- unlist(calcul_indicateur(i,input$Annee))
      }
      else{
        tIndic[,col[k]] <- unlist(calcul_indicateur(i,input$Annee,hemato =  TRUE))
      }
    }
    else{
      tIndic[,col[k]] <- unlist(calcul_indicateur(i,input$Annee,medipole = TRUE))
    }
    k<-k+1
  }
  
  CHMS <- bind_rows(liste_fichier[1:8])
  Medipole <- bind_rows(liste_fichier[9:11])
  Territoire <- bind_rows(liste_fichier)
  
  Indic_CHMS <- calcul_indic_territoire(tIndic[,1:8],CHMS,"CHMS",input$Annee)
  Indic_Med <- calcul_indic_territoire(tIndic[,9:11],Medipole,"MEDIPOLE",input$Annee)
  Indic_Territoire <- calcul_indic_territoire(tIndic,Territoire,"TERRITOIRE",input$Annee)
  tIndic <- cbind(TERRITOIRE = Indic_Territoire,tIndic[,1:8],CHMS = Indic_CHMS,tIndic[,9:11],MEDIPOLE = Indic_Med)
  rownames(tIndic) <- row
  
  return(list("data" = tIndic, "colnames" = colnames(tIndic)))
}

# ##################### Test
# source("X:/ERMIOS/3C TERRITOIRE/OUTIL BILAN/test.R", encoding = "UTF-8", local=TRUE)
# l_f <- liste_fichier
# 
# input <- list("Annee" = 2020)
# test <- data_BA(l_f,input)$data