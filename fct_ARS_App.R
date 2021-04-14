source("functions.R", encoding = "UTF-8", local=TRUE)

library(openxlsx)
library(matlab)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)

######################################################### BILAN ANNUEL ARS ##########################################################
#### Calcul indicateur

calcul_indicateur_ARS_RCP <- function(data){
  indicRCP <- list()
  
  RCP <- list_RCP(data)
  indicRCP['Nb total de réunions RCP réalisées'] <- calcul_RCP(RCP)
  
  indicRCP['Nb moyen de fiches RCP/séance'] <- calcul_dossier_RCP(data,RCP)
  
  return(indicRCP)
}

calcul_indicateur_ARS_Territoire <- function(data,annee){
  indicTerritoire <- list()
  
  indicTerritoire["Nb total de dossiers présentés en RCP"] <- calcul_dossiers(data)
  indicTerritoire["% de dossiers présentés en présence d'un radiologue"] <- calcul_taux(data,'Quorum...Radiologue')
  indicTerritoire["% de dossiers présentés en présence d'un anatomopathologiste"] <- calcul_taux(data,'Quorum...Anapath')
  
  data_70 <- subdata_age(data,annee,ageInf=70)
  indicTerritoire["% de dossiers présentés en présence d'un oncogériatre/gériatre (pour les plus de 70 ans)"] <- taux_pres_oncoGeriatre(data)
  
  indicTerritoire["% de dossiers présentés en présence d'un médecin nucléaire"] <- calcul_taux(data,'Quorum...Médecin.nucléaire')
  
  indicTerritoire["Nombre de fiches RCP validées"] <- calcul_dossiers_cond(data,"Date.Validation.CR")
  
  indicTerritoire["Nombre total de patients"] <- calcul_patients(data)
  indicTerritoire["Nombre total de nouveaux patients"] <- calcul_nvx_patients(data)
  
  data_75 <- subdata_age(data,annee,ageInf = 75)
  indicTerritoire["Nombre de fiches RCP de patients de plus de 75 ans"] <- calcul_dossiers(data_75)
  indicTerritoire["Nombre de fiches RCP de patients de plus de 75 ans avec G8"] <- round((calcul_taux(data_75,'Score.du.G8')/100)*calcul_dossiers(data_75),0)
  indicTerritoire["Nombre de fiches RCP de patients AJA (16-25 ans)"] <- calcul_dossiers(subdata_age(data,annee,ageInf = 16,ageSup = 25))
  
  indicTerritoire["% de dossiers transmis aux médecins traitants"] <- calcul_taux(data,'MedecinTraitant')
  
  return(indicTerritoire)
}

data_ARS <- function(liste_fichier,input){
  tIndic <- list()
  Territoire <- bind_rows(liste_fichier)
  
  tIndic["Nombre total d'entités RCP organisées"] <- 11
  tIndic["Nb RCP hebdomadaires"] <- 8
  tIndic["Nb RCP bimensuelles"] <- 2
  tIndic["Nb RCP mensuelles"] <- 1
  tIndic["Nb de RCP organisées en présentiel"] <- 11
  tIndic["Nb de RCP organisées en visio"] <- 11
  tIndic["Nb de RCP organisées de façon mixte"] <- 11
  tIndic["Repartition des RCP par Ets membre du 3C (CHMS)"] <- "Digestif, Endocrines, Oncologie Generale, Gynécologie, Hématologie, ORL, Thoracique, Urologie"
  tIndic["Repartition des RCP par Ets membre du 3C (Medipole)"] <- "Digestif, Gynécologie, Urologie"
  tIndic["Nombre Total de fiches RCP diffusées par messagerie sécurisées"] <- calcul_dossiers(Territoire)
  
  tIndicRCP <- c(0,0)
  names(tIndicRCP) <- c('Nb total de réunions RCP réalisées','Nb moyen de fiches RCP/séance')
  for(i in liste_fichier){
    tIndicRCP <- tIndicRCP + unlist(calcul_indicateur_ARS_RCP(i))
  }
  tIndicRCP['Nb moyen de fiches RCP/séance'] <- round(tIndicRCP['Nb moyen de fiches RCP/séance']/unlist(tIndic["Nombre total d'entités RCP organisées"]),0)
  tIndic <- c(tIndic,as.list(tIndicRCP),calcul_indicateur_ARS_Territoire(Territoire,input$Annee))
  
  return(unlist(tIndic))
  
}
