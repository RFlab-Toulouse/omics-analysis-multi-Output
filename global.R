options(xtable.include.colnames=T)
options(xtable.include.rownames=T)
#Packages
#rm(list=ls())
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("plotly")
usePackage("missMDA")#imputepca
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("tidyr")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("superml")
usePackage("shiny")
usePackage("shinythemes")
usePackage("bslib")
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
#usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
usePackage("DT")
usePackage("shinycssloaders")
usePackage("writexl")
usePackage("glmnet")#for lasso, elasticnet, ridge regression
usePackage("survival")#for cox regression
usePackage("xgboost")#for xgboost gradient boosting
usePackage("lightgbm")#for lightgbm gradient boosting
usePackage("class")#for k-nearest neighbors
usePackage("Boruta")#for Boruta variable selection
usePackage("Rtsne")#for t-SNE visualization
usePackage("umap")#for UMAP visualization
usePackage("iml")#for SHAP, PDP, feature effects
usePackage("lime")#for LIME local explanations
usePackage("fmsb")#for radar/spider plots
usePackage("pheatmap")#for clustered heatmaps
usePackage("igraph")#for correlation network graphs
usePackage("ggraph")#for ggplot-based network visualization
usePackage("tidygraph")#for tidy graph manipulation
usePackage("dplyr")#for data manipulation
usePackage("stringr")
usePackage("grid")



##########################
# importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
#   # datapath: path of the file
#   #extention: extention of the file : csv, xls, ou xlsx
#   if(extension=="csv"){
#     toto <<- read.csv2(datapath,header = F,
#                        sep =sep,dec=dec,
#                        na.strings = NAstring,
#                        stringsAsFactors = F,row.names=NULL,check.names = F )
#   }
#   if(extension=="xlsx"){
#     options(warn=-1)
#     filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
#     options(warn=0)
#     toto <<- read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet) %>% as.data.frame()
#     #toto <<- read_xlsx(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
#     #toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
#     #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
#   }
#   #remove empty column
#   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
#     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
#   #remove empty row
#   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
#     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
#   print(class(toto))
#   
#   rnames<-as.character(as.matrix(toto[,1]))
#   cnames<-as.character(as.matrix(toto[1,]))
#   toto<-toto[,-1]
#   toto<-toto[-1,]
#   row.names(toto)<-rnames[-1]
#   colnames(toto)<-cnames[-1]
# 
#   toto<-as.data.frame(toto)
#   rownames(toto)<-rnames[-1]
#   colnames(toto)<-cnames[-1]
#   return(toto)
# }

importfile <- function(datapath, extension, NAstring="NA", sheet=1, skiplines=0, dec=".", sep=",") {
  
  toto <- if(extension == "csv") {
    read.csv2(datapath, header=FALSE, sep=sep, dec=dec,
              na.strings=NAstring, stringsAsFactors=FALSE,
              row.names=NULL, check.names=FALSE, skip=skiplines)
    # read.table(
    #   datapath, header=FALSE, sep=sep, dec=dec,
    #   na.strings=NAstring, stringsAsFactors=FALSE,
    #   row.names=NULL, check.names=FALSE, skip=skiplines
    # )
  } else if(extension == "xlsx") {
    xlsx_path <- paste0(datapath, ".xlsx")
    file.rename(datapath, xlsx_path)
    read_excel(xlsx_path, na=NAstring, col_names=FALSE, skip=skiplines, sheet=sheet) %>% as.data.frame()
  } else {
    stop("Extension non supportée: ", extension)
  }
  
  
  # Supprimer lignes/colonnes vides
  toto <- toto[, colSums(!is.na(toto)) > 0]
  toto <- toto[rowSums(!is.na(toto)) > 0, ]
  
  # Déterminer si la colonne 1 contient des row names ou des données
  # Row names = valeurs uniques ; données/target = valeurs potentiellement répétées
  col1_values <- as.character(toto[-1, 1])  # exclure la ligne header
  has_rownames <- (length(col1_values) == length(unique(col1_values)))
  
  if(has_rownames) {
    # Cas 1 : colonne 1 = row names, ligne 1 = col names
    # Format classique : [corner, colnames...] / [rowname, data...] / ...
    rnames <- col1_values
    cnames <- as.character(toto[1, -1])
    cat("cnames in if state   : \n")
    print(cnames)
    
    toto <- toto[-1, -1]
    rownames(toto) <- rnames
    colnames(toto) <- cnames
  } else {
    # Cas 2 : pas de row names, ligne 1 = col names, colonne 1 = données (ex: variable target)
    # Format : [colname1, colname2, ...] / [data1, data2, ...] / ...
    cnames <- as.character(toto[1, ])
    cat("cnames in else state   : \n")
    print(cnames)
    
    toto <- toto[-1, ]
    colnames(toto) <- cnames
    rownames(toto) <- paste0("", seq_len(nrow(toto)))
  }
  
  cat("info durant l'export des données :  \n")
  print(colnames(toto)[1:5])
  print(str(toto[,  1:5]))
  as.data.frame(toto)
}

# downloaddataset <- function(x,file,cnames=T,rnames=T){
#   ext<-strsplit(x = file,split = "[.]")[[1]][2]
#   if(ext=="csv"){
#     if(sum(cnames,rnames)==2){
#       write.csv(x,file)
#       }
#     else{
#       write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
#       }
#   }
#   if(ext=="xlsx"){
#     write.xlsx(x,file,col.names = cnames,row.names =rnames )
#   }
#   
# }

# df <- reactive({
#   req(input$learningfile)
#   file <- input$learningfile
#   ext <- tools::file_ext(file$datapath)
#   
#   req(file)
#   validate(need(ext == "xlsx", "Veuillez télécharger un fichier CSV"))
#   
#   df = read_excel(file$datapath)
#   print(head(df))
#   return( df)
# })


downloaddataset <- function(x,file,cnames=T,rnames=T){
  ext = tools::file_ext(file)
  if(ext=="csv"){
    if(sum(cnames,rnames)==2){
      write.csv(x,file)
    }
    else{
      write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
    }
  }
  if(ext=="xlsx"){
    #write.xlsx(x,file,col.names = cnames,row.names =rnames )
    writexl::write_xlsx(x,file, col_names = cnames)
  }
  
}

downloadplot <- function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
# renamvar<-function(names){
#   #rename the duplicate name by adding ".1, .2 ....
#   #toto is a vector of the col names of the tab
#   names[is.na(names)]<-"NA"
#   for(i in 1:length(names)){
#     ind <- which(names%in%names[i])
#     if(length(ind)>1){
#       nb<-c(1:length(ind))
#       newnames<-paste(names[ind],".",nb,sep="")
#       
#       names[ind]<-newnames
#     }
#   }
#   return(names)
# }
# fonction to generate n different colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
transformdata<-function(toto,transpose,zeroegalNA){
  if(transpose){
    toto<-t(toto)
    }
  
  if(zeroegalNA){
    toto[which(toto==0,arr.ind = T)]<-NA
    }
  
 toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))])
}

confirmdata<-function(toto){
  toto<-as.data.frame(toto)
  toto[,1]<-as.factor(as.character(toto[,1]))
  for (i in 2:ncol(toto)){
    toto[,i]<-as.numeric(as.character(toto[,i]))
  }
  return(toto)
}

convert_egfr_to_ckd_stage <- function(col) {
  
  CKD_BREAKS <- c(0, 15, 30, 45, 60, 90, Inf)
  CKD_LABELS <- c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
  
  # Cas 1 : déjà un factor CKD stage → rien à faire
  if (is.factor(col)) {
    existing <- levels(col)
    if (any(existing %in% CKD_LABELS)) {
      cat("[import] Colonne cible : CKD stages déjà présents comme factor.\n")
      return(list(converted = FALSE, column = col,
                  n_converted = 0, n_na_produced = 0))
    }
    # Factor non-CKD (labels texte arbitraires) → garder tel quel
    cat("[import] Colonne cible : factor texte non-numérique conservé.\n")
    return(list(converted = FALSE, column = col,
                n_converted = 0, n_na_produced = 0))
  }
  
  # Cas 2 : character → tenter coercion numérique
  if (is.character(col)) {
    # Vérifier si les valeurs sont des CKD labels texte existants
    non_na_vals <- col[!is.na(col)]
    if (all(non_na_vals %in% CKD_LABELS)) {
      cat("[import] Colonne cible : CKD stages déjà présents comme character → conversion en factor.\n")
      return(list(converted = FALSE,
                  column = factor(col, levels = CKD_LABELS),
                  n_converted = 0, n_na_produced = 0))
    }
    # Tentative de conversion numérique
    num_try <- suppressWarnings(as.numeric(col))
    pct_numeric <- mean(!is.na(num_try[!is.na(col)]))
    
    if (pct_numeric >= 0.8) {
      # ≥80% des valeurs non-NA sont numériques → traiter comme eGFR
      cat(sprintf("[import] Colonne cible character avec %.0f%% valeurs numériques → conversion eGFR → CKD stage.\n",
                  pct_numeric * 100))
      col <- num_try  # on continue en numérique ci-dessous
    } else {
      # Texte non numérique → label de classe arbitraire
      cat("[import] Colonne cible : character non numérique conservée comme factor.\n")
      return(list(converted = FALSE,
                  column = factor(col),
                  n_converted = 0, n_na_produced = 0))
    }
  }
  
  # Cas 3 : numérique → détecter si valeurs eGFR plausibles
  if (is.numeric(col)) {
    non_na_vals <- col[!is.na(col)]
    
    if (length(non_na_vals) == 0) {
      cat("[import] Colonne cible : vide (que des NA).\n")
      return(list(converted = FALSE, column = col,
                  n_converted = 0, n_na_produced = 0))
    }
    
    # Plage eGFR valide : ]0 ; 200] (valeurs >200 improbables pour eGFR)
    pct_in_range <- mean(non_na_vals > 0 & non_na_vals <= 200)
    
    if (pct_in_range >= 0.8) {
      cat(sprintf("[import] Colonne cible numérique dans la plage eGFR (%.0f%% valeurs dans ]0;200]).\n",
                  pct_in_range * 100))
      cat(sprintf("         Min=%.1f | Médiane=%.1f | Max=%.1f\n",
                  min(non_na_vals), median(non_na_vals), max(non_na_vals)))
      
      n_before_na <- sum(is.na(col))
      categories <- cut(col,
                        breaks = CKD_BREAKS,
                        labels = CKD_LABELS,
                        include.lowest = FALSE,
                        right = TRUE)
      n_after_na  <- sum(is.na(categories))
      n_na_produced <- n_after_na - n_before_na
      n_converted   <- sum(!is.na(categories))
      
      if (n_na_produced > 0) {
        cat(sprintf("[import] ATTENTION : %d valeur(s) hors plage ]0;90+] → NA après conversion.\n",
                    n_na_produced))
      }
      
      cat(sprintf("[import] Conversion eGFR → CKD stage effectuée (%d observations).\n", n_converted))
      cat("[import] Distribution des stages :\n")
      print(table(categories, useNA = "ifany"))
      
      # Supprimer les niveaux de factor vides (classes avec 0 observations)
      categories <- droplevels(categories)
      n_levels_after <- length(levels(categories))
      if (n_levels_after < length(CKD_LABELS)) {
        cat(sprintf("[import] Suppression de %d niveau(x) vide(s) (0 observations).\n",
                    length(CKD_LABELS) - n_levels_after))
      }
      
      return(list(converted    = TRUE,
                  column       = categories,
                  n_converted  = n_converted,
                  n_na_produced = n_na_produced))
    } else {
      # Numérique mais pas dans la plage eGFR → label numérique discret (ex: 0/1/2)
      cat(sprintf("[import] Colonne cible numérique hors plage eGFR (%.0f%% dans ]0;200]) → conservée comme factor.\n",
                  pct_in_range * 100))
      return(list(converted = FALSE,
                  column = factor(col),
                  n_converted = 0, n_na_produced = 0))
    }
  }
  
  # Fallback
  cat("[import] Type de colonne cible non reconnu — conservé tel quel.\n")
  return(list(converted = FALSE, column = col,
              n_converted = 0, n_na_produced = 0))
}


# =============================================================================
# IMPORT FUNCTION
# =============================================================================
importfunction <- function(importparameters) {
  previousparameters <- NULL
  validation         <- NULL
  learning           <- NULL

  if (is.null(importparameters$learningfile) & is.null(importparameters$modelfile)) {
    return()
  }

  # ── Chargement depuis un fichier modèle sauvegardé ────────────────────────
  if (!is.null(importparameters$modelfile)) {
    load(file = importparameters$modelfile$datapath)
    previous           <- state
    learning           <- previous$data$LEARNING
    validation         <- previous$data$VALIDATION
    previousparameters <- previous$parameters
  }

  # ── Chargement du fichier learning ───────────────────────────────────────
  if (!is.null(importparameters$learningfile)) {

    datapath <- importparameters$learningfile$datapath
    learning <- importfile(datapath    = datapath,
                           extension   = importparameters$extension,
                           NAstring    = importparameters$NAstring,
                           sheet       = importparameters$sheetn,
                           skiplines   = importparameters$skipn,
                           dec         = importparameters$dec,
                           sep         = importparameters$sep)

    learning <- transformdata(toto       = learning,
                              transpose  = importparameters$transpose,
                              zeroegalNA = importparameters$zeroegalNA)

    # ── Conversion eGFR → CKD stage (learning) ──────────────────────────
    cat("\n[import] === Analyse de la colonne cible (learning) ===\n")
    res_learn <- convert_egfr_to_ckd_stage(learning[, 1])
    cat("res_learn$column  : \n")
    print(table(res_learn$column))
    learning[, 1] <- res_learn$column

    if (importparameters$confirmdatabutton != 0) {
      learning <- confirmdata(toto = learning)
    }
  }

  # ── Chargement du fichier validation ─────────────────────────────────────
  if (!is.null(importparameters$validationfile)) {

    datapathV  <- importparameters$validationfile$datapath
    validation <- importfile(datapath  = datapathV,
                             extension = importparameters$extension,
                             NAstring  = importparameters$NAstring,
                             sheet     = importparameters$sheetn,
                             skiplines = importparameters$skipn,
                             dec       = importparameters$dec,
                             sep       = importparameters$sep)

    validation <- transformdata(toto       = validation,
                                transpose  = importparameters$transpose,
                                zeroegalNA = importparameters$zeroegalNA)

    # ── Conversion eGFR → CKD stage (validation) ────────────────────────
    cat("\n[import] === Analyse de la colonne cible (validation) ===\n")
    res_val    <- convert_egfr_to_ckd_stage(validation[, 1])
    validation[, 1] <- res_val$column

    if (importparameters$confirmdatabutton != 0) {
      validation <- confirmdata(toto = validation)
    }
  }

  res <- list("learning"          = learning,
              "validation"        = validation,
              "previousparameters" = previousparameters)
  return(res)
}
# 
# importfunction<-function(importparameters){
#   previousparameters<-NULL
#   validation<-NULL
#   learning<-NULL
# 
#   if(is.null(importparameters$learningfile)&is.null(importparameters$modelfile)){return()}
# 
#   if(!is.null(importparameters$modelfile) ){
#     load(file = importparameters$modelfile$datapath)
#     previous<-state
#     learning<-previous$data$LEARNING
#     validation<-previous$data$VALIDATION
#     #lev<-previous$data$LEVELS
#     previousparameters<-previous$parameters
#   }
# 
#   if(!is.null(importparameters$learningfile)  ){
#     #if(importparameters$confirmdatabutton==0){
#       datapath<- importparameters$learningfile$datapath
#       #datapath <- input$learningfile$datapath
#       #print(datapath)
#       #print(paste(datapath, ".xlsx", sep=""))
#       #out<<-tryCatch(
#       learning<-importfile(datapath = datapath,extension = importparameters$extension,NAstring=importparameters$NAstring,
#                            sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
#       #              ,error=function(e) e )
#       #            if(any(class(out)=="error")){tablearn<-data.frame()}
#       #            else{tablearn<<-out}
#       #            validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
# 
#       learning<-transformdata(toto = learning,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
# 
#       cat("values of learning[,1]  :  \n")
#       print(learning[,1])
# 
#       if(is.factor(learning[,1]) || is.character(learning[,1]) ){
# 
#         # learning[,1]<-as.factor(as.character(learning[,1]))
#         # print(learning[,1])
#         # categories_learning =  cut( as.numeric(learning[, 1]),
#         #                             breaks = c(0, 15, 30, 45, 60, 90, Inf),
#         #                             labels = c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
#         # )
#         #
#         # learning[, 1] = categories_learning
#         #
#         # print(learning[,1])
#         cat("target ok!")
#       }else{
#         print(learning[, 1])
#         categories_learning =  cut( as.numeric(learning[, 1]),
#                                     breaks = c(0, 15, 30, 45, 60, 90, Inf),
#                                     labels = c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
#         )
# 
#         learning[, 1] = categories_learning
#       }
# 
# 
#     #}
#     if(importparameters$confirmdatabutton!=0){
#       learning<-confirmdata(toto = learning)
#       if(importparameters$invers){
#         #learning[,1]<-factor(learning[,1],levels = rev(levels(learning[,1])))
#         }
# 
#       #learning<-learning[-which(apply(X = learning,MARGIN=1,function(x){sum(is.na(x))})==ncol(learning)),]
# 
# #       lev<-levels(x = tablearn[,1])
# #       print(lev)
# #       names(lev)<-c("positif","negatif")
#     }
#     # else{lev<-NULL}
#   }
# 
# 
#   if(!is.null(importparameters$validationfile)){
# 
#     # if(importparameters$confirmdatabutton==0){
#       datapathV<- importparameters$validationfile$datapath
#       # out<<-tryCatch(
#       validation<-importfile(datapath = datapathV,extension = importparameters$extension,
#                  NAstring=importparameters$NAstring,sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
#       #             ,error=function(e) e)
#       #             if(any(class(out)=="error")){tabval<-NULL}
#       #            else{tabval<<-out}
#       #            validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
#         validation<-transformdata(toto = validation,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
#         # data.frame(
#         #   egfr  =  validation[, 1],
#         #   categories  = cut( validation[, 1],
#         #                      breaks = c(0, 15, 30, 45, 60, 90, Inf),
#         #                      labels = c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
#         #   )
#         # )
# 
#         if(is.factor(validation[,1]) || is.character(validation[,1]) ){
# 
#           # validation[,1]<-as.factor(as.character(validation[,1]))
#           # categories_validation =  cut(as.numeric( validation[, 1]),
#           #                               breaks = c(0, 15, 30, 45, 60, 90, Inf),
#           #                               labels = c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
#           # )
#           # validation[, 1] = categories_validation
#           #
#           # cat("validation label status : \n")
#           # print( validation[, 1])
#           cat("target ok!")
# 
#         }else{
#           categories_validation =  cut(as.numeric( validation[, 1]),
#                                         breaks = c(0, 15, 30, 45, 60, 90, Inf),
#                                         labels = c("Stage 5", "Stage 4", "Stage 3b", "Stage 3a", "Stage 2", "Stage 1")
#           )
# 
#           validation[, 1] = categories_validation
#         }
# 
#     # }
#     if(importparameters$confirmdatabutton!=0){
#       validation<-confirmdata(toto = validation)
#       if(importparameters$invers){
#         #validation[,1]<-factor(validation[,1],levels = rev(levels(validation[,1])))
#         }
# 
#       #validation<-validation[-which(apply(X = validation,MARGIN=1,function(x){sum(is.na(x))})==ncol(validation)),]
# 
#     }
# 
#   }
# 
#   res<-list("learning"=learning,
#             "validation"=validation,
#             previousparameters=
#               previousparameters)#,"lev"=lev)
#   return(res)
# }
# 

selectdatafunction<-function(learning,selectdataparameters){
  learningselect<-selectprctvalues(toto = learning,prctvalues = selectdataparameters$prctvalues,selectmethod =selectdataparameters$selectmethod)
  if(selectdataparameters$NAstructure==T){
    if(selectdataparameters$structdata=="selecteddata"){learning<-learningselect}
    restestNAstructure<-testNAstructure(toto = learning,threshold = selectdataparameters$thresholdNAstructure,maxvaluesgroupmin=selectdataparameters$maxvaluesgroupmin,
                                        minvaluesgroupmax=selectdataparameters$minvaluesgroupmax)
    if(!is.null(restestNAstructure)){
      learningselect<-cbind(learningselect[,!colnames(learningselect)%in%restestNAstructure$restestNAstructure$names],restestNAstructure$varNAstructure)}
  }
  else{restestNAstructure<-NULL}
  
  return(list(learningselect=learningselect,structuredfeatures=restestNAstructure$varNAstructure,datastructuredfeatures=restestNAstructure$restestNAstructure))
}

testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

selectprctvalues<-function(toto,prctvalues=100,selectmethod="nogroup"){ 
  n<-ncol(toto)
  if (selectmethod=="nogroup"){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(!is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec>=(prctvalues/100))
    
  } 
  
  if(selectmethod!="nogroup"){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(!is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(selectmethod=="onegroup"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(max (x) >= (prctvalues/100)) }) 
    }
    if(selectmethod=="bothgroups"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(min (x) >= (prctvalues/100)) }) 
    }
  }
  totoselect<-toto[,as.logical(vec)]
}

heatmapNA<-function(toto,maintitle="Distribution of NA",graph=T){
 
    if(ncol(toto)==1){errorplot(text = " No structured variables")}
    else{
      names<- paste(toto[,1],1:length(toto[,1]))
      tab<-as.data.frame(toto[,-1])
      tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
      tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
      #tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
      tab<-apply(tab,2,as.factor)
      rownames(tab)<-names
      if(!graph){ return(cbind(rownames(toto),tab))}
      if(graph){
      tabm <- melt(tab)
      #tabm<-tabm[-c(1:nrow(toto)),]
      colnames(tabm)<-c("individuals","variables","value")
      tabm$variables<-as.character(tabm$variables)
      tabm$individuals<-as.character(tabm$individuals)
      if(ncol(toto)>60){
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"),name="")+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
      else{
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
    }
  }
}

distributionvalues<-function(toto,prctvaluesselect,nvar,maintitle="Number of variables according to\nthe % of values's selected",graph=T,ggplot=T){
  percentagevalues<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto)
  prctvalueswhithoutgroup<-sapply(X = percentagevalues,FUN = function(x,prct=prctall){sum(x<=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  nvareachgroups<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,min))})  
  nvaronegroup<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,max))})  
  
  distribvalues<-data.frame("percentagevalues"=percentagevalues,"all samples"=prctvalueswhithoutgroup,"each groups"= nvareachgroups,"at least one group"=nvaronegroup)
  if(!graph)(return(distribvalues))
  col<-gg_color_hue(ncol(distribvalues)-1)
  if(!ggplot){
    matplot(x=distribvalues$percentagevalues,distribvalues[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of values selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribvalues[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctvaluesselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
    distribvalueslong<- melt(distribvalues,id.vars = "percentagevalues",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
    p<-ggplot(data=distribvalueslong,
              aes(x=percentagevalues, y=number_of_variables, colour=select_method)) +geom_line()+
      ggtitle(maintitle)
    p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.20,0.15))+
      geom_vline(xintercept=prctvaluesselect,linetype=3)+
      geom_hline(yintercept=nvar,linetype=3)
  }
}

proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}

testNAstructure<-function(toto,threshold=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
    colnames(totopropselect)<-resp$names[order(resp[,2])]
  }
  else{return(NULL)}

  return(list("varNAstructure"=totopropselect,"restestNAstructure"=resp))
}

transformdatafunction<-function(learningselect,structuredfeatures,datastructuresfeatures,transformdataparameters){
  learningtransform<-learningselect
  if(!is.null(structuredfeatures)){
    for(i in 1:ncol(structuredfeatures)){
      learningtransform[which(is.na(structuredfeatures[,i])&learningselect[,1]==as.character(datastructuresfeatures[i,"lessgroup"])),as.character(datastructuresfeatures[i,"names"])]<-0
    }
  }
  if(transformdataparameters$log){ 
    learningtransform[,-1]<-transformationlog(x = learningtransform[,-1]+1,logtype=transformdataparameters$logtype)}
  if(transformdataparameters$arcsin){
    learningtransform[,-1]<-apply(X = learningtransform[,-1],MARGIN = 2,FUN = function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))})
    learningtransform[,-1]<-asin(sqrt(learningtransform[,-1]))
  }
  if(transformdataparameters$standardization){
    learningtransformsd<<-learningtransform
    sdlearningtransform<-apply(X = learningtransform[-1],MARGIN = 2,FUN = sd,na.rm=T)
    #print('sdlearningtransform')
    #print(sdlearningtransform)
    learningtransform[,-1]<-scale(learningtransform[,-1],center = F,scale=sdlearningtransform)
    #learningtransform[,-1]<-scale(learningtransform[,-1], center = F, scale = TRUE)
  }
  learningtransform<-replaceNA(toto=learningtransform,rempNA=transformdataparameters$rempNA,pos=T,NAstructure = F)
  
  return(learningtransform)
}

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

histplot<-function(toto,graph=T){

    data<-data.frame("values"=as.vector(as.matrix(toto[,-1])))
    if(graph==F){ return(datahistogram(data = data,nbclass = 20))}
    if(graph==T){
    ggplot(data=data,aes(x=values) )+ 
      geom_histogram(col="lightgrey",fill="steelblue",bins=20)+ggtitle("Distribution of values")+
      theme(plot.title = element_text(size=15))+
       annotate("text",x=Inf,y=Inf,label=paste(nrow(data),"values"),size=6,vjust=2,hjust=1.5)
  }
}

datahistogram<-function(data,nbclass){
  dh<-hist(data[,1],nclass=nbclass,plot=F)
  minclass<-dh$breaks[-(length(dh$breaks))]
  maxclass<-dh$breaks[2:(length(dh$breaks))]
  count<-dh$counts
  res<-data.frame("count"=count,"minclass"=minclass,"maxclass"=maxclass)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}

mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

heatmapplot<-function(toto,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  row.names(toto)<-paste(toto[,1],1:length(toto[,1]))
  toto<-as.matrix(toto[,-1])
  if(!graph){return(toto)}
  #colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(!ggplot){
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
    }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    titi[,2]<-as.character(titi[,2])
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

#############
testfunction<-function(tabtransform,testparameters){
  #condition tests
  if (testparameters$SFtest){
    datatesthypothesis<-SFtest(tabtransform,shaptest=T,Ftest=T,threshold=0.05)
  }
  else{datatesthypothesis<-data.frame()}

  #diff test
  if(testparameters$test=="notest"){
    tabdiff<-tabtransform
    datatest<-NULL
    testparameters<-NULL
    useddata<-NULL
    multivariateresults<-NULL
  }
  else if(testparameters$test%in%c("lasso","elasticnet","ridge","cox")){
    # Multivariate selection methods
    multivariateresults<-multivariateselection(toto = tabtransform,
                                               method = testparameters$test,
                                               lambda = testparameters$lambda,
                                               alpha = testparameters$alpha,
                                               nlambda = 100)
    
    # Check for error in multivariateselection
    if (!is.null(multivariateresults$error)) {
      cat("[testfunction] ERROR in multivariateselection:", multivariateresults$error, "\n")
      tabdiff <<- data.frame()
      useddata <- NULL
      multivariateresults <- NULL
      return(list(tabdiff = tabdiff, useddata = useddata, multivariateresults = multivariateresults))
    }
    
    datatest<-multivariateresults$results
    cat(" checking datatest strurcture...\n")
    print(str(datatest))

    if(nrow(datatest)==0){
      print("no variables selected by multivariate method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      
      multivariateresults <- multivariateselection(
        toto = tabtransform,
        method = testparameters$test,
        lambda = testparameters$lambda,
        alpha = testparameters$alpha,
        nlambda = 100
      )
      
      datatest <- multivariateresults$results
      cat("Checking datatest structure...\n")
      print(str(datatest))
      
      if(nrow(datatest) == 0){
        print("No variables selected by multivariate method")
        tabdiff <<- data.frame()
        useddata <- NULL
      } else {
        selected_vars <- multivariateresults$selected_vars
        indvar <- (colnames(tabtransform) %in% selected_vars)
        indvar[1] <- TRUE  # keep the categorical variable
        tabdiff <<- tabtransform[, indvar]
        
        # Déterminer le nombre de classes
        n_classes <- length(levels(tabtransform[,1]))
        
        # Extraire les colonnes de moyennes
        mean_cols <- grep("^mean_", colnames(datatest), value = TRUE)
        
        # Calculer logFC/effect_size
        if(n_classes == 2){
          # Cas binaire : logFC traditionnel
          means_matrix <- as.matrix(datatest[, mean_cols])
          logFC <- log2((means_matrix[,1] + 0.0001) / (means_matrix[,2] + 0.0001))
          
          # Créer useddata avec mean1 et mean2
          useddata <- data.frame(
            "names" = datatest$name,
            "coefficient" = datatest$coefficient_max,
            "logFC" = logFC,
            "mean1" = datatest[, mean_cols[1]],
            "mean2" = datatest[, mean_cols[2]]
          )
        } else {
          # Cas multi-classe : effect_size
          means_matrix <- as.matrix(datatest[, mean_cols])
          
          # Effect size = étendue (range) des moyennes
          effect_size <- apply(means_matrix, 1, function(x) max(x) - min(x))
          
          # Moyenne globale
          mean_overall <- rowMeans(means_matrix)
          
          # LogFC normalisé pour compatibilité
          logFC <- log2(1 + effect_size / mean_overall)
          
          # Créer useddata avec toutes les colonnes de moyennes
          useddata <- data.frame(
            "names" = datatest$name,
            "coefficient" = datatest$coefficient_max,
            "logFC" = logFC,
            "mean_overall" = mean_overall
          )
          
          # Ajouter toutes les moyennes par classe
          for(col in mean_cols){
            useddata[, col] <- datatest[, col]
          }
        }
      }
    }
  }else if (testparameters$test=="clustEnet"){
    # Clustering + Elastic Net selection method
    cat("Running Clustering + ElasticNet variable selection...\n")
    
    # Get parameters with defaults
    n_clusters <- if(!is.null(testparameters$n_clusters)) testparameters$n_clusters else 100
    n_bootstrap <- if(!is.null(testparameters$n_bootstrap)) testparameters$n_bootstrap else 500
    alpha_enet <- if(!is.null(testparameters$alpha)) testparameters$alpha else 0.5
    min_selection_freq <- if(!is.null(testparameters$min_selection_freq)) testparameters$min_selection_freq else 0.5
    preprocess <- if(!is.null(testparameters$preprocess)) testparameters$preprocess else TRUE
    min_patients <- if(!is.null(testparameters$min_patients)) testparameters$min_patients else 20
    
    multivariateresults <- clustEnetSelection(toto = tabtransform,
                                              n_clusters = n_clusters,
                                              n_bootstrap = n_bootstrap,
                                              alpha_enet = alpha_enet,
                                              min_selection_freq = min_selection_freq,
                                              preprocess = preprocess,
                                              min_patients = min_patients)
    datatest <- multivariateresults$results
    
    if(nrow(datatest)==0){
      print("no variables selected by clustering + elasticnet method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars <- multivariateresults$selected_vars
      indvar <- (colnames(tabtransform) %in% selected_vars)
      indvar[1] <- T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata <- data.frame("names"=datatest$name,
                             "SelectionFrequency"=datatest$SelectionFrequency,
                             "logFC"=datatest$logFoldChange,
                             "mean1"=datatest$mean_group1,
                             "mean2"=datatest$mean_group2)
    }
  } else if(testparameters$test == "boruta") {
    cat("Running Boruta variable selection...\n")
    multivariateresults <- NULL
    
    boruta_maxRuns <- if(!is.null(testparameters$boruta_maxRuns)) testparameters$boruta_maxRuns else 100
    boruta_pValue <- if(!is.null(testparameters$boruta_pValue)) testparameters$boruta_pValue else 0.01
    
    boruta_res <- boruta_selection(toto = tabtransform, maxRuns = boruta_maxRuns, pValue = boruta_pValue)
    
    tabdiff <<- boruta_res$tabdiff
    datatest <- boruta_res$importance
    
    n_classes <- length(levels(tabtransform[,1]))
    
    if(nrow(datatest) > 0) {
      useddata <- data.frame(
        "names" = datatest$name,
        "decision" = datatest$decision,
        "meanImportance" = datatest$meanImp
      )
    } else {
      useddata <- NULL
    }
    
    return(list("tabdiff" = tabdiff,
                "datatest" = datatest,
                "hypothesistest" = datatesthypothesis,
                "useddata" = useddata,
                "testparameters" = testparameters,
                "multivariateresults" = multivariateresults,
                "boruta_result" = boruta_res))
  }
  else{
    # Univariate tests (Kruskal, ANOVA)
    multivariateresults <- NULL
    # cat("dimension of tabtransform in testfunction : ", dim(tabtransform), "\n")
    # print(dim(tabtransform))
    
    datatest <- diffexptest(toto = tabtransform, test = testparameters$test)
    
    # Get number of classes
    n_classes <- length(levels(tabtransform[,1]))
    
    # Extract logFC (will be either traditional logFC for 2 classes or effect_size for multi-class)
    logFC <- datatest$logFC
    
    # Extract p-values
    if(testparameters$adjustpval){
      pval <- datatest[,3]  # adjusted pval
    } else {
      pval <- datatest[,2]  # raw pval
    }
    
    # Filter differentially expressed variables
    datatestdiff <- datatest[which((pval < testparameters$thresholdpv) & 
                                     abs(logFC) > testparameters$thresholdFC), ]
    
    if(dim(datatestdiff)[1] == 0){
      print("no differentially expressed variables")
      tabdiff <<- data.frame()
    } else {
      indvar <- (colnames(tabtransform) %in% datatestdiff$name)
      indvar[1] <- TRUE  # keep the categorical variable
      tabdiff <<- tabtransform[, indvar]
    }
    
    # Create useddata with appropriate columns based on number of classes
    if(n_classes == 2){
      # Binary case: use traditional mean1 and mean2
      mean_cols <- grep("^mean_", colnames(datatest), value = TRUE)
      useddata <- data.frame(
        "names" = datatest$name,
        "pval" = pval,
        "logFC" = datatest$logFC,
        "mean1" = datatest[, mean_cols[1]],
        "mean2" = datatest[, mean_cols[2]]
      )
    } else {
      # Multi-class case: include all means and effect_size
      mean_cols <- grep("^mean_", colnames(datatest), value = TRUE)
      useddata <- data.frame(
        "names" = datatest$name,
        "pval" = pval,
        "logFC" = datatest$logFC,  # This is actually effect_size for multi-class
        "mean_overall" = datatest$mean_overall
      )
      # Add all class means
      for(col in mean_cols){
        useddata[, col] <- datatest[, col]
      }
    }
  }
  return(list("tabdiff"=tabdiff,
              "datatest"=datatest,
              "hypothesistest"=datatesthypothesis,
              "useddata"=useddata,
              "testparameters"=testparameters,
              "multivariateresults"=multivariateresults))
}
  

# diffexptest <- function(toto, test="Kruskal"){
#   # Test statistical pour multi-classe (fonctionne aussi pour 2 classes)
#   # test = "Kruskal" : Kruskal-Wallis (non-paramétrique)
#   # test = "ANOVA" : ANOVA (paramétrique)
#   
#   
#   cat("debug of difftest at start : \n")
#   cat("dimension of toto : \n")
#   print( dim(toto))
#   group <- toto[,1]
#   cat("affichage des level de groupe : ")
#   toto <- toto[,-1]
#   n_classes <- length(levels(group))
#   
#   pval <- vector()
#   adjustpval <- vector()
#   
#   # Calculate mean for each class
#   means_by_class <- matrix(nrow = ncol(toto), ncol = n_classes)
#   colnames_means <- paste("mean", levels(group), sep = "_")
#   
#   # Calculate overall mean
#   mean_overall <- vector()
#   
#   # Multi-class AUC
#   auc_multiclass <- vector()
#   
#   for (i in 1:max(1,ncol(toto))){
#     # Statistical test
#     if(test == "Kruskal"){
#       # Kruskal-Wallis test (non-parametric)
#       pval[i] <- tryCatch({
#         kruskal.test(toto[,i] ~ group)$p.value
#       }, error = function(e) return(1))
#     } else if(test == "ANOVA"){
#       # ANOVA (parametric)
#       pval[i] <- tryCatch({
#         summary(aov(toto[,i] ~ group))[[1]][1,"Pr(>F)"]
#       }, error = function(e) return(1))
#     }
#     
#     # Calculate means for each class
#     for(j in 1:n_classes){
#       class_data <- toto[which(group == levels(group)[j]), i]
#       means_by_class[i, j] <- mean(class_data, na.rm = TRUE) + 0.0001
#     }
#     
#     # Overall mean
#     mean_overall[i] <- mean(toto[,i], na.rm = TRUE) + 0.0001
#     
#     # Multi-class AUC (one-vs-rest average)
#     auc_multiclass[i] <- tryCatch({
#       roc_obj <- multiclass.roc(group, toto[,i], quiet=TRUE)
#       as.numeric(auc(roc_obj))
#     }, error = function(e) return(0.5))
#   }
#   
#   pval[which(is.na(pval))] <- 1
#   adjustpval <- p.adjust(pval, method = "BH")
#   
#   cat("debug of difftest  : \n")
#   cat("length  of colnames(toto) : ", length(colnames(toto)), "\n")
#   cat("length of pval : ", length(pval), "\n")
#   cat("length of adjustpval : ", length(adjustpval), "\n")
#   cat("length of auc_multiclass : ", length(auc_multiclass), "\n")
#   cat("length of mean_overall : ", length(mean_overall), "\n")
#   
#   # Build result dataframe
#   listgen <- data.frame(
#     name = colnames(toto),
#     pval = pval,
#     adjustpval = adjustpval,
#     auc = auc_multiclass,
#     mean_overall = mean_overall
#   )
#   
#   # Add means for each class
#   for(j in 1:n_classes){
#     listgen[, paste("mean", levels(group)[j], sep = "_")] <- means_by_class[, j]
#   }
#   
#   # Rename columns
#   colnames(listgen)[2] <- paste("pval", test, sep = "")
#   colnames(listgen)[3] <- paste("BHadjustpval", test, sep = "")
#   colnames(listgen)[4] <- "AUC_multiclass"
#   
#   return(listgen)
#}

diffexptest <- function(toto, test="Kruskal"){
  # Test statistical pour multi-classe (fonctionne aussi pour 2 classes)
  # test = "Kruskal" : Kruskal-Wallis (non-paramétrique)
  # test = "ANOVA" : ANOVA (paramétrique)
  
  cat("debug of difftest at start : \n")
  cat("dimension of toto : \n")
  print(dim(toto))
  
  group <- toto[,1]
  cat("affichage des level de groupe : ", levels(group), "\n")
  toto <- toto[,-1]
  n_classes <- length(levels(group))
  
  pval <- vector()
  adjustpval <- vector()
  
  # Calculate mean for each class
  means_by_class <- matrix(nrow = ncol(toto), ncol = n_classes)
  colnames_means <- paste("mean", levels(group), sep = "_")
  
  # Calculate overall mean
  mean_overall <- vector()
  
  # Multi-class AUC
  auc_multiclass <- vector()
  
  for (i in 1:max(1, ncol(toto))){
    # Statistical test
    if(test == "Kruskal"){
      # Kruskal-Wallis test (non-parametric)
      pval[i] <- tryCatch({
        kruskal.test(toto[,i] ~ group)$p.value
      }, error = function(e) return(1))
    } else if(test == "ANOVA"){
      # ANOVA (parametric)
      pval[i] <- tryCatch({
        summary(aov(toto[,i] ~ group))[[1]][1,"Pr(>F)"]
      }, error = function(e) return(1))
    }
    
    # Calculate means for each class
    for(j in 1:n_classes){
      class_data <- toto[which(group == levels(group)[j]), i]
      means_by_class[i, j] <- mean(class_data, na.rm = TRUE) + 0.0001
    }
    
    # Overall mean
    mean_overall[i] <- mean(toto[,i], na.rm = TRUE) + 0.0001
    
    # Multi-class AUC (one-vs-rest average)
    auc_multiclass[i] <- tryCatch({
      roc_obj <- pROC::multiclass.roc(group, toto[,i], quiet=TRUE)
      as.numeric(pROC::auc(roc_obj))
    }, error = function(e) return(0.5))
  }
  
  pval[which(is.na(pval))] <- 1
  adjustpval <- p.adjust(pval, method = "BH")
  
  cat("debug of difftest  : \n")
  cat("length of colnames(toto) : ", length(colnames(toto)), "\n")
  cat("length of pval : ", length(pval), "\n")
  cat("length of adjustpval : ", length(adjustpval), "\n")
  cat("length of auc_multiclass : ", length(auc_multiclass), "\n")
  cat("length of mean_overall : ", length(mean_overall), "\n")
  
  # Build result dataframe
  listgen <- data.frame(
    name = colnames(toto),
    pval = pval,
    adjustpval = adjustpval,
    auc = auc_multiclass,
    mean_overall = mean_overall
  )
  
  # Calculate logFC for binary comparison or effect size for multi-class
  if(n_classes == 2){
    # Binary case: traditional logFC
    FC <- means_by_class[, 1] / (means_by_class[, 2] + 0.0001)
    logFC <- log2(abs(FC))
    listgen$logFC <- logFC
    listgen$FoldChange <- FC
  } else {
    # Multi-class: use range of means as effect size
    # This represents the spread between highest and lowest group means
    effect_size <- apply(means_by_class, 1, function(x) max(x) - min(x))
    listgen$effect_size <- effect_size
    # For compatibility with downstream code that expects logFC
    # Use effect_size normalized by overall mean
    listgen$logFC <- log2(1 + effect_size / mean_overall)
  }
  
  # Add means for each class
  for(j in 1:n_classes){
    listgen[, paste("mean", levels(group)[j], sep = "_")] <- means_by_class[, j]
  }
  
  # Rename columns
  colnames(listgen)[2] <- paste("pval", test, sep = "")
  colnames(listgen)[3] <- paste("BHadjustpval", test, sep = "")
  colnames(listgen)[4] <- "AUC_multiclass"
  
  return(listgen)
}


compute_multiclass_metrics <- function(predicted, actual) {
  # Calcule les métriques pour la classification multi-classe
  # Utilise le macro-averaging pour sensibilité, spécificité et F1-score
  
  n_classes <- length(levels(actual))
  classes <- levels(actual)
  
  # Accuracy globale
  accuracy <- sum(predicted == actual) / length(actual)
  
  # Sensibilité et spécificité moyennes (déjà implémentées)
  sens <- sensitivity_multiclass(predicted, actual)
  spec <- specificity_multiclass(predicted, actual)
  
  # F1-score macro-average
  f1_scores <- numeric(n_classes)
  precision_scores <- numeric(n_classes)
  recall_scores <- numeric(n_classes)
  
  for(i in 1:n_classes) {
    class_i <- classes[i]
    
    # True Positives, False Positives, False Negatives
    TP <- sum(predicted == class_i & actual == class_i)
    FP <- sum(predicted == class_i & actual != class_i)
    FN <- sum(predicted != class_i & actual == class_i)
    
    # Precision et Recall pour cette classe
    precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
    recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
    
    precision_scores[i] <- precision
    recall_scores[i] <- recall
    
    # F1-score pour cette classe
    f1_scores[i] <- ifelse(precision + recall > 0, 
                           2 * precision * recall / (precision + recall), 
                           0)
  }
  
  # Macro-averages
  precision_macro <- mean(precision_scores)
  recall_macro <- mean(recall_scores)
  f1_macro <- mean(f1_scores)
  
  return(list(
    accuracy = round(accuracy, 3),
    sensitivity = sens,
    specificity = spec,
    precision_macro = round(precision_macro, 3),
    recall_macro = round(recall_macro, 3),
    f1_score = round(f1_macro, 3),
    
    # Métriques par classe
    precision_per_class = precision_scores,
    recall_per_class = recall_scores,
    f1_per_class = f1_scores
  ))
}


# Works for 2+ classes using One-vs-Rest approach
specificity <- function(predict, class){
  # Ensure both are factors with same levels
  if(!is.factor(class)) class <- as.factor(class)
  if(!is.factor(predict)) predict <- as.factor(predict)
  
  # Get confusion matrix
  conf_matrix <- table(Predicted = predict, Actual = class)
  
  # Get class levels
  lev <- levels(class)
  n_classes <- length(lev)
  
  # Calculate specificity per class (One-vs-Rest)
  # Specificity = TN / (TN + FP)
  specificity_per_class <- numeric(n_classes)
  names(specificity_per_class) <- lev
  
  for(i in 1:n_classes){
    class_name <- lev[i]
    
    # True Negatives: correctly predicted as NOT this class
    # Sum of all cells except the row and column of this class
    TN <- sum(conf_matrix) - sum(conf_matrix[class_name, ]) - sum(conf_matrix[, class_name]) + conf_matrix[class_name, class_name]
    
    # False Positives: predicted as this class but actually another
    FP <- sum(conf_matrix[class_name, ]) - conf_matrix[class_name, class_name]
    
    # Specificity for this class
    if((TN + FP) > 0){
      specificity_per_class[i] <- TN / (TN + FP)
    } else {
      specificity_per_class[i] <- NA
    }
  }
  
  # Calculate macro-average (mean of per-class specificities)
  macro_specificity <- mean(specificity_per_class, na.rm = TRUE)
  
  # Round results
  specificity_per_class <- round(specificity_per_class, digits = 3)
  macro_specificity <- round(macro_specificity, digits = 3)
  
  # Return results
  return(list(
    per_class = specificity_per_class,
    macro_average = macro_specificity,
    n_classes = n_classes
  ))
}

##########################
# Multivariate variable selection functions
##########################
multivariateselection<-function(toto, method="lasso", lambda=NULL, alpha=0.5, nlambda=100){
  # Function for multivariate variable selection using regularization methods
  # toto: dataframe with first column as group (factor) and other columns as features
  # method: "lasso" (alpha=1), "elasticnet" (0<alpha<1), "ridge" (alpha=0)
  # lambda: regularization parameter (NULL for automatic selection via CV)
  # alpha: elastic net mixing parameter (0=ridge, 1=lasso)
  # nlambda: number of lambda values to test
  
  lev <- levels(toto[,1])
  n_classes <- length(lev)
  x <- as.matrix(toto[,-1])
  
  # Set alpha based on method
  if(method == "lasso"){
    alpha <- 1
  } else if(method == "ridge" | method == "cox"){
    alpha <- 0
  }
  
  # Use factor for multinomial (works for 2+ classes)
  y <- toto[,1]
  
  # Check minimum observations per class for multinomial glmnet
  class_counts <- table(y)
  min_obs_per_class <- min(class_counts)
  if (min_obs_per_class < 2) {
    cat(sprintf("[multivariateselection] ERROR: Class '%s' has only %d observation(s). Minimum 2 required for multinomial glmnet.\n",
                names(which.min(class_counts)), min_obs_per_class))
    cat("Class distribution:\n")
    print(class_counts)
    return(list(
      results = data.frame(),
      selected_vars = character(0),
      error = sprintf("Class '%s' has only %d observation(s). Minimum 2 required for multinomial glmnet.",
                      names(which.min(class_counts)), min_obs_per_class)
    ))
  }
  
  # Regrouper les classes avec moins de 8 observations pour glmnet
  # (pour éviter le warning "dangerous ground" et l'erreur)
  small_classes <- names(class_counts[class_counts < 8])
  y_glmnet <- y  # copie pour glmnet
  merged_info <- NULL  # stocker l'info de regroupement
  
  if (length(small_classes) > 0 && n_classes > 2) {
    cat(sprintf("[multivariateselection] %d classe(s) avec < 8 observations: %s\n",
                length(small_classes), paste(small_classes, collapse = ", ")))
    
    # Pour CKD stages, l'ordre est : Stage 5 → Stage 4 → Stage 3b → Stage 3a → Stage 2 → Stage 1
    # On regroupe avec la classe adjacente la plus proche
    lev_ordered <- lev
    
    for (small_cls in small_classes) {
      cls_idx <- which(lev_ordered == small_cls)
      
      # Trouver la classe adjacente la plus proche
      if (cls_idx == 1) {
        # Première classe → regrouper avec la suivante
        target_cls <- lev_ordered[cls_idx + 1]
      } else if (cls_idx == length(lev_ordered)) {
        # Dernière classe → regrouper avec la précédente
        target_cls <- lev_ordered[cls_idx - 1]
      } else {
        # Classe intermédiaire → regrouper avec la plus proche en effectif
        prev_cls <- lev_ordered[cls_idx - 1]
        next_cls <- lev_ordered[cls_idx + 1]
        if (class_counts[prev_cls] >= class_counts[next_cls]) {
          target_cls <- prev_cls
        } else {
          target_cls <- next_cls
        }
      }
      
      cat(sprintf("[multivariateselection] Regroupement: '%s' → '%s'\n", small_cls, target_cls))
      
      # Renommer les niveaux
      levels(y_glmnet)[levels(y_glmnet) == small_cls] <- target_cls
    }
    
    # Supprimer les niveaux vides après regroupement
    y_glmnet <- droplevels(y_glmnet)
    merged_info <- list(
      original_levels = lev,
      merged_levels = levels(y_glmnet),
      small_classes = small_classes
    )
    cat(sprintf("[multivariateselection] Classes après regroupement: %s\n",
                paste(levels(y_glmnet), collapse = ", ")))
  } else {
    y_glmnet <- y
  }
  
  # Perform cross-validation to find optimal lambda if not provided
  if(is.null(lambda)){
    set.seed(20011203)
    cat("Dans la section glmnet : \n")
    print(table(y_glmnet))
    cvfit <- cv.glmnet(x, y_glmnet, family="multinomial",
                       alpha=alpha, nlambda=nlambda,
                       type.measure="class", nfolds = 3,
                         #min(5, nrow(toto)-1),
                       type.multinomial = "grouped")
    lambda <- cvfit$lambda.min
    lambda_1se <- cvfit$lambda.1se
  } else {
    cvfit <- NULL
    lambda_1se <- lambda
  }
  
  # Fit model with optimal lambda
  fit <- glmnet(x, y_glmnet, family="multinomial", alpha=alpha, lambda=lambda,
                type.multinomial = "grouped")
  
  # Extract coefficients (list of matrices, one per class)
  coef_list <- coef(fit, s=lambda)
  
  # Aggregate coefficients across classes (use max absolute value)
  coef_aggregated <- rep(0, ncol(x))
  names(coef_aggregated) <- colnames(x)
  
  # Utiliser le nombre de classes après regroupement (peut être < n_classes)
  n_classes_glmnet <- length(coef_list)
  for(class_idx in 1:n_classes_glmnet){
    coef_matrix <- as.matrix(coef_list[[class_idx]])
    coef_values_class <- coef_matrix[-1, 1]  # Remove intercept
    # Keep maximum absolute coefficient across classes
    coef_aggregated <- pmax(abs(coef_aggregated), abs(coef_values_class))
  }
  
  # Select non-zero coefficients
  selected_vars <- names(coef_aggregated[coef_aggregated > 1e-10])
  
  # Calculate additional statistics for selected variables
  if(length(selected_vars) > 0){
    # Multi-class AUC for each selected variable
    auc_values <- sapply(selected_vars, function(var){
      tryCatch({
        roc_obj <- pROC::multiclass.roc(toto[,1], x[, var], quiet=TRUE)
        round(as.numeric(pROC::auc(roc_obj)),3)
      }, error = function(e) return(0.5))
    })
    
    # Mean values by group for each class
    means_matrix <- matrix(nrow=length(selected_vars), ncol=n_classes)
    for(j in 1:n_classes){
      means_matrix[, j] <- colMeans(x[which(toto[,1] == lev[j]), selected_vars, drop=FALSE], na.rm=TRUE)
    }
    colnames(means_matrix) <- paste("mean", lev, sep="_")
    
    # ============================================================
    # CALCUL DU LOGFC / EFFECT SIZE
    # ============================================================
    
    if(n_classes == 2){
      # Cas binaire : logFC traditionnel
      FC <- means_matrix[, 1] / (means_matrix[, 2] + 0.0001)
      logFC <- log2(abs(FC))
      FoldChange <- FC
    } else {
      # Cas multi-classe : effect size (étendue des moyennes)
      effect_size <- apply(means_matrix, 1, function(x) max(x) - min(x))
      mean_overall <- rowMeans(means_matrix)
      
      # LogFC normalisé pour compatibilité
      logFC <- log2(1 + effect_size / mean_overall)
      FoldChange <- effect_size
    }
    
    # ============================================================
    # CRÉER LE DATAFRAME DE RÉSULTATS
    # ============================================================
    
    # Create results dataframe
    results <- data.frame(
      name = selected_vars,
      coefficient_max = coef_aggregated[selected_vars],
      AUC_multiclass = auc_values,
      logFoldChange = logFC,
      FoldChange = FoldChange,
      stringsAsFactors = FALSE
    )
    
    # Ajouter mean_overall pour multi-classe
    if(n_classes > 2){
      results$mean_overall <- round(rowMeans(means_matrix), 3)
    }
    
    # Add means for each class
    for(j in 1:n_classes){
      results[, colnames(means_matrix)[j]] <- means_matrix[, j]
    }
    
    # Sort by absolute coefficient value
    results <- results[order(abs(results$coefficient_max), decreasing=TRUE), ]
    
  } else {
    results <- data.frame()
  }
  
  # Return results with model information
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_coefficients = coef_aggregated,
    coef_list = coef_list,
    lambda = lambda,
    lambda_1se = lambda_1se,
    alpha = alpha,
    cvfit = cvfit,
    fit = fit,
    method = method,
    n_classes = n_classes
  ))
}

# multivariateselection<-function(toto, method="lasso", lambda=NULL, alpha=0.5, nlambda=100){
#   # Support for multi-class classification
#   group <- factor(toto[,1])
#   n_classes <- length(levels(group))
#   x <- as.matrix(toto[,-1])
#   y <- group
#   min_features = 2
#   
#   # Déterminer la famille selon le nombre de classes
#   family_type <- if(n_classes == 2) "binomial" else "multinomial"
#   
#   cat(sprintf("Variable selection using %s (n_classes=%d, family=%s)...\n", 
#               method, n_classes, family_type))
#   
#   if(method %in% c("lasso", "elasticnet", "ridge")){
#     # Alpha values: lasso=1, ridge=0, elasticnet=0.5
#     if(method == "lasso") alpha <- 1
#     if(method == "ridge") alpha <- 0
#     
#     # Cross-validation pour trouver le meilleur lambda
#     cv_fit <- cv.glmnet(
#       x = x, 
#       y = y, 
#       alpha = alpha, 
#       family = family_type,  # Multinomial pour multi-classe
#       nlambda = nlambda,
#       type.measure = "class",  # Mesure: erreur de classification
#       parallel = FALSE
#     )
#     
#     # Extraire le lambda optimal
#     lambda_opt <- if(is.null(lambda)) cv_fit$lambda.min else lambda
#     
#     # Entraîner le modèle final avec lambda optimal
#     final_model <- glmnet(
#       x = x, 
#       y = y, 
#       alpha = alpha,
#       lambda = lambda_opt,
#       family = family_type
#     )
#     
#     # Extraire les coefficients
#     coef_matrix <- coef(final_model, s = lambda_opt)
#     
#     # Pour multi-classe, coef_matrix est une liste de matrices (une par classe)
#     if(n_classes > 2) {
#       # Combiner les coefficients de toutes les classes
#       # On prend la somme des valeurs absolues pour chaque variable
#       all_coefs <- matrix(0, nrow = nrow(coef_matrix[[1]]), ncol = 1)
#       rownames(all_coefs) <- rownames(coef_matrix[[1]])
#       
#       for(class_idx in 1:n_classes) {
#         all_coefs <- all_coefs + abs(as.matrix(coef_matrix[[class_idx]]))
#       }
#       
#       # Retirer l'intercept et garder les variables non nulles
#       selected_vars_idx <- which(all_coefs[-1, 1] != 0)
#       
#     } else {
#       # Classification binaire
#       coef_values <- as.matrix(coef_matrix)[-1, 1]  # Retirer l'intercept
#       selected_vars_idx <- which(coef_values != 0)
#     }
#     
#     # S'assurer d'avoir au moins min_features variables
#     if(length(selected_vars_idx) < min_features) {
#       cat(sprintf("  Warning: Only %d features selected, minimum is %d\n", 
#                   length(selected_vars_idx), min_features))
#       cat("  Selecting top features by coefficient magnitude...\n")
#       
#       # Sélectionner les top variables par magnitude
#       if(n_classes > 2) {
#         feature_importance <- all_coefs[-1, 1]
#       } else {
#         feature_importance <- abs(coef_values)
#       }
#       selected_vars_idx <- order(feature_importance, decreasing = TRUE)[1:min_features]
#     }
#     
#     # Noms des variables sélectionnées
#     selected_vars <- colnames(x)[selected_vars_idx]
#     
#     cat(sprintf("  Selected %d features out of %d (lambda=%.4f)\n", 
#                 length(selected_vars), ncol(x), lambda_opt))
#     
#     return(list(
#       selected_variables = selected_vars,
#       selected_indices = selected_vars_idx,
#       model = final_model,
#       lambda = lambda_opt,
#       alpha = alpha,
#       cv_model = cv_fit
#     ))
#     
#   } else {
#     stop(paste("Unknown method:", method))
#   }
# }

##########################
# Clustering + Elastic Net selection function
##########################

# Preprocess peptides: filter low variance and low frequency variables
preprocess_peptides <- function(peptide_data, min_patients = 20) {
  # Filter variables with too few non-zero patients
  n_nonzero <- colSums(peptide_data != 0, na.rm = TRUE)
  keep_peptides <- n_nonzero >= min_patients
  
  # Filter variables with near-zero variance
  variances <- apply(peptide_data, 2, var, na.rm = TRUE)
  keep_var <- variances > 1e-10
  
  return(peptide_data[, keep_peptides & keep_var, drop=FALSE])
}

# Variable selection using clustering and elastic net with bootstrap
varselClust <- function(toto, n_clusters = 100, n_bootstrap = 500, alpha_enet = 0.5,
                        min_selection_freq = 0.5, preprocess = TRUE, min_patients = 20){
  
  withProgress(message = 'Selecting variables in progress...', value = 0, {
    
    # Extract group and data
    lev <- levels(toto[,1])
    group <- ifelse(toto[,1] == lev[1], 1, 0)
    y <- group
    data <- as.matrix(toto[,-1])
    
    # Optional preprocessing
    if(preprocess && ncol(data) > min_patients){
      incProgress(0.05, detail = "Data pre-processing...")
      cat("Preprocessing data: filtering low variance and low frequency variables...\n")
      data_preprocessed <- preprocess_peptides(data, min_patients = min_patients)
      if(ncol(data_preprocessed) < ncol(data)){
        cat(sprintf("  Preprocessing: %d → %d variables (removed %d)\n",
                    ncol(data), ncol(data_preprocessed), ncol(data) - ncol(data_preprocessed)))
        data <- data_preprocessed
      }
    }
    
    if(ncol(data) == 0){
      warning("No variables remaining after preprocessing")
      return(list(
        selected_peptides_per_cluster = character(0),
        final_selected_peptides = character(0),
        selection_frequencies = data.frame()
      ))
    }
    
    # Step 1: Clustering based on Spearman correlation
    incProgress(0.1, detail = sprintf("Clustering (%d variables)...", ncol(data)))
    cat(sprintf("Step 1: Clustering %d variables into %d clusters...\n", ncol(data), n_clusters))
    correlation_matrix <- cor(data, use = "pairwise.complete.obs", method = "spearman")
    #distance_matrix <- 1 - abs(correlation_matrix)
    distance_matrix <- sqrt(2 - 2*correlation_matrix)
    distance_matrix[is.na(distance_matrix)] <- 1
    hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
    k <- min(n_clusters, ncol(data))
    clusters <- cutree(hc, k = k)
    
    # Step 2: Select one variable per cluster using Wilcoxon test
    incProgress(0.05, detail = "Cluster selection...")
    cat(sprintf("Step 2: Selecting one variable per cluster (Wilcoxon test)...\n"))
    selected_peptides <- c()
    
    for (i in 1:k){
      cluster_peptides <- names(clusters[clusters == i])
      
      if (length(cluster_peptides) > 1){
        p_values <- c()
        for (peptide in cluster_peptides){
          test_result <- tryCatch({
            wilcox.test(data[, peptide] ~ y, exact = FALSE)
          }, error = function(e){
            list(p.value = 1)
          })
          p_values <- c(p_values, test_result$p.value)
        }
        min_p_value_index <- which.min(p_values)
        selected_peptide <- cluster_peptides[min_p_value_index]
      } else {
        selected_peptide <- cluster_peptides[1]
      }
      selected_peptides <- c(selected_peptides, selected_peptide)
    }
    
    data_clust <- data[, selected_peptides, drop=FALSE]
    cat(sprintf("  Selected %d variables (one per cluster)\n", ncol(data_clust)))
    
    # Step 3: Bootstrap + Elastic Net selection (70% de la progression)
    incProgress(0, detail = sprintf("Bootstrap + Elastic Net (0/%d)...", n_bootstrap))
    cat(sprintf("Step 3: Bootstrap + Elastic Net selection (%d iterations)...\n", n_bootstrap))
    set.seed(123)
    selected_peptides_list <- list()
    
    progress_step <- 0.7 / n_bootstrap  # 70% du total pour le bootstrap
    
    for (b in 1:n_bootstrap) {
      if(b %% 50 == 0) {
        incProgress(progress_step * 50, 
                    detail = sprintf("Bootstrap: %d/%d (%.1f%%)", b, n_bootstrap, (b/n_bootstrap)*100))
        cat(sprintf("  Bootstrap iteration: %d/%d\n", b, n_bootstrap))
      }
      
      bootstrap_indices <- sample(1:nrow(data_clust), replace = TRUE)
      X_bootstrap <- data_clust[bootstrap_indices, , drop=FALSE]
      y_bootstrap <- y[bootstrap_indices]
      
      lasso_model <- tryCatch({
        cv.glmnet(as.matrix(X_bootstrap),
                  y_bootstrap,
                  family = "binomial",
                  alpha = alpha_enet)
      }, error = function(e){
        NULL
      })
      
      if(!is.null(lasso_model)){
        coef_lasso <- coef(lasso_model, s = "lambda.min")
        selected_peptides_iter <- rownames(coef_lasso)[which(coef_lasso != 0)][-1]
        selected_peptides_list[[b]] <- selected_peptides_iter
      }
    }
    
    # Step 4: Count selection frequencies
    incProgress(0.05, detail = "Frequency calculation...")
    peptide_selection_counts <- table(unlist(selected_peptides_list))
    data_of_frequencies <- sort(peptide_selection_counts, decreasing = TRUE)
    data_of_frequencies_df <- as.data.frame(data_of_frequencies)
    colnames(data_of_frequencies_df) <- c("Variable", "SelectionCount")
    data_of_frequencies_df$SelectionFrequency <- data_of_frequencies_df$SelectionCount / n_bootstrap
    
    # Step 5: Select final variables
    incProgress(0.05, detail = "Final selection...")
    threshold_count <- ceiling(n_bootstrap * min_selection_freq)
    final_selected_peptides <- names(peptide_selection_counts[peptide_selection_counts >= threshold_count])
    
    cat(sprintf("  Final selection: %d variables selected in >= %.0f%% of bootstraps (threshold: %d/%d)\n",
                length(final_selected_peptides), min_selection_freq * 100, threshold_count, n_bootstrap))
    
    incProgress(0, detail = "Done!")
    
    return(list(
      selected_peptides_per_cluster = selected_peptides,
      final_selected_peptides = final_selected_peptides,
      selection_frequencies = data_of_frequencies_df,
      n_clusters = k,
      n_bootstrap = n_bootstrap,
      alpha = alpha_enet,
      min_selection_freq = min_selection_freq
    ))
    
  }) # Fin withProgress
}

##########################
# Wrapper function for clustering + elasticnet to match other test methods
##########################

clustEnetSelection <- function(toto, n_clusters = 100, n_bootstrap = 500,
                               alpha_enet = 0.5, min_selection_freq = 0.5,
                               preprocess = TRUE, min_patients = 20){
  # Run varselClust
  clust_result <- varselClust(toto,
                              n_clusters = n_clusters,
                              n_bootstrap = n_bootstrap,
                              alpha_enet = alpha_enet,
                              min_selection_freq = min_selection_freq,
                              preprocess = preprocess,
                              min_patients = min_patients)
  
  selected_vars <- clust_result$final_selected_peptides
  
  # If no variables selected, return empty results
  if(length(selected_vars) == 0){
    return(list(
      results = data.frame(),
      selected_vars = character(0),
      all_coefficients = numeric(0),
      clust_result = clust_result,
      method = "clustEnet"
    ))
  }
  
  # Calculate statistics for selected variables 
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])
  
  # Get selection frequencies for selected variables
  freq_df <- clust_result$selection_frequencies
  freq_values <- freq_df$SelectionFrequency[match(selected_vars, freq_df$Variable)]
  
  # AUC for each selected variable
  auc_values <- sapply(selected_vars, function(var){
    pROC::auc(pROC::roc(group, x[, var], quiet=TRUE))
  })
  
  # Mean values by group
  mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
  mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
  
  # Fold change
  FC1o2 <- mlev1 / (mlev2 + 0.0001)
  logFC1o2 <- log2(abs(FC1o2))
  
  # Create results dataframe
  results <- data.frame(
    name = selected_vars,
    SelectionFrequency = freq_values,
    AUC = auc_values,
    FoldChange = FC1o2,
    logFoldChange = logFC1o2,
    mean_group1 = mlev1,
    mean_group2 = mlev2,
    stringsAsFactors = FALSE
  )
  
  # Sort by selection frequency
  results <- results[order(results$SelectionFrequency, decreasing=TRUE), ]
  
  # Return results
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_frequencies = clust_result$selection_frequencies,
    clust_result = clust_result,
    method = "clustEnet"
  ))
}

PlotPca = function(data, y, title = "PCA of selected peptides") {
  pca_result = prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  pca_data = data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Group = as.factor(y)
  )
  
  ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    #stat_ellipse(aes(fill = Group), geom = "polygon", alpha = 0.1, show.legend = FALSE) +
    labs(
      title = title, 
      x = paste0("PC1 (", var_explained[1], "% variance)"),
      y = paste0("PC2 (", var_explained[2], "% variance)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, face = 'bold'),
      axis.text.y = element_text(size = 10, face = 'bold'),
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10)
    )
}

volcanoplot<-function(logFC,pval,thresholdFC=0,thresholdpv=0.05,graph=T,maintitle="Volcano plot",completedata){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  
  threshold <- (as.numeric(abs(logFC) > thresholdFC &pval< thresholdpv ) +1)*2
  listgen<-data.frame("logFC"=logFC,"pval"=pval,"threshold"=threshold)
  if(!graph){return(completedata)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    #xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+
    annotate("text",x=Inf,y=Inf,label=paste(substring(colnames(completedata)[3],first=4)),size=6,vjust=2,hjust=1.5)
  
  g
} 
# 
# barplottest<-function(feature,logFC,levels,pval,mean1,mean2,thresholdpv=0.05,thresholdFC=1,graph=T,
#                       maintitle="Mean by group for differentially expressed variables"){
#   feature<-rep(feature,each=2)
#   group<-rep(c(levels[1],levels[2]),times=(length(feature)/2))
#   group<-factor(group,levels =c(levels[1],levels[2]))
#   pval2<-rep((pval< thresholdpv),each=2)
#   logFC2<-rep((abs(logFC)> thresholdFC),each=2) 
#   mean<-vector() 
#   mean[seq(from=1,to=length(feature),by = 2)]<-mean1
#   mean[seq(from=2,to=length(feature),by = 2)]<-mean2
#   data<-data.frame(feature,group,pval,logFC,mean,logFC2,pval2)
#   data<-data[order(data$pval),]
#   if(!graph){
#     data<-data[order(data[,1]),]
#     return(data[which((data$pval2==TRUE)& (data$logFC2==TRUE)),c(1,2,5)])}
#   else{
#     ggplot(data[which( ( data$pval2) & (data$logFC2) ),], aes(feature, mean,fill=group))+geom_bar(stat="identity", position="dodge")+ 
#       ggtitle(maintitle)+theme(plot.title=element_text( size=15))
#   }
# }


barplottest <- function(feature, logFC, levels, pval, means, 
                        thresholdpv = 0.05, thresholdFC = 1, 
                        graph = TRUE, 
                        maintitle = "Mean by group for differentially expressed variables",
                        max_features = 20,
                        order_by = "pval") {
  # feature: vecteur des noms de variables
  # logFC: vecteur des log fold changes ou effect sizes
  # levels: vecteur des noms des groupes/classes
  # pval: vecteur des p-values
  # means: matrice ou dataframe avec les moyennes (lignes = variables, colonnes = classes)
  # thresholdpv: seuil de p-value
  # thresholdFC: seuil de logFC/effect size
  # graph: TRUE pour retourner le graphique, FALSE pour retourner les données
  # maintitle: titre du graphique
  # max_features: nombre maximum de features à afficher
  # order_by: "pval" ou "effect_size" pour l'ordre des features
  
  # Vérifications
  if(length(feature) == 0){
    if(graph){
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No differentially expressed variables", 
                        size = 6) +
               theme_void())
    } else {
      return(data.frame())
    }
  }
  
  # Convertir means en matrice si ce n'est pas le cas
  if(is.data.frame(means)){
    means <- as.matrix(means)
  }
  
  # S'assurer que means a le bon nombre de colonnes
  if(ncol(means) != length(levels)){
    stop("Number of columns in 'means' must match length of 'levels'")
  }
  
  # Filtrer les variables significatives
  sig_idx <- which((pval < thresholdpv) & (abs(logFC) > thresholdFC))
  
  if(length(sig_idx) == 0){
    if(graph){
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No differentially expressed variables\nwith current thresholds", 
                        size = 6) +
               theme_void())
    } else {
      return(data.frame())
    }
  }
  
  # Sélectionner les variables significatives
  feature_sig <- feature[sig_idx]
  logFC_sig <- logFC[sig_idx]
  pval_sig <- pval[sig_idx]
  means_sig <- means[sig_idx, , drop = FALSE]
  
  # Trier selon order_by
  if(order_by == "pval"){
    order_idx <- order(pval_sig)
  } else if(order_by == "effect_size"){
    order_idx <- order(abs(logFC_sig), decreasing = TRUE)
  } else {
    order_idx <- 1:length(feature_sig)
  }
  
  feature_sig <- feature_sig[order_idx]
  logFC_sig <- logFC_sig[order_idx]
  pval_sig <- pval_sig[order_idx]
  means_sig <- means_sig[order_idx, , drop = FALSE]
  
  # Limiter le nombre de features affichées
  if(length(feature_sig) > max_features){
    feature_sig <- feature_sig[1:max_features]
    logFC_sig <- logFC_sig[1:max_features]
    pval_sig <- pval_sig[1:max_features]
    means_sig <- means_sig[1:max_features, , drop = FALSE]
    warning(paste("Only showing top", max_features, "features. Adjust max_features to show more."))
  }
  
  # Créer le dataframe long format pour ggplot
  n_features <- length(feature_sig)
  n_groups <- length(levels)
  
  # Répéter les features pour chaque groupe
  feature_rep <- rep(feature_sig, each = n_groups)
  
  # Répéter les groupes pour chaque feature
  group_rep <- rep(levels, times = n_features)
  group_rep <- factor(group_rep, levels = levels)
  
  # Répéter pval et logFC pour chaque groupe
  pval_rep <- rep(pval_sig, each = n_groups)
  logFC_rep <- rep(logFC_sig, each = n_groups)
  
  # Extraire les moyennes en format long
  mean_values <- as.vector(t(means_sig))
  
  # Créer le dataframe
  data_plot <- data.frame(
    feature = feature_rep,
    group = group_rep,
    pval = pval_rep,
    logFC = logFC_rep,
    mean = mean_values,
    stringsAsFactors = FALSE
  )
  
  # Ordonner les features par leur ordre d'apparition
  data_plot$feature <- factor(data_plot$feature, levels = unique(feature_sig))
  
  if(!graph){
    # Retourner les données au format large pour export
    result <- data.frame(
      feature = feature_sig,
      pval = pval_sig,
      logFC = logFC_sig
    )
    for(i in 1:n_groups){
      result[, paste0("mean_", levels[i])] <- means_sig[, i]
    }
    return(result)
  }
  
  # Créer le graphique
  p <- ggplot(data_plot, aes(x = feature, y = mean, fill = group)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = maintitle,
      x = "Variables",
      y = "Mean intensity",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Ajouter une palette de couleurs adaptée au nombre de classes
  if(n_groups <= 3){
    p <- p + scale_fill_brewer(palette = "Set1")
  } else if(n_groups <= 8){
    p <- p + scale_fill_brewer(palette = "Set2")
  } else {
    # Pour plus de 8 groupes, utiliser une palette continue
    p <- p + scale_fill_viridis_d(option = "turbo")
  }
  
  return(p)
}



errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

barplottestSF<-function(toto,graph=T){
  #toto: dataframe res from conditiontest function
  if(!graph){return(toto)}
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text(size=15))
}

SFtest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){ conditiontest<-data.frame(conditiontest,pvalnormlev1,pvalnormlev2,"samplenorm"=samplenorm)
                colnames(conditiontest)<-c("names",paste("pvalshapiro",levels(x)[1],sep=""),paste("pvalshapiro",levels(x)[2],sep = ""),"samplenorm")
  }
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
}

####

#' GridSearchCV wrapper for Random Forest using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune (ntree, mtry, nodesize, maxnodes)
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_rf_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)
  # library(randomForest)
  if(!requireNamespace("superml", quietly = TRUE)) {
    stop("Package 'superml' is required but not installed")
  }

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(100, 500, 1000),  # ntree in randomForest
      max_depth = c(5, 10, 15, 20, NULL),  # maxnodes (NULL = unlimited)
      min_samples_split = c(2, 5, 10),  # nodesize
      max_features = c("sqrt", "log2", floor(ncol(X)/3), floor(ncol(X)/2))  # mtry
    )
  }

  # Create trainer object
  rf_trainer <- superml::RFTrainer$new()

  # Create GridSearchCV object
  gst <-  superml::GridSearchCV$new(
    trainer = rf_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for XGBoost using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_xgb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(50, 100, 200),  # nrounds
      max_depth = c(3, 6, 9, 12),
      learning_rate = c(0.01, 0.05, 0.1, 0.3),  # eta
      gamma = c(0, 0.1, 0.5),
      subsample = c(0.6, 0.8, 1.0),
      colsample_bytree = c(0.6, 0.8, 1.0),
      min_child_weight = c(1, 3, 5)
    )
  }

  # Create trainer object
  xgb_trainer <- XGBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = xgb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Naive Bayes using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_nb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      laplace = c(0, 0.5, 1, 2, 5)  # Smoothing parameter
    )
  }

  # Create trainer object
  nb_trainer <- NBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = nb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for KNN using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
#' 

tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    max_k <- min(floor(sqrt(length(y))), 30)
    param_grid <- list(
      n_neighbors = seq(3, max_k, by = 2),  # k parameter, odd numbers only
      weights = c("uniform", "distance"),
      algorithm = c("brute", "kd_tree")
    )
  }

  # Create trainer object
  knn_trainer <- superml::KNNTrainer$new(type = "class")

  # Create GridSearchCV object
  gst <- superml::GridSearchCV$new(
    trainer = knn_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Logistic Regression (ElasticNet) using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      alpha = c(0, 0.25, 0.5, 0.75, 1.0),  # 0=Ridge, 1=Lasso, 0.5=ElasticNet
      lambda = c(0.001, 0.01, 0.1, 1.0, 10),
      penalty = c("elasticnet")
    )
  }

  # Create trainer object
  lm_trainer <- LMTrainer$new(family = "binomial")

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = lm_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

####

print_multiclass_performance <- function(predicted, actual, score_matrix = NULL, 
                                         set_name = "Training") {
  # Affiche un résumé complet des performances pour multi-classe
  
  cat("\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat(sprintf("  PERFORMANCE METRICS - %s Set\n", set_name))
  cat(paste(rep("=", 70), collapse=""), "\n\n")
  
  # Calculer les métriques
  metrics <- compute_multiclass_metrics(predicted, actual)
  
  # Afficher les métriques globales
  cat("Global Metrics (Macro-Average):\n")
  cat(sprintf("  Accuracy:      %.3f\n", metrics$accuracy))
  cat(sprintf("  Sensitivity:   %.3f\n", metrics$sensitivity))
  cat(sprintf("  Specificity:   %.3f\n", metrics$specificity))
  cat(sprintf("  Precision:     %.3f\n", metrics$precision_macro))
  cat(sprintf("  F1-Score:      %.3f\n", metrics$f1_score))
  
  # AUC si disponible
  if(!is.null(score_matrix)) {
    auc_results <- compute_multiclass_auc(actual, score_matrix)
    cat(sprintf("  AUC (macro):   %.3f\n", auc_results$auc_macro))
    cat(sprintf("  AUC (weighted): %.3f\n", auc_results$auc_weighted))
  }
  
  # Métriques par classe
  cat("\nPer-Class Metrics:\n")
  classes <- levels(actual)
  for(i in 1:length(classes)) {
    cat(sprintf("  %s: Precision=%.3f, Recall=%.3f, F1=%.3f", 
                classes[i],
                metrics$precision_per_class[i],
                metrics$recall_per_class[i],
                metrics$f1_per_class[i]))
    
    if(!is.null(score_matrix)) {
      auc_results <- compute_multiclass_auc(actual, score_matrix)
      cat(sprintf(", AUC=%.3f", auc_results$auc_per_class[i]))
    }
    cat("\n")
  }
  
  cat(paste(rep("=", 70), collapse=""), "\n\n")
}


compute_multiclass_auc <- function(actual, score_matrix) {
  # Calcule l'AUC pour la classification multi-classe
  # Utilise l'approche One-vs-All et retourne le macro-average
  #
  # Args:
  #   actual: vecteur de classes réelles (factor)
  #   score_matrix: matrice de probabilités (n_obs x n_classes)
  #
  # Returns:
  #   list avec auc_macro, auc_weighted, et auc_per_class
  
  n_classes <- length(levels(actual))
  classes <- levels(actual)
  auc_values <- numeric(n_classes)
  
  # Vérifier que score_matrix est une matrice
  if(!is.matrix(score_matrix)) {
    warning("score_matrix must be a matrix for multi-class AUC")
    return(NA)
  }
  
  # Calculer l'AUC pour chaque classe (One-vs-All)
  for(i in 1:n_classes) {
    # Créer un indicateur binaire pour cette classe
    binary_response <- ifelse(actual == classes[i], 1, 0)
    
    # Obtenir les probabilités pour cette classe
    class_probs <- score_matrix[, i]
    
    # Calculer l'AUC
    tryCatch({
      roc_obj <- pROC::roc(binary_response, class_probs, quiet = TRUE)
      auc_values[i] <- as.numeric(pROC::auc(roc_obj))
    }, error = function(e) {
      auc_values[i] <- NA
    })
  }
  
  # Macro-average (moyenne simple)
  auc_macro <- mean(auc_values, na.rm = TRUE)
  
  # Weighted average (pondéré par la taille des classes)
  class_sizes <- table(actual)
  weights <- class_sizes / sum(class_sizes)
  auc_weighted <- sum(auc_values * weights, na.rm = TRUE)
  
  return(list(
    auc_macro = round(auc_macro, 3),
    auc_weighted = round(auc_weighted, 3),
    auc_per_class = auc_values
  ))
}

# ==========================================================================
# BORUTA VARIABLE SELECTION
# ==========================================================================
boruta_selection <- function(toto, maxRuns = 100, pValue = 0.01, doTrace = 2) {
  set.seed(20011203)
  group <- toto[, 1]
  X <- toto[, -1]
  
  boruta_result <- Boruta(x = X, y = group, maxRuns = maxRuns, 
                          pValue = pValue, doTrace = doTrace)
  
  boruta_final <- TentativeRoughFix(boruta_result)
  
  confirmed_vars <- getSelectedAttributes(boruta_final, withTentative = FALSE)
  tentative_vars <- names(boruta_final$finalDecision[boruta_final$finalDecision == "Tentative"])
  rejected_vars <- names(boruta_final$finalDecision[boruta_final$finalDecision == "Rejected"])
  
  importance_df <- data.frame(
    name = names(boruta_final$finalDecision),
    decision = as.character(boruta_final$finalDecision),
    meanImp = colMeans(boruta_final$ImpHistory[, names(boruta_final$finalDecision)], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  importance_df <- importance_df[order(-importance_df$meanImp), ]
  
  shadow_max <- max(colMeans(boruta_final$ImpHistory[, grep("^shadow", colnames(boruta_final$ImpHistory))], na.rm = TRUE))
  
  selected_vars <- confirmed_vars
  
  if(length(selected_vars) == 0) {
    tabdiff <- data.frame()
  } else {
    indvar <- colnames(toto) %in% selected_vars
    indvar[1] <- TRUE
    tabdiff <- toto[, indvar]
  }
  
  return(list(
    tabdiff = tabdiff,
    boruta_result = boruta_final,
    importance = importance_df,
    selected_vars = selected_vars,
    confirmed = confirmed_vars,
    tentative = tentative_vars,
    rejected = rejected_vars,
    shadow_max = shadow_max,
    n_selected = length(selected_vars)
  ))
}

boruta_importance_plot <- function(boruta_result, graph = TRUE) {
  if(!graph) {
    imp_df <- data.frame(
      Variable = names(boruta_result$finalDecision),
      Decision = as.character(boruta_result$finalDecision),
      MeanImportance = colMeans(boruta_result$ImpHistory[, names(boruta_result$finalDecision)], na.rm = TRUE)
    )
    return(imp_df)
  }
  
  imp_history <- boruta_result$ImpHistory
  var_names <- names(boruta_result$finalDecision)
  decisions <- boruta_result$finalDecision
  
  imp_means <- colMeans(imp_history[, var_names, drop = FALSE], na.rm = TRUE)
  shadow_cols <- grep("^shadow", colnames(imp_history), value = TRUE)
  shadow_means <- colMeans(imp_history[, shadow_cols, drop = FALSE], na.rm = TRUE)
  
  df <- data.frame(
    Variable = c(var_names, shadow_cols),
    Importance = c(imp_means, shadow_means),
    Type = c(as.character(decisions), rep("Shadow", length(shadow_cols))),
    stringsAsFactors = FALSE
  )
  df$Type <- factor(df$Type, levels = c("Confirmed", "Tentative", "Rejected", "Shadow"))
  df <- df[order(df$Importance), ]
  df$Variable <- factor(df$Variable, levels = df$Variable)
  
  color_map <- c("Confirmed" = "#2ecc71", "Tentative" = "#f39c12", 
                  "Rejected" = "#e74c3c", "Shadow" = "#3498db")
  
  p <- ggplot(df, aes(x = Variable, y = Importance, fill = Type)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = color_map) +
    labs(title = "Boruta Variable Importance", x = "", y = "Mean Importance (Z-score)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10, face = "bold"),
      legend.title = element_text(size = 12, face = "bold")
    )
  
  return(p)
}

# ==========================================================================
# t-SNE AND UMAP VISUALIZATION
# ==========================================================================
plot_tsne <- function(data, y, perplexity = 30, title = "t-SNE Visualization") {
  set.seed(20011203)
  X <- as.matrix(data)
  
  perplexity <- min(perplexity, floor((nrow(X) - 1) / 3))
  if(perplexity < 1) perplexity <- 1
  
  tsne_result <- Rtsne(X, dims = 2, perplexity = perplexity, 
                        verbose = FALSE, max_iter = 1000, check_duplicates = FALSE)
  
  tsne_data <- data.frame(
    Dim1 = tsne_result$Y[, 1],
    Dim2 = tsne_result$Y[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  plot_ly(tsne_data, x = ~Dim1, y = ~Dim2, color = ~Group,
          type = 'scatter', mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = "t-SNE 1"),
      yaxis = list(title = "t-SNE 2"),
      legend = list(title = list(text = "Group"))
    )
}

plot_umap <- function(data, y, n_neighbors = 15, title = "UMAP Visualization") {
  set.seed(20011203)
  X <- as.matrix(data)
  
  n_neighbors <- min(n_neighbors, nrow(X) - 1)
  if(n_neighbors < 2) n_neighbors <- 2
  
  umap_config <- umap.defaults
  umap_config$n_neighbors <- n_neighbors
  umap_config$random_state <- 20011203
  
  umap_result <- umap(X, config = umap_config)
  
  umap_data <- data.frame(
    Dim1 = umap_result$layout[, 1],
    Dim2 = umap_result$layout[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  plot_ly(umap_data, x = ~Dim1, y = ~Dim2, color = ~Group,
          type = 'scatter', mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = "UMAP 1"),
      yaxis = list(title = "UMAP 2"),
      legend = list(title = list(text = "Group"))
    )
}

# ==========================================================================
# CLUSTERED HEATMAP OF TOP FEATURES
# ==========================================================================
plot_clustered_heatmap <- function(data, y, n_top = 30, title = "Clustered Heatmap - Top Features") {
  X <- as.matrix(data)
  
  if(ncol(X) > n_top) {
    var_importance <- apply(X, 2, function(col) {
      tryCatch({
        kruskal.test(col ~ as.factor(y))$p.value
      }, error = function(e) 1)
    })
    top_idx <- order(var_importance)[1:n_top]
    X <- X[, top_idx]
  }
  
  # X_scaled <- scale(X)
  X_scaled = X 
  
  annotation_row <- data.frame(Group = as.factor(y))
  rownames(annotation_row) <- rownames(data)[1:nrow(X)]
  
  n_groups <- length(unique(y))
  group_colors <- setNames(
    c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[1:n_groups],
    levels(as.factor(y))
  )
  ann_colors <- list(Group = group_colors)
  
  tryCatch({
    p = pheatmap(t(X_scaled),
             annotation_col = annotation_row,
             annotation_colors = ann_colors,
             clustering_method = "ward.D2",
             show_colnames = FALSE,
             main = title,
             fontsize_row = 8,
             color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100))
    
    gt <- p$gtable
    
    library(gridExtra)
    for (i in seq_along(gt$grobs)) {
      if ("row_names" %in% gt$grobs[[i]]$name) {
        gt$grobs[[i]]$gp <- gpar(fontface = "bold", fontsize = 10)
      }
    }
    
    grid.draw(gt)
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Heatmap error:", e$message), cex = 1.2, col = "red")
  })
}

# ==========================================================================
# CORRELATION NETWORK
# ==========================================================================
# plot_correlation_network <- function(data, cor_threshold = 0.6, title = "Correlation Network") {
#   X <- data
#   if(ncol(X) < 2) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 variables", size = 6) + theme_void())
#   }
#   
#   cor_matrix <- cor(X, use = "pairwise.complete.obs")
#   diag(cor_matrix) <- 0
#   
#   cor_matrix[abs(cor_matrix) < cor_threshold] <- 0
#   
#   if(sum(cor_matrix != 0) == 0) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
#                                 label = paste("No correlations above threshold", cor_threshold), size = 5) + theme_void())
#   }
#   
#   graph <- graph_from_adjacency_matrix(abs(cor_matrix), mode = "undirected", weighted = TRUE, diag = FALSE)
#   graph <- delete_vertices(graph, which(degree(graph) == 0))
#   
#   if(vcount(graph) == 0) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
#                                 label = "No connected variables", size = 5) + theme_void())
#   }
#   
#   tg <- as_tbl_graph(graph)
#   
#   p <- ggraph(tg, layout = "fr") +
#     geom_edge_link(aes(width = weight, alpha = weight), color = "steelblue") +
#     geom_node_point(size = 5, color = "#E74C3C") +
#     geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
#     scale_edge_width(range = c(0.5, 3)) +
#     scale_edge_alpha(range = c(0.3, 1)) +
#     labs(title = title, subtitle = paste("Correlation threshold:", cor_threshold)) +
#     theme_void() +
#     theme(
#       plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#       plot.subtitle = element_text(size = 11, hjust = 0.5),
#       legend.position = "none"
#     )
#   
#   return(p)
# }
plot_correlation_network <- function(data, cor_threshold = 0.6, title = "Correlation Network") {
  X <- data
  if(ncol(X) < 2) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 variables", size = 6) + theme_void())
  }
  
  cor_matrix <- cor(X, use = "pairwise.complete.obs")
  diag(cor_matrix) <- 0
  cor_matrix[abs(cor_matrix) < cor_threshold] <- 0
  
  if(sum(cor_matrix != 0) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                               label = paste("No correlations above threshold", cor_threshold), size = 5) + theme_void())
  }
  
  graph <- igraph::graph_from_adjacency_matrix(
    abs(cor_matrix), mode = "undirected", weighted = TRUE, diag = FALSE
  )
  
  # ✅ Qualification explicite pour éviter le conflit de namespace
  isolated <- which(igraph::degree(graph) == 0)
  if(length(isolated) > 0) {
    graph <- igraph::delete_vertices(graph, isolated)
  }
  
  if(igraph::vcount(graph) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                               label = "No connected variables", size = 5) + theme_void())
  }
  
  tg <- tidygraph::as_tbl_graph(graph)
  
  p <- ggraph(tg, layout = "fr") +
    geom_edge_link(aes(width = weight, alpha = weight), color = "steelblue") +
    geom_node_point(size = 5, color = "#E74C3C") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_alpha(range = c(0.3, 1)) +
    labs(title = title, subtitle = paste("Correlation threshold:", cor_threshold)) +
    theme_void() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "none"
    )
  
  return(p)
}

# ==========================================================================
# CALIBRATION PLOT
# ==========================================================================
plot_calibration <- function(actual, predicted_probs, n_bins = 10, title = "Calibration Plot") {
  if(is.matrix(predicted_probs) || is.data.frame(predicted_probs)) {
    n_classes <- ncol(predicted_probs)
    class_names <- if(!is.null(colnames(predicted_probs))) colnames(predicted_probs) else paste("Class", 1:n_classes)
    
    all_data <- data.frame()
    for(i in 1:n_classes) {
      binary_actual <- as.numeric(actual == levels(actual)[i])
      probs <- predicted_probs[, i]
      
      bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
      bin_data <- data.frame(probs = probs, actual = binary_actual, bin = bins)
      
      bin_summary <- bin_data %>%
        dplyr::group_by(bin) %>%
        dplyr::summarise(
          mean_predicted = mean(probs, na.rm = TRUE),
          mean_actual = mean(actual, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(Class = class_names[i])
      
      all_data <- rbind(all_data, bin_summary)
    }
    
    p <- ggplot(all_data, aes(x = mean_predicted, y = mean_actual, color = Class)) +
      geom_point(aes(size = count), alpha = 0.7) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_size_continuous(range = c(2, 8)) +
      labs(title = title, x = "Mean Predicted Probability", y = "Observed Proportion",
           size = "N samples") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10, face = "bold")
      ) +
      coord_fixed()
    
  } else {
    binary_actual <- as.numeric(actual == levels(actual)[2])
    probs <- predicted_probs
    bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
    bin_data <- data.frame(probs = probs, actual = binary_actual, bin = bins)
    
    bin_summary <- bin_data %>%
      group_by(bin) %>%
      summarise(
        mean_predicted = mean(probs, na.rm = TRUE),
        mean_actual = mean(actual, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(bin_summary, aes(x = mean_predicted, y = mean_actual)) +
      geom_point(aes(size = count), color = "#3498DB", alpha = 0.7) +
      geom_line(color = "#3498DB") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_size_continuous(range = c(2, 8)) +
      labs(title = title, x = "Mean Predicted Probability", y = "Observed Proportion",
           size = "N samples") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      coord_fixed()
  }
  
  return(p)
}

# ==========================================================================
# MODEL COMPARISON: RUN ALL MODELS
# ==========================================================================
run_all_models <- function(learningmodel, validation, transformdataparameters,
                           datastructuresfeatures, learningselect, models_to_run = NULL) {
  
  if(is.null(models_to_run)) {
    models_to_run <- c("randomforest", "svm", "elasticnet", "xgboost", "naivebayes", "knn")
  }
  
  results_list <- list()
  metrics_summary <- data.frame()
  
  for(model_type in models_to_run) {
    cat(sprintf("\n=== Training: %s ===\n", model_type))
    
    # modelparameters <- list(
    #   modeltype = model_type,
    #   invers = FALSE,
    #   thresholdmodel = 0.5,
    #   fs = FALSE,
    #   adjustval = !is.null(validation),
    #   autotunerf = TRUE,
    #   autotunesvm = TRUE,
    #   autotunexgb = TRUE,
    #   autotunelgb = TRUE,
    #   autotuneknn = TRUE,
    #   ntree = 500,
    #   k_neighbors = 5
    # )
    
    modelparameters <- list(
      modeltype      = model_type,
      invers         = FALSE,
      thresholdmodel = threshold,
      fs             = FALSE,
      adjustval      = !is.null(validation),
      
      # ── Activation du GridSearchCV pour tous les modèles qui le supportent ──
      use_gridsearch = TRUE,
      
      # ── Random Forest ────────────────────────────────────────────────────────
      autotunerf       = TRUE,
      rf_grid_ntree    = c(100, 300, 500, 1000),
      rf_grid_mtry     = c("sqrt", "log2"),
      rf_grid_nodesize = c(1, 3, 5),
      ntree            = 500,
      
      # ── SVM ──────────────────────────────────────────────────────────────────
      autotunesvm     = TRUE,
      svm_scoring     = "accuracy",
      svm_gamma_range = 10^seq(-6, 2, by = 1),
      svm_cost_range  = 10^seq(-3, 3, by = 1),
      
      # ── XGBoost ──────────────────────────────────────────────────────────────
      autotunexgb          = TRUE,
      use_gridsearch       = FALSE,
      xgb_grid_nrounds     = c(50, 100, 200, 300),
      xgb_grid_maxdepth    = c(3, 6, 9),
      xgb_grid_eta         = c(0.01, 0.05, 0.1, 0.3),
      xgb_grid_gamma       = c(0, 0.1, 0.5),
      xgb_grid_subsample   = c(0.8, 1.0),
      
      # ── LightGBM ─────────────────────────────────────────────────────────────
      autotunelgb = TRUE,
      
      # ── KNN ──────────────────────────────────────────────────────────────────
      autotuneknn = TRUE,
      k_neighbors = 5,
      
      # ── Naive Bayes ──────────────────────────────────────────────────────────
      nb_grid_laplace = c(0, 0.5, 1, 2, 5)
    )
    
    out <- tryCatch(
      modelfunction(learningmodel = learningmodel,
                    validation = validation,
                    modelparameters = modelparameters,
                    transformdataparameters = transformdataparameters,
                    datastructuresfeatures = datastructuresfeatures,
                    learningselect = learningselect),
      error = function(e) {
        cat(sprintf("Error training %s: %s\n", model_type, e$message))
        return(NULL)
      }
    )
    
    if(!is.null(out)) {
      results_list[[model_type]] <- out
      
      train_metrics <- compute_multiclass_metrics(
        out$datalearningmodel$reslearningmodel$predictclasslearning,
        out$datalearningmodel$reslearningmodel$classlearning
      )
      
      train_scores <- out$datalearningmodel$reslearningmodel[, 2:(ncol(out$datalearningmodel$reslearningmodel) - 1)]
      train_auc <- tryCatch({
        auc_res <- compute_multiclass_auc(
          out$datalearningmodel$reslearningmodel$classlearning,
          as.matrix(train_scores)
        )
        auc_res$auc_macro
      }, error = function(e) NA)
      
      row_data <- data.frame(
        Model = model_type,
        Train_Accuracy = train_metrics$accuracy,
        Train_Sensitivity = train_metrics$sensitivity,
        Train_Specificity = train_metrics$specificity,
        Train_AUC = train_auc,
        stringsAsFactors = FALSE
      )
      
      if(!is.null(validation) && !is.null(out$datavalidationmodel)) {
        val_metrics <- compute_multiclass_metrics(
          out$datavalidationmodel$resvalidationmodel$predictclassval,
          out$datavalidationmodel$resvalidationmodel$classval
        )
        
        val_scores <- out$datavalidationmodel$resvalidationmodel[, 2:(ncol(out$datavalidationmodel$resvalidationmodel) - 1)]
        val_auc <- tryCatch({
          auc_res <- compute_multiclass_auc(
            out$datavalidationmodel$resvalidationmodel$classval,
            as.matrix(val_scores)
          )
          auc_res$auc_macro
        }, error = function(e) NA)
        
        row_data$Val_Accuracy <- val_metrics$accuracy
        row_data$Val_Sensitivity <- val_metrics$sensitivity
        row_data$Val_Specificity <- val_metrics$specificity
        row_data$Val_AUC <- val_auc
      }
      
      metrics_summary <- rbind(metrics_summary, row_data)
    }
  }
  
  return(list(
    results = results_list,
    metrics = metrics_summary
  ))
}

# ==========================================================================
# DELONG TEST FOR AUC COMPARISON
# ==========================================================================#
# Fonctionnement : pour chaque paire de modèles, les K p-values DeLong (une par classe One-vs-All) 
#sont combinées via la statistique de Fisher "−2 Σ log(pᵢ)", qui suit un χ²(2K).
# Cela donne une p-value globale rigoureuse tenant compte de toutes les classes. 
#Une protection contre log(0) est aussi ajoutée via .Machine$double.xmin.
delong_compare_models <- function(comparison_results) {
  models <- names(comparison_results$results)
  n_models <- length(models)
  
  if(n_models < 2) {
    return(list(
      global = data.frame(message = "Need at least 2 models to compare"),
      corrected = data.frame(message = "Need at least 2 models to compare"),
      per_class = data.frame(message = "Need at least 2 models to compare")
    ))
  }
  
  pval_matrix <- matrix(NA, nrow = n_models, ncol = n_models)
  rownames(pval_matrix) <- models
  colnames(pval_matrix) <- models
  
  # Collect per-class detail rows
  per_class_rows <- list()
  
  for(i in 1:(n_models - 1)) {
    for(j in (i + 1):n_models) {
      tryCatch({
        res_i <- comparison_results$results[[models[i]]]
        res_j <- comparison_results$results[[models[j]]]
        
        # Try learning data first, then validation
        if(!is.null(res_i$datalearningmodel) && !is.null(res_j$datalearningmodel)) {
          actual <- res_i$datalearningmodel$reslearningmodel$classlearning
          scores_i <- res_i$datalearningmodel$reslearningmodel[, 2:(ncol(res_i$datalearningmodel$reslearningmodel) - 1)]
          scores_j <- res_j$datalearningmodel$reslearningmodel[, 2:(ncol(res_j$datalearningmodel$reslearningmodel) - 1)]
        } else if(!is.null(res_i$datavalidationmodel) && !is.null(res_j$datavalidationmodel)) {
          actual <- res_i$datavalidationmodel$resvalidationmodel$classval
          scores_i <- res_i$datavalidationmodel$resvalidationmodel[, 2:(ncol(res_i$datavalidationmodel$resvalidationmodel) - 1)]
          scores_j <- res_j$datavalidationmodel$resvalidationmodel[, 2:(ncol(res_j$datavalidationmodel$resvalidationmodel) - 1)]
        } else {
          next
        }
        
        if(!is.matrix(scores_i)) scores_i <- as.matrix(scores_i)
        if(!is.matrix(scores_j)) scores_j <- as.matrix(scores_j)
        
        n_classes <- length(levels(actual))
        pair_label <- paste(models[i], "vs", models[j])
        
        if(ncol(scores_i) == 1) {
          # Binary case
          roc_i <- pROC::roc(actual, scores_i[, 1], quiet = TRUE)
          roc_j <- pROC::roc(actual, scores_j[, 1], quiet = TRUE)
          test_result <- pROC::roc.test(roc_i, roc_j, method = "delong")
          pval_matrix[i, j] <- round(test_result$p.value, 4)
          pval_matrix[j, i] <- round(test_result$p.value, 4)
          per_class_rows[[length(per_class_rows) + 1]] <- data.frame(
            Pair = pair_label,
            Class = "binary",
            AUC_Model1 = round(as.numeric(pROC::auc(roc_i)), 4),
            AUC_Model2 = round(as.numeric(pROC::auc(roc_j)), 4),
            p_value = round(test_result$p.value, 4),
            stringsAsFactors = FALSE
          )
        } else {
          # Multi-class: DeLong per class (One-vs-All), then combine p-values
          classes <- levels(actual)
          pvals_per_class <- numeric(0)
          names_per_class <- character(0)
          
          for(k in 1:n_classes) {
            binary_response <- ifelse(actual == classes[k], 1, 0)
            roc_i_k <- tryCatch(pROC::roc(binary_response, scores_i[, k], quiet = TRUE), error = function(e) NULL)
            roc_j_k <- tryCatch(pROC::roc(binary_response, scores_j[, k], quiet = TRUE), error = function(e) NULL)
            
            auc_i_val <- if(!is.null(roc_i_k)) round(as.numeric(pROC::auc(roc_i_k)), 4) else NA
            auc_j_val <- if(!is.null(roc_j_k)) round(as.numeric(pROC::auc(roc_j_k)), 4) else NA
            
            if(!is.null(roc_i_k) && !is.null(roc_j_k)) {
              # Skip if both AUCs == 1 (DeLong is uninformative)
              if(isTRUE(auc_i_val == 1) && isTRUE(auc_j_val == 1)) {
                per_class_rows[[length(per_class_rows) + 1]] <- data.frame(
                  Pair = pair_label, Class = classes[k],
                  AUC_Model1 = auc_i_val, AUC_Model2 = auc_j_val,
                  p_value = NA_real_, Note = "Both AUC=1, skipped",
                  stringsAsFactors = FALSE
                )
                next
              }
              
              test_k <- tryCatch({
                pROC::roc.test(roc_i_k, roc_j_k, method = "delong")
              }, error = function(e) NULL)
              if(!is.null(test_k)) {
                pv <- as.numeric(test_k$p.value)[1]
                if(!is.na(pv)) {
                  pvals_per_class <- c(pvals_per_class, pv)
                  names_per_class <- c(names_per_class, classes[k])
                }
                per_class_rows[[length(per_class_rows) + 1]] <- data.frame(
                  Pair = pair_label, Class = classes[k],
                  AUC_Model1 = auc_i_val, AUC_Model2 = auc_j_val,
                  p_value = if(!is.na(pv)) round(pv, 4) else NA_real_,
                  Note = "",
                  stringsAsFactors = FALSE
                )
              } else {
                per_class_rows[[length(per_class_rows) + 1]] <- data.frame(
                  Pair = pair_label, Class = classes[k],
                  AUC_Model1 = auc_i_val, AUC_Model2 = auc_j_val,
                  p_value = NA_real_, Note = "DeLong test failed",
                  stringsAsFactors = FALSE
                )
              }
            }
          }
          
          pvals_per_class <- as.numeric(pvals_per_class)
          pvals_per_class <- pvals_per_class[!is.na(pvals_per_class)]
          
          if(length(pvals_per_class) > 0) {
            # Fisher's method: combine p-values via -2 * sum(log(p)) ~ chi²(2K)
            pvals_safe <- pvals_per_class
            pvals_safe[pvals_safe == 0] <- .Machine$double.xmin
            fisher_stat <- -2 * sum(log(pvals_safe))
            combined_pval <- pchisq(fisher_stat, df = 2 * length(pvals_safe), lower.tail = FALSE)
            pval_matrix[i, j] <- round(combined_pval, 4)
            pval_matrix[j, i] <- round(combined_pval, 4)
          } else {
            # All classes had AUC==1 for both models: no meaningful difference
            pval_matrix[i, j] <- 1
            pval_matrix[j, i] <- 1
          }
        }
      }, error = function(e) {
        cat(sprintf("DeLong test error (%s vs %s): %s\n", models[i], models[j], e$message))
      })
    }
  }
  
  # --- Global p-value matrix (Fisher combined) ---
  global_df <- as.data.frame(pval_matrix)
  for(k in 1:n_models) global_df[k, k] <- "-"
  
  # --- FDR-corrected p-value matrix (Benjamini-Hochberg) ---
  upper_idx <- which(upper.tri(pval_matrix))
  raw_pvals <- pval_matrix[upper_idx]
  adjusted_pvals <- p.adjust(raw_pvals, method = "BH")
  corrected_matrix <- pval_matrix
  corrected_matrix[upper_idx] <- round(adjusted_pvals, 4)
  # Mirror to lower triangle
  for(i in 1:(n_models - 1)) {
    for(j in (i + 1):n_models) {
      corrected_matrix[j, i] <- corrected_matrix[i, j]
    }
  }
  corrected_df <- as.data.frame(corrected_matrix)
  for(k in 1:n_models) corrected_df[k, k] <- "-"
  
  # --- Per-class detail table ---
  if(length(per_class_rows) > 0) {
    per_class_df <- do.call(rbind, per_class_rows)
    # Add Note column for binary case if missing
    if(!"Note" %in% colnames(per_class_df)) per_class_df$Note <- ""
  } else {
    per_class_df <- data.frame(
      Pair = character(0), Class = character(0),
      AUC_Model1 = numeric(0), AUC_Model2 = numeric(0),
      p_value = numeric(0), Note = character(0)
    )
  }
  
  return(list(
    global = global_df,
    corrected = corrected_df,
    per_class = per_class_df
  ))
}

# ==========================================================================
# RADAR/SPIDER PLOT FOR MODEL COMPARISON
# ==========================================================================
plot_radar_comparison <- function(metrics_summary, type = "validation") {
  if(nrow(metrics_summary) < 1) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No models to compare", size = 6) + theme_void())
  }
  
  if(type == "validation" && "Val_AUC" %in% colnames(metrics_summary)) {
    radar_data <- metrics_summary %>%
      dplyr::select(Model, Val_Accuracy, Val_Sensitivity, Val_Specificity, Val_AUC) %>%
      dplyr::rename(Accuracy = Val_Accuracy, Sensitivity = Val_Sensitivity,
             Specificity = Val_Specificity, AUC = Val_AUC)
  } else {
    radar_data <- metrics_summary %>%
      dplyr::select(Model, Train_Accuracy, Train_Sensitivity, Train_Specificity, Train_AUC) %>%
      dplyr::rename(Accuracy = Train_Accuracy, Sensitivity = Train_Sensitivity,
             Specificity = Train_Specificity, AUC = Train_AUC)
  }
  
  models <- radar_data$Model
  radar_values <- radar_data[, -1]
  radar_values[is.na(radar_values)] <- 0
  
  radar_df <- rbind(rep(1, ncol(radar_values)), rep(0, ncol(radar_values)), radar_values)
  rownames(radar_df) <- c("Max", "Min", models)
  
  n_models <- length(models)
  colors_fill <- c("#E74C3C44", "#3498DB44", "#2ECC7144", "#F39C1244", "#9B59B644", "#1ABC9C44")[1:n_models]
  colors_line <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C")[1:n_models]
  
  par(mar = c(1, 1, 2, 1))
  radarchart(radar_df,
             axistype = 1,
             pcol = colors_line,
             pfcol = adjustcolor(topo.colors(n_models), 0.15),
               #colors_fill,
             plwd = 2,
             plty = 1,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey40",
             vlcex = 1.1,
             title = paste("Model Comparison -", tools::toTitleCase(type)))
  
  legend("topright", legend = models, col = colors_line, 
         lty = 1, lwd = 2, bty = "n", cex = 1.5)
}

# ==========================================================================
# SHAP VALUES (via iml package)
# ==========================================================================
compute_shap_values <- function(model, learningmodel, modeltype, n_samples = 50) {
  X <- learningmodel[, -1]
  y <- learningmodel[, 1]
  
  predict_fn <- function(model, newdata) {
    newdata <- as.data.frame(newdata)
    for(i in 1:ncol(newdata)) {
      newdata[, i] <- as.numeric(as.character(newdata[, i]))
    }
    
    if(modeltype == "randomforest") {
      preds <- randomForest:::predict.randomForest(model, newdata, type = "prob")
      return(as.data.frame(preds))
    } else if(modeltype == "svm") {
      preds <- e1071:::predict.svm(model, newdata, probability = TRUE)
      probs <- attr(preds, "probabilities")
      if(!is.null(probs)) return(as.data.frame(probs))
      return(data.frame(pred = as.numeric(preds)))
    } else if(modeltype == "elasticnet") {
      newdata_matrix <- as.matrix(newdata)
      preds <- glmnet::predict.glmnet(model$glmnet_model, newdata_matrix, s = model$lambda, type = "response")
      if(length(dim(preds)) == 3) preds <- preds[, , 1]
      return(as.data.frame(preds))
    } else if(modeltype == "xgboost") {
      dmat <- xgb.DMatrix(data = as.matrix(newdata))
      preds <- xgboost:::predict.xgb.Booster(model, dmat, reshape = TRUE)
      return(as.data.frame(preds))
    } else if(modeltype == "naivebayes") {
      preds <- e1071:::predict.naiveBayes(model, newdata, type = "raw")
      return(as.data.frame(preds))
    } else if(modeltype == "knn") {
      return(data.frame(pred = rep(0.5, nrow(newdata))))
    }
  }
  
  predictor <- Predictor$new(
    model = model,
    data = X,
    y = y,
    predict.function = function(model, newdata) predict_fn(model, newdata)
  )
  
  sample_idx <- sample(1:nrow(X), min(n_samples, nrow(X)))
  
  shapley_results <- list()
  for(idx in sample_idx) {
    tryCatch({
      shap <- Shapley$new(predictor, x.interest = X[idx, , drop = FALSE])
      shapley_results[[as.character(idx)]] <- shap$results
    }, error = function(e) {
      cat(sprintf("SHAP error for sample %d: %s\n", idx, e$message))
    })
  }
  
  if(length(shapley_results) == 0) return(NULL)
  
  all_shap <- do.call(rbind, shapley_results)
  
  shap_importance <- all_shap %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_abs_shap = mean(abs(phi), na.rm = TRUE), .groups = 'drop') %>%
    dplyr::arrange(desc(mean_abs_shap))
  
  return(list(
    shap_details = all_shap,
    shap_importance = shap_importance,
    predictor = predictor
  ))
}

plot_shap_importance <- function(shap_result, n_top = 20, title = "SHAP Feature Importance") {
  if(is.null(shap_result)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "SHAP computation failed", size = 6) + theme_void())
  }
  
    df <- head(shap_result$shap_importance, n_top)
  df$feature <- factor(df$feature, levels = rev(df$feature))
  
  p <- ggplot(df, aes(x = feature, y = mean_abs_shap, fill = mean_abs_shap)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
    labs(title = title, x = "", y = "Mean |SHAP value|") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
  
  return(p)
}

# ==========================================================================
# PARTIAL DEPENDENCE PLOTS (PDP)
# ==========================================================================
plot_pdp <- function(model, learningmodel, modeltype, feature_name, title = NULL) {
  X <- learningmodel[, -1]
  y <- learningmodel[, 1]
  
  if(!(feature_name %in% colnames(X))) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                label = paste("Feature not found:", feature_name), size = 5) + theme_void())
  }
  
  predict_fn <- function(model, newdata) {
    newdata <- as.data.frame(newdata)
    for(i in 1:ncol(newdata)) {
      newdata[, i] <- as.numeric(as.character(newdata[, i]))
    }
    
    if(modeltype == "randomforest") {
      preds <- randomForest:::predict.randomForest(model, newdata, type = "prob")
      return(preds[, 1])
    } else if(modeltype == "svm") {
      preds <- e1071:::predict.svm(model, newdata, probability = TRUE)
      probs <- attr(preds, "probabilities")
      if(!is.null(probs)) return(probs[, 1])
      return(as.numeric(preds))
    } else if(modeltype == "xgboost") {
      dmat <- xgb.DMatrix(data = as.matrix(newdata))
      preds <- xgboost:::predict.xgb.Booster(model, dmat)
      return(preds)
    } else if(modeltype == "naivebayes") {
      preds <-  e1071:::predict.naiveBayes(model, newdata, type = "raw")
      return(preds[, 1])
    } else {
      return(rep(0.5, nrow(newdata)))
    }
  }
  
  predictor <- Predictor$new(
    model = model,
    data = X,
    y = y,
    predict.function = function(model, newdata) predict_fn(model, newdata)
  )
  
  pdp_result <- FeatureEffect$new(predictor, feature = feature_name, method = "pdp")
  
  if(is.null(title)) title <- paste("Partial Dependence Plot -", feature_name)
  
  p <- plot(pdp_result) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold")
    )
  
  return(p)
}

# ==========================================================================
# LIME LOCAL EXPLANATIONS
# ==========================================================================

# Wrapper class for LIME compatibility
model_type.lime_wrapper <- function(x, ...) "classification"

predict_model.lime_wrapper <- function(x, newdata, type = "raw", ...) {
  newdata <- as.data.frame(newdata)
  for(i in 1:ncol(newdata)) {
    newdata[, i] <- as.numeric(as.character(newdata[, i]))
  }
  
  mt <- x$modeltype
  m  <- x$raw_model
  yt <- x$y_levels
  
  if(mt == "randomforest") {
    preds <- randomForest:::predict.randomForest(m, newdata, type = "prob")
    return(as.data.frame(preds))
  } else if(mt == "svm") {
    preds <- e1071:::predict.svm(m, newdata, probability = TRUE)
    probs <- attr(preds, "probabilities")
    if(!is.null(probs)) {
      probs <- as.data.frame(probs)
      probs <- probs[, yt, drop = FALSE]
      return(probs)
    }
    return(data.frame(pred = as.numeric(preds)))
  } else if(mt == "elasticnet") {
    x_mat <- as.matrix(newdata)
    if(inherits(m$glmnet_model, "cv.glmnet")) {
      preds <- glmnet:::predict.cv.glmnet(m$glmnet_model, newx = x_mat,
                                            s = m$lambda, type = "response")
    } else {
      preds <- glmnet::predict.glmnet(m$glmnet_model, newx = x_mat,
                                       s = m$lambda, type = "response")
    }
    if(length(dim(preds)) == 3) preds <- preds[, , 1]
    preds <- as.data.frame(preds)
    if(ncol(preds) == 1) {
      preds <- data.frame(p1 = 1 - preds[, 1], p2 = preds[, 1])
      colnames(preds) <- yt
    }
    return(preds)
  } else if(mt == "xgboost") {
    dmat <- xgb.DMatrix(data = as.matrix(newdata))
    preds <- xgboost:::predict.xgb.Booster(m, dmat, reshape = TRUE)
    preds_df <- as.data.frame(preds)
    if(ncol(preds_df) == length(yt)) colnames(preds_df) <- yt
    return(preds_df)
  } else if(mt == "naivebayes") {
    preds <- e1071:::predict.naiveBayes(m, newdata, type = "raw")
    return(as.data.frame(preds))
  } else {
    n_classes <- length(yt)
    return(as.data.frame(matrix(1/n_classes, nrow = nrow(newdata), ncol = n_classes,
                                dimnames = list(NULL, yt))))
  }
}

# lime function 
explain_lime <- function(model, learningmodel, modeltype, sample_indices = 1:3, n_features = 10) {
  X_train <- learningmodel[, -1]
  y_train <- learningmodel[, 1]
  
  for(i in 1:ncol(X_train)) {
    X_train[, i] <- as.numeric(as.character(X_train[, i]))
  }
  
  # Create wrapper with lime_wrapper class
  wrapper <- list(raw_model = model, modeltype = modeltype, y_levels = levels(y_train))
  class(wrapper) <- "lime_wrapper"
  
  explainer <- tryCatch({
    lime::lime(X_train, wrapper, bin_continuous = TRUE)
  }, error = function(e) {
    cat(sprintf("LIME explainer error: %s\n", e$message))
    return(NULL)
  })
  
  if(is.null(explainer)) return(NULL)
  
  sample_idx <- sample_indices[sample_indices <= nrow(X_train)]
  if(length(sample_idx) == 0) sample_idx <- 1
  
  explanations <- tryCatch({
    lime::explain(X_train[sample_idx, , drop = FALSE], explainer, 
                  n_labels = min(length(levels(y_train)), 3),
                  n_features = n_features,
                  n_permutations = 1000)
  }, error = function(e) {
    cat(sprintf("LIME explanation error: %s\n", e$message))
    return(NULL)
  })
  
  return(explanations)
}

plot_lime_explanation <- function(lime_result, title = "LIME Explanation") {
  if(is.null(lime_result)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                label = "LIME explanation not available", size = 6) + theme_void())
  }
  
  p <- plot_features(lime_result) +
    labs(title = title) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 15 , face =  "bold"),
      axis.text.y = element_text(size = 15 , face = "bold"),
      axis.text.x  = element_text(size = 12, face = "bold"),
      legend.title = element_text(size  = 15 , face = "bold")
    )
  
  return(p)
}

tune_svm_stratified <- function(data,
                                gamma_range = 10^(-5:2),
                                cost_range  = 10^(-3:2),
                                kernel      = "radial",
                                n_folds     = 5,
                                scoring     = "accuracy",
                                seed        = 20011203) {
  set.seed(seed)
  scoring <- match.arg(scoring, c("auc", "accuracy"))
  
  y <- as.factor(data[, 1])
  levels_y <- levels(y)
  n_classes <- nlevels(y)
  
  # ── Folds stratifiés ──────────────────────────────────────────────────────
  folds <- caret::createFolds(y, k = n_folds, list = TRUE, returnTrain = TRUE)
  
  grid <- expand.grid(gamma = gamma_range, cost = cost_range)
  
  grid$mean_score <- sapply(seq_len(nrow(grid)), function(i) {
    g <- grid$gamma[i]
    c <- grid$cost[i]
    
    fold_scores <- sapply(folds, function(train_idx) {
      train <- data[ train_idx, ]
      test  <- data[-train_idx, ]
      
      y_train <- as.factor(train[, 1])
      y_test  <- as.factor(test[, 1])
      
      # ── Entraînement ────────────────────────────────────────────────────
      model <- tryCatch(
        e1071::svm(
          x            = train[, -1],
          y            = y_train,
          kernel       = kernel,
          gamma        = g,
          cost         = c,
          type         = "C-classification",
          probability  = (scoring == "auc")   # nécessaire pour l'AUC
        ),
        error = function(e) NULL
      )
      if (is.null(model)) return(NA)
      
      # ── Scoring ─────────────────────────────────────────────────────────
      if (scoring == "accuracy") {
        
        preds <- e1071:::predict.svm(model, test[, -1])
        mean(preds == y_test)
        
      } else {
        # AUC multiclasse : moyenne One-vs-Rest (macro-average)
        # Requires: pROC
        prob_mat <- attr(
          e1071:::predict.svm(model, test[, -1], probability = TRUE),
          "probabilities"
        )
        
        # S'assurer que toutes les classes sont présentes dans prob_mat
        missing_cls <- setdiff(levels_y, colnames(prob_mat))
        if (length(missing_cls) > 0) {
          pad <- matrix(0, nrow = nrow(prob_mat), ncol = length(missing_cls),
                        dimnames = list(NULL, missing_cls))
          prob_mat <- cbind(prob_mat, pad)
        }
        prob_mat <- prob_mat[, levels_y, drop = FALSE]   # ordre cohérent
        
        auc_per_class <- sapply(levels_y, function(cls) {
          true_bin <- as.integer(y_test == cls)
          if (sum(true_bin) == 0 || sum(true_bin) == length(true_bin))
            return(NA)          # classe absente du fold → on ignore
          scores <- prob_mat[, cls]
          as.numeric(pROC::auc(
            pROC::roc(true_bin, scores, quiet = TRUE,
                      levels = c(0, 1), direction = "<")
          ))
        })
        mean(auc_per_class, na.rm = TRUE)   # macro-AUC OvR
      }
    })
    
    mean(fold_scores, na.rm = TRUE)
  })
  
  # ── Meilleure combinaison ────────────────────────────────────────────────
  best_idx <- which.max(grid$mean_score)
  cat(sprintf(
    "Best SVM params → gamma: %g | cost: %g | %s: %.4f\n",
    grid$gamma[best_idx], grid$cost[best_idx],
    scoring, grid$mean_score[best_idx]
  ))
  
  list(
    best.parameters = list(gamma = grid$gamma[best_idx],
                           cost  = grid$cost[best_idx]),
    best.score      = grid$mean_score[best_idx],
    scoring         = scoring,
    n_classes       = n_classes,
    class_levels    = levels_y,
    all.results     = grid
  )
}

accuracy_error_fun <- function(true, pred) {
  
  if (is.matrix(pred)) {
    predicted_class <- colnames(pred)[apply(pred, 1, which.max)]
  } else {
    predicted_class <- as.character(pred)
  }
  accuracy <- mean(predicted_class == as.character(true))
  return(1 - accuracy)  #  minimise function
}
# Fonction custom AUC pour tune.svm 
auc_error_fun <- function(true, pred) {
  if (is.matrix(pred)) {
    scores <- pred[, 2]
  } else {
    scores <- as.numeric(pred)
  }
  true_bin <- as.integer(true == levels(true)[2])
  r        <- rank(scores)
  n_pos    <- sum(true_bin)
  n_neg    <- length(true_bin) - n_pos
  if (n_pos == 0 || n_neg == 0) return(0.5)
  auc <- (sum(r[true_bin == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(1 - auc)  # tune minimise 
}


create_stratified_folds <- function(y, k = 5, seed = 20011203) {
  set.seed(seed)
  if (is.factor(y)) {
    classes <- levels(y)
  } else {
    classes <- unique(y)
  }
  fold_ids <- rep(0L, length(y))
  for (cl in classes) {
    idx <- which(y == cl)
    idx <- sample(idx)
    fold_ids[idx] <- rep(1:k, length.out = length(idx))
  }
  folds <- lapply(1:k, function(i) which(fold_ids == i))
  names(folds) <- paste0("Fold", 1:k)
  return(folds)
}


modelfunction <- function(learningmodel, validation, modelparameters, 
                          transformdataparameters, datastructuresfeatures, 
                          learningselect = NULL) {
  if(modelparameters$modeltype!="nomodel"){ 
    # colnames(learningmodel)[1]<-"group"
    # Définir les groupes/niveaux
    lev <- levels(learningmodel[,1])
    # groups <- c("positif" = lev[1], "negatif" = lev[2])
    # lev <- groups[c("positif","negatif")]
    
    # Détecter si classification binaire ou multi-classe
    n_classes <- length(levels(learningmodel[,1]))
    is_binary <- (n_classes == 2)
    
    if(is_binary) {
      # Définir positif/negatif pour la classification binaire
      lev_positif <- lev["positif"]
      lev_negatif <- lev["negatif"]
      cat("=== Classification binaire détectée ===\n")
      cat("  Positif:", lev_positif, "\n")
      cat("  Négatif:", lev_negatif, "\n")
    } else {
      cat("=== Classification multi-classe détectée ===\n")
      cat("  Nombre de classes:", n_classes, "\n")
      cat("  Classes:", paste(levels(learningmodel[,1]), collapse = ", "), "\n")
      # Redéfinir lev pour inclure toutes les classes
      lev <- levels(learningmodel[,1])
    }
    
    cat("the levels are : \n")
    print(lev)
    # Variable pour stocker le modèle
    model <- NULL
    classlearning <- learningmodel[,1]
    
    if(modelparameters$modeltype == "randomforest"){
      cat("\n--- Training Random Forest ---\n")
      
      x <- as.data.frame(learningmodel[, -1])
      colnames(x) <- colnames(learningmodel)[-1]
      x <- as.data.frame(x[, sort(colnames(x))])
      set.seed(20011203)
      ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)
      
      # Default values for all RF hyperparameters
      nodesize_param  <- 1
      maxnodes_param  <- NULL
      sampsize_param  <- NULL
      replace_param   <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      sampsize_frac   <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
      
      
      # Déterminer mtry optimal
      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        # Automatic tuning
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # ── GridSearchCV tuning ──
          cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")
          param_grid <- list(
            n_estimators    = if (!is.null(modelparameters$rf_grid_ntree))    modelparameters$rf_grid_ntree    else c(100, 500, 1000),
            max_features    = if (!is.null(modelparameters$rf_grid_mtry))     modelparameters$rf_grid_mtry     else c("sqrt", "log2"),
            min_samples_split = if (!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(1, 5, 10)
          )
          grid_result <- tryCatch({
            tune_rf_gridsearch(X = x, y = learningmodel[, 1],
                               param_grid = param_grid, n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n"); NULL
          })
          if (!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_mtry <- if (!is.null(best_params$max_features)) {
              if (best_params$max_features == "sqrt")  floor(sqrt(ncol(x)))
              else if (best_params$max_features == "log2") floor(log2(ncol(x)))
              else as.numeric(best_params$max_features)
            } else floor(sqrt(ncol(x)))
            ntree_param    <- if (!is.null(best_params$n_estimators))      best_params$n_estimators      else ntree_param
            nodesize_param <- if (!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1
            cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
                        ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
          } else {
            cat("GridSearchCV failed, falling back to stratified CV for mtry...\n")
            p <- ncol(x)
            mtry_candidates <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p/3))))
            fb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
            pos_lev  <- levels(learningmodel[, 1])[1]
            best_fb  <- -Inf
            for (mt in mtry_candidates) {
              sc <- mean(sapply(fb_folds, function(ti) {
                tri <- setdiff(1:nrow(x), ti)
                tryCatch({
                  mdl <- randomForest(x = x[tri,,drop=FALSE], y = learningmodel[tri,1],
                                      ntree = ntree_param, mtry = mt, importance = FALSE)
                  prbs <- randomForest:::predict.randomForest(mdl, x[ti,,drop=FALSE], type = "prob")
                  compute_auc_manual(learningmodel[ti,1], prbs[, pos_lev], pos_lev)
                }, error = function(e) NA_real_)
              }), na.rm = TRUE)
              if (!is.na(sc) && sc > best_fb) { best_fb <- sc; optimal_mtry <- mt }
            }
            nodesize_param <- 1
          }
          # maxnodes tuning: pick best from grid via OOB after fixing ntree/mtry/nodesize
          maxnodes_candidates <- if (!is.null(modelparameters$rf_grid_maxnodes)) modelparameters$rf_grid_maxnodes else c(0)
          maxnodes_candidates <- as.numeric(maxnodes_candidates)
          if (length(maxnodes_candidates) > 1 || any(maxnodes_candidates != 0)) {
            best_oob <- Inf
            best_maxnodes <- NULL
            n_train <- nrow(x)
            samp_sz <- if (sampsize_frac < 1.0) round(n_train * sampsize_frac) else n_train
            for (mn in maxnodes_candidates) {
              mn_val <- if (mn == 0) NULL else mn
              tmp_model <- randomForest(x = x, y = learningmodel[, 1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = mn_val,
                                        sampsize = if (sampsize_frac < 1.0) samp_sz else nrow(x),
                                        replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              cat(sprintf("  maxnodes=%s -> OOB=%.4f\n", ifelse(is.null(mn_val), "NULL", as.character(mn_val)), oob_err))
              if (oob_err < best_oob) { best_oob <- oob_err; best_maxnodes <- mn_val }
            }
            maxnodes_param <- best_maxnodes
            cat(sprintf("Best maxnodes: %s\n", ifelse(is.null(maxnodes_param), "NULL (unlimited)", as.character(maxnodes_param))))
          }
          # sampsize tuning: pick best fraction from grid
          sampsize_candidates <- if (!is.null(modelparameters$rf_grid_sampsize)) as.numeric(modelparameters$rf_grid_sampsize) else c(1.0)
          if (length(sampsize_candidates) > 1) {
            best_oob <- Inf
            best_frac <- 1.0
            for (sf in sampsize_candidates) {
              samp_sz <- round(nrow(x) * sf)
              tmp_model <- randomForest(x = x, y = learningmodel[, 1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = maxnodes_param,
                                        sampsize = samp_sz, replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              cat(sprintf("  sampsize_frac=%.3f -> OOB=%.4f\n", sf, oob_err))
              if (oob_err < best_oob) { best_oob <- oob_err; best_frac <- sf }
            }
            sampsize_frac <- best_frac
            cat(sprintf("Best sampsize fraction: %.3f\n", sampsize_frac))
          }
        } else {
          # ── Traditional mode: stratified CV over ntree, mtry, nodesize ──
          ntree_values    <- if (!is.null(modelparameters$rf_ntree_range)) modelparameters$rf_ntree_range else c(500, 1000)
          nodesize_values <- if (!is.null(modelparameters$rf_nodesize_range)) modelparameters$rf_nodesize_range else c(1)
          p <- ncol(x)
          mtry_values <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p / 3))))
          
          folds_rf   <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
          pos_level  <- levels(learningmodel[, 1])[1]
          best_score <- -Inf
          optimal_mtry <- floor(sqrt(p))
          
          cat("Traditional stratified CV: searching over ntree, mtry and nodesize...\n")
          for (nt in ntree_values) {
            for (ns in nodesize_values) {
              for (mt in mtry_values) {
                fold_scores <- sapply(folds_rf, function(test_idx) {
                  train_idx <- setdiff(1:nrow(x), test_idx)
                  tryCatch({
                    mdl   <- randomForest(x = x[train_idx, , drop = FALSE], y = learningmodel[train_idx, 1],
                                          ntree = nt, mtry = mt, nodesize = ns, importance = FALSE)
                    probs <- randomForest:::predict.randomForest(mdl, x[test_idx, , drop = FALSE], type = "prob")
                    compute_auc_manual(learningmodel[test_idx, 1], probs[, pos_level], pos_level)
                  }, error = function(e) NA_real_)
                })
                sc <- mean(fold_scores, na.rm = TRUE)
                cat(sprintf("  ntree=%d, mtry=%d, nodesize=%d -> AUC=%.4f\n", nt, mt, ns, sc))
                if (!is.na(sc) && sc > best_score) {
                  best_score     <- sc
                  ntree_param    <- nt
                  optimal_mtry   <- mt
                  nodesize_param <- ns
                }
              }
            }
          }
          cat(sprintf("Best stratified CV params: ntree=%d, mtry=%d, nodesize=%d, AUC=%.4f\n",
                      ntree_param, optimal_mtry, nodesize_param, best_score))
        }
      } else {
        # ── Manual mode ──
        optimal_mtry   <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
        nodesize_param <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
        maxnodes_param <- if (!is.null(modelparameters$maxnodes)) modelparameters$maxnodes else NULL
        sampsize_frac  <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
        replace_param  <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      }
      
      # Train final model
      set.seed(20011203)
      # Compute actual sampsize from fraction
      sampsize_param <- if (!is.null(sampsize_frac) && sampsize_frac < 1.0) round(nrow(x) * sampsize_frac) else nrow(x)
      
      model <- randomForest(x = x, y = learningmodel[, 1],
                            ntree = ntree_param, mtry = optimal_mtry,
                            nodesize = nodesize_param, maxnodes = maxnodes_param,
                            sampsize = sampsize_param, replace = replace_param,
                            importance = TRUE)
      model$optimal_mtry    <- optimal_mtry
      model$ntree_used      <- ntree_param
      model$nodesize_used   <- nodesize_param
      model$maxnodes_used   <- maxnodes_param
      model$sampsize_used   <- sampsize_param
      model$replace_used    <- replace_param
      
      cat("  Optimal mtry:", optimal_mtry, "\n")
      cat("  Number of trees:", modelparameters$ntree, "\n")
      
      # Make predictions
      if(is_binary) {
        # Classification binaire
        scorelearning <- data.frame(model$votes[, lev["positif"]])
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
        predictclasslearning <- as.factor(predictclasslearning)
      } else {
        # Classification multi-classe
        scorelearning <- model$votes  # Matrice n_samples x n_classes
        #cat("scorelearning dimensions: nombre de lignes : ", dim(scorelearning)[1]," et ncol : ", dim(scorelearning)[2],"\n")
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        # cat("scorelearning dans le train  : \n")
        # print(head(scorelearning))
        # Prédiction = classe avec la probabilité maximale
        predictclasslearning <- randomForest:::predict.randomForest(model, learningmodel)
        predictclasslearning <- model$predicted
        print(model$confusion[, -end(colnames(model$confusion))[1]])
        predictclasslearning <- as.factor(predictclasslearning)
        cat("affchage de la prediction \n")
        print(head(predictclasslearning) )
        cat("class of predictclasslearning :", class(predictclasslearning), "\n" )
        cat("predictclasslearning dimensions: nombre de lignes : ", dim(predictclasslearning)[1]," et ncol : ", dim(predictclasslearning)[2],"\n")
      }
    }
    
    
    if(modelparameters$modeltype == "svm"){
      cat("\n--- Training SVM ---\n")
      
      
      # Déterminer les hyperparamètres
      if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        svm_scoring <- if (!is.null(modelparameters$svm_scoring)) modelparameters$svm_scoring else "auc"
        svm_gamma_range <- if (!is.null(modelparameters$svm_gamma_range)) modelparameters$svm_gamma_range else 10^(-5:2)
        svm_cost_range  <- if (!is.null(modelparameters$svm_cost_range))  modelparameters$svm_cost_range  else 10^(-3:2)
        tune_result <- tune_svm_stratified(data = learningmodel,
                                           gamma_range = svm_gamma_range,
                                           cost_range  = svm_cost_range,
                                           kernel      = kernel_param,
                                           n_folds     = min(5, nrow(learningmodel) - 1),
                                           scoring     = svm_scoring,
                                           seed        = 123)
        cat("Stratified SVM tuning results:\n"); print(tune_result$best.parameters)
        cost_param   <- tune_result$best.parameters$cost
        gamma_param  <- tune_result$best.parameters$gamma
      } else {
        cat("define svm parameters manually \n")
        cost_param   <- ifelse(is.null(modelparameters$cost),   1,       modelparameters$cost)
        gamma_param  <- ifelse(is.null(modelparameters$gamma),  0.1,     modelparameters$gamma)
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        # epsilon_param <- ifelse(is.null(modelparameters$epsilon), 0.1, modelparameters$epsilon)
      }
      
      # Train model
      if(is_binary) {
        # Classification binaire - sans probabilités
        model <- svm(x = learningmodel[,-1], 
                     y = learningmodel[,1],
                     kernel = "radial", # kernel_model, 
                     cost = cost_param, 
                     gamma = gamma_param,
                     probability = FALSE)
        
        model$cost <- cost_param
        model$gamma <- gamma_param
        # model$kernel <- kernel_model
        
        scorelearning <- data.frame(as.vector(model$decision.values))
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        # Classification multi-classe - avec probabilités
        cat("  Multi-class SVM with probability=TRUE\n")
        print(colnames(learningmodel))
        model <- svm( #group ~ ., data = learningmodel,
          x = learningmodel[,-1], 
          y = learningmodel[,1],
          kernel = kernel_param, 
          cost = cost_param, 
          gamma = gamma_param,
          scale = FALSE,
          probability = TRUE)
        
        cat("affchage du modele svm  :  \n")
        print(model)
        
        model$cost <- cost_param
        model$gamma <- gamma_param
        # model$kernel <- kernel_model
        
        # Obtenir les probabilités
        # tryCatch({
        pred_with_prob <- e1071:::predict.svm(model, learningmodel[,-1], probability = TRUE)
        
        # }, error =  function(e){
        #   print(e$message)
        #   pred_with_prob = data.frame()
        # } )  
        #validate(need(nrow(pred_with_prob)>0, "Erreur: Le modèle SVM n'a pas pu générer de prédictions. Vérifiez vos données et paramètres."))
        cat("section ajustement du modèle svm : affichage des predictions , \n")
        print(pred_with_prob)
        scorelearning <- attr(pred_with_prob, "probabilities")
        
        # Réorganiser les colonnes pour correspondre à l'ordre de lev
        if(!is.null(colnames(scorelearning))) {
          col_order <- match(lev, colnames(scorelearning))
          if(!any(is.na(col_order))) {
            scorelearning <- scorelearning[, col_order, drop = FALSE]
          }
        }
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        # Prédiction = classe avec probabilité maximale
        predictclasslearning <- pred_with_prob
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    
    if(modelparameters$modeltype == "elasticnet"){
      cat("\n--- Training ElasticNet ---\n")
      
      x <- as.matrix(learningmodel[,-1])
      
      # Déterminer la famille
      if(is_binary) {
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
        family_param <- "binomial"
        type_measure <- "auc"
        cat("  Family: binomial\n")
      } else {
        y <- learningmodel[,1]
        family_param <- "multinomial"
        type_measure <- "class"
        cat("  Family: multinomial (", n_classes, "classes)\n")
      }
      
      # Get hyperparameters
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda
      
      cat("  Alpha:", alpha_param, "\n")
      
      # Check if GridSearchCV should be used
      if(!is.null(modelparameters$use_gridsearch) && 
         modelparameters$use_gridsearch && 
         is.null(lambda_param)){
        
        cat("  Using GridSearchCV for hyperparameter tuning...\n")
        
        param_grid <- list(
          alpha = if(!is.null(modelparameters$en_grid_alpha)) {
            modelparameters$en_grid_alpha
          } else {
            c(0, 0.25, 0.5, 0.75, 1.0)
          },
          lambda = if(!is.null(modelparameters$en_grid_lambda)) {
            modelparameters$en_grid_lambda
          } else {
            c(0.001, 0.01, 0.1, 1.0)
          }
        )
        
        grid_result <- tryCatch({
          X_df <- as.data.frame(x)
          tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
                                     param_grid = param_grid,
                                     n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("  GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          alpha_param <- if(!is.null(best_params$alpha)) best_params$alpha else 0.5
          lambda_param <- if(!is.null(best_params$lambda)) best_params$lambda else NULL
          
          cat(sprintf("  GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
                      alpha_param, lambda_param, grid_result$best_score))
        }
      }
      
      # Fit model
      if(is.null(lambda_param)){
        # Perform cross-validation
        cat("  Performing cross-validation for lambda...\n")
        set.seed(20011203)
        cvfit <- cv.glmnet(x, y, 
                           family = family_param, 
                           alpha = alpha_param,
                           type.measure = type_measure, 
                           nfolds = min(10, nrow(learningmodel)-1))
        lambda_param <- cvfit$lambda.min
        model <- list(glmnet_model = cvfit, 
                      lambda = lambda_param, 
                      alpha = alpha_param,
                      cvfit = cvfit, 
                      optimal_lambda = lambda_param, 
                      lambda_1se = cvfit$lambda.1se)
        cat("  Optimal lambda:", lambda_param, "\n")
      } else {
        # Manual lambda
        cat("  Using manual lambda:", lambda_param, "\n")
        fit <- glmnet(x, y, 
                      family = family_param, 
                      alpha = alpha_param, 
                      lambda = lambda_param)
        model <- list(glmnet_model = fit, 
                      lambda = lambda_param, 
                      alpha = alpha_param,
                      cvfit = NULL, 
                      optimal_lambda = lambda_param, 
                      lambda_1se = NULL)
      }
      
      # Feature selection
      if(modelparameters$fs){
        cat("  Performing feature selection...\n")
        
        if(is_binary) {
          # Binaire: simple vecteur de coefficients
          coef_values <- as.matrix(coef(model$glmnet_model, s = lambda_param))
          selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
        } else {
          # Multi-classe: liste de matrices de coefficients
          coef_list <- coef(model$glmnet_model, s = lambda_param)
          all_selected <- c()
          for(i in 1:n_classes) {
            coef_matrix <- as.matrix(coef_list[[i]])
            class_selected <- rownames(coef_matrix)[which(coef_matrix[-1,1] != 0)]
            all_selected <- c(all_selected, class_selected)
          }
          selected_features <- unique(all_selected)
        }
        
        if(length(selected_features) > 0){
          cat("  Selected", length(selected_features), "features\n")
          learningmodel <- learningmodel[, c("group", selected_features)]
          x <- as.matrix(learningmodel[,-1])
          
          # Refit model with selected features
          if(is.null(modelparameters$lambda)){
            cvfit <- cv.glmnet(x, y, 
                               family = family_param, 
                               alpha = alpha_param,
                               type.measure = type_measure, 
                               nfolds = min(10, nrow(learningmodel)-1))
            lambda_param <- cvfit$lambda.min
            fit <- glmnet(x, y, 
                          family = family_param, 
                          alpha = alpha_param, 
                          lambda = lambda_param)
            model <- list(glmnet_model = fit, 
                          lambda = lambda_param, 
                          alpha = alpha_param,
                          cvfit = cvfit, 
                          optimal_lambda = lambda_param, 
                          lambda_1se = cvfit$lambda.1se)
          } else {
            fit <- glmnet(x, y, 
                          family = family_param, 
                          alpha = alpha_param, 
                          lambda = lambda_param)
            model <- list(glmnet_model = fit, 
                          lambda = lambda_param, 
                          alpha = alpha_param,
                          cvfit = NULL, 
                          optimal_lambda = lambda_param, 
                          lambda_1se = NULL)
          }
        } else {
          cat("  Warning: No features selected, keeping all features\n")
        }
      }
      
      # Make predictions
      if(is_binary) {
        # Classification binaire
        if(inherits(model$glmnet_model, "cv.glmnet")){
          scorelearning <- as.vector( glmnet:::predict.cv.glmnet(model$glmnet_model, newx = x, 
                                                                 s = lambda_param, type = "response"))
        } else {
          scorelearning <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx = x, 
                                                            s = lambda_param, type = "response"))
        }
        scorelearning <- data.frame(scorelearning)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        # Classification multi-classe
        if(inherits(model$glmnet_model, "cv.glmnet")){
          score_array <- glmnet:::predict.cv.glmnet(model$glmnet_model, newx = x, 
                                                    s = lambda_param, type = "response")
        } else {
          score_array <- glmnet::predict.glmnet(model$glmnet_model, newx = x, 
                                                s = lambda_param, type = "response")
        }
        
        # Extraire la matrice 2D
        if(is.array(score_array) && length(dim(score_array)) == 3) {
          scorelearning <- score_array[,,1]
        } else if(is.matrix(score_array)) {
          scorelearning <- score_array
        } else {
          scorelearning <- as.matrix(score_array)
        }
        
        # Réorganiser les colonnes si nécessaire
        if(ncol(scorelearning) == n_classes && !is.null(colnames(scorelearning))) {
          col_order <- match(lev, colnames(scorelearning))
          if(!any(is.na(col_order))) {
            scorelearning <- scorelearning[, col_order, drop = FALSE]
          }
        }
        
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        # Prédiction = classe avec probabilité maximale
        class_indices <- apply(scorelearning, 1, which.max)
        predictclasslearning <- factor(lev[class_indices], levels = lev)
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    
    if(modelparameters$modeltype == "xgboost"){
      cat("\n--- Training XGBoost ---\n")
      
      x <- as.matrix(learningmodel[,-1])
      
      if(is_binary) {
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
        objective_param <- "binary:logistic"
        eval_metric_param <- "auc"
        cat("  Objective: binary:logistic\n")
      } else {
        # Encoder les classes en 0, 1, 2, ...
        y <- as.numeric(learningmodel[,1]) - 1
        objective_param <- "multi:softprob"
        eval_metric_param <- "mlogloss"
        cat("  Objective: multi:softprob (", n_classes, "classes)\n")
      }
      
      dtrain <- xgb.DMatrix(data = x, label = y)
      
      # Déterminer les hyperparamètres
      if(is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb){
        
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          cat("  Using GridSearchCV for hyperparameter tuning...\n")
          
          alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
          lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
          param_grid <- list(
            n_estimators = if (!is.null(modelparameters$xgb_grid_nrounds))   modelparameters$xgb_grid_nrounds   else c(50, 100, 200),
            max_depth    = if (!is.null(modelparameters$xgb_grid_maxdepth))  modelparameters$xgb_grid_maxdepth  else c(3, 6, 9),
            learning_rate = if (!is.null(modelparameters$xgb_grid_eta))      modelparameters$xgb_grid_eta       else c(0.01, 0.1, 0.3),
            gamma        = if (!is.null(modelparameters$xgb_grid_gamma))     modelparameters$xgb_grid_gamma     else c(0, 0.1, 0.5),
            subsample    = if (!is.null(modelparameters$xgb_grid_subsample)) modelparameters$xgb_grid_subsample else c(0.8, 1.0)
          )
          
          grid_result <- tryCatch({
            X_df <- as.data.frame(x)
            tune_xgb_gridsearch(X = X_df, y = learningmodel[,1],
                                param_grid = param_grid,
                                n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("  GridSearchCV failed, falling back to xgb.cv:", e$message, "\n")
            NULL
          })
          
          if(!is.null(grid_result)) {
            best_params              <- grid_result$best_params
            nrounds_param <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            maxdepth_param <- if(!is.null(best_params$max_depth)) best_params$max_depth else 6
            eta_param <- if(!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_nrounds          <- if (!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            optimal_max_depth        <- if (!is.null(best_params$max_depth))    best_params$max_depth    else 6
            optimal_eta              <- if (!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_gamma            <- if (!is.null(best_params$gamma))        best_params$gamma        else 0
            optimal_subsample        <- if (!is.null(best_params$subsample))    best_params$subsample    else 1.0
            optimal_min_child_weight <- if (!is.null(best_params$min_child_weight)) best_params$min_child_weight else 1
            optimal_alpha =  if (!is.null(best_params$alpha)) best_params$alpha else 0
            optimal_lambda = if (!is.null(best_params$lambda)) best_params$lambda else 0
            
            cat(sprintf("GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, gamma=%.3f, score=%.4f\n",
                        optimal_nrounds, optimal_max_depth, optimal_eta, optimal_gamma, grid_result$best_score))
            final_params <- list(objective = objective_param, eval_metric = eval_metric_param,
                                 max_depth = optimal_max_depth, eta = optimal_eta,
                                 gamma = optimal_gamma, subsample = optimal_subsample,
                                 lambda = optimal_lambda, alpha = optimal_alpha,
                                 min_child_weight = optimal_min_child_weight)
            if(!is_binary) final_params$num_class <- n_classes
            model <- xgb.train(params = final_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
            model$optimal_nrounds          <- optimal_nrounds
            model$optimal_max_depth        <- optimal_max_depth
            model$optimal_eta              <- optimal_eta
            model$optimal_gamma            <- optimal_gamma
            model$optimal_subsample        <- optimal_subsample
            model$optimal_min_child_weight <- optimal_min_child_weight
          } else {
            alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
            lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
            maxdepth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
            eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)
            nrounds_param <- ifelse(is.null(modelparameters$nrounds_model), 100, modelparameters$nrounds)
            gamma_param <- ifelse(is.null(modelparameters$gamma_xgm), 0, modelparameters$gamma_xgm)
            subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 0.8, modelparameters$subsample_xgb)
            set.seed(20011203)
            best_params <- list(objective = objective_param, eval_metric = eval_metric_param,
                                max_depth = 6, eta = 0.3, min_child_weight = 1,
                                gamma = gamma_param, subsample = subsample_param,
                                alpha = alpha_param, lambda = lambda_param)
            if(!is_binary) best_params$num_class <- n_classes
            xgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
            tryCatch({
              cv_results      <- xgb.cv(params = best_params, data = dtrain, nrounds = 200,
                                        folds = xgb_folds,
                                        early_stopping_rounds = 10, verbose = 0)
              optimal_nrounds <- cv_results$best_iteration
            }, error = function(e) { optimal_nrounds <<- 20 })
            model <- xgb.train(params = best_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
            model$optimal_nrounds          <- optimal_nrounds
            model$optimal_max_depth        <- best_params$max_depth
            model$optimal_eta              <- best_params$eta
            model$optimal_min_child_weight <- best_params$min_child_weight
            model$optimal_gamma            <- best_params$gamma
            model$optimal_subsample        <- best_params$subsample
          }
        } else {
          set.seed(20011203)
          alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
          lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
          maxdepth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
          eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)
          nrounds_param <- ifelse(is.null(modelparameters$nrounds_model), 100, modelparameters$nrounds)
          gamma_param <- ifelse(is.null(modelparameters$gamma_xgm), 0, modelparameters$gamma_xgm)
          subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 0.8, modelparameters$subsample_xgb)
          
          best_params <- list(objective = "multi:softprob", eval_metric = "mlogloss",
                              alpha = alpha_param, lambda = lambda_param,
                              num_class = n_classes,
                              max_depth = 6, eta = 0.3, min_child_weight = 1,
                              gamma = gamma_param, subsample = subsample_param)
          # if(!is_binary) best_params$num_class <-  
          xgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
          cv_results      <- xgb.cv(params = best_params, data = dtrain, nrounds = 200,
                                    folds = xgb_folds,
                                    early_stopping_rounds = 10, verbose = 0)
          optimal_nrounds <- cv_results$best_iteration
          cat("best parameters\n"); print(best_params)
          cat("optimal rounds : ", optimal_nrounds, "\n")
          model <- xgb.train(params = best_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
          model$optimal_nrounds          <- optimal_nrounds
          model$optimal_max_depth        <- best_params$max_depth
          model$optimal_eta              <- best_params$eta
          model$optimal_min_child_weight <- best_params$min_child_weight
          model$optimal_gamma            <- best_params$gamma
          model$optimal_subsample        <- best_params$subsample
          cat("optimal_nrounds :  ", nrounds_param, "\n")
          cat("optimal_max_depth :  ", maxdepth_param, "\n")
          cat("optimal_eta :  ", eta_param, "\n")
          cat("optimal alpha : ", alpha_param, "\n")
          cat("optimal lambda : ", lambda_param, "\n")
          cat("optimal_subsample :  ", subsample_param, "\n")
          cat("optimal_gamma :  ", gamma_param, "\n")
        }
        
        minchild_param <- 1
        subsample_param <- 0.8
        colsample_param <- 0.8
      } else {
        cat("  Define XGBoost parameters manually...\n")
        set.seed(20011203)
        alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
        lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
        maxdepth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
        eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)
        minchild_param <- ifelse(is.null(modelparameters$min_child_weight), 1, modelparameters$min_child_weight)
        subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 0.8, modelparameters$subsample_xgb)
        colsample_param <- ifelse(is.null(modelparameters$colsample_bytree), 0.8, modelparameters$colsample_bytree)
        nrounds_param <- ifelse(is.null(modelparameters$nrounds_model), 100, modelparameters$nrounds_model)
        gamma_param <- ifelse(is.null(modelparameters$gamma_xgb), 0, modelparameters$gamma_xgb)
        
        best_params <- list(objective = "multi:softprob", eval_metric ="mlogloss",
                            alpha = alpha_param, lambda = lambda_param, num_class = n_classes,
                            subsample = subsample_param, colsample_bytree = colsample_param,
                            gamma = gamma_param, max_depth = 6, 
                            eta = 0.3, min_child_weight = 1)
        if(!is_binary) best_params$num_class <- n_classes
        xgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
        cv_results      <- xgb.cv(params = best_params, data = dtrain, nrounds = 200,
                                  folds = xgb_folds,
                                  early_stopping_rounds = 10, verbose = 0)
        optimal_nrounds <- cv_results$best_iteration
        cat("best parameters\n"); print(best_params)
        cat("optimal rounds : ", optimal_nrounds, "\n")
        model <- xgb.train(params = best_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
        model$optimal_nrounds          <- nrounds_param
        model$optimal_max_depth        <- maxdepth_param
        model$optimal_eta              <- eta_param
        
        model$optimal_min_child_weight <- minchild_param
        model$optimal_gamma <- gamma_param
        model$optimal_subsample <- subsample_param
        model$optimal_alpha <- alpha_param
        model$optimal_lambda <- lambda_param
        cat("optimal_nrounds :  ", nrounds_param, "\n")
        cat("optimal_max_depth :  ", maxdepth_param, "\n")
        cat("optimal_eta :  ", eta_param, "\n")
        cat("optimal alpha : ", alpha_param, "\n")
        cat("optimal lambda : ", lambda_param, "\n")
        cat("optimal_subsample :  ", subsample_param, "\n")
        cat("optimal_gamma :  ", gamma_param, "\n")
      }
      
      # Train final model
      params <- list(
        objective = objective_param,
        eval_metric = eval_metric_param,
        max_depth = maxdepth_param,
        eta = eta_param,
        gamma = gamma_param,
        subsample = subsample_param,
        min_child_weight = minchild_param,
        subsample = subsample_param,
        colsample_bytree = colsample_param
      )
      
      if(!is_binary) {
        params$num_class <- n_classes
      }
      
      set.seed(20011203)
      model <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds_param,
        verbose = 0
      )
      
      # Store optimal parameters
      model$optimal_nrounds <- nrounds_param
      model$optimal_max_depth <- maxdepth_param
      model$optimal_eta <- eta_param
      model$optimal_min_child_weight <- minchild_param
      model$optimal_gamma <- gamma_param
      model$optimal_subsample <- subsample_param
      
      cat("  Optimal parameters:\n")
      cat("    nrounds:", nrounds_param, "\n")
      cat("    max_depth:", maxdepth_param, "\n")
      cat("    eta:", eta_param, "\n")
      
      # Make predictions
      if(is_binary) {
        scorelearning <- xgboost:::predict.xgb.Booster(model, x)
        scorelearning <- data.frame(scorelearning)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        # Prédictions multi-classe (matrice de probabilités)
        score_matrix <- xgboost:::predict.xgb.Booster(model, x, reshape = TRUE)
        scorelearning <- score_matrix
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        # Prédiction = classe avec probabilité maximale
        class_indices <- apply(scorelearning, 1, which.max)
        predictclasslearning <- factor(lev[class_indices], levels = lev)
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    
    if(modelparameters$modeltype == "lightgbm"){
      cat("\n--- Training LightGBM ---\n")
      
      x <- as.matrix(learningmodel[,-1])
      
      if(is_binary) {
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
        objective_param <- "binary"
        metric_param <- "auc"
        cat("  Objective: binary\n")
      } else {
        y <- as.numeric(learningmodel[,1]) - 1
        objective_param <- "multiclass"
        metric_param <- "multi_logloss"
        cat("  Objective: multiclass (", n_classes, "classes)\n")
      }
      
      dtrain <- lgb.Dataset(data = x, label = y)
      
      # Déterminer les hyperparamètres
      if(is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb){
        cat("  Performing cross-validation...\n")
        
        # Simple CV with default parameters
        params_cv <- list(
          objective = objective_param,
          metric = metric_param,
          num_leaves = 31,
          learning_rate = 0.1,
          feature_fraction = 0.9,
          bagging_fraction = 0.8,
          bagging_freq = 5,
          verbose = -1
        )
        if(!is_binary) params_cv$num_class <- n_classes
        
        set.seed(20011203)
        cv_result <- lgb.cv(
          params = params_cv,
          data = dtrain,
          nrounds = 200,
          nfold = 5,
          early_stopping_rounds = 10,
          verbose = -1
        )
        
        nrounds_param <- cv_result$best_iter
        num_leaves_param <- 31
        learning_rate_param <- 0.1
      } else {
        # Manual mode
        nrounds_param <- if(!is.null(modelparameters$nrounds_lgb)) modelparameters$nrounds_lgb else 100
        num_leaves_param <- if(!is.null(modelparameters$num_leaves)) modelparameters$num_leaves else 31
        learning_rate_param <- if(!is.null(modelparameters$learning_rate_lgb)) modelparameters$learning_rate_lgb else 0.1
        
        cat("  Using manual parameters:\n")
        cat("  nrounds:", nrounds_param, "\n")
        cat("  num_leaves:", num_leaves_param, "\n")
        cat("  learning_rate:", learning_rate_param, "\n")
      }
      
      # Train final model
      params <- list(
        objective = objective_param,
        metric = metric_param,
        num_leaves = num_leaves_param,
        learning_rate = learning_rate_param,
        feature_fraction = 0.9,
        bagging_fraction = 0.8,
        bagging_freq = 5,
        verbose = -1
      )
      
      if(!is_binary) {
        params$num_class <- n_classes
      }
      
      set.seed(20011203)
      model <- lgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds_param,
        verbose = -1
      )
      
      # Store optimal parameters
      model$optimal_nrounds <- nrounds_param
      model$optimal_num_leaves <- num_leaves_param
      model$optimal_learning_rate <- learning_rate_param
      
      cat("  Optimal parameters:\n")
      cat("    nrounds:", nrounds_param, "\n")
      cat("    num_leaves:", num_leaves_param, "\n")
      cat("    learning_rate:", learning_rate_param, "\n")
      
      # Make predictions
      if(is_binary) {
        scorelearning <- predict(model, x)
        scorelearning <- data.frame(scorelearning)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        score_matrix <- predict(model, x, reshape = TRUE)
        scorelearning <- score_matrix
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        class_indices <- apply(scorelearning, 1, which.max)
        predictclasslearning <- factor(lev[class_indices], levels = lev)
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    if(modelparameters$modeltype == "naivebayes"){
      cat("\n--- Training Naive Bayes ---\n")
      
      # Determine laplace parameter
      optimal_laplace <- 0
      
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
        cat("  Using GridSearchCV for hyperparameter tuning...\n")
        
        param_grid <- list(
          laplace = if(!is.null(modelparameters$nb_grid_laplace)) {
            modelparameters$nb_grid_laplace
          } else {
            c(0, 0.5, 1, 2, 5)
          }
        )
        
        grid_result <- tryCatch({
          X_df <- as.data.frame(learningmodel[,-1])
          tune_nb_gridsearch(X = X_df, y = learningmodel[,1],
                             param_grid = param_grid,
                             n_folds = 5,
                             scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("  GridSearchCV failed, using default laplace=0:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          optimal_laplace <- if(!is.null(best_params$laplace)) best_params$laplace else 0
          cat(sprintf("  GridSearchCV best params: laplace=%.2f, score=%.4f\n",
                      optimal_laplace, grid_result$best_score))
        }
      }
      
      # Build model
      model <- naiveBayes(x = learningmodel[,-1], 
                          y = learningmodel[,1], 
                          laplace = optimal_laplace)
      
      model$model_type <- "naivebayes"
      model$optimal_laplace <- optimal_laplace
      
      cat("  Laplace smoothing:", optimal_laplace, "\n")
      
      # Make predictions
      pred_probs <- e1071:::predict.naiveBayes(model, learningmodel[,-1], type = "raw")
      
      if(is_binary) {
        scorelearning <- data.frame(pred_probs[, lev["positif"]])
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        scorelearning <- pred_probs
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        class_indices <- apply(scorelearning, 1, which.max)
        predictclasslearning <- factor(lev[class_indices], levels = lev)
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    
    if(modelparameters$modeltype == "knn"){
      cat("\n--- Training KNN ---\n")
      
      # Determine k parameter
      if(is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn){
        
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          cat("  Using GridSearchCV for hyperparameter tuning...\n")
          
          max_k <- min(floor(sqrt(nrow(learningmodel))), 30)
          param_grid <- list(
            n_neighbors = if(!is.null(modelparameters$knn_grid_k)) {
              modelparameters$knn_grid_k
            } else {
              seq(3, max_k, by=2)
            }
          )
          
          grid_result <- tryCatch({
            X_df <- as.data.frame(learningmodel[,-1])
            tune_knn_gridsearch(X = X_df, y = learningmodel[,1],
                                param_grid = param_grid,
                                n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("  GridSearchCV failed, falling back to manual CV:", e$message, "\n")
            NULL
          })
          
          if(!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_k <- if(!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
            cat(sprintf("  GridSearchCV best params: k=%d, score=%.4f\n",
                        optimal_k, grid_result$best_score))
          } else {
            # Fallback to traditional CV
            set.seed(20011203)
            max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
            k_values <- seq(3, max_k, by=2)
            
            best_k <- 3
            best_acc <- 0
            for(k_test in k_values){
              n_folds <- min(5, nrow(learningmodel))
              fold_size <- floor(nrow(learningmodel) / n_folds)
              accuracies <- numeric(n_folds)
              for(fold in 1:n_folds){
                test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
                train_idx <- setdiff(1:nrow(learningmodel), test_idx)
                pred <- class::knn(train = learningmodel[train_idx, -1],
                                   test = learningmodel[test_idx, -1],
                                   cl = learningmodel[train_idx, 1],
                                   k = k_test)
                accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
              }
              avg_acc <- mean(accuracies)
              if(avg_acc > best_acc){
                best_acc <- avg_acc
                best_k <- k_test
              }
            }
            optimal_k <- best_k
          }
        } else {
          # Traditional CV
          cat("  Performing cross-validation...\n")
          set.seed(20011203)
          max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
          k_values <- seq(3, max_k, by=2)
          
          best_k <- 3
          best_acc <- 0
          for(k_test in k_values){
            n_folds <- min(5, nrow(learningmodel))
            fold_size <- floor(nrow(learningmodel) / n_folds)
            accuracies <- numeric(n_folds)
            for(fold in 1:n_folds){
              test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
              train_idx <- setdiff(1:nrow(learningmodel), test_idx)
              pred <- class::knn(train = learningmodel[train_idx, -1],
                                 test = learningmodel[test_idx, -1],
                                 cl = learningmodel[train_idx, 1],
                                 k = k_test)
              accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
            }
            avg_acc <- mean(accuracies)
            if(avg_acc > best_acc){
              best_acc <- avg_acc
              best_k <- k_test
            }
          }
          optimal_k <- best_k
        }
      } else {
        # Manual mode
        optimal_k <- if(!is.null(modelparameters$k_neighbors)) {
          modelparameters$k_neighbors
        } else {
          5
        }
        cat("  Using manual k:", optimal_k, "\n")
      }
      
      cat("  Optimal k:", optimal_k, "\n")
      
      # Store as model object
      model <- list(
        optimal_k = optimal_k,
        train_data = learningmodel[, -1],
        train_labels = learningmodel[, 1]
      )
      class(model) <- "knn_model"
      
      # Make predictions
      predicted_classes <- class::knn(train = learningmodel[, -1],
                                      test = learningmodel[, -1],
                                      cl = learningmodel[, 1],
                                      k = optimal_k,
                                      prob = TRUE)
      
      probs <- attr(predicted_classes, "prob")
      
      if(is_binary) {
        # Pour binaire, prob est la proba de la classe prédite
        scorelearning <- ifelse(predicted_classes == lev["positif"], probs, 1 - probs)
        scorelearning <- data.frame(scorelearning)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        
        predictclasslearning <- factor(levels = lev)
        predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
        predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
      } else {
        # Pour multi-classe, créer une matrice de probabilités approximative
        scorelearning <- matrix(0, nrow = nrow(learningmodel), ncol = n_classes)
        for(i in 1:nrow(scorelearning)) {
          class_idx <- which(lev == predicted_classes[i])
          scorelearning[i, class_idx] <- probs[i]
          # Distribuer le reste uniformément
          other_idx <- setdiff(1:n_classes, class_idx)
          if(length(other_idx) > 0) {
            scorelearning[i, other_idx] <- (1 - probs[i]) / length(other_idx)
          }
        }
        colnames(scorelearning) <- paste("Prob", lev, sep="_")
        
        predictclasslearning <- predicted_classes
      }
      predictclasslearning <- as.factor(predictclasslearning)
    }
    
    
    if(is_binary) {
      # Format binaire original
      reslearningmodel <- data.frame(classlearning, scorelearning, predictclasslearning)
      colnames(reslearningmodel) <- c("classlearning", "scorelearning", "predictclasslearning")
    } else {
      # Format multi-classe
      reslearningmodel <- data.frame(classlearning, scorelearning, predictclasslearning)
      # reslearningmodel <- list(
      #   "classlearning" = classlearning,
      #   "scorelearning" = scorelearning,
      #   "predictclasslearning" = predictclasslearning
      # )
      score_cols <- paste("score", lev, sep="_")
      colnames(reslearningmodel) <- c("classlearning", score_cols, "predictclasslearning")
    }
    rownames(reslearningmodel) <- rownames(learningmodel)
    
    datalearningmodel <- list(
      "learningmodel" = learningmodel,
      "reslearningmodel" = reslearningmodel
    )
    
    cat("\n=== Learning completed ===\n")
    cat("Samples:", nrow(learningmodel), "\n")
    cat("Features:", ncol(learningmodel)-1, "\n")
    if(is_binary) {
      cat("Score column:", colnames(scorelearning), "\n")
    } else {
      cat("Score columns:", paste(colnames(scorelearning), collapse=", "), "\n")
    }
    
    # ==========================================================================
    #  VALIDATION
    # ==========================================================================
    
    datavalidationmodel <- NULL
    
    if(!is.null(validation)){
      cat("\n=== Processing validation data ===\n")
      
      # Transform validation data
      validationmodel <- validation
      colnames(validationmodel)[1] <- "group"
      validationmodel[,1] <- as.factor(as.character(validationmodel[,1]))
      
      # Apply transformations
      if(transformdataparameters$rempNA == "0"){
        validationmodel[,-1][is.na(validationmodel[,-1])] <- 0
      } else if(transformdataparameters$rempNA %in% c("pca", "missforest")){
        # Complex imputation for validation
        if(!is.null(datastructuresfeatures) && ncol(datastructuresfeatures) > 0){
          validationdatastructuresfeatures <- validation[, colnames(datastructuresfeatures)]
          validationdatastructuresfeatures[is.na(validationdatastructuresfeatures)] <- 0
          learningselectfull <- cbind(learningselect, datastructuresfeatures)
          validationselectfull <- cbind(validation, validationdatastructuresfeatures)
          
          if(transformdataparameters$rempNA == "pca"){
            res <- imputePCA(X = rbind(learningselectfull, validationselectfull)[,-1], 
                             ncp = min(3, ncol(learningselectfull)-2))
            validationmodel <- cbind(validationselectfull[,1], 
                                     res$completeObs[(nrow(learningselectfull)+1):nrow(rbind(learningselectfull, validationselectfull)),])
          } else if(transformdataparameters$rempNA == "missforest"){
            res <- missForest(xmis = rbind(learningselectfull, validationselectfull)[,-1])
            validationmodel <- cbind(validationselectfull[,1], 
                                     res$ximp[(nrow(learningselectfull)+1):nrow(rbind(learningselectfull, validationselectfull)),])
          }
        } else {
          validationmodel[,-1][is.na(validationmodel[,-1])] <- 0
        }
      }
      
      # Apply other transformations
      if(transformdataparameters$log){
        if(transformdataparameters$logtype == "log2"){
          validationmodel[,-1] <- log2(validationmodel[,-1] + 1)
        } else {
          validationmodel[,-1] <- log(validationmodel[,-1] + 1)
        }
      }
      if(transformdataparameters$arcsin){
        validationmodel[,-1] <- asin(sqrt(validationmodel[,-1]))
      }
      if(transformdataparameters$standardization){
        validationmodel[,-1] <- scale(validationmodel[,-1])
      }
      
      classval <- validationmodel[,1]
      
      # Vérifications de débogage
      cat("\n=== Validation class captured ===\n")
      cat("classval type:", class(classval), "\n")
      cat("classval levels:", paste(levels(classval), collapse=", "), "\n")
      cat("classval head:", paste(head(classval), collapse=", "), "\n")
      
      # Select same features as training
      common_features <- intersect(colnames(learningmodel), colnames(validationmodel))
      cat("checking des colonnes de validation par rapport au modele d'apprentissage...\n")
      print(intersect(colnames(learningmodel), colnames(validationmodel)))
      validationmodel <- cbind(validationmodel[,1], validationmodel[, common_features])
      colnames(validationmodel)[1] <- "group"
      
      # classval <- validationmodel[,1]
      cat("varification que la premiere colonne est bien le groupe:\n")
      print((table(validationmodel[,1])))
      
      # Make predictions based on model type
      if(modelparameters$modeltype == "randomforest"){
        if(is_binary) {
          scoreval <- data.frame(randomForest:::predict.randomForest(model, validationmodel, type="prob")[, lev["positif"]])
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          scoreval <- randomForest:::predict.randomForest(model, validationmodel, type="prob")
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          predictclassval <- randomForest:::predict.randomForest(model, validationmodel)
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "svm"){
        if(is_binary) {
          scoreval <- data.frame(as.vector(e1071:::predict.svm(model, validationmodel[,-1], decision.values=TRUE)))
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          pred_with_prob <- e1071:::predict.svm(model, validationmodel[,-1], probability=TRUE)
          scoreval <- attr(pred_with_prob, "probabilities")
          if(!is.null(colnames(scoreval))) {
            col_order <- match(lev, colnames(scoreval))
            if(!any(is.na(col_order))) {
              scoreval <- scoreval[, col_order, drop=FALSE]
            }
          }
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          predictclassval <- pred_with_prob
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "elasticnet"){
        x_val <- as.matrix(validationmodel[,-1])
        
        if(is_binary) {
          if(inherits(model$glmnet_model, "cv.glmnet")){
            scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, 
                                                             s=model$lambda, type="response"))
          } else {
            scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, 
                                                         s=model$lambda, type="response"))
          }
          scoreval <- data.frame(scoreval)
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          if(inherits(model$glmnet_model, "cv.glmnet")){
            score_array <- glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, 
                                                      s=model$lambda, type="response")
          } else {
            score_array <- glmnet::predict.glmnet(model$glmnet_model, newx=x_val, 
                                                  s=model$lambda, type="response")
          }
          
          if(is.array(score_array) && length(dim(score_array)) == 3) {
            scoreval <- score_array[,,1]
          } else if(is.matrix(score_array)) {
            scoreval <- score_array
          } else {
            scoreval <- as.matrix(score_array)
          }
          
          if(ncol(scoreval) == n_classes && !is.null(colnames(scoreval))) {
            col_order <- match(lev, colnames(scoreval))
            if(!any(is.na(col_order))) {
              scoreval <- scoreval[, col_order, drop=FALSE]
            }
          }
          
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          class_indices <- apply(scoreval, 1, which.max)
          predictclassval <- factor(lev[class_indices], levels = lev)
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "xgboost"){
        x_val <- as.matrix(validationmodel[,-1])
        
        if(is_binary) {
          scoreval <- xgboost:::predict.xgb.Booster(model, x_val)
          scoreval <- data.frame(scoreval)
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          score_matrix <- xgboost:::predict.xgb.Booster(model, x_val, reshape = TRUE)
          scoreval <- score_matrix
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          
          class_indices <- apply(scoreval, 1, which.max)
          predictclassval <- factor(lev[class_indices], levels = lev)
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "lightgbm"){
        x_val <- as.matrix(validationmodel[,-1])
        
        if(is_binary) {
          scoreval <- predict(model, x_val)
          scoreval <- data.frame(scoreval)
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          score_matrix <- predict(model, x_val, reshape = TRUE)
          scoreval <- score_matrix
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          
          class_indices <- apply(scoreval, 1, which.max)
          predictclassval <- factor(lev[class_indices], levels = lev)
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "naivebayes"){
        pred_probs_val <- e1071:::predict.naiveBayes(model, validationmodel[,-1], type="raw")
        
        if(is_binary) {
          scoreval <- data.frame(pred_probs_val[, lev["positif"]])
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          scoreval <- pred_probs_val
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          
          class_indices <- apply(scoreval, 1, which.max)
          predictclassval <- factor(lev[class_indices], levels = lev)
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype == "knn"){
        predicted_classes_val <- class::knn(train = model$train_data,
                                            test = validationmodel[, -1],
                                            cl = model$train_labels,
                                            k = model$optimal_k,
                                            prob = TRUE)
        
        probs_val <- attr(predicted_classes_val, "prob")
        
        if(is_binary) {
          scoreval <- ifelse(predicted_classes_val == lev["positif"], probs_val, 1 - probs_val)
          scoreval <- data.frame(scoreval)
          colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
          
          predictclassval <- factor(levels = lev)
          predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
          predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
        } else {
          scoreval <- matrix(0, nrow = nrow(validationmodel), ncol = n_classes)
          for(i in 1:nrow(scoreval)) {
            class_idx <- which(lev == predicted_classes_val[i])
            scoreval[i, class_idx] <- probs_val[i]
            other_idx <- setdiff(1:n_classes, class_idx)
            if(length(other_idx) > 0) {
              scoreval[i, other_idx] <- (1 - probs_val[i]) / length(other_idx)
            }
          }
          colnames(scoreval) <- paste("Prob", lev, sep="_")
          
          predictclassval <- predicted_classes_val
        }
        predictclassval <- as.factor(predictclassval)
      }
      
      # Format validation results
      if(is_binary) {
        resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
        colnames(resvalidationmodel) <- c("classval", "scoreval", "predictclassval")
      } else {
        resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
        score_cols <- paste("score", lev, sep="_")
        colnames(resvalidationmodel) <- c("classval", score_cols, "predictclassval")
      }
      rownames(resvalidationmodel) <- rownames(validationmodel)
      
      datavalidationmodel <- list(
        "validationmodel" = validationmodel,  # contient les features et la classe
        "resvalidationmodel" = resvalidationmodel # contient les résultats de la prédiction
      )
      
      cat("Validation samples:", nrow(validationmodel), "\n")
      cat("Validation completed\n")
    }
    
    # ==========================================================================
    # RETURN
    # ==========================================================================
    
    cat("\n=== Model training completed successfully ===\n\n")
    
    return(list(
      "datalearningmodel" = datalearningmodel,
      "model" = model,
      "datavalidationmodel" = datavalidationmodel,
      "groups" = if(is_binary) groups else lev,
      "modelparameters" = modelparameters,
      "n_classes" = n_classes,
      "is_binary" = is_binary
    ))
    
  }
}


# modelfunction <- function(learningmodel, validation, modelparameters, 
#                           transformdataparameters, datastructuresfeatures, 
#                           learningselect = NULL) {
#   if(modelparameters$modeltype!="nomodel"){ 
#     # colnames(learningmodel)[1]<-"group"
#     # Définir les groupes/niveaux
#     lev <- levels(learningmodel[,1])
#     # groups <- c("positif" = lev[1], "negatif" = lev[2])
#     # lev <- groups[c("positif","negatif")]
#     
#     # Détecter si classification binaire ou multi-classe
#     n_classes <- length(levels(learningmodel[,1]))
#     is_binary <- (n_classes == 2)
#     
#     if(is_binary) {
#       # Définir positif/negatif pour la classification binaire
#       lev_positif <- lev["positif"]
#       lev_negatif <- lev["negatif"]
#       cat("=== Classification binaire détectée ===\n")
#       cat("  Positif:", lev_positif, "\n")
#       cat("  Négatif:", lev_negatif, "\n")
#     } else {
#       cat("=== Classification multi-classe détectée ===\n")
#       cat("  Nombre de classes:", n_classes, "\n")
#       cat("  Classes:", paste(levels(learningmodel[,1]), collapse = ", "), "\n")
#       # Redéfinir lev pour inclure toutes les classes
#       lev <- levels(learningmodel[,1])
#     }
#     
#     cat("the levels are : \n")
#     print(lev)
#     # Variable pour stocker le modèle
#     model <- NULL
#     classlearning <- learningmodel[,1]
#     
#     if(modelparameters$modeltype == "randomforest"){
#       cat("\n--- Training Random Forest ---\n")
#       
#       x <- as.data.frame(learningmodel[, -1])
#       colnames(x) <- colnames(learningmodel)[-1]
#       x <- as.data.frame(x[, sort(colnames(x))])
#       set.seed(20011203)
#       ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)
#       
#       # Default values for all RF hyperparameters
#       nodesize_param  <- 1
#       maxnodes_param  <- NULL
#       sampsize_param  <- NULL
#       replace_param   <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
#       sampsize_frac   <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
#       
#       
#       # Déterminer mtry optimal
#       if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
#         # Automatic tuning
#         if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
#           # ── GridSearchCV tuning ──
#           cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")
#           param_grid <- list(
#             n_estimators    = if (!is.null(modelparameters$rf_grid_ntree))    modelparameters$rf_grid_ntree    else c(100, 500, 1000),
#             max_features    = if (!is.null(modelparameters$rf_grid_mtry))     modelparameters$rf_grid_mtry     else c("sqrt", "log2"),
#             min_samples_split = if (!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(1, 5, 10)
#           )
#           grid_result <- tryCatch({
#             tune_rf_gridsearch(X = x, y = learningmodel[, 1],
#                                param_grid = param_grid, n_folds = 5,
#                                scoring = c("auc", "accuracy"))
#           }, error = function(e) {
#             cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n"); NULL
#           })
#           if (!is.null(grid_result)) {
#             best_params <- grid_result$best_params
#             optimal_mtry <- if (!is.null(best_params$max_features)) {
#               if (best_params$max_features == "sqrt")  floor(sqrt(ncol(x)))
#               else if (best_params$max_features == "log2") floor(log2(ncol(x)))
#               else as.numeric(best_params$max_features)
#             } else floor(sqrt(ncol(x)))
#             ntree_param    <- if (!is.null(best_params$n_estimators))      best_params$n_estimators      else ntree_param
#             nodesize_param <- if (!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1
#             cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
#                         ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
#           } else {
#             cat("GridSearchCV failed, falling back to stratified CV for mtry...\n")
#             p <- ncol(x)
#             mtry_candidates <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p/3))))
#             fb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
#             pos_lev  <- levels(learningmodel[, 1])[1]
#             best_fb  <- -Inf
#             for (mt in mtry_candidates) {
#               sc <- mean(sapply(fb_folds, function(ti) {
#                 tri <- setdiff(1:nrow(x), ti)
#                 tryCatch({
#                   mdl <- randomForest(x = x[tri,,drop=FALSE], y = learningmodel[tri,1],
#                                       ntree = ntree_param, mtry = mt, importance = FALSE)
#                   prbs <- randomForest:::predict.randomForest(mdl, x[ti,,drop=FALSE], type = "prob")
#                   compute_auc_manual(learningmodel[ti,1], prbs[, pos_lev], pos_lev)
#                 }, error = function(e) NA_real_)
#               }), na.rm = TRUE)
#               if (!is.na(sc) && sc > best_fb) { best_fb <- sc; optimal_mtry <- mt }
#             }
#             nodesize_param <- 1
#           }
#           # maxnodes tuning: pick best from grid via OOB after fixing ntree/mtry/nodesize
#           maxnodes_candidates <- if (!is.null(modelparameters$rf_grid_maxnodes)) modelparameters$rf_grid_maxnodes else c(0)
#           maxnodes_candidates <- as.numeric(maxnodes_candidates)
#           if (length(maxnodes_candidates) > 1 || any(maxnodes_candidates != 0)) {
#             best_oob <- Inf
#             best_maxnodes <- NULL
#             n_train <- nrow(x)
#             samp_sz <- if (sampsize_frac < 1.0) round(n_train * sampsize_frac) else n_train
#             for (mn in maxnodes_candidates) {
#               mn_val <- if (mn == 0) NULL else mn
#               tmp_model <- randomForest(x = x, y = learningmodel[, 1],
#                                         ntree = ntree_param, mtry = optimal_mtry,
#                                         nodesize = nodesize_param, maxnodes = mn_val,
#                                         sampsize = if (sampsize_frac < 1.0) samp_sz else nrow(x),
#                                         replace = replace_param, importance = FALSE)
#               oob_err <- mean(tmp_model$err.rate[, "OOB"])
#               cat(sprintf("  maxnodes=%s -> OOB=%.4f\n", ifelse(is.null(mn_val), "NULL", as.character(mn_val)), oob_err))
#               if (oob_err < best_oob) { best_oob <- oob_err; best_maxnodes <- mn_val }
#             }
#             maxnodes_param <- best_maxnodes
#             cat(sprintf("Best maxnodes: %s\n", ifelse(is.null(maxnodes_param), "NULL (unlimited)", as.character(maxnodes_param))))
#           }
#           # sampsize tuning: pick best fraction from grid
#           sampsize_candidates <- if (!is.null(modelparameters$rf_grid_sampsize)) as.numeric(modelparameters$rf_grid_sampsize) else c(1.0)
#           if (length(sampsize_candidates) > 1) {
#             best_oob <- Inf
#             best_frac <- 1.0
#             for (sf in sampsize_candidates) {
#               samp_sz <- round(nrow(x) * sf)
#               tmp_model <- randomForest(x = x, y = learningmodel[, 1],
#                                         ntree = ntree_param, mtry = optimal_mtry,
#                                         nodesize = nodesize_param, maxnodes = maxnodes_param,
#                                         sampsize = samp_sz, replace = replace_param, importance = FALSE)
#               oob_err <- mean(tmp_model$err.rate[, "OOB"])
#               cat(sprintf("  sampsize_frac=%.3f -> OOB=%.4f\n", sf, oob_err))
#               if (oob_err < best_oob) { best_oob <- oob_err; best_frac <- sf }
#             }
#             sampsize_frac <- best_frac
#             cat(sprintf("Best sampsize fraction: %.3f\n", sampsize_frac))
#           }
#         } else {
#           # ── Traditional mode: stratified CV over ntree, mtry, nodesize ──
#           ntree_values    <- if (!is.null(modelparameters$rf_ntree_range)) modelparameters$rf_ntree_range else c(500, 1000)
#           nodesize_values <- if (!is.null(modelparameters$rf_nodesize_range)) modelparameters$rf_nodesize_range else c(1)
#           p <- ncol(x)
#           mtry_values <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p / 3))))
#           
#           folds_rf   <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
#           pos_level  <- levels(learningmodel[, 1])[1]
#           best_score <- -Inf
#           optimal_mtry <- floor(sqrt(p))
#           
#           cat("Traditional stratified CV: searching over ntree, mtry and nodesize...\n")
#           for (nt in ntree_values) {
#             for (ns in nodesize_values) {
#               for (mt in mtry_values) {
#                 fold_scores <- sapply(folds_rf, function(test_idx) {
#                   train_idx <- setdiff(1:nrow(x), test_idx)
#                   tryCatch({
#                     mdl   <- randomForest(x = x[train_idx, , drop = FALSE], y = learningmodel[train_idx, 1],
#                                           ntree = nt, mtry = mt, nodesize = ns, importance = FALSE)
#                     probs <- randomForest:::predict.randomForest(mdl, x[test_idx, , drop = FALSE], type = "prob")
#                     compute_auc_manual(learningmodel[test_idx, 1], probs[, pos_level], pos_level)
#                   }, error = function(e) NA_real_)
#                 })
#                 sc <- mean(fold_scores, na.rm = TRUE)
#                 cat(sprintf("  ntree=%d, mtry=%d, nodesize=%d -> AUC=%.4f\n", nt, mt, ns, sc))
#                 if (!is.na(sc) && sc > best_score) {
#                   best_score     <- sc
#                   ntree_param    <- nt
#                   optimal_mtry   <- mt
#                   nodesize_param <- ns
#                 }
#               }
#             }
#           }
#           cat(sprintf("Best stratified CV params: ntree=%d, mtry=%d, nodesize=%d, AUC=%.4f\n",
#                       ntree_param, optimal_mtry, nodesize_param, best_score))
#         }
#       } else {
#         # ── Manual mode ──
#         optimal_mtry   <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
#         nodesize_param <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
#         maxnodes_param <- if (!is.null(modelparameters$maxnodes)) modelparameters$maxnodes else NULL
#         sampsize_frac  <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
#         replace_param  <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
#       }
#       
#       # Train final model
#       set.seed(20011203)
#       # Compute actual sampsize from fraction
#       sampsize_param <- if (!is.null(sampsize_frac) && sampsize_frac < 1.0) round(nrow(x) * sampsize_frac) else nrow(x)
#       
#       model <- randomForest(x = x, y = learningmodel[, 1],
#                             ntree = ntree_param, mtry = optimal_mtry,
#                             nodesize = nodesize_param, maxnodes = maxnodes_param,
#                             sampsize = sampsize_param, replace = replace_param,
#                             importance = TRUE)
#       model$optimal_mtry    <- optimal_mtry
#       model$ntree_used      <- ntree_param
#       model$nodesize_used   <- nodesize_param
#       model$maxnodes_used   <- maxnodes_param
#       model$sampsize_used   <- sampsize_param
#       model$replace_used    <- replace_param
#       
#       cat("  Optimal mtry:", optimal_mtry, "\n")
#       cat("  Number of trees:", modelparameters$ntree, "\n")
#       
#       # Make predictions
#       if(is_binary) {
#         # Classification binaire
#         scorelearning <- data.frame(model$votes[, lev["positif"]])
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#         predictclasslearning <- as.factor(predictclasslearning)
#       } else {
#         # Classification multi-classe
#         scorelearning <- model$votes  # Matrice n_samples x n_classes
#         #cat("scorelearning dimensions: nombre de lignes : ", dim(scorelearning)[1]," et ncol : ", dim(scorelearning)[2],"\n")
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         # cat("scorelearning dans le train  : \n")
#         # print(head(scorelearning))
#         # Prédiction = classe avec la probabilité maximale
#         predictclasslearning <- randomForest:::predict.randomForest(model, learningmodel)
#         predictclasslearning <- model$predicted
#         print(model$confusion[, -end(colnames(model$confusion))[1]])
#         predictclasslearning <- as.factor(predictclasslearning)
#         cat("affchage de la prediction \n")
#         print(head(predictclasslearning) )
#         cat("class of predictclasslearning :", class(predictclasslearning), "\n" )
#         cat("predictclasslearning dimensions: nombre de lignes : ", dim(predictclasslearning)[1]," et ncol : ", dim(predictclasslearning)[2],"\n")
#       }
#     }
#     
#     
#     if(modelparameters$modeltype == "svm"){
#       cat("\n--- Training SVM ---\n")
#       
#       
#       # Déterminer les hyperparamètres
#       if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
#         kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
#         svm_scoring <- if (!is.null(modelparameters$svm_scoring)) modelparameters$svm_scoring else "auc"
#         svm_gamma_range <- if (!is.null(modelparameters$svm_gamma_range)) modelparameters$svm_gamma_range else 10^(-5:2)
#         svm_cost_range  <- if (!is.null(modelparameters$svm_cost_range))  modelparameters$svm_cost_range  else 10^(-3:2)
#         tune_result <- tune_svm_stratified(data = learningmodel,
#                                            gamma_range = svm_gamma_range,
#                                            cost_range  = svm_cost_range,
#                                            kernel      = kernel_param,
#                                            n_folds     = min(5, nrow(learningmodel) - 1),
#                                            scoring     = svm_scoring,
#                                            seed        = 123)
#         cat("Stratified SVM tuning results:\n"); print(tune_result$best.parameters)
#         cost_param   <- tune_result$best.parameters$cost
#         gamma_param  <- tune_result$best.parameters$gamma
#       } else {
#         cat("define svm parameters manually \n")
#         cost_param   <- ifelse(is.null(modelparameters$cost),   1,       modelparameters$cost)
#         gamma_param  <- ifelse(is.null(modelparameters$gamma),  0.1,     modelparameters$gamma)
#         kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
#         # epsilon_param <- ifelse(is.null(modelparameters$epsilon), 0.1, modelparameters$epsilon)
#       }
#       
#       # Train model
#       if(is_binary) {
#         # Classification binaire - sans probabilités
#         model <- svm(x = learningmodel[,-1], 
#                      y = learningmodel[,1],
#                      kernel = "radial", # kernel_model, 
#                      cost = cost_param, 
#                      gamma = gamma_param,
#                      probability = FALSE)
#         
#         model$cost <- cost_param
#         model$gamma <- gamma_param
#         # model$kernel <- kernel_model
#         
#         scorelearning <- data.frame(as.vector(model$decision.values))
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         # Classification multi-classe - avec probabilités
#         cat("  Multi-class SVM with probability=TRUE\n")
#         print(colnames(learningmodel))
#         model <- svm( #group ~ ., data = learningmodel,
#                       x = learningmodel[,-1], 
#                       y = learningmodel[,1],
#                      kernel = kernel_param, 
#                      cost = cost_param, 
#                      gamma = gamma_param,
#                      scale = FALSE,
#                      probability = TRUE)
#         
#         cat("affchage du modele svm  :  \n")
#         print(model)
#         
#         model$cost <- cost_param
#         model$gamma <- gamma_param
#         # model$kernel <- kernel_model
#         
#         # Obtenir les probabilités
#         # tryCatch({
#           pred_with_prob <- e1071:::predict.svm(model, learningmodel[,-1], probability = TRUE)
#           
#         # }, error =  function(e){
#         #   print(e$message)
#         #   pred_with_prob = data.frame()
#         # } )  
#         #validate(need(nrow(pred_with_prob)>0, "Erreur: Le modèle SVM n'a pas pu générer de prédictions. Vérifiez vos données et paramètres."))
#         cat("section ajustement du modèle svm : affichage des predictions , \n")
#         print(pred_with_prob)
#         scorelearning <- attr(pred_with_prob, "probabilities")
#         
#         # Réorganiser les colonnes pour correspondre à l'ordre de lev
#         if(!is.null(colnames(scorelearning))) {
#           col_order <- match(lev, colnames(scorelearning))
#           if(!any(is.na(col_order))) {
#             scorelearning <- scorelearning[, col_order, drop = FALSE]
#           }
#         }
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         # Prédiction = classe avec probabilité maximale
#         predictclasslearning <- pred_with_prob
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#     
#     if(modelparameters$modeltype == "elasticnet"){
#       cat("\n--- Training ElasticNet ---\n")
#       
#       x <- as.matrix(learningmodel[,-1])
#       
#       # Déterminer la famille
#       if(is_binary) {
#         y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
#         family_param <- "binomial"
#         type_measure <- "auc"
#         cat("  Family: binomial\n")
#       } else {
#         y <- learningmodel[,1]
#         family_param <- "multinomial"
#         type_measure <- "class"
#         cat("  Family: multinomial (", n_classes, "classes)\n")
#       }
#       
#       # Get hyperparameters
#       alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
#       lambda_param <- modelparameters$lambda
#       
#       cat("  Alpha:", alpha_param, "\n")
#       
#       # Check if GridSearchCV should be used
#       if(!is.null(modelparameters$use_gridsearch) && 
#          modelparameters$use_gridsearch && 
#          is.null(lambda_param)){
#         
#         cat("  Using GridSearchCV for hyperparameter tuning...\n")
#         
#         param_grid <- list(
#           alpha = if(!is.null(modelparameters$en_grid_alpha)) {
#             modelparameters$en_grid_alpha
#           } else {
#             c(0, 0.25, 0.5, 0.75, 1.0)
#           },
#           lambda = if(!is.null(modelparameters$en_grid_lambda)) {
#             modelparameters$en_grid_lambda
#           } else {
#             c(0.001, 0.01, 0.1, 1.0)
#           }
#         )
#         
#         grid_result <- tryCatch({
#           X_df <- as.data.frame(x)
#           tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
#                                      param_grid = param_grid,
#                                      n_folds = 5,
#                                      scoring = c("auc", "accuracy"))
#         }, error = function(e) {
#           cat("  GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
#           NULL
#         })
#         
#         if(!is.null(grid_result)) {
#           best_params <- grid_result$best_params
#           alpha_param <- if(!is.null(best_params$alpha)) best_params$alpha else 0.5
#           lambda_param <- if(!is.null(best_params$lambda)) best_params$lambda else NULL
#           
#           cat(sprintf("  GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
#                       alpha_param, lambda_param, grid_result$best_score))
#         }
#       }
#       
#       # Fit model
#       if(is.null(lambda_param)){
#         # Perform cross-validation
#         cat("  Performing cross-validation for lambda...\n")
#         set.seed(20011203)
#         cvfit <- cv.glmnet(x, y, 
#                            family = family_param, 
#                            alpha = alpha_param,
#                            type.measure = type_measure, 
#                            nfolds = min(10, nrow(learningmodel)-1))
#         lambda_param <- cvfit$lambda.min
#         model <- list(glmnet_model = cvfit, 
#                       lambda = lambda_param, 
#                       alpha = alpha_param,
#                       cvfit = cvfit, 
#                       optimal_lambda = lambda_param, 
#                       lambda_1se = cvfit$lambda.1se)
#         cat("  Optimal lambda:", lambda_param, "\n")
#       } else {
#         # Manual lambda
#         cat("  Using manual lambda:", lambda_param, "\n")
#         fit <- glmnet(x, y, 
#                       family = family_param, 
#                       alpha = alpha_param, 
#                       lambda = lambda_param)
#         model <- list(glmnet_model = fit, 
#                       lambda = lambda_param, 
#                       alpha = alpha_param,
#                       cvfit = NULL, 
#                       optimal_lambda = lambda_param, 
#                       lambda_1se = NULL)
#       }
#       
#       # Feature selection
#       if(modelparameters$fs){
#         cat("  Performing feature selection...\n")
#         
#         if(is_binary) {
#           # Binaire: simple vecteur de coefficients
#           coef_values <- as.matrix(coef(model$glmnet_model, s = lambda_param))
#           selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
#         } else {
#           # Multi-classe: liste de matrices de coefficients
#           coef_list <- coef(model$glmnet_model, s = lambda_param)
#           all_selected <- c()
#           for(i in 1:n_classes) {
#             coef_matrix <- as.matrix(coef_list[[i]])
#             class_selected <- rownames(coef_matrix)[which(coef_matrix[-1,1] != 0)]
#             all_selected <- c(all_selected, class_selected)
#           }
#           selected_features <- unique(all_selected)
#         }
#         
#         if(length(selected_features) > 0){
#           cat("  Selected", length(selected_features), "features\n")
#           learningmodel <- learningmodel[, c("group", selected_features)]
#           x <- as.matrix(learningmodel[,-1])
#           
#           # Refit model with selected features
#           if(is.null(modelparameters$lambda)){
#             cvfit <- cv.glmnet(x, y, 
#                                family = family_param, 
#                                alpha = alpha_param,
#                                type.measure = type_measure, 
#                                nfolds = min(10, nrow(learningmodel)-1))
#             lambda_param <- cvfit$lambda.min
#             fit <- glmnet(x, y, 
#                           family = family_param, 
#                           alpha = alpha_param, 
#                           lambda = lambda_param)
#             model <- list(glmnet_model = fit, 
#                           lambda = lambda_param, 
#                           alpha = alpha_param,
#                           cvfit = cvfit, 
#                           optimal_lambda = lambda_param, 
#                           lambda_1se = cvfit$lambda.1se)
#           } else {
#             fit <- glmnet(x, y, 
#                           family = family_param, 
#                           alpha = alpha_param, 
#                           lambda = lambda_param)
#             model <- list(glmnet_model = fit, 
#                           lambda = lambda_param, 
#                           alpha = alpha_param,
#                           cvfit = NULL, 
#                           optimal_lambda = lambda_param, 
#                           lambda_1se = NULL)
#           }
#         } else {
#           cat("  Warning: No features selected, keeping all features\n")
#         }
#       }
#       
#       # Make predictions
#       if(is_binary) {
#         # Classification binaire
#         if(inherits(model$glmnet_model, "cv.glmnet")){
#           scorelearning <- as.vector( glmnet:::predict.cv.glmnet(model$glmnet_model, newx = x, 
#                                              s = lambda_param, type = "response"))
#         } else {
#           scorelearning <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx = x, 
#                                              s = lambda_param, type = "response"))
#         }
#         scorelearning <- data.frame(scorelearning)
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         # Classification multi-classe
#         if(inherits(model$glmnet_model, "cv.glmnet")){
#           score_array <- glmnet:::predict.cv.glmnet(model$glmnet_model, newx = x, 
#                                  s = lambda_param, type = "response")
#         } else {
#           score_array <- glmnet::predict.glmnet(model$glmnet_model, newx = x, 
#                                  s = lambda_param, type = "response")
#         }
#         
#         # Extraire la matrice 2D
#         if(is.array(score_array) && length(dim(score_array)) == 3) {
#           scorelearning <- score_array[,,1]
#         } else if(is.matrix(score_array)) {
#           scorelearning <- score_array
#         } else {
#           scorelearning <- as.matrix(score_array)
#         }
#         
#         # Réorganiser les colonnes si nécessaire
#         if(ncol(scorelearning) == n_classes && !is.null(colnames(scorelearning))) {
#           col_order <- match(lev, colnames(scorelearning))
#           if(!any(is.na(col_order))) {
#             scorelearning <- scorelearning[, col_order, drop = FALSE]
#           }
#         }
#         
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         # Prédiction = classe avec probabilité maximale
#         class_indices <- apply(scorelearning, 1, which.max)
#         predictclasslearning <- factor(lev[class_indices], levels = lev)
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#     
#     if(modelparameters$modeltype == "xgboost"){
#       cat("\n--- Training XGBoost ---\n")
#       
#       x <- as.matrix(learningmodel[,-1])
#       
#       if(is_binary) {
#         y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
#         objective_param <- "binary:logistic"
#         eval_metric_param <- "auc"
#         cat("  Objective: binary:logistic\n")
#       } else {
#         # Encoder les classes en 0, 1, 2, ...
#         y <- as.numeric(learningmodel[,1]) - 1
#         objective_param <- "multi:softprob"
#         eval_metric_param <- "mlogloss"
#         cat("  Objective: multi:softprob (", n_classes, "classes)\n")
#       }
#       
#       dtrain <- xgb.DMatrix(data = x, label = y)
#       
#       # Déterminer les hyperparamètres
#       if(is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb){
#         
#         if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
#           cat("  Using GridSearchCV for hyperparameter tuning...\n")
#           
#           param_grid <- list(
#             n_estimators = if(!is.null(modelparameters$xgb_grid_nrounds)) {
#               modelparameters$xgb_grid_nrounds
#             } else {
#               c(50, 100, 200)
#             },
#             max_depth = if(!is.null(modelparameters$xgb_grid_maxdepth)) {
#               modelparameters$xgb_grid_maxdepth
#             } else {
#               c(3, 6, 9)
#             },
#             learning_rate = if(!is.null(modelparameters$xgb_grid_eta)) {
#               modelparameters$xgb_grid_eta
#             } else {
#               c(0.01, 0.1, 0.3)
#             }
#           )
#           
#           grid_result <- tryCatch({
#             X_df <- as.data.frame(x)
#             tune_xgb_gridsearch(X = X_df, y = learningmodel[,1],
#                                 param_grid = param_grid,
#                                 n_folds = 5,
#                                 scoring = c("auc", "accuracy"))
#           }, error = function(e) {
#             cat("  GridSearchCV failed, falling back to xgb.cv:", e$message, "\n")
#             NULL
#           })
#           
#           if(!is.null(grid_result)) {
#             best_params <- grid_result$best_params
#             nrounds_param <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else 100
#             maxdepth_param <- if(!is.null(best_params$max_depth)) best_params$max_depth else 6
#             eta_param <- if(!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
#             
#             cat(sprintf("  GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, score=%.4f\n",
#                         nrounds_param, maxdepth_param, eta_param, grid_result$best_score))
#           } else {
#             # Fallback to CV
#             set.seed(20011203)
#             cv_params <- list(
#               objective = objective_param,
#               eval_metric = eval_metric_param,
#               max_depth = 6,
#               eta = 0.3
#             )
#             if(!is_binary) cv_params$num_class <- n_classes
#             
#             cv_result <- xgb.cv(
#               params = cv_params,
#               data = dtrain,
#               nrounds = 200,
#               nfold = 5,
#               early_stopping_rounds = 10,
#               verbose = 0
#             )
#             
#             nrounds_param <- cv_result$best_iteration
#             maxdepth_param <- 6
#             eta_param <- 0.3
#           }
#         } else {
#           # Traditional CV
#           cat("  Performing cross-validation...\n")
#           set.seed(20011203)
#           cv_params <- list(
#             objective = objective_param,
#             eval_metric = eval_metric_param,
#             max_depth = 6,
#             eta = 0.3
#           )
#           if(!is_binary) cv_params$num_class <- n_classes
#           
#           cv_result <- xgb.cv(
#             params = cv_params,
#             data = dtrain,
#             nrounds = 200,
#             nfold = 5,
#             early_stopping_rounds = 10,
#             verbose = 0
#           )
#           
#           nrounds_param <- cv_result$best_iteration
#           maxdepth_param <- 6
#           eta_param <- 0.3
#         }
#         
#         minchild_param <- 1
#         subsample_param <- 0.8
#         colsample_param <- 0.8
#       } else {
#         # Manual mode
#         nrounds_param <- if(!is.null(modelparameters$nrounds)) modelparameters$nrounds else 100
#         maxdepth_param <- if(!is.null(modelparameters$max_depth)) modelparameters$max_depth else 6
#         eta_param <- if(!is.null(modelparameters$eta)) modelparameters$eta else 0.3
#         minchild_param <- 1
#         subsample_param <- 0.8
#         colsample_param <- 0.8
#         
#         cat("  Using manual parameters:\n")
#         cat("  nrounds:", nrounds_param, "\n")
#         cat("  max_depth:", maxdepth_param, "\n")
#         cat("  eta:", eta_param, "\n")
#       }
#       
#       # Train final model
#       params <- list(
#         objective = objective_param,
#         eval_metric = eval_metric_param,
#         max_depth = maxdepth_param,
#         eta = eta_param,
#         min_child_weight = minchild_param,
#         subsample = subsample_param,
#         colsample_bytree = colsample_param
#       )
#       
#       if(!is_binary) {
#         params$num_class <- n_classes
#       }
#       
#       set.seed(20011203)
#       model <- xgb.train(
#         params = params,
#         data = dtrain,
#         nrounds = nrounds_param,
#         verbose = 0
#       )
#       
#       # Store optimal parameters
#       model$optimal_nrounds <- nrounds_param
#       model$optimal_max_depth <- maxdepth_param
#       model$optimal_eta <- eta_param
#       model$optimal_min_child_weight <- minchild_param
#       
#       cat("  Optimal parameters:\n")
#       cat("    nrounds:", nrounds_param, "\n")
#       cat("    max_depth:", maxdepth_param, "\n")
#       cat("    eta:", eta_param, "\n")
#       
#       # Make predictions
#       if(is_binary) {
#         scorelearning <- xgboost:::predict.xgb.Booster(model, x)
#         scorelearning <- data.frame(scorelearning)
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         # Prédictions multi-classe (matrice de probabilités)
#         score_matrix <- xgboost:::predict.xgb.Booster(model, x, reshape = TRUE)
#         scorelearning <- score_matrix
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         # Prédiction = classe avec probabilité maximale
#         class_indices <- apply(scorelearning, 1, which.max)
#         predictclasslearning <- factor(lev[class_indices], levels = lev)
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#    
#     if(modelparameters$modeltype == "lightgbm"){
#       cat("\n--- Training LightGBM ---\n")
#       
#       x <- as.matrix(learningmodel[,-1])
#       
#       if(is_binary) {
#         y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
#         objective_param <- "binary"
#         metric_param <- "auc"
#         cat("  Objective: binary\n")
#       } else {
#         y <- as.numeric(learningmodel[,1]) - 1
#         objective_param <- "multiclass"
#         metric_param <- "multi_logloss"
#         cat("  Objective: multiclass (", n_classes, "classes)\n")
#       }
#       
#       dtrain <- lgb.Dataset(data = x, label = y)
#       
#       # Déterminer les hyperparamètres
#       if(is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb){
#         cat("  Performing cross-validation...\n")
#         
#         # Simple CV with default parameters
#         params_cv <- list(
#           objective = objective_param,
#           metric = metric_param,
#           num_leaves = 31,
#           learning_rate = 0.1,
#           feature_fraction = 0.9,
#           bagging_fraction = 0.8,
#           bagging_freq = 5,
#           verbose = -1
#         )
#         if(!is_binary) params_cv$num_class <- n_classes
#         
#         set.seed(20011203)
#         cv_result <- lgb.cv(
#           params = params_cv,
#           data = dtrain,
#           nrounds = 200,
#           nfold = 5,
#           early_stopping_rounds = 10,
#           verbose = -1
#         )
#         
#         nrounds_param <- cv_result$best_iter
#         num_leaves_param <- 31
#         learning_rate_param <- 0.1
#       } else {
#         # Manual mode
#         nrounds_param <- if(!is.null(modelparameters$nrounds_lgb)) modelparameters$nrounds_lgb else 100
#         num_leaves_param <- if(!is.null(modelparameters$num_leaves)) modelparameters$num_leaves else 31
#         learning_rate_param <- if(!is.null(modelparameters$learning_rate_lgb)) modelparameters$learning_rate_lgb else 0.1
#         
#         cat("  Using manual parameters:\n")
#         cat("  nrounds:", nrounds_param, "\n")
#         cat("  num_leaves:", num_leaves_param, "\n")
#         cat("  learning_rate:", learning_rate_param, "\n")
#       }
#       
#       # Train final model
#       params <- list(
#         objective = objective_param,
#         metric = metric_param,
#         num_leaves = num_leaves_param,
#         learning_rate = learning_rate_param,
#         feature_fraction = 0.9,
#         bagging_fraction = 0.8,
#         bagging_freq = 5,
#         verbose = -1
#       )
#       
#       if(!is_binary) {
#         params$num_class <- n_classes
#       }
#       
#       set.seed(20011203)
#       model <- lgb.train(
#         params = params,
#         data = dtrain,
#         nrounds = nrounds_param,
#         verbose = -1
#       )
#       
#       # Store optimal parameters
#       model$optimal_nrounds <- nrounds_param
#       model$optimal_num_leaves <- num_leaves_param
#       model$optimal_learning_rate <- learning_rate_param
#       
#       cat("  Optimal parameters:\n")
#       cat("    nrounds:", nrounds_param, "\n")
#       cat("    num_leaves:", num_leaves_param, "\n")
#       cat("    learning_rate:", learning_rate_param, "\n")
#       
#       # Make predictions
#       if(is_binary) {
#         scorelearning <- predict(model, x)
#         scorelearning <- data.frame(scorelearning)
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         score_matrix <- predict(model, x, reshape = TRUE)
#         scorelearning <- score_matrix
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         class_indices <- apply(scorelearning, 1, which.max)
#         predictclasslearning <- factor(lev[class_indices], levels = lev)
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#     if(modelparameters$modeltype == "naivebayes"){
#       cat("\n--- Training Naive Bayes ---\n")
#       
#       # Determine laplace parameter
#       optimal_laplace <- 0
#       
#       if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
#         cat("  Using GridSearchCV for hyperparameter tuning...\n")
#         
#         param_grid <- list(
#           laplace = if(!is.null(modelparameters$nb_grid_laplace)) {
#             modelparameters$nb_grid_laplace
#           } else {
#             c(0, 0.5, 1, 2, 5)
#           }
#         )
#         
#         grid_result <- tryCatch({
#           X_df <- as.data.frame(learningmodel[,-1])
#           tune_nb_gridsearch(X = X_df, y = learningmodel[,1],
#                              param_grid = param_grid,
#                              n_folds = 5,
#                              scoring = c("auc", "accuracy"))
#         }, error = function(e) {
#           cat("  GridSearchCV failed, using default laplace=0:", e$message, "\n")
#           NULL
#         })
#         
#         if(!is.null(grid_result)) {
#           best_params <- grid_result$best_params
#           optimal_laplace <- if(!is.null(best_params$laplace)) best_params$laplace else 0
#           cat(sprintf("  GridSearchCV best params: laplace=%.2f, score=%.4f\n",
#                       optimal_laplace, grid_result$best_score))
#         }
#       }
#       
#       # Build model
#       model <- naiveBayes(x = learningmodel[,-1], 
#                           y = learningmodel[,1], 
#                           laplace = optimal_laplace)
#       
#       model$model_type <- "naivebayes"
#       model$optimal_laplace <- optimal_laplace
#       
#       cat("  Laplace smoothing:", optimal_laplace, "\n")
#       
#       # Make predictions
#       pred_probs <- e1071:::predict.naiveBayes(model, learningmodel[,-1], type = "raw")
#       
#       if(is_binary) {
#         scorelearning <- data.frame(pred_probs[, lev["positif"]])
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         scorelearning <- pred_probs
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         class_indices <- apply(scorelearning, 1, which.max)
#         predictclasslearning <- factor(lev[class_indices], levels = lev)
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#     
#     if(modelparameters$modeltype == "knn"){
#       cat("\n--- Training KNN ---\n")
#       
#       # Determine k parameter
#       if(is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn){
#         
#         if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
#           cat("  Using GridSearchCV for hyperparameter tuning...\n")
#           
#           max_k <- min(floor(sqrt(nrow(learningmodel))), 30)
#           param_grid <- list(
#             n_neighbors = if(!is.null(modelparameters$knn_grid_k)) {
#               modelparameters$knn_grid_k
#             } else {
#               seq(3, max_k, by=2)
#             }
#           )
#           
#           grid_result <- tryCatch({
#             X_df <- as.data.frame(learningmodel[,-1])
#             tune_knn_gridsearch(X = X_df, y = learningmodel[,1],
#                                 param_grid = param_grid,
#                                 n_folds = 5,
#                                 scoring = c("auc", "accuracy"))
#           }, error = function(e) {
#             cat("  GridSearchCV failed, falling back to manual CV:", e$message, "\n")
#             NULL
#           })
#           
#           if(!is.null(grid_result)) {
#             best_params <- grid_result$best_params
#             optimal_k <- if(!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
#             cat(sprintf("  GridSearchCV best params: k=%d, score=%.4f\n",
#                         optimal_k, grid_result$best_score))
#           } else {
#             # Fallback to traditional CV
#             set.seed(20011203)
#             max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
#             k_values <- seq(3, max_k, by=2)
#             
#             best_k <- 3
#             best_acc <- 0
#             for(k_test in k_values){
#               n_folds <- min(5, nrow(learningmodel))
#               fold_size <- floor(nrow(learningmodel) / n_folds)
#               accuracies <- numeric(n_folds)
#               for(fold in 1:n_folds){
#                 test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
#                 train_idx <- setdiff(1:nrow(learningmodel), test_idx)
#                 pred <- class::knn(train = learningmodel[train_idx, -1],
#                             test = learningmodel[test_idx, -1],
#                             cl = learningmodel[train_idx, 1],
#                             k = k_test)
#                 accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
#               }
#               avg_acc <- mean(accuracies)
#               if(avg_acc > best_acc){
#                 best_acc <- avg_acc
#                 best_k <- k_test
#               }
#             }
#             optimal_k <- best_k
#           }
#         } else {
#           # Traditional CV
#           cat("  Performing cross-validation...\n")
#           set.seed(20011203)
#           max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
#           k_values <- seq(3, max_k, by=2)
#           
#           best_k <- 3
#           best_acc <- 0
#           for(k_test in k_values){
#             n_folds <- min(5, nrow(learningmodel))
#             fold_size <- floor(nrow(learningmodel) / n_folds)
#             accuracies <- numeric(n_folds)
#             for(fold in 1:n_folds){
#               test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
#               train_idx <- setdiff(1:nrow(learningmodel), test_idx)
#               pred <- class::knn(train = learningmodel[train_idx, -1],
#                           test = learningmodel[test_idx, -1],
#                           cl = learningmodel[train_idx, 1],
#                           k = k_test)
#               accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
#             }
#             avg_acc <- mean(accuracies)
#             if(avg_acc > best_acc){
#               best_acc <- avg_acc
#               best_k <- k_test
#             }
#           }
#           optimal_k <- best_k
#         }
#       } else {
#         # Manual mode
#         optimal_k <- if(!is.null(modelparameters$k_neighbors)) {
#           modelparameters$k_neighbors
#         } else {
#           5
#         }
#         cat("  Using manual k:", optimal_k, "\n")
#       }
#       
#       cat("  Optimal k:", optimal_k, "\n")
#       
#       # Store as model object
#       model <- list(
#         optimal_k = optimal_k,
#         train_data = learningmodel[, -1],
#         train_labels = learningmodel[, 1]
#       )
#       class(model) <- "knn_model"
#       
#       # Make predictions
#       predicted_classes <- class::knn(train = learningmodel[, -1],
#                                test = learningmodel[, -1],
#                                cl = learningmodel[, 1],
#                                k = optimal_k,
#                                prob = TRUE)
#       
#       probs <- attr(predicted_classes, "prob")
#       
#       if(is_binary) {
#         # Pour binaire, prob est la proba de la classe prédite
#         scorelearning <- ifelse(predicted_classes == lev["positif"], probs, 1 - probs)
#         scorelearning <- data.frame(scorelearning)
#         colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
#         
#         predictclasslearning <- factor(levels = lev)
#         predictclasslearning[which(scorelearning >= modelparameters$thresholdmodel)] <- lev["positif"]
#         predictclasslearning[which(scorelearning < modelparameters$thresholdmodel)] <- lev["negatif"]
#       } else {
#         # Pour multi-classe, créer une matrice de probabilités approximative
#         scorelearning <- matrix(0, nrow = nrow(learningmodel), ncol = n_classes)
#         for(i in 1:nrow(scorelearning)) {
#           class_idx <- which(lev == predicted_classes[i])
#           scorelearning[i, class_idx] <- probs[i]
#           # Distribuer le reste uniformément
#           other_idx <- setdiff(1:n_classes, class_idx)
#           if(length(other_idx) > 0) {
#             scorelearning[i, other_idx] <- (1 - probs[i]) / length(other_idx)
#           }
#         }
#         colnames(scorelearning) <- paste("Prob", lev, sep="_")
#         
#         predictclasslearning <- predicted_classes
#       }
#       predictclasslearning <- as.factor(predictclasslearning)
#     }
#     
#      
#     if(is_binary) {
#       # Format binaire original
#       reslearningmodel <- data.frame(classlearning, scorelearning, predictclasslearning)
#       colnames(reslearningmodel) <- c("classlearning", "scorelearning", "predictclasslearning")
#     } else {
#       # Format multi-classe
#       reslearningmodel <- data.frame(classlearning, scorelearning, predictclasslearning)
#       # reslearningmodel <- list(
#       #   "classlearning" = classlearning,
#       #   "scorelearning" = scorelearning,
#       #   "predictclasslearning" = predictclasslearning
#       # )
#       score_cols <- paste("score", lev, sep="_")
#       colnames(reslearningmodel) <- c("classlearning", score_cols, "predictclasslearning")
#     }
#     rownames(reslearningmodel) <- rownames(learningmodel)
#     
#     datalearningmodel <- list(
#       "learningmodel" = learningmodel,
#       "reslearningmodel" = reslearningmodel
#     )
#     
#     cat("\n=== Learning completed ===\n")
#     cat("Samples:", nrow(learningmodel), "\n")
#     cat("Features:", ncol(learningmodel)-1, "\n")
#     if(is_binary) {
#       cat("Score column:", colnames(scorelearning), "\n")
#     } else {
#       cat("Score columns:", paste(colnames(scorelearning), collapse=", "), "\n")
#     }
#     
#     # ==========================================================================
#     #  VALIDATION
#     # ==========================================================================
#     
#     datavalidationmodel <- NULL
#     
#     if(!is.null(validation)){
#       cat("\n=== Processing validation data ===\n")
#       
#       # Transform validation data
#       validationmodel <- validation
#       colnames(validationmodel)[1] <- "group"
#       validationmodel[,1] <- as.factor(as.character(validationmodel[,1]))
#       
#       # Apply transformations
#       if(transformdataparameters$rempNA == "0"){
#         validationmodel[,-1][is.na(validationmodel[,-1])] <- 0
#       } else if(transformdataparameters$rempNA %in% c("pca", "missforest")){
#         # Complex imputation for validation
#         if(!is.null(datastructuresfeatures) && ncol(datastructuresfeatures) > 0){
#           validationdatastructuresfeatures <- validation[, colnames(datastructuresfeatures)]
#           validationdatastructuresfeatures[is.na(validationdatastructuresfeatures)] <- 0
#           learningselectfull <- cbind(learningselect, datastructuresfeatures)
#           validationselectfull <- cbind(validation, validationdatastructuresfeatures)
#           
#           if(transformdataparameters$rempNA == "pca"){
#             res <- imputePCA(X = rbind(learningselectfull, validationselectfull)[,-1], 
#                              ncp = min(3, ncol(learningselectfull)-2))
#             validationmodel <- cbind(validationselectfull[,1], 
#                                      res$completeObs[(nrow(learningselectfull)+1):nrow(rbind(learningselectfull, validationselectfull)),])
#           } else if(transformdataparameters$rempNA == "missforest"){
#             res <- missForest(xmis = rbind(learningselectfull, validationselectfull)[,-1])
#             validationmodel <- cbind(validationselectfull[,1], 
#                                      res$ximp[(nrow(learningselectfull)+1):nrow(rbind(learningselectfull, validationselectfull)),])
#           }
#         } else {
#           validationmodel[,-1][is.na(validationmodel[,-1])] <- 0
#         }
#       }
#       
#       # Apply other transformations
#       if(transformdataparameters$log){
#         if(transformdataparameters$logtype == "log2"){
#           validationmodel[,-1] <- log2(validationmodel[,-1] + 1)
#         } else {
#           validationmodel[,-1] <- log(validationmodel[,-1] + 1)
#         }
#       }
#       if(transformdataparameters$arcsin){
#         validationmodel[,-1] <- asin(sqrt(validationmodel[,-1]))
#       }
#       if(transformdataparameters$standardization){
#         validationmodel[,-1] <- scale(validationmodel[,-1])
#       }
#       
#       classval <- validationmodel[,1]
#       
#       # Vérifications de débogage
#       cat("\n=== Validation class captured ===\n")
#       cat("classval type:", class(classval), "\n")
#       cat("classval levels:", paste(levels(classval), collapse=", "), "\n")
#       cat("classval head:", paste(head(classval), collapse=", "), "\n")
#       
#       # Select same features as training
#       common_features <- intersect(colnames(learningmodel), colnames(validationmodel))
#       cat("checking des colonnes de validation par rapport au modele d'apprentissage...\n")
#       print(intersect(colnames(learningmodel), colnames(validationmodel)))
#       validationmodel <- cbind(validationmodel[,1], validationmodel[, common_features])
#       colnames(validationmodel)[1] <- "group"
#       
#       # classval <- validationmodel[,1]
#       cat("varification que la premiere colonne est bien le groupe:\n")
#       print((table(validationmodel[,1])))
#       
#       # Make predictions based on model type
#       if(modelparameters$modeltype == "randomforest"){
#         if(is_binary) {
#           scoreval <- data.frame(randomForest:::predict.randomForest(model, validationmodel, type="prob")[, lev["positif"]])
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           scoreval <- randomForest:::predict.randomForest(model, validationmodel, type="prob")
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           predictclassval <- randomForest:::predict.randomForest(model, validationmodel)
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "svm"){
#         if(is_binary) {
#           scoreval <- data.frame(as.vector(e1071:::predict.svm(model, validationmodel[,-1], decision.values=TRUE)))
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           pred_with_prob <- e1071:::predict.svm(model, validationmodel[,-1], probability=TRUE)
#           scoreval <- attr(pred_with_prob, "probabilities")
#           if(!is.null(colnames(scoreval))) {
#             col_order <- match(lev, colnames(scoreval))
#             if(!any(is.na(col_order))) {
#               scoreval <- scoreval[, col_order, drop=FALSE]
#             }
#           }
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           predictclassval <- pred_with_prob
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "elasticnet"){
#         x_val <- as.matrix(validationmodel[,-1])
#         
#         if(is_binary) {
#           if(inherits(model$glmnet_model, "cv.glmnet")){
#             scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, 
#                                           s=model$lambda, type="response"))
#           } else {
#             scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, 
#                                           s=model$lambda, type="response"))
#           }
#           scoreval <- data.frame(scoreval)
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           if(inherits(model$glmnet_model, "cv.glmnet")){
#             score_array <- glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, 
#                                    s=model$lambda, type="response")
#           } else {
#             score_array <- glmnet::predict.glmnet(model$glmnet_model, newx=x_val, 
#                                    s=model$lambda, type="response")
#           }
#           
#           if(is.array(score_array) && length(dim(score_array)) == 3) {
#             scoreval <- score_array[,,1]
#           } else if(is.matrix(score_array)) {
#             scoreval <- score_array
#           } else {
#             scoreval <- as.matrix(score_array)
#           }
#           
#           if(ncol(scoreval) == n_classes && !is.null(colnames(scoreval))) {
#             col_order <- match(lev, colnames(scoreval))
#             if(!any(is.na(col_order))) {
#               scoreval <- scoreval[, col_order, drop=FALSE]
#             }
#           }
#           
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           class_indices <- apply(scoreval, 1, which.max)
#           predictclassval <- factor(lev[class_indices], levels = lev)
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "xgboost"){
#         x_val <- as.matrix(validationmodel[,-1])
#         
#         if(is_binary) {
#           scoreval <- xgboost:::predict.xgb.Booster(model, x_val)
#           scoreval <- data.frame(scoreval)
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           score_matrix <- xgboost:::predict.xgb.Booster(model, x_val, reshape = TRUE)
#           scoreval <- score_matrix
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           
#           class_indices <- apply(scoreval, 1, which.max)
#           predictclassval <- factor(lev[class_indices], levels = lev)
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "lightgbm"){
#         x_val <- as.matrix(validationmodel[,-1])
#         
#         if(is_binary) {
#           scoreval <- predict(model, x_val)
#           scoreval <- data.frame(scoreval)
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           score_matrix <- predict(model, x_val, reshape = TRUE)
#           scoreval <- score_matrix
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           
#           class_indices <- apply(scoreval, 1, which.max)
#           predictclassval <- factor(lev[class_indices], levels = lev)
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "naivebayes"){
#         pred_probs_val <- e1071:::predict.naiveBayes(model, validationmodel[,-1], type="raw")
#         
#         if(is_binary) {
#           scoreval <- data.frame(pred_probs_val[, lev["positif"]])
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           scoreval <- pred_probs_val
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           
#           class_indices <- apply(scoreval, 1, which.max)
#           predictclassval <- factor(lev[class_indices], levels = lev)
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       if(modelparameters$modeltype == "knn"){
#         predicted_classes_val <- class::knn(train = model$train_data,
#                                      test = validationmodel[, -1],
#                                      cl = model$train_labels,
#                                      k = model$optimal_k,
#                                      prob = TRUE)
#         
#         probs_val <- attr(predicted_classes_val, "prob")
#         
#         if(is_binary) {
#           scoreval <- ifelse(predicted_classes_val == lev["positif"], probs_val, 1 - probs_val)
#           scoreval <- data.frame(scoreval)
#           colnames(scoreval) <- paste(lev[1],"/",lev[2],sep="")
#           
#           predictclassval <- factor(levels = lev)
#           predictclassval[which(scoreval >= modelparameters$thresholdmodel)] <- lev["positif"]
#           predictclassval[which(scoreval < modelparameters$thresholdmodel)] <- lev["negatif"]
#         } else {
#           scoreval <- matrix(0, nrow = nrow(validationmodel), ncol = n_classes)
#           for(i in 1:nrow(scoreval)) {
#             class_idx <- which(lev == predicted_classes_val[i])
#             scoreval[i, class_idx] <- probs_val[i]
#             other_idx <- setdiff(1:n_classes, class_idx)
#             if(length(other_idx) > 0) {
#               scoreval[i, other_idx] <- (1 - probs_val[i]) / length(other_idx)
#             }
#           }
#           colnames(scoreval) <- paste("Prob", lev, sep="_")
#           
#           predictclassval <- predicted_classes_val
#         }
#         predictclassval <- as.factor(predictclassval)
#       }
#       
#       # Format validation results
#       if(is_binary) {
#         resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
#         colnames(resvalidationmodel) <- c("classval", "scoreval", "predictclassval")
#       } else {
#         resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
#         score_cols <- paste("score", lev, sep="_")
#         colnames(resvalidationmodel) <- c("classval", score_cols, "predictclassval")
#       }
#       rownames(resvalidationmodel) <- rownames(validationmodel)
#       
#       datavalidationmodel <- list(
#         "validationmodel" = validationmodel,  # contient les features et la classe
#         "resvalidationmodel" = resvalidationmodel # contient les résultats de la prédiction
#       )
#       
#       cat("Validation samples:", nrow(validationmodel), "\n")
#       cat("Validation completed\n")
#     }
#     
#     # ==========================================================================
#     # RETURN
#     # ==========================================================================
#     
#     cat("\n=== Model training completed successfully ===\n\n")
#     
#     return(list(
#       "datalearningmodel" = datalearningmodel,
#       "model" = model,
#       "datavalidationmodel" = datavalidationmodel,
#       "groups" = if(is_binary) groups else lev,
#       "modelparameters" = modelparameters,
#       "n_classes" = n_classes,
#       "is_binary" = is_binary
#     ))
#     
#   }
# }

replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-validationdiff
  for(i in 1:nrow(validationdiff)){
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]        }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}


ROCcurve <- function(validation, decisionvalues, maintitle="ROC Curves (One-vs-All)", graph=T, ggplot=T){
  # ROC curve for multi-class (One-vs-All approach)
  
  cat("=======================================================================\n")
  cat(" show decisionvalues  : \n ")
  print(decisionvalues)
  cat("validation :  \n")
  print(validation)
  cat("=======================================================================\n")
  
  validation <- factor(validation, levels = rev(levels(validation)), ordered = TRUE)
  n_classes <- length(levels(validation))
  
  # cat("affcihege de la n_classes:", n_classes, "\n")
  # cat("affichage de la tete des proba dans roccurve : \n")
  # print(head(decisionvalues))
  # Pour la classification binaire, garder le comportement original
  # if(n_classes == 2 && !is.matrix(decisionvalues)) {
  #   res <- roc(validation, decisionvalues, quiet = TRUE)
  #   auc_value <- as.numeric(auc(res))
  #   
  #   if(!graph) {
  #     df <- data.frame(sensitivities = res$sensitivities, 
  #                      specificities = res$specificities)
  #     return(df)
  #   }
  #   
  #   if(ggplot) {
  #     col <- gg_color_hue(2)
  #     bin <- 0.01
  #     diag <- data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
  #     df <- data.frame(
  #       sensitivities = res$sensitivities,
  #       specificities = res$specificities
  #     )
  #     
  #     p <- ggplot() +
  #       geom_line(data = diag, aes(x = x, y = y), color = col[2], linetype = "dashed") +
  #       geom_line(data = df, aes(x = 1 - specificities, y = sensitivities), 
  #                 color = col[1], size = 1) +
  #       labs(
  #         title = paste0(maintitle, " (AUC = ", round(auc_value, 3), ")"),
  #         x = "1 - Specificity (False Positive Rate)",
  #         y = "Sensitivity (True Positive Rate)"
  #       ) +
  #       theme_minimal() +
  #       theme(
  #         plot.title = element_text(size = 15, face = "bold"),
  #         axis.text = element_text(size = 12),
  #         axis.title = element_text(size = 14, face = "bold"),
  #         legend.position = c(0.8, 0.2),
  #         axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
  #         axis.text.y =  element_text(size = 12 , face =  'bold'),
  #         axis.title.x = element_text(size = 15 , face = 'bold'), 
  #         axis.title.y =  element_text(size = 15 , face = 'bold')
  #       ) +
  #       coord_fixed()
  #     
  #     return(p)
  #   }
  # }
  
  # Pour la classification multi-classe
  if(!is.matrix(decisionvalues)){
    # if(!graph){
    #   return(data.frame(message="Multi-class ROC requires probability matrix"))
    # }
    if(ggplot){
      p <- ggplot() +
        annotate("text", x=0.5, y=0.5, 
                 label="Multi-class ROC requires\nprobability matrix for each class", 
                 size=6) +
        labs(title = maintitle) +
        theme_minimal()
      return(p)
    }
  }
  
  # Calculer les courbes ROC One-vs-All pour chaque classe
  roc_list <- list()
  auc_values <- vector()
  class_names <- levels(validation)
  roc_data <- data.frame()
  
  for(i in 1:n_classes){
    # Créer un indicateur binaire pour cette classe
    binary_response <- ifelse(as.numeric(validation) == (n_classes - i + 1), 1, 0)
    
    # Obtenir les valeurs de décision pour cette classe
    class_scores <- decisionvalues[, i]
    
    # Calculer la ROC
    roc_obj <- tryCatch({
      pROC::roc(binary_response, class_scores, quiet=TRUE)
    }, error = function(e){
      return(NULL)
    })
    
    if(!is.null(roc_obj)){
      class_name <- class_names[n_classes - i + 1]
      roc_list[[class_name]] <- roc_obj
      auc_values[i] <- as.numeric(pROC::auc(roc_obj))
      
      # Ajouter les données pour le graphique
      temp_df <- data.frame(
        FPR = 1 - roc_obj$specificities,
        TPR = roc_obj$sensitivities,
        Class = class_name,
        AUC = auc_values[i],
        ClassNum = i
      )
      
      # CORRECTION CRITIQUE : Trier par FPR pour assurer la monotonicité
      temp_df <- temp_df[order(temp_df$FPR, temp_df$TPR), ]
      
      roc_data <- rbind(roc_data, temp_df)
    }
  }
  
  # Calculer l'AUC moyenne (macro-average)
  mean_auc <- mean(auc_values, na.rm=TRUE)
  
  # if(!graph){
  #   return(data.frame(
  #     class = names(roc_list),
  #     auc = auc_values,
  #     mean_auc = mean_auc
  #   ))
  # }
     
    
  if(ggplot){
    # Créer le graphique avec toutes les courbes ROC
    colords_ref = c("#FF4C4C", "#4C9AFF", "#FFA500", "#9B59FF", "#3EB489", 
                     "#33FF57",  "#FF1493",  "#FF4500", "#00CED1")
    col <-  colords_ref[1:(n_classes+1)] #gg_color_hue(n_classes + 1)
    bin <- 0.01
    diag <- data.frame(x = seq(0, 1, by = bin), y = seq(0, 1, by = bin))
    
    # Créer les labels pour la légende avec les AUC
    roc_data$Class_Label <- paste0(roc_data$Class, " (AUC = ", 
                                   round(roc_data$AUC, 3), ")")
    
    p <- ggplot() +
      geom_line(data = roc_data, 
                aes(x = FPR, y = TPR, color = Class_Label),
                linewidth = 1.5) +
       geom_line(data = diag, aes(x = x, y = y), 
                 color = "gray50", linetype = "dashed", size = 0.8) +
      labs(
        title = paste0(maintitle, " (Mean AUC = ", round(mean_auc, 3), ")"),
        x = "1 - Specificity",
        y = "Sensitivity",
        color = "Class"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "#F8F9FA",color =  "white"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
        axis.text.y =  element_text(size = 12 , face =  'bold'),
        axis.title.x = element_text(size = 15 , face = 'bold'), 
        axis.title.y =  element_text(size = 15 , face = 'bold')
      ) +
       #coord_fixed() +
      scale_color_manual(values = col[1:n_classes])
    
    return(p)
  }else{
    roc_data
  }
}


scoremodelplot<-function(class,score,names,threshold,type,graph,printnames){
  class<-factor(class,levels =rev(levels(class)))

  if(type=="boxplot"){
    boxplotggplot(class =class,score =score,names=names,threshold=threshold,
                  graph = graph)
  }
  else if(type=="points"){
    plot_pred_type_distribution(class = class, score = score,names=names,threshold=threshold,graph=graph,printnames=printnames  )
  } 
}

boxplotggplot<-function(class,score,names,threshold,maintitle="Score representation ",graph=T){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") ) +
    geom_boxplot(aes(fill=class)) +
    geom_hline(yintercept = threshold, color='red', alpha=0.6) +
    ggtitle(maintitle) + 
    theme(plot.title=element_text( size=15), 
          axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
          axis.text.y =  element_text(size = 12 , face =  'bold'),
          axis.title.x = element_text(size = 15 , face = 'bold'), 
          axis.title.y =  element_text(size = 15 , face = 'bold'),
          legend.text = element_text( size = 12 , face = 'bold'),
          legend.title = element_text(size = 14 , face =  'bold'))
  
  p
}

plot_pred_type_distribution <- function(class,score,names, threshold,maintitle="Score representation",printnames=F,graph=T) {
  #in this function the levels of the class is inverted in order to have the control group on the left side of the graph
  df<-data.frame(names,class,score)
  colnames(df)<-c("names","class","score")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$score >= threshold & df$class == levels(class)[2], "TruePositiv", v)
  v <- ifelse(df$score >= threshold & df$class == levels(class)[1], "FalsePositiv", v)
  v <- ifelse(df$score < threshold & df$class ==  levels(class)[2], "FalseNegativ", v)
  v <- ifelse(df$score < threshold & df$class == levels(class)[1], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  if(printnames){
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_text(label=names,colour=palet(df$predtype,multiple = TRUE))+
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15),
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  else{
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15), 
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  g
  
}

palet<-function(predtype,multiple=FALSE){
  if(multiple){col<-as.character(predtype)}
  else{col<-sort(unique(as.character(predtype)))}
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}


selectedfeature<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype="learn"){
  rmvar<-testmodel(model=model,modeltype = modeltype,tab=tab,validation=validation,
                   criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  i=0
  tabdiff2<-tab
  while(rmvar!=0){
    i<-i+1
    print(paste(i,"eliminates features"))
    tabdiff2<-tabdiff2[,-rmvar]
    if(modeltype=="svm"){
      tune_result <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                             gamma = 10^(-5:2), cost = 10^(-3:2),
                             cross=min(dim(tabdiff2)[1]-2,10))
      model <- tune_result$best.model
      model$cost <- tune_result$best.parameters$cost
      model$gamma <- tune_result$best.parameters$gamma
    }
    if (modeltype=="randomforest"){      
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T)
    }
      rmvar<-testmodel(model=model,modeltype = modeltype,tab=tabdiff2,validation=validation,
                     criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  }
  res<-list("dataset"=tabdiff2,"model"=model)
  return(res)
}


testmodel<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype){
  #retourn la variable a enlever
  importancevar<-importancemodelsvm(model = model,modeltype=modeltype,tabdiff=tab,criterion = criterionimportance)
  lessimportantevar<-which(importancevar==min(importancevar,na.rm =T) )
  test<-vector()
  if(modeltype=="svm"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$fitted)}
    if(criterionmodel=="auc"){
      if (fstype=='learn'){aucmod<-pROC::auc(pROC::roc(tab[,1], as.vector(model$decision.values),quiet=T))}
      if (fstype=='val'){
        print("")
        #predict sur la validation
        #mais pour ca validation doit etre = a validationmodel, avec toute les transformation
        }}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tune_result_diff <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                                  gamma = 10^(-5:2), cost = 10^(-3:2),
                                  cross=min(dim(tabdiff2)[1]-2,10))
      resmodeldiff <- tune_result_diff$best.model
      if(criterionmodel=="accuracy"){test[i]<-resmodeldiff$tot.accuracy-model$tot.accuracy}
      if(criterionmodel=="BER"){
        #print(paste("Ber test :",BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted) ))
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted)}
      if(criterionmodel=="auc"){
        test[i]<-pROC::auc(pROC::roc(tabdiff2[,1], as.vector(resmodeldiff$decision.values),quiet=T))-aucmod}
    }}
  if(modeltype=="randomforest"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$predicted)}
    if(criterionmodel=="auc"){aucmod<-pROC::auc(pROC::roc(tab[,1], as.vector(model$votes[,1]),quiet=T))}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      resmodeldiff <-randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T,trace=T)
      if(criterionmodel=="accuracy"){test[i]<-mean(resmodeldiff$confusion[,3])-mean(model$confusion[,3])}
      if(criterionmodel=="BER"){
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$predicted)}
      if(criterionmodel=="auc"){
        test[i]<-pROC::auc(pROC::roc(tabdiff2[,1], as.vector(resmodeldiff$votes[,1]),quiet=T))-aucmod}
    }
  }
  #print(paste("test :",max(test)))
  if(max(test)>=0){num<-lessimportantevar[which(test==max(test))[1]]}
  else(num<-0)
  #print(paste( "num", num))
  return(num)
} 

importancemodelsvm<-function(model,modeltype,tabdiff,criterion){
  #function calculate the importance of each variable of the model
  #first column of tabdiff is the group
  importancevar<-vector()
  if(criterion=="accuracy"){
    if(modeltype=="svm"){
      for (i in 2:ncol(tabdiff)){
        vec<-vector()
        tabdiffmodif<-tabdiff
        for( j in 1:20){
          tabdiffmodif[,i]<-tabdiffmodif[sample(1:nrow(tabdiff)),i]
          #tabdiffmodif<-tabdiffmodif[,-i]
          
          resmodeldiff<-svm(y =tabdiffmodif[,1],x=tabdiffmodif[,-1],cross=10,
                            type ="C-classification",
                            kernel= ifelse(is.null(model$kernel),"radial",model$kernel),
                            cost=model$cost,
                            gamma=model$gamma)
          vec[j]<-abs(resmodeldiff$tot.accuracy-model$tot.accuracy)
        }
        importancevar[i]<-mean(vec)}
      
    }
    if(modeltype=="randomforest"){
      
      tabdiff<-as.data.frame(tabdiff[,c(colnames(tabdiff)[1],sort(colnames(tabdiff[,-1])))])
      tabdiff<-as.data.frame(tabdiff2[sort(rownames(tabdiff)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff[,-1],tabdiff[,1],ntree=1000,importance=T,keep.forest=T)
      importancevar<-model$importance[,4]
      importancevar<-c(NA,importancevar)
    }
  }
  if(criterion=="fscore"){
    importancevar<-Fscore(tab = as.data.frame(tabdiff[,-1]),class=tabdiff[,1])
  }
  return(importancevar)
}

Fscore<-function(tab,class){
  tabpos<-as.data.frame(tab[which(class==levels(class)[1]),])
  npos<-nrow(tabpos)
  tabneg<-as.data.frame(tab[which(class==levels(class)[2]),])
  nneg<-nrow(tabneg)
  fscore<-vector()
  for(i in 1:ncol(tab)){
    moypos<-mean(tabpos[,i])
    moyneg<-mean(tabneg[,i])
    moy<-mean(tab[,i])
    numerateur<-(moypos-moy)^2+(moyneg-moy)^2
    denominateur<-(sum((tabpos[,i]-moypos)^2)*(1/(npos-1)))+(sum((tabneg[,i]-moyneg)^2)*(1/(nneg-1)))
    fscore[i]<-numerateur/denominateur
  }
  return(c(NA,fscore))
}

BER<-function(class,classpredict){
  pos<-which(class==levels(class)[1])
  neg<-which(class==levels(class)[2])
  (1/2)*( sum(class[pos]!=classpredict[pos])/length(pos)+ sum(class[neg]!=classpredict[neg])/length(neg)  )
}

nll<-function(element){
  if(is.null(element)){return("")}
  else{return(element)}
}

multiclass_metrics <- function(predicted, actual, average = "macro") {
  # predicted: vecteur de classes prédites
  # actual: vecteur de classes réelles
  # average: "macro" (moyenne non pondérée) ou "weighted" (pondérée par support)
   
  if(!is.factor(predicted)) {
    stop("predicted doit être un facteur, mais est de type: ", class(predicted))
  }
  if(!is.factor(actual)) {
    stop("actual doit être un facteur, mais est de type: ", class(actual))
  }
  
  # Vérifier que les longueurs correspondent
  if(length(predicted) != length(actual)) {
    stop("predicted et actual doivent avoir la même longueur. predicted: ", 
         length(predicted), ", actual: ", length(actual))
  }
  
  # Vérifier qu'on a des données
  if(length(actual) == 0) {
    stop("actual est vide")
  }
  
  cat("\n=== multiclass_metrics debug ===\n")
  cat("predicted type:", class(predicted), "length:", length(predicted), "\n")
  cat("actual type:", class(actual), "length:", length(actual), "\n")
  cat("predicted levels:", paste(levels(predicted), collapse=", "), "\n")
  cat("actual levels:", paste(levels(actual), collapse=", "), "\n")
  
  classes <- levels(actual)
  n_classes <- length(classes)
  
  # Initialiser les métriques
  precision_vec <- numeric(n_classes)
  recall_vec <- numeric(n_classes)
  f1_vec <- numeric(n_classes)
  support_vec <- numeric(n_classes)
  
  # Calculer les métriques pour chaque classe (One-vs-All)
  for(i in 1:n_classes) {
    class_i <- classes[i]
    
    # True Positives, False Positives, False Negatives
    TP <- sum(predicted == class_i & actual == class_i)
    FP <- sum(predicted == class_i & actual != class_i)
    FN <- sum(predicted != class_i & actual == class_i)
    
    # Support (nombre d'instances réelles de cette classe)
    support_vec[i] <- sum(actual == class_i)
    
    # Precision, Recall, F1
    precision_vec[i] <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
    recall_vec[i] <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
    f1_vec[i] <- ifelse(precision_vec[i] + recall_vec[i] > 0,
                        2 * (precision_vec[i] * recall_vec[i]) / (precision_vec[i] + recall_vec[i]),
                        0)
  }
  
  # Calculer les moyennes
  if(average == "macro") {
    # Moyenne non pondérée
    precision_avg <- mean(precision_vec, na.rm = TRUE)
    recall_avg <- mean(recall_vec, na.rm = TRUE)
    f1_avg <- mean(f1_vec, na.rm = TRUE)
  } else if(average == "weighted") {
    # Moyenne pondérée par le support
    weights <- support_vec / sum(support_vec)
    precision_avg <- sum(precision_vec * weights, na.rm = TRUE)
    recall_avg <- sum(recall_vec * weights, na.rm = TRUE)
    f1_avg <- sum(f1_vec * weights, na.rm = TRUE)
  }
  
  # Accuracy globale
  accuracy <- sum(predicted == actual) / length(actual)
  
  # Retourner les résultats
  result <- list(
    per_class = data.frame(
      Class = classes,
      Precision = round(precision_vec, 3),
      Recall = round(recall_vec, 3),
      F1_Score = round(f1_vec, 3),
      Support = support_vec
    ),
    average = data.frame(
      Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
      Value = round(c(accuracy, precision_avg, recall_avg, f1_avg), 3)
    ),
    accuracy = accuracy,
    precision = precision_avg,
    recall = recall_avg,
    f1 = f1_avg
  )
  
  return(result)
}

# Fonction pour remplacer sensibility (devient recall macro-average)
sensitivity_multiclass <- function(predicted, actual) {
  metrics <- multiclass_metrics(predicted, actual, average = "macro")
  return(round(metrics$recall, 3))
}


specificity_multiclass <- function(predicted, actual) {
  classes <- levels(actual)
  n_classes <- length(classes)
  spec_vec <- numeric(n_classes)
  
  for(i in 1:n_classes) {
    class_i <- classes[i]
    TN <- sum(predicted != class_i & actual != class_i)
    FP <- sum(predicted == class_i & actual != class_i)
    spec_vec[i] <- ifelse(TN + FP > 0, TN / (TN + FP), 0)
  }
  
  return(round(mean(spec_vec, na.rm = TRUE), 3))
} 
# Fonction pour matrice de confusion multi-classe
confusion_matrix_multiclass <- function(predicted, actual, normalize = FALSE, graph = TRUE) {
  # Créer la matrice de confusion
  conf_mat <- table(Predicted = predicted, Actual = actual)
  
  # Normalisation optionnelle
  if(normalize) {
    # Normalisation par colonne (vraie classe)
    conf_mat <- sweep(conf_mat, 2, colSums(conf_mat), "/")
  }
  
  if(!graph) {
    return(as.data.frame.matrix(conf_mat))
  }
  
  # Visualisation avec ggplot2
  conf_df <- as.data.frame(conf_mat)
  colnames(conf_df) <- c("Predicted", "Actual", "Count")
  
  # Formatage des valeurs
  if(normalize) {
    conf_df$Count <- round(conf_df$Count, 3)
    conf_df$Label <- paste0(round(conf_df$Count * 100, 1), "%")
  } else {
    conf_df$Label <- as.character(conf_df$Count)
  }
  
  # CORRECTION ICI: Définir le titre
  plot_title <- if(normalize) "Confusion Matrix (Normalized)" else "Confusion Matrix"
  
  # Créer le graphique
  p <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = Label), size = 5, fontface = "bold", color = "black") +
    scale_fill_gradient(low = "white", high = "steelblue", 
                        name = if(normalize) "Proportion" else "Count") +
    labs(
      title = plot_title,  # CORRECTION ICI
      x = "True Class",
      y = "Predicted Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    coord_fixed()
  
  return(p)
}

# cette fonction construit un tableau de parametres a tester a partir d'une liste de parametres
# chaque element de la liste est un vecteur de valeurs a tester pour le parametre correspondant
constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}



testparametersfunction<-function(learning,validation,tabparameters){
  set.seed(20011203)
  
  results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=10)
  colnames(results)<-c("auc validation","sensibility validation","specificity validation",
                       "auc learning","sensibility learning","specificity learning",
                       "threshold used","number of features in model",
                       "number of differented features","number of features selected")
  print(paste(nrow(tabparameters),"parameters "))
  
  for (i in 1:nrow(tabparameters)){
    print(i)
    parameters<-tabparameters[i,]
    if(!parameters$NAstructure){tabparameters[i,c("thresholdNAstructure","structdata","maxvaluesgroupmin","minvaluesgroupmax")]<-rep(x = NA,4)    }
    
    selectdataparameters<<-list("prctvalues"=parameters$prctvalues,
                                "selectmethod"=parameters$selectmethod,
                                "NAstructure"=parameters$NAstructure,
                                "structdata"=parameters$structdata,
                                "thresholdNAstructure"=parameters$thresholdNAstructure,
                                "maxvaluesgroupmin"=parameters$maxvaluesgroupmin,
                                "minvaluesgroupmax"=parameters$minvaluesgroupmax)
    resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
    
    if(!parameters$log){tabparameters[i,"logtype"]<-NA}
    transformdataparameters<<-list("log"=parameters$log,
                                   "logtype"=parameters$logtype,
                                   "standardization"=parameters$standardization,
                                   "arcsin"=parameters$arcsin,
                                   "rempNA"=parameters$rempNA)
    
    learningtransform<-transformdatafunction(learningselect = resselectdata$learningselect,
                                             structuredfeatures = resselectdata$structuredfeatures,
                                             datastructuresfeatures = resselectdata$datastructuresfeatures,
                                             transformdataparameters = transformdataparameters)
    
    testparameters<<-list("SFtest"=FALSE,
                          "test"=parameters$test,
                          "adjustpval"=as.logical(parameters$adjustpv),
                          "thresholdpv"=parameters$thresholdpv,
                          "thresholdFC"=parameters$thresholdFC)
    restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters)
    
    if(parameters$test=="notest"){
      learningmodel<-learningtransform
      tabparameters[i,c("adjustpv","thresholdpv","thresholdFC")]<-rep(x = NA,3)
    }
    else{learningmodel<-restest$tabdiff}
    
    if(ncol(learningmodel)!=0){
      
      # Determine if automatic tuning should be used based on tuning_method parameter
      use_autotuning <- (!is.null(parameters$tuning_method) && parameters$tuning_method == "automatic")
      
      # Set autotuning flags for each model type
      autotunerf_flag <- use_autotuning
      autotunesvm_flag <- use_autotuning
      autotunexgb_flag <- use_autotuning
      autotunelgb_flag <- use_autotuning
      autotuneknn_flag <- use_autotuning
      
      modelparameters<<-list("modeltype"=parameters$model,
                             "invers"=FALSE,
                             "thresholdmodel"=parameters$thresholdmodel,
                             "fs"=as.logical(parameters$fs),
                             "adjustval"=!is.null(validation),
                             "autotunerf"=autotunerf_flag,
                             "autotunesvm"=autotunesvm_flag,
                             "autotunexgb"=autotunexgb_flag,
                             "autotunelgb"=autotunelgb_flag,
                             "autotuneknn"=autotuneknn_flag)
      shiny::validate(need(ncol(learning)!=0,"No select dataset"))
      
      # Execute model training
      out<- tryCatch(modelfunction(learningmodel = learningmodel,
                                   validation = validation,
                                   modelparameters = modelparameters,
                                   transformdataparameters = transformdataparameters,
                                   datastructuresfeatures = datastructuresfeatures,
                                   learningselect = resselectdata$learningselect), 
                     error = function(e) e)
      
      if(any(class(out)=="error")){
        parameters$model<-"nomodel"
      }
      else{
        resmodel<-out
        
        # ============================================================================
        # MULTI-CLASSE UNIQUEMENT - Pas de threshold optimization
        # ============================================================================
        # En multi-classe, la prédiction utilise argmax(probabilités)
        # Il n'y a pas de notion de seuil unique
        # Le threshold est toujours NA
        parameters$thresholdmodel <- NA
        
        classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
        n_classes <- length(levels(classlearning))
        cat(sprintf("  → Multi-class classification: %d classes (%s)\n", 
                    n_classes, paste(levels(classlearning), collapse=", ")))
      }
    }
    else{
      parameters$model<-"nomodel"
    }
    
    # ============================================================================
    # CALCUL DES RESULTATS (Multi-classe uniquement)
    # ============================================================================
    
    # Number of features selected (shifted from 9 to 10)
    results[i,10]<-positive(dim(resselectdata$learningselect)[2]-1)
    
    # Number of differented features (shifted from 8 to 9)
    if(parameters$test!="notest"){
      results[i,9]<-positive(dim(restest$tabdiff)[2]-1)
    }
    
    # Calculate results if model was successfully trained
    if(parameters$model!="nomodel"){
      
      # Number of features in model
      results[i,8]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      
      # Threshold used (always NA for multi-class)
      results[i,7]<-NA
      
      # ============================================================================
      # AUC LEARNING (Multi-classe avec multiclass.roc)
      # ============================================================================
      classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
      scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
      
      # Pour multi-classe, scorelearning doit être une matrice (n_samples x n_classes)
      # multiclass.roc() calcule l'AUC macro (moyenne des AUC one-vs-all)
      results[i,4] <- round(as.numeric(pROC::auc(pROC::multiclass.roc(
        classlearning, 
        scorelearning, 
        quiet=TRUE
      ))), digits=3)
      
      # Sensibility learning (macro average)
      results[i,5]<-sensibility(
        resmodel$datalearningmodel$reslearningmodel$predictclasslearning,
        resmodel$datalearningmodel$reslearningmodel$classlearning
      )
      
      # Specificity learning (macro average)
      results[i,6]<-specificity(
        resmodel$datalearningmodel$reslearningmodel$predictclasslearning,
        resmodel$datalearningmodel$reslearningmodel$classlearning
      )
      
      # ============================================================================
      # AUC VALIDATION (Multi-classe avec multiclass.roc)
      # ============================================================================
      if(!is.null(validation)){
        classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
        scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
        
        # Pour multi-classe, scoreval doit être une matrice (n_samples x n_classes)
        results[i,1] <- round(as.numeric(pROC::auc(pROC::multiclass.roc(
          classval, 
          scoreval, 
          quiet=TRUE
        ))), digits=3)
        
        # Sensibility validation (macro average)
        results[i,2]<-sensibility(
          resmodel$datavalidationmodel$resvalidationmodel$predictclassval,
          resmodel$datavalidationmodel$resvalidationmodel$classval
        )
        
        # Specificity validation (macro average)
        results[i,3]<-specificity(
          resmodel$datavalidationmodel$resvalidationmodel$predictclassval,
          resmodel$datavalidationmodel$resvalidationmodel$classval
        )
      }
    }
  }
  
  return(cbind(results,tabparameters))
}

##
importanceplot<-function(model,learningmodel,modeltype,graph=T){
  shiny::validate(need(!is.null(model),"No model"))
  shiny::validate(need(ncol(learningmodel)>2,"only one feature"))
  
  n_classes <- length(levels(learningmodel[,1]))
  
  if(modeltype=="randomforest"){
    # Random Forest : importance différente selon binaire/multi-classe
    if(n_classes == 2) {
      # Classification binaire : utiliser colonne 4 (MeanDecreaseGini)
      var_importance <- data.frame(
        variables = rownames(model$importance),
        importance = as.vector(model$importance[,4])
      )
    } else {
      # Classification multi-classe : dernière colonne (MeanDecreaseGini)
      importance_col <- ncol(model$importance)
      var_importance <- data.frame(
        variables = rownames(model$importance),
        importance = as.vector(model$importance[, importance_col])
      )
    }
    
    varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
    var_importance$variables <- as.character(var_importance$variables)
    var_importance$variables <- factor(x = var_importance$variables, levels = varo)
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
    g <- p + geom_bar() + coord_flip() + 
      ylab("Variable Importance (Mean Decrease in Gini Index)") +
      theme(legend.position="none",
            plot.title=element_text(size=15),
            axis.text.x = element_text(size = 10, face = 'bold'),
            axis.text.y = element_text(size = 10, face = 'bold')
            ) +
      ggtitle("Importance of variables in the model") +
      scale_fill_grey()
  }
  
  if(modeltype=="svm"){
    # SVM : utiliser la même approche mais adapter pour multi-classe
    importancevar <- importancemodelsvm(model = model, modeltype="svm", 
                                        tabdiff=learningmodel, criterion = "fscore")
    
    var_importance <- as.data.frame(cbind(colnames(learningmodel), importancevar)[-1,])
    var_importance[,1] <- as.character(var_importance[,1])
    var_importance[,2] <- as.numeric(as.character(var_importance[,2]))
    colnames(var_importance) <- c("variables","importance")
    varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
    var_importance$variables <- as.character(var_importance$variables)
    var_importance$variables <- factor(x = var_importance$variables, levels = varo)
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
    g <- p + geom_bar() + coord_flip() + ylab("Variable Importance (fscore)") +
      theme(legend.position="none",
            plot.title=element_text(size=15),
            axis.text.x = element_text(size = 10, face = 'bold'),
            axis.text.y = element_text(size = 10, face = 'bold')
            ) +
      ggtitle("Importance of variables in the model") +
      scale_fill_grey()
  }
  
  if(modeltype=="elasticnet"){
    # Extract coefficients from elasticnet model
    coef_matrix <- coef(model$glmnet_model, s=model$lambda)
    
    if(n_classes > 2) {
      # Multi-classe : combiner les coefficients de toutes les classes
      all_coefs <- numeric(nrow(coef_matrix[[1]]) - 1)
      names(all_coefs) <- rownames(coef_matrix[[1]])[-1]
      
      for(class_idx in 1:n_classes) {
        class_coefs <- as.matrix(coef_matrix[[class_idx]])[-1, 1]
        all_coefs <- all_coefs + abs(class_coefs)
      }
      
      nonzero_coefs <- all_coefs[all_coefs != 0]
      
    } else {
      # Binaire
      coef_values <- as.matrix(coef_matrix)[-1, 1]
      names(coef_values) <- colnames(learningmodel)[-1]
      nonzero_coefs <- coef_values[coef_values != 0]
    }
    
    if(length(nonzero_coefs) > 0){
      var_importance <- data.frame(
        variables = names(nonzero_coefs),
        importance = abs(nonzero_coefs),
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar() + coord_flip() + 
        ylab("Variable Importance (Absolute Coefficient)") +
        theme(legend.position="none", 
              plot.title=element_text(size=15),
              axis.text.x = element_text(size = 10, face = 'bold'),
              axis.text.y = element_text(size = 10, face = 'bold')
              ) +
        ggtitle("Importance of variables in the model") +
        scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No variables with non-zero coefficients")
    }
  }
  
  if(modeltype=="xgboost"){
    # XGBoost fonctionne bien pour multi-classe
    importance_matrix <- xgb.importance(model = model)
    
    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar() + coord_flip() + ylab("Variable Importance (Gain)") +
        theme(legend.position="none", plot.title=element_text(size=15),
              axis.text.x = element_text(size = 10, face = 'bold'),
              axis.text.y = element_text(size = 10, face = 'bold')) +
        ggtitle("Importance of variables in the model") +
        scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
    }
  }
  
  if(modeltype=="lightgbm"){
    # LightGBM fonctionne bien pour multi-classe
    importance_matrix <- lgb.importance(model = model)
    
    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar() + coord_flip() + ylab("Variable Importance (Gain)") +
        theme(legend.position="none", plot.title=element_text(size=15)) +
        ggtitle("Importance of variables in the model") +
        scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
    }
  }
  
  if(modeltype=="naivebayes"){
    var_importance <- data.frame()
    g <- errorplot(text = "Naive Bayes: Feature importance not available\nModel uses probabilistic independence assumptions")
  }
  
  if(modeltype=="knn"){
    var_importance <- data.frame()
    g <- errorplot(text = "KNN: Feature importance not available\nModel uses distance-based classification")
  }
  
  if(!graph){return(var_importance)}
  if(graph){g}
}


positive<-function(x){
  if(x<0){x<-0}
  else{x}
  return(x)
}

# Fonction pour créer une visualisation PCA 2D interactive avec plotly
PlotPca2D_interactive <- function(data, y, title = "2D PCA of selected variables") {
  # Effectuer la PCA
  pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  # Créer le dataframe pour plotly
  pca_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  colors  =  c("#FF4C4C", "#4C9AFF", "#FFA500", "#9B59FF", "#3EB489", 
               "#33FF57",  "#FF1493",  "#FF4500", "#00CED1")[1:length(levels(y))]
  
  # Créer le graphique interactif avec plotly
  plot_ly(pca_data, 
          x = ~PC1, 
          y = ~PC2, 
          color = ~Group,
          colors = colors,
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16, face = "bold")),
      xaxis = list(title = paste0("PC1 (", var_explained[1], "% variance)"),
                   titlefont = list(size = 14, face = "bold")),
      yaxis = list(title = paste0("PC2 (", var_explained[2], "% variance)"),
                   titlefont = list(size = 14, face = "bold")),
      legend = list(title = list(text = "Group"))
    )
}


# Fonction pour créer une visualisation PCA 3D interactive avec plotly
PlotPca3D_interactive <- function(data, y, title = "PCA of selected variables") {
  # Effectuer la PCA
  pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  # Vérifier qu'il y a au moins 3 composantes principales
  if(ncol(pca_result$x) < 3) {
    stop("Not enough main components for 3D visualisation")
  }
  
  colors  =  c("#FF4C4C", "#4C9AFF", "#FFA500", "#9B59FF", "#3EB489", 
               "#33FF57",  "#FF1493",  "#FF4500", "#00CED1")[1:length(levels(y))]
 # colors = c("#495C87","#39A27B", "#E2E6A5","#F09462", "#D42A2A", "blue")[1:length(levels(y))]
  #c("#FF4C4C", "#4C9AFF", "#FFA500",  "#33FF57",  "#FF4500", "#00CED1")
  # Créer le dataframe pour plotly
  pca_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  # Créer le graphique 3D interactif avec plotly
  plot_ly(pca_data, 
          x = ~PC1, 
          y = ~PC2, 
          z = ~PC3,
          color = ~Group,
          colors = colors,
          type = 'scatter3d',
          mode = 'markers',
          marker = list(size = 6, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      scene = list(
        xaxis = list(title = paste0("PC1 (", var_explained[1], "%)")),
        yaxis = list(title = paste0("PC2 (", var_explained[2], "%)")),
        zaxis = list(title = paste0("PC3 (", var_explained[3], "%)"))
      ),
      legend = list(title = list(text = "Group"))
    )
}


# Fonction combinée qui crée les deux visualisations (2D et 3D)
PlotPca_Combined <- function(data, y, title_prefix = "PCA") {
  list(
    pca_2d = PlotPca2D_interactive(data, y, paste(title_prefix, "- Vue 2D")),
    pca_3d = PlotPca3D_interactive(data, y, paste(title_prefix, "- Vue 3D"))
  )
}

# plot_learning_curve <- function(learningmodel, validation, model_type = "randomforest",
#                                 modelparameters = NULL, train_sizes = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
#                                 cv_folds = 5, metric = "accuracy", seed = 20011203) {
#   
#   set.seed(seed)
#   metric <- match.arg(metric, c("accuracy", "auc", "f1", "precision", "recall"))
#   
#   # Prepare data
#   x_learn <- as.data.frame(learningmodel[, -1])
#   y_learn <- learningmodel[, 1]
#   n_total <- nrow(x_learn)
#   
#   # If validation provided, combine for learning curve (or use validation as test)
#   if (!is.null(validation)) {
#     x_val <- as.data.frame(validation[, -1])
#     y_val <- validation[, 1]
#   } else {
#     # Use cross-validation on training only
#     x_val <- NULL
#     y_val <- NULL
#   }
#   
#   # Initialize results
#   results <- data.frame(
#     train_size = numeric(),
#     train_score = numeric(),
#     cv_score = numeric(),
#     test_score = numeric()
#   )
#   
#   lev <- levels(y_learn)
#   is_binary <- length(lev) == 2
#   
#   for (size_frac in train_sizes) {
#     n_train <- max(10, round(n_total * size_frac))
#     cat(sprintf("Training size: %d (%.0f%%)\n", n_train, size_frac * 100))
#     
#     # Sample training subset
#     train_idx <- sample(1:n_total, n_train)
#     x_subset <- x_learn[train_idx, ]
#     y_subset <- y_learn[train_idx]
#     
#     # Train model on subset
#     temp_params <- if (!is.null(modelparameters)) modelparameters else list(modeltype = model_type)
#     temp_params$adjustval <- FALSE  # No validation during learning curve
#     
#     tryCatch({
#       # Train on subset
#       model <- modelfunction(
#         learningmodel = cbind(y_subset, x_subset),
#         validation = NULL,
#         modelparameters = temp_params,
#         transformdataparameters = list(standardization = FALSE, log = FALSE),
#         datastructuresfeatures = NULL
#       )
#       
#       # Training score
#       train_preds <- predict_model(model, x_subset, y_subset, metric)
#       train_score <- train_preds$score
#       
#       # CV score (if no validation provided)
#       if (is.null(x_val)) {
#         # Stratified CV on the subset
#         folds <- create_stratified_folds(y_subset, k = min(cv_folds, n_train - 1))
#         cv_scores <- sapply(folds, function(test_idx) {
#           train_idx_cv <- setdiff(1:n_train, test_idx)
#           x_train_cv <- x_subset[train_idx_cv, ]
#           y_train_cv <- y_subset[train_idx_cv]
#           x_test_cv <- x_subset[test_idx, ]
#           y_test_cv <- y_subset[test_idx]
#           
#           model_cv <- modelfunction(
#             learningmodel = cbind(y_train_cv, x_train_cv),
#             validation = NULL,
#             modelparameters = temp_params,
#             transformdataparameters = list(standardization = FALSE, log = FALSE),
#             datastructuresfeatures = NULL
#           )
#           
#           pred_cv <- predict_model(model_cv, x_test_cv, y_test_cv, metric)
#           pred_cv$score
#         })
#         cv_score <- mean(cv_scores, na.rm = TRUE)
#         test_score <- NA
#       } else {
#         # Test score on validation set
#         test_preds <- predict_model(model, x_val, y_val, metric)
#         test_score <- test_preds$score
#         cv_score <- NA
#       }
#       
#       results <- rbind(results, data.frame(
#         train_size = size_frac * 100,
#         train_score = train_score,
#         cv_score = cv_score,
#         test_score = test_score
#       ))
#       
#     }, error = function(e) {
#       cat(sprintf("Error at train size %d: %s\n", n_train, e$message))
#     })
#   }
#   
#   # Plot
#   p <- ggplot(results, aes(x = train_size)) +
#     geom_line(aes(y = train_score, color = "Training"), linewidth = 1.2) +
#     geom_point(aes(y = train_score, color = "Training"), size = 3) +
#     
#     if (!is.null(x_val)) {
#       geom_line(aes(y = test_score, color = "Validation"), linewidth = 1.2) +
#         geom_point(aes(y = test_score, color = "Validation"), size = 3)
#     } else {
#       geom_line(aes(y = cv_score, color = "Cross-Validation"), linewidth = 1.2) +
#         geom_point(aes(y = cv_score, color = "Cross-Validation"), size = 3)
#     }
#   
#   labs(
#     title = sprintf("Learning Curve - %s (%s)", model_type, metric),
#     x = "Training Set Size (%)",
#     y = toupper(metric),
#     color = "Dataset"
#   ) +
#     theme_minimal(base_size = 12) +
#     theme(
#       plot.title = element_text(size = 14, face = "bold"),
#       axis.title = element_text(size = 12, face = "bold"),
#       legend.position = "bottom"
#     ) +
#     scale_color_manual(values = c("Training" = "#2E86AB", "Validation" = "#A23B72", "Cross-Validation" = "#F18F01")) +
#     ylim(0, 1)
#   
#   return(list(plot = p, data = results))
# }
# 
# # Helper function to compute metrics
# predict_model <- function(model, x, y, metric) {
#   # choix du modèle 
#   if (class(model$MODEl=="randomforest")){
#     preds =  randomForest:::predict.randomForest(model$MODEL, x, type = "prob")
#   }else if(class(model$MODEL=="svm")){
#     preds =  e1071:::predict.svm(model$MODEL, x, probability = TRUE)
#     preds = attr(preds, "probabilities")
#   }else if(class(model$MODEL=="elasticnet")){
#     preds = predict(model$MODEL, as.matrix(x), s = model$lambda, type = "response")
#     if (is.matrix(preds)) {
#       pred_class <- colnames(preds)[apply(preds, 1, which.max)]
#     } else {
#       pred_class <- ifelse(preds > 0.5, levels(y)[2], levels(y)[1])
#     }
#   }else if(class(model$MODEL) %in% c("xgboost", "lightgbm")){
#     preds = predict(model$MODEL, as.matrix(x))
#     if (is.matrix(preds)) {
#       pred_class <- colnames(preds)[apply(preds, 1, which.max)]
#     } else {      pred_class <- ifelse(preds > 0.5, levels(y)[2], levels(y)[1])
#     }
#   }else{
#     
#   }
#   #preds <- predict(model$MODEL, x, type = "response")
#   
#   if (is.matrix(preds)) {
#     pred_class <- colnames(preds)[apply(preds, 1, which.max)]
#   } else {
#     pred_class <- as.character(preds)
#   }
#   
#   score <- switch(metric,
#                   "accuracy" = mean(pred_class == as.character(y)),
#                   "auc" = {
#                     if (is.matrix(preds)) {
#                       # Use probability of positive class
#                       prob_pos <- preds[, 2]
#                     } else {
#                       prob_pos <- as.numeric(preds)
#                     }
#                     true_bin <- as.integer(as.character(y) == levels(y)[2])
#                     if (sum(true_bin) == 0 || sum(true_bin) == length(true_bin)) {
#                       NA
#                     } else {
#                       r <- rank(prob_pos)
#                       n_pos <- sum(true_bin)
#                       n_neg <- length(true_bin) - n_pos
#                       (sum(r[true_bin == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
#                     }
#                   },
#                   "f1" = {
#                     tp <- sum(pred_class == as.character(y) & as.character(y) == levels(y)[2])
#                     fp <- sum(pred_class != as.character(y) & pred_class == levels(y)[2])
#                     fn <- sum(pred_class != as.character(y) & as.character(y) == levels(y)[2])
#                     precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
#                     recall <- if (tp + fn == 0) 0 else tp / (tp + fn)
#                     if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
#                   },
#                   "precision" = {
#                     tp <- sum(pred_class == as.character(y) & as.character(y) == levels(y)[2])
#                     fp <- sum(pred_class != as.character(y) & pred_class == levels(y)[2])
#                     if (tp + fp == 0) 0 else tp / (tp + fp)
#                   },
#                   "recall" = {
#                     tp <- sum(pred_class == as.character(y) & as.character(y) == levels(y)[2])
#                     fn <- sum(pred_class != as.character(y) & as.character(y) == levels(y)[2])
#                     if (tp + fn == 0) 0 else tp / (tp + fn)
#                   }
#   )
#   
#   return(list(score = score, predictions = pred_class))
# }

# =============================================================================
# LEARNING CURVE — Multi-classe
# Entraîne le modèle sur des sous-ensembles croissants du learning set
# et calcule AUC macro-OvR + accuracy sur train et sur CV interne (5-fold)
# =============================================================================
learning_curve_multiclass <- function(learningmodel, modelparameters,
                                      train_sizes = seq(0.1, 0.9, by = 0.1),
                                      n_folds = 5) {
  set.seed(20011203)
  
  X_all <- as.data.frame(learningmodel[, -1])
  y_all <- learningmodel[, 1]
  n_total <- nrow(learningmodel)
  lev <- levels(y_all)
  n_classes <- length(lev)
  
  # Tailles absolues à tester
  sizes <- unique(round(train_sizes * n_total))
  sizes <- sizes[sizes >= max(n_classes * 2, 10)]   # au moins 2 obs/classe
  sizes[length(sizes)] <- n_total                    # forcer le 100%
  
  results <- data.frame(
    train_size      = integer(),
    train_size_pct  = numeric(),
    train_auc       = numeric(),
    train_accuracy  = numeric(),
    cv_auc          = numeric(),
    cv_accuracy     = numeric(),
    cv_auc_sd       = numeric(),
    cv_accuracy_sd  = numeric()
  )
  
  # ── helper : entraîner un modèle simple sur un sous-ensemble ──────────────
  fit_model <- function(X_tr, y_tr) {
    mp <- modelparameters
    mp$fs          <- FALSE    # pas de feature selection dans la LC
    mp$adjustval   <- FALSE
    
    # Désactiver l'autotuning pour accélérer (on utilise des défauts raisonnables)
    mp$autotunerf  <- FALSE
    mp$autotunesvm <- FALSE
    mp$autotunexgb <- FALSE
    mp$autotunelgb <- FALSE
    mp$autotuneknn <- FALSE
    
    dat <- cbind(y_tr, X_tr)
    colnames(dat)[1] <- colnames(learningmodel)[1]
    dat[, 1] <- factor(dat[, 1], levels = lev)
    
    tryCatch(
      modelfunction(
        learningmodel          = dat,
        validation             = NULL,
        modelparameters        = mp,
        transformdataparameters = list(log = FALSE, logtype = "log10",
                                       standardization = FALSE,
                                       arcsin = FALSE, rempNA = "none"),
        datastructuresfeatures  = NULL,
        learningselect          = dat
      ),
      error = function(e) NULL
    )
  }
  
  # ── helper : AUC macro OvR ─────────────────────────────────────────────────
  macro_auc <- function(actual, score_mat) {
    actual <- factor(actual, levels = lev)
    if (is.data.frame(score_mat)) score_mat <- as.matrix(score_mat)
    aucs <- sapply(lev, function(cls) {
      bin  <- as.integer(actual == cls)
      if (sum(bin) == 0 || sum(bin) == length(bin)) return(NA)
      tryCatch(
        as.numeric(pROC::auc(pROC::roc(bin, score_mat[, cls],
                                       quiet = TRUE,
                                       levels = c(0,1), direction = "<"))),
        error = function(e) NA
      )
    })
    mean(aucs, na.rm = TRUE)
  }
  
  # ── helper : accuracy ──────────────────────────────────────────────────────
  get_accuracy <- function(predicted, actual) {
    mean(as.character(predicted) == as.character(actual), na.rm = TRUE)
  }
  
  # ── helper : extraire scores depuis reslearningmodel ──────────────────────
  extract_scores <- function(res_learning) {
    col_names  <- colnames(res_learning)
    score_cols <- grep("^score_|^Prob_", col_names, value = TRUE)
    if (length(score_cols) == 0) return(NULL)
    mat <- as.matrix(res_learning[, score_cols, drop = FALSE])
    # Renommer : "score_ClassA" → "ClassA"
    colnames(mat) <- sub("^score_|^Prob_", "", colnames(mat))
    mat
  }
  
  # ── Boucle sur les tailles ─────────────────────────────────────────────────
  for (sz in sizes) {
    
    pct <- round(100 * sz / n_total, 1)
    cat(sprintf("  Learning curve — taille %d/%d (%.0f%%)...\n", sz, n_total, pct))
    
    # Stratified sampling for the subset
    idx_subset <- c()
    for (cl in lev) {
      idx_cl <- which(y_all == cl)
      n_cl   <- max(2, round(sz * length(idx_cl) / n_total))
      n_cl   <- min(n_cl, length(idx_cl))
      idx_subset <- c(idx_subset, sample(idx_cl, n_cl))
    }
    
    X_sub <- X_all[idx_subset, , drop = FALSE]
    y_sub <- y_all[idx_subset]
    
    # ── 1. Score TRAIN (resubstitution sur le sous-ensemble complet) ─────────
    fit_full <- fit_model(X_sub, y_sub)
    train_auc_val <- NA
    train_acc_val <- NA
    
    if (!is.null(fit_full)) {
      res_learn <- fit_full$datalearningmodel$reslearningmodel
      pred_col  <- res_learn$predictclasslearning
      train_acc_val <- get_accuracy(pred_col, res_learn$classlearning)
      score_mat <- extract_scores(res_learn)
      if (!is.null(score_mat) && all(lev %in% colnames(score_mat))) {
        train_auc_val <- macro_auc(res_learn$classlearning, score_mat[, lev, drop = FALSE])
      }
    }
    
    # ── 2. Score CV (cross-validation interne sur le sous-ensemble) ──────────
    folds <- create_stratified_folds(y_sub, k = min(n_folds, floor(length(y_sub) / n_classes)))
    
    cv_aucs <- c()
    cv_accs <- c()
    
    for (fold_idx in seq_along(folds)) {
      test_idx  <- folds[[fold_idx]]
      train_idx <- setdiff(seq_along(y_sub), test_idx)
      
      if (length(train_idx) < n_classes * 2) next
      
      X_tr  <- X_sub[train_idx, , drop = FALSE]
      y_tr  <- y_sub[train_idx]
      X_te  <- X_sub[test_idx, , drop = FALSE]
      y_te  <- y_sub[test_idx]
      
      fit_fold <- fit_model(X_tr, y_tr)
      if (is.null(fit_fold)) next
      
      # Prédire sur le fold test via le modèle entraîné
      model_obj  <- fit_fold$model
      modeltype  <- modelparameters$modeltype
      
      tryCatch({
        if (modeltype == "randomforest") {
          probs <- randomForest:::predict.randomForest(model_obj, X_te, type = "prob")
          probs <- probs[, lev, drop = FALSE]
          preds <- colnames(probs)[apply(probs, 1, which.max)]
        } else if (modeltype == "svm") {
          probs <- attr(e1071:::predict.svm(model_obj, X_te, probability = TRUE), "probabilities")
          missing <- setdiff(lev, colnames(probs))
          if (length(missing) > 0) {
            pad <- matrix(0, nrow = nrow(probs), ncol = length(missing),
                          dimnames = list(NULL, missing))
            probs <- cbind(probs, pad)
          }
          probs <- probs[, lev, drop = FALSE]
          preds <- colnames(probs)[apply(probs, 1, which.max)]
        } else if (modeltype == "elasticnet") {
          probs <- glmnet:::predict.cv.glmnet(model_obj$glmnet_model, as.matrix(X_te),
                           s = model_obj$lambda, type = "response")
          if (is.array(probs)) probs <- probs[, , 1]
          probs <- as.data.frame(probs)
          colnames(probs) <- lev
          probs <- as.matrix(probs[, lev, drop = FALSE])
          preds <- colnames(probs)[apply(probs, 1, which.max)]
        } else if (modeltype == "xgboost") {
          dtest  <- xgboost::xgb.DMatrix(data = as.matrix(X_te))
          raw    <- xgboost:::predict.xgb.Booster(model_obj, dtest)
          probs  <- matrix(raw, ncol = n_classes, byrow = TRUE)
          colnames(probs) <- lev
          preds  <- lev[apply(probs, 1, which.max)]
        } else if (modeltype == "naivebayes") {
          probs <- e1071:::predict.naiveBayes(model_obj, X_te, type = "raw")
          missing <- setdiff(lev, colnames(probs))
          if (length(missing) > 0) {
            pad <- matrix(1e-10, nrow = nrow(probs), ncol = length(missing),
                          dimnames = list(NULL, missing))
            probs <- cbind(probs, pad)
          }
          probs <- probs[, lev, drop = FALSE]
          preds <- colnames(probs)[apply(probs, 1, which.max)]
        } else if (modeltype == "knn") {
          # KNN : prédiction directe (pas de probas fiables)
          preds <- as.character(class::knn(train = X_tr, test = X_te,
                                           cl = y_tr, k = model_obj$k))
          probs <- NULL
        } else {
          next
        }
        
        cv_accs <- c(cv_accs, get_accuracy(preds, y_te))
        
        if (!is.null(probs) && all(lev %in% colnames(probs))) {
          cv_aucs <- c(cv_aucs, macro_auc(y_te, probs[, lev, drop = FALSE]))
        }
        
      }, error = function(e) {
        cat("    Fold", fold_idx, "erreur:", e$message, "\n")
      })
    }  # end fold loop
    
    results <- rbind(results, data.frame(
      train_size      = sz,
      train_size_pct  = pct,
      train_auc       = ifelse(is.na(train_auc_val), NA, round(train_auc_val, 4)),
      train_accuracy  = ifelse(is.na(train_acc_val), NA, round(train_acc_val, 4)),
      cv_auc          = ifelse(length(cv_aucs) == 0, NA, round(mean(cv_aucs, na.rm = TRUE), 4)),
      cv_accuracy     = ifelse(length(cv_accs) == 0, NA, round(mean(cv_accs, na.rm = TRUE), 4)),
      cv_auc_sd       = ifelse(length(cv_aucs) < 2,  NA, round(sd(cv_aucs, na.rm = TRUE), 4)),
      cv_accuracy_sd  = ifelse(length(cv_accs) < 2,  NA, round(sd(cv_accs, na.rm = TRUE), 4))
    ))
  }  # end size loop
  
  return(results)
}


# =============================================================================
# PLOT Learning Curve
# =============================================================================
plot_learning_curve <- function(lc_data, metric = "auc",
                                title = "Learning Curve") {
  if (is.null(lc_data) || nrow(lc_data) == 0) {
    return(errorplot("No learning curve data available"))
  }
  
  if (metric == "auc") {
    y_train <- lc_data$train_auc
    y_cv    <- lc_data$cv_auc
    y_sd    <- lc_data$cv_auc_sd
    ylab    <- "AUC (macro OvR)"
  } else {
    y_train <- lc_data$train_accuracy
    y_cv    <- lc_data$cv_accuracy
    y_sd    <- lc_data$cv_accuracy_sd
    ylab    <- "Accuracy"
  }
  
  x <- lc_data$train_size
  
  # Données long format pour ggplot
  df_train <- data.frame(x = x, y = y_train,
                         ymin = y_train, ymax = y_train,
                         set = "Training (resubstitution)")
  df_cv <- data.frame(x = x, y = y_cv,
                      ymin = ifelse(is.na(y_sd), y_cv, y_cv - y_sd),
                      ymax = ifelse(is.na(y_sd), y_cv, y_cv + y_sd),
                      set = "Cross-validation (5-fold)")
  
  df <- rbind(df_train, df_cv)
  df$set <- factor(df$set, levels = c("Training (resubstitution)",
                                      "Cross-validation (5-fold)"))
  
  ggplot(df, aes(x = x, y = y, colour = set, fill = set)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15, colour = NA) +
    geom_line(size = 1.1) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Training (resubstitution)" = "#E74C3C",
                                   "Cross-validation (5-fold)" = "#2980B9")) +
    scale_fill_manual(values   = c("Training (resubstitution)" = "#E74C3C",
                                   "Cross-validation (5-fold)" = "#2980B9")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title   = title,
         x       = "Training set size (samples)",
         y       = ylab,
         colour  = NULL, fill = NULL) +
    theme_bw(base_size = 13) +
    theme(legend.position  = "bottom",
          plot.title       = element_text(size = 18, face = "bold"),
          axis.text.x =  element_text(size  = 15 , face =  "bold"),
          axis.text.y =  element_text(size  = 15 , face =  "bold"),
           axis.title.x  =  element_text(size = 18  , face =  "bold"),
          axis.title.y = element_text(size = 18  , face = "bold"),
          
          legend.text = element_text(size = 15 , face = "bold"),
          panel.grid.minor = element_blank())
}
