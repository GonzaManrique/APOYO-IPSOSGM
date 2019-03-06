
#### README ==================================================================== 

# Proyecto:     SAE - Proyección Conglictos     
# Objetivo:     Limpiar encuestas realizadas por Ipsos
# Output:       data_completa_ipsos (data frame)      


#### PAQUETES ==================================================================

library(foreign)        # Importar de otros formatos (SPSS, Stata, etc.)
library(stringr)        # Manipular characters
library(dplyr)          # Manejo eficiente de data frames
library(rlist)          # Trabajo eficiente con listas


#### RUTAS =====================================================================

path <- ("G:/Economia Aplicada/PROYECTOS/2018/2018-050-E SAE Proyeccion conflictos/")
input <- paste0(path, "4 Analisis/1 Bases/1 Input/1 Ipsos/2 Bases/")
output <- paste0(path, "4 Analisis/1 Bases/2 Output/")


#### DICCIONARIO ===============================================================

## Cargar diccionario
diccionario <- read.csv(paste0(input, "variables.csv"))

## Limpiar missings
diccionario <- diccionario[complete.cases(diccionario$Pregunta.Base), ]

## Generar columna con número de indicador
diccionario$check <- gsub("[^0-9]", "", diccionario$Indicador)


#### CARGAR LISTA DE ENCUESTAS =================================================

## Cargar "lista_encuestas"
load(paste0(output, "ListaEncuestas.RData"))


#### LIMPIEZA ==================================================================

## Ubigeos ---------------------------------------------------------------------

## Nota: Cada encuesta tiene su propio poceso de limpieza.

## 6183117 (Barrick)
lista_encuestas[["6183117"]] <- lista_encuestas[["6183117"]] %>%
        mutate(DISTRITO = as.character(DISTRITO))

## 6183410
lista_encuestas[["6183410"]] <- lista_encuestas[["6183410"]] %>%
        mutate(DISTRITO = "080101")

## 6185016
lista_encuestas[["6185016"]] <- lista_encuestas[["6185016"]] %>%
        mutate(SHELL_SCH2 = str_extract(SHELL_SCH2, "[A-Z]+")) %>%
        mutate(DISTRITO = "") %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "CUS", "080101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "ICA", "110101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "HYO", "120101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "LIM", "150101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "IQT", "160101"))

# 6181017
lista_encuestas[["6181017"]] <- lista_encuestas[["6181017"]] %>%
        mutate(DISTRITO = as.character(DISTRITO)) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "040701")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "040702")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "4", "040705")) %>%
        mutate(DISTRITO = replace(DISTRITO, !(DISTRITO %in% c("040701", 
                                                              "040702",
                                                              "040705")),
                                  "040706"))

# 6185215
lista_encuestas[["6185215"]] <- lista_encuestas[["6185215"]] %>%
        mutate(DISTRITO = as.character(DISTRI)) %>%
        mutate(DISTRI = str_extract(toupper(DISTRI), "[A-Z]+")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("SATIP",
                                                          "SATIPO",
                                                          "SATPO",
                                                          "SSATIPO",
                                                          "STIPO"),
                                  "120601")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("MASAMARI",
                                                          "mazamari",
                                                          "MAZAMARI",
                                                          "MAZAMATI",
                                                          "MAZARI"),
                                  "120604")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("VILLA",
                                                          "VILLARICA",
                                                          "VILLARICA",
                                                          "VILLARICQ"),
                                  "190307")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("MASAMARI",
                                                          "mazamari",
                                                          "MAZAMARI",
                                                          "MAZAMATI",
                                                          "MAZARI"),
                                  "120604")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "PISCO",
                                  "110501")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "PUENTE",
                                  "120302")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "HUASAHUASI",
                                  "120704")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "IQUITOS",
                                  "160101"))

## 6182015 
lista_encuestas[["6182015"]] <- lista_encuestas[["6182015"]] %>%
        mutate(DISTRITO = as.character(distrito))

## 6182314
lista_encuestas[["6182314"]] <- lista_encuestas[["6182314"]] %>%
        mutate(DISTRITO = as.character(distrito))

# 6186017
lista_encuestas[["6186017"]] <- lista_encuestas[["6186017"]] %>%
        mutate(DISTRITO = DISTRITOA) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "040101")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "040102")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "3", "040103")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "4", "040104")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "5", "040107")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "6", "040109")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "7", "040110")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "8", "040112")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "9", "040116")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "10", "040117"))

# 6188716
lista_encuestas[["6188716"]] <- lista_encuestas[["6188716"]] %>%
        mutate(DISTRITO = "") %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 40, "061309")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 3, "060411" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 4, "060409" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% 9:36, "060415" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(5, 37:399), "060607"))

# 6480811
lista_encuestas[["6480811"]] <- lista_encuestas[["6480811"]] %>%
        mutate(DISTRITO = as.character(DISTRI)) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "060105")) %>%
        mutate(DISTRITO = replace(DISTRITO, !(DISTRITO %in% c("060101",
                                                              "060105")),
                                  "060108"))

# 6185210
lista_encuestas[["6185210"]] <- lista_encuestas[["6185210"]] %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(2:5,7:9,11:13),
                                  "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(1,15:17),
                                  "060108")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(6,10,14,18,19),
                                  "060105"))

# 6181509
lista_encuestas[["6181509"]] <- lista_encuestas[["6181509"]] %>%
        mutate(DISTRITO = DISTRITO, AREA %in% c(1, 4), "030506") %>%
        mutate(DISTRITO = DISTRITO, AREA == 2, "030501") %>%
        mutate(DISTRITO = DISTRITO, AREA == 3, "030401") %>%
        mutate(DISTRITO = DISTRITO, AREA == 5, "030503") %>%
        mutate(DISTRITO = DISTRITO, AREA == 6, "030708") %>%
        mutate(DISTRITO = DISTRITO, AREA == 7, "030504")

# 6181109
lista_encuestas[["6181109"]] <- lista_encuestas[["6181109"]] %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "1", "080801") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "5", "080802") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "2", "080803") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "4", "080804") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "7", "080805") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "3", "080806") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "8", "080807") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "6", "080808")

# 6184109
lista_encuestas[["6184109"]] <- lista_encuestas[["6184109"]] %>%
        mutate(DISTRITO = replace(DISTRITO,  CIUDAD %in% c(2:5,7:9,11:13),
                                  "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(1,15:17),
                                  "060108")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(6,10,14,18,19),
                                  "060105"))

        
## ID --------------------------------------------------------------------------

## Generar ID de cada observación
lista_encuestas <- lapply(lista_encuestas, 
                          function(x) {
                                  x$id <- c(1:nrow(x))
                                  x
                          })


## VARIABLE DE PROYECTO --------------------------------------------------------


data$proyecto_minero <- c(encuesta)
data$minera <- as.vector(unique(diccionario$Caso.Elegido))

if(encuesta %in% "6183117"){
        data$minera <- ifelse(data$proy %in% "Barrick Lagunas Norte", "Barrick Lagunas Norte", 
                              ifelse(data$proy %in% "Barrick Pierina", "Barrick Pierina", data$minera))
}


## Limpieza de dummys y otros


for(encuesta in id_encuestas){
        
        print(encuesta)
        
        vars_ind <- NULL        ## ??????
        diccionario <- read.csv(paste0(input, "variables.csv"))
        diccionario <- diccionario[!diccionario$Pregunta.Base %in% "", ]
        diccionario <- diccionario[diccionario$Código %in% encuesta, ]
        
        # Indicadores en base
        diccionario$check <- gsub("[^0-9]", "", diccionario$Indicador) 
        indicadores <- unique(diccionario$check)
        
        if(encuesta %in% c("6183117", "6186109")){
                if(encuesta %in% "6183117"){
                        
                }else{
                        data <- readRDS(paste0(input, encuesta, "RDS"))
                        data$DISTRITO <- as.character(data$DISTRITO)
                }
        }else{
                data <- read.spss(paste0(input, encuesta, ".sav"), use.value.labels=FALSE, to.data.frame=TRUE)
                data$proy <- NA
        }
        
        
        # Dicotómica
        
        diccionario$pos <- regexpr("Dicotómica|dicotómica", diccionario$Tipo.de.variable)
        dummies_vars <- as.vector(diccionario[diccionario$pos %in% 1, "Pregunta.Base"])
        if(length(dummies_vars) >= 1){
                dummies_vars <- unlist(strsplit(dummies_vars, split = "/"))
                dummies_vars <- unlist(strsplit(dummies_vars, split = ", "))
                dummies_vars <- unlist(strsplit(dummies_vars, split = ","))
                dummies_vars <- unlist(strsplit(dummies_vars, split = " "))
        }
        
        for(var in dummies_vars){
                print(var)
                data[, var] <- dplyr::recode(data[, var], "1"=5, "2"=1, "96"= as.numeric(NA), "99"= as.numeric(NA))
                data[, paste0(var,"_o")] <- NA
        }
        
        # Likert - Categórica
        
        diccionario$pos <- regexpr("Likert|Escala|Contribuye|Progresando|Respuesta|Categórica|categórica", diccionario$Tipo.de.variable)
        likert <- as.vector(diccionario[diccionario$pos %in% 1, "Pregunta.Base"])
        
        for(var in likert){
                print(var)
                data[, var] <- dplyr::recode(data[, var], "99"= as.numeric(NA))
                data[, var] <- dplyr::recode(data[, var], "96"= as.numeric(NA))
                data[, paste0(var,"_o")] <- NA
        } 
        
        # Abierta - Múltiple
        
        diccionario$pos <- regexpr("Abierta|Múltiple", diccionario$Tipo.de.variable)
        abierta <- as.vector(diccionario[diccionario$pos %in% 1, "Pregunta.Base"])
        if(length(abierta)>0){
                abierta <- unlist(strsplit(abierta, split = ", "))
                abierta <- unlist(strsplit(abierta, split = ","))
                for(var in abierta){
                        print(var)
                        data[, paste0(var,"_o")] <- ifelse(data[, var] %in% 96, 1, NA)
                        data[, var] <- dplyr::recode(data[, var], "96"=as.numeric(NA), "99"=as.numeric(NA))
                        data[, var] <- ifelse(!is.na(data[, var]), 1, NA)
                } 
        }
        
        # Limpieza para 6166308
        if(encuesta %in% "6166308"){
                
                vars_tofix <- c(paste0("P03_", 1:10))
                for(var in vars_tofix){
                        data[, var] <- ifelse(!is.na(data[, var]) & data[, var]!=1 & data[, var]!=99, 0, data[, var])
                        data[, var] <- ifelse(data[, var]==1, 5, ifelse(data[, var]==0, 1, data[, var]))
                }
        }
        
        # Limpieza para 6180712
        if(encuesta %in% "6180712"){
                
                vars_tofix <- c(paste0("P7_", 1:3))
                for(var in vars_tofix){
                        data[, var] <- ifelse(!is.na(data[, var]) & data[, var]!=6 & data[, var]!=99, 1, 5)
                }
                
                vars_tofix <- c(paste0("P30_", 1:3))
                
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% c(7,8,11), 5, 1))
                }
                
        }
        
        # Limpieza para 6181509
        if(encuesta %in% "6181509"){
                
                vars_tofix <- c(paste0("P31_", 1:3))
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% 3, 5, 1))
                }
        }
        
        # Limpieza para 6185908
        if(encuesta %in% "6185908"){
                vars_tofix <- c(paste0("P49_", 1:5))
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% 3, 5, 1))
                }
        }
        
        # Limpieza para 6181513
        if(encuesta %in% "6181513"){
                vars_tofix <- c(paste0("P28_", 1:10))
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% 4, 5, 1))
                }
        }
        
        # Limpieza para 6185016
        if(encuesta %in% "6185016"){
                vars_tofix <- c(paste0("P1", 1:3))
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% c(6, 9), 5, 1))
                }
        }
        
        # Limpieza para 6185215
        if(encuesta %in% "6185215"){
                vars_tofix <- c(paste0("P3", 1:3))
                for(var in vars_tofix){
                        data[, var] <- ifelse(is.na(data[, var]) | data[, var] %in% 99, NA, ifelse(data[, var] %in% c(6, 9), 5, 1))
                }
        }
        
        
################################
### Creación de indicadores ####
################################
        
        for(i in indicadores){
                
                vars_ind <- NULL
                vars <- as.vector(diccionario[diccionario$check %in% i, "Pregunta.Base"])
                vars <- unlist(strsplit(vars, split = ", "))
                vars <- unlist(strsplit(vars, split = ","))
                vars <- unlist(strsplit(vars, split = "/"))-
                        vars <- unlist(strsplit(vars, split = " "))
                
                if(length(vars) %in% 1){
                        
                        data[, paste0("indicador_", i)] <- data[, vars]
                        data[, paste0(encuesta, "i", i, "_", vars)] <- data[, vars]
                        vars_ind <- c(vars_ind, paste0(encuesta, "i", i, "_", vars))
                        data$nvarsenc <- ifelse(is.na(data[, vars]), 0 , 1)
                        
                }else{
                        
                        data$nvarsenc <- apply(data[, vars], 1, function(x) sum(!is.na(x)))
                        data[, paste0("indicador_", i)] <- rowSums(data[, vars],na.rm = TRUE)
                        data$missings <- apply(data[, vars], 1, function(x) sum(is.na(x)))
                        data[, paste0("indicador_", i)] <- ifelse(data$missings %in% ncol(data[, vars]), NA, data[, paste0("indicador_", i)])
                        
                        vars_o <- paste0(vars, "_o")
                        data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                        data[, paste0("indicador_", i)] <- ifelse(data$otros >= 1, 0, data[, paste0("indicador_", i)])
                        
                        for(var in vars){
                                data[, paste0(encuesta, "i", i, "_", var)] <- data[, var]
                                vars_ind <- c(vars_ind, paste0(encuesta, "i", i, "_", vars))
                        }
                        
                }
                
                if(i %in% 8){
                        
                        vars1 <- as.vector(diccionario[diccionario$check %in% i & diccionario$i8_con==1, "Pregunta.Base"]) 
                        vars2 <- as.vector(diccionario[diccionario$check %in% i & diccionario$i8_con==0, "Pregunta.Base"]) 
                        
                        if(length(vars1) %in% 1){
                                data$indicador_8_con <- data[, vars1]
                                data$n_indicador_8_con <- ifelse(is.na(data[, vars1]), 0, 1)
                        }
                        if(length(vars1) > 1){
                                data$n_indicador_8_con <- apply(data[, vars1], 1, function(x) sum(!is.na(x)))
                                data$indicador_8_con <- rowSums(data[, vars1],na.rm = TRUE)
                                data$missings <- apply(data[, vars1], 1, function(x) sum(is.na(x)))
                                data$indicador_8_con <- ifelse(data$missings %in% ncol(data[, vars1]), NA, data$indicador_8_con)
                                
                                vars_o <- paste0(vars1, "_o")
                                data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                                data$indicador_8_con <- ifelse(data$otros >= 1, 0, data$indicador_8_con)
                        }
                        
                        if(length(vars2) %in% 1){
                                data$indicador_8_opi <- data[, vars2]
                                data$n_indicador_8_opi <- ifelse(is.na(data[, vars2]), 0, 1)
                        }
                        if(length(vars2) > 1){
                                data$n_indicador_8_opi <- apply(data[, vars2], 1, function(x) sum(!is.na(x)))
                                data$indicador_8_opi <- rowSums(data[, vars2],na.rm = TRUE)
                                data$missings <- apply(data[, vars2], 1, function(x) sum(is.na(x)))
                                data$indicador_8_opi <- ifelse(data$missings %in% ncol(data[, vars2]), NA, data$indicador_8_opi)
                                
                                vars_o <- paste0(vars2, "_o")
                                data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                                data$indicador_8_opi <- ifelse(data$otros >= 1, 0, data$indicador_8_opi)
                        }
                        
                }
                
                if(i %in% 18){
                        
                        vars3 <- as.vector(diccionario[diccionario$check %in% i & diccionario$i18_fut==1, "Pregunta.Base"])
                        vars4 <- as.vector(diccionario[diccionario$check %in% i & diccionario$i18_fut==0, "Pregunta.Base"])
                        
                        if(length(vars3) %in% 1){
                                data$indicador_18_fut <- data[, vars3]
                                data$n_indicador_18_fut <- ifelse(is.na(data[, vars3]), 0, 1)
                        }
                        if(length(vars3) > 1){
                                data$n_indicador_18_fut <- apply(data[, vars3], 1, function(x) sum(!is.na(x)))
                                data$indicador_18_fut <- rowSums(data[, vars3],na.rm = TRUE)
                                data$missings <- apply(data[, vars3], 1, function(x) sum(is.na(x)))
                                data$indicador_18_fut <- ifelse(data$missings %in% ncol(data[, vars3]), NA, data$indicador_18_fut)
                                
                                vars_o <- paste0(vars3, "_o")
                                data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                                data$indicador_18_fut <- ifelse(data$otros >= 1, 0, data$indicador_18_fut)
                        }
                        
                        if(length(vars4) %in% 1){
                                data$indicador_18_pre <- data[, vars4]
                                data$n_indicador_18_pre <- ifelse(is.na(data[, vars4]), 0, 1)
                        }
                        if(length(vars4) > 1){
                                data$n_indicador_18_pre <- apply(data[, vars4], 1, function(x) sum(!is.na(x)))
                                data$indicador_18_pre <- rowSums(data[, vars4],na.rm = TRUE)
                                data$missings <- apply(data[, vars4], 1, function(x) sum(is.na(x)))
                                data$indicador_18_pre <- ifelse(data$missings %in% ncol(data[, vars4]), NA, data$indicador_18_pre)
                                
                                vars_o <- paste0(vars4, "_o")
                                data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                                data$indicador_18_pre <- ifelse(data$otros >= 1, 0, data$indicador_18_pre)
                        }
                        
                }
                
                extrai <- c("indicador_8_con", "indicador_8_opi", "indicador_18_fut", "indicador_18_pre")
                extrai <- intersect(names(data), extrai)
                
                if(length(extrai>0)){
                        for(var in extrai){
                                data[, paste0(var, "_v2")] <- (data[, var] - data[, paste0("n_", var)]) / (4*data[, paste0("n_", var)])
                                
                                data[, var] <- ifelse(data[, paste0(var, "_v2")]<0 | data[, paste0(var, "_v2")]>1,
                                                      NA, data[, var])
                                data[, paste0(var, "_v2")] <- ifelse(data[, paste0(var, "_v2")]<0 | data[, paste0(var, "_v2")]>1,
                                                                     NA, data[, paste0(var, "_v2")])
                                
                                data[, var] <- (data[, var] - mean(data[, var], na.rm = TRUE)) / sd(data[, var], na.rm = TRUE)
                        }
                }
                
                if(i %in% 19){
                        
                        data[, paste0("indicador_", i, "_v2")]<- (data[, paste0("indicador_", i)] - data$nvarsenc)/ (49*data$nvarsenc)
                        
                }else{
                        
                        data[, paste0("indicador_", i, "_v2")]<- (data[, paste0("indicador_", i)] - data$nvarsenc)/ (4*data$nvarsenc)
                        
                        if(i %in% 8){
                                data[, paste0("indicador_", i, "b", "_v2")] <-ifelse(data[, paste0("indicador_", i, "_v2")]<0, 0, data[, paste0("indicador_", i, "_v2")])
                                data[, paste0("indicador_", i, "b")] <-ifelse(data[, paste0("indicador_", i)]<0, 1, data[, paste0("indicador_", i)])
                                data[, paste0("indicador_", i, "b")] <- (data[, paste0("indicador_", i, "b")] - mean(data[, paste0("indicador_", i, "b")], na.rm = TRUE)) / 
                                        sd(data[, paste0("indicador_", i, "b")], na.rm = TRUE)
                        }
                        
                        data[, paste0("indicador_", i)] <- ifelse(data[, paste0("indicador_", i, "_v2")]<0 | data[, paste0("indicador_", i, "_v2")]>1,
                                                                  NA, data[, paste0("indicador_", i)])
                        
                        data[, paste0("indicador_", i, "_v2")]<- ifelse(data[, paste0("indicador_", i, "_v2")]<0 | data[, paste0("indicador_", i, "_v2")]>1,
                                                                        NA, data[, paste0("indicador_", i, "_v2")])
                }
                
                data[, paste0("indicador_", i)] <- (data[, paste0("indicador_", i)] - mean(data[, paste0("indicador_", i)], na.rm = TRUE)) / 
                        sd(data[, paste0("indicador_", i)], na.rm = TRUE)
                
                
                vars_comun <- as.vector(diccionario[diccionario$Variable.comun %in% paste0("indicador_", i), "Pregunta.Base"])
                
                if(length(vars_comun) >= 1){
                        
                        if(length(vars_comun) %in% 1){
                                
                                data[, paste0("var_indicador_", i)] <- data[, vars_comun]
                                
                        }else{
                                
                                data[, paste0("var_indicador_", i)] <- rowSums(data[, vars_comun],na.rm = TRUE)
                                data$missings <- apply(data[, vars_comun], 1, function(x) sum(is.na(x)))
                                data[, paste0("var_indicador_", i)] <- ifelse(data$missings %in% ncol(data[, vars_comun]), NA, 
                                                                              data[, paste0("var_indicador_", i)])
                                
                                vars_o <- paste0(vars_comun, "_o")
                                data$otros <- apply(data[, vars_o], 1, function(x) sum(!is.na(x)))
                                data[, paste0("var_indicador_", i)] <- ifelse(data$otros >= 1, 0, data[, paste0("var_indicador_", i)])
                                
                        }
                }
                
        }
        
        extrai <- c("indicador_8_con", "indicador_8_opi", "indicador_18_fut", "indicador_18_pre")
        vars <- c("minera", "proyecto_minero", "DISTRITO", "id", "EDADE", "OCUPACX", "GENERO", "EDJF", "NIVEL3", extrai, paste0(extrai, "_v2"),
                  "indicador_8b", "indicador_8b_v2", paste0("indicador_", 1:19), paste0("indicador_", 1:19, "_v2"), paste0("var_indicador_", 6:8), 
                  vars_ind, "proy")
        diff <- setdiff(vars,colnames(data))
        data[, c(as.character(diff))] <- NA
        data$DISTRITO <- as.character(data$DISTRITO)
        data_indicadores <- data.frame(data[, vars])
        
        if(is.null(datacompleta_indicadores)==TRUE){
                
                datacompleta_indicadores <- data_indicadores
                
        }else{
                
                diff <- setdiff(colnames(data_indicadores), colnames(datacompleta_indicadores))
                datacompleta_indicadores[, c(as.character(diff))] <- NA
                
                diff <- setdiff(colnames(datacompleta_indicadores),colnames(data_indicadores))
                data_indicadores[, c(as.character(diff))] <- NA
                
                datacompleta_indicadores <- rbind(datacompleta_indicadores, data_indicadores, fill=TRUE)
                
        }
        
}


#### LIMPIEZA DE BASE COMPLETA =================================================


datacompleta_indicadores <- datacompleta_indicadores[datacompleta_indicadores$proyecto_minero %in% encuestas, ]
datacompleta_indicadores$tipo_proyecto <- ifelse(datacompleta_indicadores$minera %in% c("Barrick", "Chinalco", "Quellaveco"), "Éxito",
                                                 ifelse(datacompleta_indicadores$minera %in% c("Yanacocha", "Tía María"), "Fracaso",
                                                        "Problemas"))
datacompleta_indicadores$minera <- ifelse(datacompleta_indicadores$proy %in% "Barrick Lagunas Norte",
                                          "Barrick Lagunas Norte", ifelse(datacompleta_indicadores$proy %in% "Barrick Pierina",
                                                                          "Barrick Pierina", datacompleta_indicadores$minera))
datacompleta_indicadores$minera <- ifelse(datacompleta_indicadores$minera %in% "Barrick", "Barrick Pierina", 
                                          datacompleta_indicadores$minera)

datacompleta_indicadores$ubigeo <- as.character(datacompleta_indicadores$DISTRITO)
datacompleta_indicadores$ubigeo <- str_extract(datacompleta_indicadores$ubigeo, "\\d+")
datacompleta_indicadores$aux <- nchar(datacompleta_indicadores$ubigeo)
datacompleta_indicadores$ubigeo <- ifelse(datacompleta_indicadores$aux %in% 5, paste0("0", as.numeric(datacompleta_indicadores$ubigeo)), 
                                          datacompleta_indicadores$ubigeo)

saveRDS(datacompleta_indicadores, paste0(output, "Basefull4.RDS"))

extrai <- c("indicador_8_con", "indicador_8_opi", "indicador_18_fut", "indicador_18_pre")
vars <- c("minera", "proyecto_minero", "ubigeo", "id", "EDADE", "OCUPACX", "GENERO", "EDJF","tipo_proyecto", "NIVEL3", extrai, paste0(extrai, "_v2"),
          "indicador_8b", "indicador_8b_v2", paste0("indicador_", 1:19), paste0("indicador_", 1:19, "_v2"))
datacompleta_indicadores <- datacompleta_indicadores[, vars]
saveRDS(datacompleta_indicadores, paste0(output, "Base4.RDS"))



#### LISTA DE PROYECTOS ===========================================================

data <- datacompleta_indicadores[, c("minera", "proyecto_minero", "ubigeo")]
data <- data[!duplicated(data), ]
write.csv(data, paste0(output, "Lista_proyectos.csv"))
