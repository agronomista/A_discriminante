#Inferencia para la tesis_analisis de discriminante
file.choose()

#Realizar un data frame en donde se puedan agrupar los datos para un analisis
#de discriminante 

#Set de datos BOCA ARENAL escoger los predictores para hacer un data frame
ruta <- "C:\\Users\\Lenovo\\OneDrive - Universidad de Costa Rica\\2. Tesis_antonio_identificación y diferenciación de Cizaña\\inferencia_analisis_discriminante\\datos\\RGB_SC.csv"
RGB

Azul<-df_SC[df_SC$Banda == "Azul",]
Verde <- df_SC[df_SC$Banda == "Verde",]
Rojo<-df_SC[df_SC$Banda == "Rojo",]
Red_edge <- df_SC[df_SC$Banda == "Red_edge",]
NIR <- df_SC[df_SC$Banda == "NIR",]

#Asystasia gangetica
Asys_flo_Azul<-Azul[Azul$ID == "Asys_floracion(SC)",] 
Asys_flo_Verde<-Verde[Verde$ID == "Asys_floracion(SC)",] 
Asys_flo_Rojo<-Rojo[Rojo$ID == "Asys_floracion(SC)",] 
Asys_flo_Red_edge<-Red_edge[Red_edge$ID == "Asys_floracion(SC)",] 
Asys_flo_NIR <- NIR[NIR$ID == "Asys_floracion(SC)", ]

#Piña
Pina_Azul <- Azul[Azul$ID == "Pina_26meses(SC)",]
Pina_Verde <- Verde[Verde$ID == "Pina_26meses(SC)",]
Pina_Rojo <- Rojo[Rojo$ID == "Pina_26meses(SC)",]
Pina_red_edge <- Red_edge[Red_edge$ID == "Pina_26meses(SC)",]
Pina_NIR <- NIR[NIR$ID == "Pina_26meses(SC)",]

#Asystasia gangetica planta
Asys_flono_Azul<-Azul[Azul$ID == "Asys_nofloracion(SC",] 
Asys_flono_Verde<-Verde[Verde$ID == "Asys_nofloracion(SC)",] 
Asys_flono_Rojo<-Rojo[Rojo$ID == "Asys_nofloracion(SC)",] 
Asys_flono_Red_edge<-Red_edge[Red_edge$ID == "Asys_nofloracion(SC)",] 
Asys_flono_NIR <- NIR[NIR$ID == "Asys_nofloracion(SC)", ]



setwd("/home/antonio/2. Tesis_antonio_identificación y diferenciación de Cizaña/inferencia_analisis_discriminante")

write_excel_csv(Asys_flo_Azul, "Asys_flo_Azul.csv")
write_excel_csv(Asys_flo_Verde, "Asys_flo_Verde.csv")
write_excel_csv(Asys_flo_Rojo, "Asys_flo_Rojo.csv")
write_excel_csv(Asys_flo_Red_edge, "Asys_flo_Red_edge.csv")
write_excel_csv(Asys_flo_NIR, "Asys_flo_NIR.csv")

write_excel_csv(Pina_Azul, "Pina_Azul.csv")
write_excel_csv(Pina_Verde, "Pina_verde.csv")
write_excel_csv(Pina_Rojo, "Pina_rojo.csv")
write_excel_csv(Pina_red_edge, "Pina_red_edge.csv")
write_excel_csv(Pina_NIR, "Pina_NIR.csv")

write_excel_csv(Asys_flono_Azul, "Asys_noflo_Azul.csv")
write_excel_csv(Asys_flono_Verde, "Asys_noflo_Verde.csv")
write_excel_csv(Asys_flono_Rojo, "Asys_noflo_Rojo.csv")
write_excel_csv(Asys_flono_Red_edge, "Asys_noflo_Red_edge.csv")
write_excel_csv(Asys_flono_NIR, "Asys_noflo_NIR.csv")



#Cargado de datos
ruta_2 <- "F:\\inferencia_analisis_discriminante\\Boca_Arenal\\QDA_final.csv"
RGB <- read.csv(ruta_2, header = TRUE, sep = ",")
RGB$ID <- as.factor(RGB$ID)
NIR_Verde$ID <- as.factor(NIR_Verde$ID)

# Data partition
set.seed(555)
ind <- sample(2, nrow(RGB),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- RGB[ind==1,]
testing <- RGB[ind==2,]

# Linear discriminant analysis
library(MASS)
linear <- qda(ID~., training)
linear
attributes(linear)

predicted.qda = predict(linear, newdata = testing)
table(testing$ID, predicted.qda$class, dnn = c('Actual Group','Predicted Group'))
mean(predicted.qda$class == testing$ID)

library(klaR)
partimat(ID ~ Verde + Rojo+ Azul + NIR + Borde.rojo, data = testing, plot.matrix = FALSE, method = "qda",
         col.mean ="blue", nplots.ver = 2, main = "Cizaña (floración), Cizaña (plántula), y piña (26 meses)",
         image.colors = c("white", "grey88", "orange"))
