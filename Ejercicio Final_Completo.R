# **Proyecto Final**

### **Importando las librerías necesarias para trabajar nuestra Base de Datos**

library("stats")#funciones estadisticas
library("dplyr")#manipulación y operaciones con data frames
library("ggplot2")#creacion de graficos avanzados
library("tidyverse")#conjunto de paquetes para manejar, transformar, y visualizar datos
library("psych")#manejo estadisticas creados por psicologos
library("knitr")
library('nortest')# nos entra el Lillie Test
library('gplots')#graficar medias

### **PASO 1: Importa Data a Analizar**
#Importamos el archivo con la data a través del siguiente comando:**

Compradores_datos<- read.csv("Dataset_Compradores.csv", header = TRUE,dec='.')


### **PASO 2: Revisión Contenido Data**

kable(head(Compradores_datos))
kable(tail(Compradores_datos))


### **PASO 3: Tratamiento de Data**

#LIMPIEZA Podemos ver que existen columnas llamadas RowNumber y Exited que debemos 
#eliminar de los datos, ya que aportan información util:
  
Compradores_datos<- select(Compradores_datos, -RowNumber,-Exited) 
kable(head(Compradores_datos))
#Comprobamos que las columnas Nrow y Exited ya no existen en nuestra data

# **Identificación de valores ausentes con is.na:**

sum(is.na(Compradores_datos))# La presencia de datos ausentes dificulta la mayoria 
#de operaciones matemáticas y de análisis.
#En este caso no existen valores NA entre los datos

### **PASO 4: Analisis Exploratorio**

describe(Compradores_datos)
dim(Compradores_datos)# nos entrega las dimensiones de nuestro Data Frame Filas x Columnas
class(Compradores_datos)# nos indica que es un DataFrame


### **PASO 5:Estadistica descriptiva y Grafica de las Variables**

summary(Compradores_datos)
#Genera un resumen del contenido,para variables númericas aporta estadísticos básicos 
#y para variables cualitativas entrega un conteo de apariciones por posible valor.

#A continuacion se van a graficar las variables, para poder tener un mejor entencimiento,
#de los valores que ofrecen.

### **Graficando Variable: PAISES**

paises<- ggplot(Compradores_datos)+
  geom_bar(mapping = aes(x=Geography,color=Geography,fill=Geography))+
  geom_text(stat='count', aes(x = Geography, label = ..count..), vjust = -1)+
  ylim(0, 7000)+
  ggtitle('Cantidad de Observaciones Países')+
  theme_classic();paises

table(Compradores_datos$Geography)

#Con la Tabla y Gráfico podemos ver como se distribuyen la cantidad de datos por paises.

### **Graficando Variable: GENERO**

table(Compradores_datos$Gender)# nos indica la cantidad por hombre y mujeres
genero<- as.factor(Compradores_datos$Gender)

Genero_bar<- ggplot(Compradores_datos)+
  geom_bar(mapping = aes(x=Gender,color=Gender,fill=Gender))+
  geom_text(stat='count', aes(x = Gender, label = ..count..), vjust = -1)+
  ylim(0, 7000)+
  ggtitle('Cantidad de Observaciones por Genero')+
  theme_classic();Genero_bar

### **Variables GENDER_PAIS**

#Cantidad de  Hombres Y Mujeres por País

Genero_Pais<-ggplot(Compradores_datos,mapping = aes(x=Gender,color=Gender,fill=Gender))
  
   Genero_Pais+geom_bar()+
  geom_text(stat='count', aes(x = Gender, label = ..count..), vjust = -1)+
  ylim(0, 3500)+
  ggtitle('Cantidad de Observaciones por Genero')+
  facet_grid(.~Geography)

### **Variables GENERO_SALARIO_PAÍS**

Genero_salario<-ggplot(Compradores_datos,aes(x=Gender,y=EstimatedSalary))+
  geom_boxplot(aes(fill=Gender),outlier.shape ='x')+
  xlab('Genero')+ ylab('Salario Estimado') + 
  ggtitle('Distribucion de Salario por Genero-País')+
  facet_grid(.~Geography)+
  theme_bw();Genero_salario

### **Variables SALARIO_ESTIMADO_EDAD_PAIS**

salario_Edad<- ggplot(Compradores_datos,aes(x=Age,y=EstimatedSalary))+
  geom_point(mapping = aes(x=Age,y=EstimatedSalary,color=Geography,fill=Geography))+
  xlab('Edad')+ ylab('Salario Estimado') + 
  ggtitle('Distribucion de Salario por País-Edad');salario_Edad

salario_Edad<- salario_Edad+
  geom_smooth(method='lm',color='black')+
  facet_grid(.~Geography);salario_Edad

#Podemos apreciar que no existe una correlacion lineal entre las variables, ya que 
#no hay una tendencia clara en la dispersión de sus datos
#Nota: Si utilizamos un nivel de confianza del 95% y obtenemos que p < .05, rechazamos 
#la H0 y decimos que existe una correlación significativa (H1)

### **Variables: CREDIT_SCORE_BALANCE**

CreditScore_Balance<- ggplot(Compradores_datos,aes(x=CreditScore,y=Balance))+
  geom_point(mapping = aes(x=CreditScore,y=Balance,color=Geography,fill=Geography))+
  xlab('CrediScore')+ ylab('Balance') + 
  ggtitle('Distribucion de Credit Score y Balance');CreditScore_Balance

CreditScore_Balance<- CreditScore_Balance+
  geom_smooth(method='lm',color='black')+
  facet_grid(.~Geography);CreditScore_Balance

#No se aprecia una correlacion por lo que se adeuda (Balance) y rating de crédito 
#(CreditScore)

### **Variable: GENERO_PAIS_SALARIO**

Salario_genero_normal <-ggplot(Compradores_datos)
Salario_genero_normal+geom_histogram(aes(x=EstimatedSalary,y=..density.., fill=Gender), 
                                     bins = 50, color = 'black') + 
  xlab('Salario Anual Estimado')+ ylab('Densidad') + 
  stat_function(fun = dnorm,args = list(mean = mean(Compradores_datos$EstimatedSalary), 
                                        sd = sd(Compradores_datos$EstimatedSalary))) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  ggtitle('Distribucion de Salario por Genero')+
  facet_grid(Geography~Gender)

#Podemos apreciar que la Curtosis es tipo Platicúrtica

### **Variables: TENURE_BALANCE_GENDER**

#Relación de deuda vencida por Genero 

Ternure0<-Compradores_datos %>%
  filter(Tenure == '0') %>%
  filter(Balance > 0)

Deudavencida <- ggplot(Ternure0, aes(x=Gender, fill=Gender))+
  geom_bar()+
  ylim(0,70)+
  geom_text(stat='count', aes(x = Gender, label = ..count..), vjust = -1)+
  facet_grid(~Geography);Deudavencida

#Observamos que hay tendencia que las mujeres son las que quedan con mas deuda vencida.

### **Variable: CREDITSCORE_SEGMENTACIÓN_CLIENTE**

Compradores_datos$CalificacionCreditScore<-NA

Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore <=550]<-'Bad'
Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore>=551 & 
                                            Compradores_datos$CreditScore<=750]<-'Good'
Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore>=751]<-'Excellent'
any(is.na(Compradores_datos$CalificacionCreditScore))
Compradores_datos$CalificacionCreditScore<- factor(Compradores_datos$CalificacionCreditScore, 
                                                   levels = c('Bad','Good','Excellent'), 
                                                   ordered = T)
table(Compradores_datos$CalificacionCreditScore)

library(scales)
summary(Compradores_datos$CreditScore)
creditscorebygender<-ggplot(Compradores_datos)+
  geom_bar(mapping = aes(x=CalificacionCreditScore,fill=CalificacionCreditScore))+
  theme(panel.background = element_rect(fill='white'),axis.line.x = element_line(colour = 'black', 
                                                                                 linetype =1, size=1), 
        axis.line.y=element_line(colour = 'black',linetype=1, size=1))+
  ggtitle('Credit Score by Country')+
  theme(plot.title = element_text(family='',
                                  size=rel(1.5),
                                  face="bold",
                                  color="black",
                                  lineheight=1.5,
                                  hjust = 0.5))+
  xlab('Calsificación Credit Score')+
  ylab('Count')+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=rel(1.5)))+
  facet_grid(.~Geography);creditscorebygender

#Para esta grafica utilizamos un supuesto para segmentar a los clientes en base a su 
#credit_score en donde: 
  
Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore <=550]<-'Bad'
Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore>=551 & 
                                            Compradores_datos$CreditScore<=750]<-'Good'
Compradores_datos$CalificacionCreditScore[Compradores_datos$CreditScore>=751]<-'Excellent'

# **Parte 3 Trabajo: Análisis Estadístico e Interpretación de Resultados**

### **hipotesis inicial:_"el salario promedio de los consumidores es igual en los diferentes 
#paises donde se han recogido los datos"_**

### **H~0~: la media de los salarios en Alemania,Francia y España son iguales**

### **H~1~: la media de los salarios en Alemania,Francia y España son distintas**

#Para realizar el análisis hemos escogido el test de ANOVA, ya que existe una variable 
#numérica (Estimated_ Salary) y tres categoricas (Alemania,Francia y España)

plotmeans(Compradores_datos$EstimatedSalary~Compradores_datos$Geography,
          mean.labels=T,main = 'Salario Promedio por País',ylab='Salario Estimado',xlab='País')

#Como primera impresión podemos ver que existe una similaridad entre medias.Siendo el salario
#promedio ligeramente supeior en Germany.

## **Analizar Supuestos:**

### **Normalidad**

Datos<- Compradores_datos[c(4,12)]
Datos<-split (Datos, Datos$Geography)
Datos<- rbind(Datos$France,Datos$Spain,Datos$Germany)

library(plyr)
m <- ddply(Datos, "Geography", summarise, grp.mean=mean(EstimatedSalary,na.rm = TRUE));
head(m);

Estimado2<- Datos$EstimatedSalary
ggplot(data= Compradores_datos,aes(x=EstimatedSalary,fill=Geography))+
  geom_histogram(aes(y = ..density..),bins=15, color= 'cyan')+
  stat_function(fun = dnorm,args = list(mean = mean(Compradores_datos$EstimatedSalary), 
                                        sd = sd(Compradores_datos$EstimatedSalary)))+
  geom_vline(data=m, aes(xintercept=grp.mean, color="medias"),
             linetype="dashed")+
  labs(x='Estamated Salary',y= 'Density', 
       title = "Salario Estimado por pais: Distribucion normal y Media")+facet_wrap(~Geography,nrow=3)

#Podemos observar que la distribucion del salario estimado entre pais es mas o menos 
#normal,nos apoyaremos de otros métodos para concluir sobre la distribucion normal

Spain<- filter(Compradores_datos,Geography=='Spain' )
Salary_Spain<-Spain$EstimatedSalary
Germany<-filter(Compradores_datos,Geography=='Germany' )
Salary_Germany<-Germany$EstimatedSalary
France<-filter(Compradores_datos,Geography=='France')
Salary_France<-France$EstimatedSalary

EstimatedSalary_1<-Compradores_datos$EstimatedSalary
normalize_Spain<-mean(EstimatedSalary_1)+(mean(Salary_Spain)-mean(EstimatedSalary_1))+
  ((Salary_Spain)-mean(Salary_Spain))
normalize_Germany<-mean(EstimatedSalary_1)+(mean(Salary_Germany)-mean(EstimatedSalary_1))+
  ((Salary_Germany)-mean(Salary_Germany))
normalize_France<-mean(EstimatedSalary_1)+(mean(Salary_France)-mean(EstimatedSalary_1))+
  ((Salary_France)-mean(Salary_France))

df<-rbind(normalize_Spain,normalize_Germany,normalize_France)
normalizado<-(EstimatedSalary_1-mean(EstimatedSalary_1))/sd(EstimatedSalary_1)
df_2<-mutate(Compradores_datos,normalizado)

ggplot(df_2,aes(x=normalizado,fill= Geography))+geom_histogram(aes(y = ..density..),
                                                               bins=15, color= 'cyan')+
  stat_function(fun = dnorm, args = list(mean = mean(normalizado), sd = sd(normalizado)))+
  facet_wrap(~Geography,nrow=3) +
  labs(x='Estamated Salary',y= 'Density')+
  geom_vline( xintercept=mean(normalizado),color = "red")
#Aspecto que presentaría la distribución con los datos normalizados

#**QQ-TEST**

qqnorm(Compradores_datos$EstimatedSalary, main="Q-Q plot de los salarios");
qqline(Compradores_datos$EstimatedSalary,distribution = qnorm, col = "steelblue",lwd = 4)

#Este test nos representa la distribución que seguirá nuestra distribución si fuera normal.
#Se puede observar que existen diferencias entre la curva teórica y la de nuestros datos.


#**Kolmogorov-Smirnov Tests**

ks.test(Compradores_datos$EstimatedSalary,'pnorm')

#Según el resultado de este test, la distribución de la variable salario no es normal 
#ya que p value<0.05


#**Lillie test**
  
install.packages('nortest')
lillie.test(Compradores_datos$EstimatedSalary)

#De nuevo esta hipótesis nos dice que la distribución de la muestra no es normal p-value>0.05


### **HOMOCEDASTICIDAD**

bartlett.test(Compradores_datos$EstimatedSalary,Compradores_datos$Geography )

#Según este test, las varianzas son iguales,el p-value > 0.05

#También se puede chequear este supuesto mediante un by y observando los resultados 
#que se obtienen

by(Compradores_datos$EstimatedSalary,Compradores_datos$Geography,var)

#Queda demostrado que las varianzas son practicamente iguales. 

## {-}

# ANOVA

anova(lm(Compradores_datos$EstimatedSalary~Compradores_datos$Geography))

#La primera linea nos habla de la variacion entre grupos, se aprueba ya que el P value es > a 0.05

x <- aov(EstimatedSalary ~ Geography, data = Compradores_datos)
summary(x)
#Se comprueba mediante otro metodo que la hipótesis nula será aceptada, p-value>0.05


pairwise.t.test(Compradores_datos$EstimatedSalary, Compradores_datos$Geography, p.adj="bonferroni")

## Con esta función lo que hacemos es comparar las variables dos a dos. No sería necesario hacerlo
# ya que el test en el test anova ha dado como resultados p-value>0.05.
##Finalmente con esta función podemo observar que las medias entre las variables son casi iguales.

### **CONCLUSIÓN: aceptamos la hipótesis nula  H~0~ que la media de los salarios por país son igual.
