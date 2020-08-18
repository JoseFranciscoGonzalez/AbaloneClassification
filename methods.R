\documentclass[a4paper]{article}

\usepackage{fullpage} 
\usepackage{parskip} 
\usepackage{amsmath}
\usepackage{anysize}
\usepackage[dvipsnames]{xcolor}
\usepackage{hyperref}
\usepackage[spanish]{babel}  
\usepackage[utf8x]{inputenc}   
\usepackage[margin=2cm]{geometry}
\usepackage{multirow}
\usepackage{booktabs}
\usepackage{mathtools}
\usepackage{float}
\usepackage{adjustbox}
\usepackage[bottom]{footmisc}
\usepackage{makecell}
    \setcellgapes{5pt}


\begin{document}


%
% Hago que las páginas se comiencen a contar a partir de aquí:
%
\setcounter{page}{1}

%
% Pongo el índice en una página aparte:
%


%
% Inicio del TP:
%
\thispagestyle{empty}

\begin{center}
{\LARGE{\bfseries Trabajo Práctico N\textsuperscript{o}2 - Modelos de Clasificación}}\\
\hspace
{\large{\bfseries Aprendizaje Estadístico - FIUBA}}\\
{\large{\bfseries 1\textsuperscript{er} Cuatrimestre - 2020}}
\end{center}

\hspace

\begin{center}
{\large{\textfont{José F. González - 100063 - \url{jfgonzalez@fi.uba.ar}}}\\
\end{center}

\begin{center}
{\large{\textfont{Atento a: Ing. Jemina García}}} \hfill {\large{\textfont{Revisión 1.8}}}\\
\end{center}


\section{Introducción}

Nos interesa construir un modelo de clasificación para predecir si un espécimen de abulone\footnote{\href{https://es.wikipedia.org/wiki/Haliotis}{Haliotis - Wikipedia}} es adulto o infante dados su \texttt{\textcolor{BurntOrange}{longitud}}, \texttt{\textcolor{BurntOrange}{peso.total}} y \texttt{\textcolor{BurntOrange}{anillos}}. Para entrenar el modelo de clasificación disponemos de 4177 observaciones de abulones ya clasificados con sus distintas medidas, de las cuales definimos el subconjunto \textt{\textcolor{BurntOrange}{data.tr}} con el $80\%$ (3341 obsevaciones) para entrenar los distintos modelos y el subconjunto \textt{\textcolor{BurntOrange}{data.te}} con el restante $20\%$ para evaluarlos y compararlos. \textbf{Como criterio general buscamos minimizar la tasa de error en los conjuntos de entrenamiento, maximizando la medida \textit{accuracy}}. Siendo que hay suficientes datos disponibles para entrenar y evaluar no se usaran métodos de remuestreo.


\section{Modelos de Regresión Logística}

\subsection{Primer Modelo Logístico - Clasificación por longitud}

Comenzamos proponiendo distintos modelos logísticos y comparandolos entre ellos. Estos modelos serán distintas formas de estimar la probabilidad de que la variable \texttt{\textcolor{BurntOrange}{adulto}} tome uno entre dos valores \texttt{\textcolor{BurntOrange}{Sí}} o \texttt{\textcolor{BurntOrange}{No}}.

En la Figura \ref{fig:data} se graficaron la \texttt{\textcolor{BurntOrange}{longitud}} y \texttt{\textcolor{BurntOrange}{peso.total}} para el subconjunto \texttt{\textcolor{BurntOrange}{data.tr}}. En el panel izquierdo de la Figura \ref{fig:data} se muestran los primeros doscientos casos de especímenes adultos en celeste y los infantes en naranja . Parece que los casos infantes tienden a tener menor longitud que los adultos. En el panel derecho de la Figura \ref{fig:data} se muestra el diagrama de cajas de la distribución de la \texttt{\textcolor{BurntOrange}{longitud}} partida en la variable binaria \textttt{\textcolor{BurntOrange}{adulto}}.


<<echo=FALSE, message=FALSE>>=
library(xtable)

data <- read.csv("./data/abalone.csv",sep=",",header = TRUE)
data <- data[c("sexo", "longitud", "peso.total", "anillos")]

@

<<echo=FALSE>>=
## Simbolos y colores
data$colour="cadetblue3"
data$symbol=1
## Adultos e Infantes
data$adulto="Sí"
## Femeninos
data$colour[data$sexo=="F"]="cadetblue3"
data$symbol[data$sexo=="F"]=1
data$adulto[data$sexo=="F"]="Sí"
## Masculinos
data$colour[data$sexo=="M"]="cadetblue3"
data$symbol[data$sexo=="M"]=1
data$adulto[data$sexo=="M"]="Sí"
## Infantes
data$colour[data$sexo=="I"]="darkorange3"
data$symbol[data$sexo=="I"]=3
data$adulto[data$sexo=="I"]="No"

data.tr <- data[1:3341,]
data.te <- data[3342:4177,]
@

\begin{figure}[!b]
\caption{\textbf{Izquierda:} La longitud y peso total de doscientos especímenes. Los especímenes adultos se muestran en celeste, los infantes en naranja. \textbf{Derecha:} Boxplot de la longitud como función de la varaible binaria \texttt{\textcolor{BurntOrange}{adulto}} que toma valores \texttt{\textcolor{BurntOrange}{Sí}} o \texttt{\textcolor{BurntOrange}{No}}.}
\label{fig:data}
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cahce=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

plot(data.tr$peso.total[c(1:200)],
	data.tr$longitud[c(1:200)],
	col=data.tr$colour,
	pch=data.tr$symbol,
    xlab="Peso.Total",
    ylab="Longitud",
    cex.lab=0.75,
	cex.axis=0.75)

boxplot(data.tr$longitud ~ data.tr$adulto ,
	col=c("darkorange3","cadetblue3"),
	ylab="Longitud",
	xlab="Adulto",
	cex.lab=0.75,
	cex.axis=0.75)
@
\end{figure}


El primer modelo logístico que analizamos es el más simple, queremos estimar la probabilidad \texttt{\textcolor{BurntOrange}{adulto}} dado \texttt{\textcolor{BurntOrange}{longitud}}. Para ello utilizamos la función logística de la Ecuación 1 donde los parámetros $\beta_0$ y $\beta_1$ se estiman por máxima verosimilitud y se muestran en el Cuadro 1.

\begin{equation}
\mathbb P(\texttt{\textcolor{BurntOrange}{adulto}} = \texttt{\textcolor{BurntOrange}{Sí}} | \texttt{\textcolor{BurntOrange}{longitud}}) = \frac{e^{\beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{longitud}}}}{1+e^{\beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{longitud}}}}
\end{equation}


<<echo=FALSE, results = "asis">>=
data.tr$adulto <- factor(data.tr$adult)
model <- glm(adulto ~ longitud, data = data.tr, family = binomial)
xtable(summary(model)$coefficients, caption = "Coeficientes de regresión logística para la probabilidad de un espécimen sea adulto dada su longitud estimados con los datos de data.tr", label = "tab:model1", table.placement = "!t")
@

El Cuadro \ref{tab:model1} muestra las estimaciones de coeficientes para el primer modelo. Vemos que $\hat{\beta_1}=12.25$ indicando que un incremento en \texttt{\textcolor{BurntOrange}{longitud}} de $0.1$ aumenta un $1.2$ el $log(odds)$ equivalente a un aumento de $0.016$ en la probabilidad de ser \texttt{\textcolor{BurntOrange}{adulto}}. Los p-valores del Cuadro XXX están asociados al test con hipótesis nula $H_0: \beta_1=0$ y la rechazan a un nivel de significación $\alpha << 1$, es decir, hay suficiente evidencia de una asociación entre \texttt{\textcolor{BurntOrange}{longitud}} y la probabilidad de \texttt{\textcolor{BurntOrange}{adulto}}. Utilizando estas estimaciones el modelo nos dice que, por ejemplo, la probabilidad estimada de que un espécimen sea adulto cuando su longitud $0.4$ será

\begin{equation}
\hat{p} = \frac{1}{1+e^{5.35-12.25\times0.4}} = 0.39
\end{equation} 

Antes de comenzar a evaluar el modelo con los datos de prueba se debe elegir un umbral de decisión. Utilizando la curva ROC de la Figura \ref{fig:model1} se usa el umbral de probabilidad $p=0.74$ para separar entre adulto e infante. Ahora, utilizando el conjunto \texttt{\textcolor{BurntOrange}{data.te}} evaluamos al modelo sobre datos distintos a los de entrenamiento. Estimando las probabilidades y utilizando el corte $p=0.74$ se construye la matriz de confusión del Cuadro \ref{tab:conf1}. En ella se ve que se predicen correctamente 417 adultos y 232 infantes, una tasa global de $78\%$. Mientras que se clasifican incorrectamente 116 adultos como infantes y 71 infantes como adultos, dando una tasa de error de $22\%$. Resumimos el desempeño de este clasificador sobre las métricas \textit{accuracy}, \textit{precision} y \textit{recall} que se muestran en el Cuadro


<<echo=FALSE, eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(model, newdata = test, type = "response")
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.74] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del primer modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf1")
@

\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 232 & 116 \\ 
  Sí &  71 & 417 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.78\\
Precision 			& $TP/(TP+FP)$  & 0.85\\
Recall 				& $TP/(TP+FN)$  & 0.78\\
\hline
\end{tabular}
\caption{\textbf{Izquierda:} Matriz de confusión comparando las predicciones del primer modelo con los resultados reales en el conjunto de entrenamiento \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Derecha:} Medidas de desempeño del modelo de regresión logística utilizando la \texttt{\textcolor{BurntOrange}{longitud}} como predictora sobre el conjunto de prueba \texttt{\textcolor{BurntOrange}{data.te}}}.
\label{tab:medidas1}
\end{table}

\begin{figure}[H]
\caption{\textbf{Izquierda:} Probabilidades estimadas de \texttt{\textcolor{BurntOrange}{adulto}} por el modelo logístico. \textbf{Derecha:} Curva ROC para el clasificador logístico sobre \texttt{\textcolor{BurntOrange}{data.tr}}. TPR es la fracción de casos clasificados correctamente como adultos. FPR la fracción de infantes clasificados incorrectamente como adultos. Se elige un umbral de decisión $p=0.8$ cerca del extremo (0,1).}
\label{fig:model1}
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cahce=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

data.tr$dummy[data.tr$adulto=="Sí"]=1
data.tr$dummy[data.tr$adulto=="No"]=0

plot(data.tr$longitud[c(1:200)],
    data.tr$dummy[c(1:200)],
    col=data.tr$colour,
    pch=data.tr$symbol,
    xlab="Longitud",
    ylab="Probabilidad de Adulto",
    cex.lab=0.75,
	cex.axis=0.75)

x<-seq(0,0.7,0.001)
coef<-summary(model)$coefficients
lines(x, exp(coef[1]+coef[2]*x)/(1+exp(coef[1]+coef[2]*x)), col="cadetblue3")

library(pROC)
prediccion <- predict(model, type = "response")

r<-roc(data.tr$adulto,prediccion)
plot(x=1-r$specificities,y=r$sensitivities,
	col="cadetblue3",
	lwd=1,
	type="l",
	xlab="FPR",
    ylab="TPR",
    cex.lab=0.75,
	cex.axis=0.75)
points(1-r$specificities[81], r$sensitivities[81])
text(1-r$specificities[81]-0.03, r$sensitivities[81]+0.05, labels=round(r$thresholds[81],2), cex= 0.7)
@
\end{figure}

\clearpage
\subsection{Segundo Modelo Logístico - Clasificación por peso}

\begin{figure}[t]
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cahce=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

plot(y=data.tr$peso.total[c(1:200)],
	x=data.tr$longitud[c(1:200)],
	col=data.tr$colour,
	pch=data.tr$symbol,
    ylab="Peso.Total",
    xlab="Longitud",
    cex.lab=0.75,
	cex.axis=0.75)

boxplot(data.tr$peso.total ~ data.tr$adulto ,
	col=c("darkorange3","cadetblue3"),
	ylab="Peso Total",
	xlab="Adulto",
	cex.lab=0.75,
	cex.axis=0.75)
@
\caption{\textbf{Izquierda:} La longitud y peso total de doscientos especímenes. Los especímenes adultos se muestran en celeste, los infantes en naranja. \textbf{Derecha:} Boxplot del peso total como función de la variable binaria \texttt{\textcolor{BurntOrange}{adulto}}.}
\label{fig:data2}
\end{figure}

En el panel izquierdo de la Figura \ref{fig:data2} se muestran el \texttt{\textcolor{BurntOrange}{peso.total}} y \texttt{\textcolor{BurntOrange}{longitud}} de los primeros 200 casos del conjunto de datos \texttt{\textcolor{BurntOrange}{data.tr}}. En el panel derecho de la Figura \ref{fig:data2} se graficó el diagrama de cajas para la distribución de \texttt{\textcolor{BurntOrange}{peso.total}} sobre la variable binaria \texttt{\textcolor{BurntOrange}{adulto}}. Estos sugieren que el peso como predictora sea una buena forma de clasificar entre adultos e infantes de la forma


\begin{equation}
\mathbb P(\texttt{\textcolor{BurntOrange}{adulto}} = \texttt{\textcolor{BurntOrange}{Sí}} | \texttt{\textcolor{BurntOrange}{peso.total}}) = \frac{e^{\beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{peso.total}}}}{1+e^{\beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{peso.total}}}}
\end{equation}

En el Cuadro \ref{tab:model2} se muestran los resultados de los estimadores de máxima verosimilitud para $\beta_0$ y $\beta_1$. Vemos que $\hat{\beta_1}=4.10$, indicando un aumento de la probabilidad de ser adulto ante incrementos positivos en el peso. El p-valor indica evidencia a favor de $\beta_1 \neq 0$, es decir, que la variable \texttt{\textcolor{BurntOrange}{peso.total}} contribuye a explicar la variable \texttt{\textcolor{BurntOrange}{adulto}}.

<<echo=FALSE, results = "asis">>=
model2 <- glm(adulto ~ peso.total, data = data.tr, family = binomial)
xtable(summary(model2)$coefficients, caption = "Para data.tr, coeficientes estimados de regresión logística para la probabilidad de un espécimen sea adulto dado su peso total", label = "tab:model2", table.placement = "h")
@

En el gráfico izquierdo de la Figura \ref{fig:model2} se muestra las probabilidades estimadas de $p(\texttt{\textcolor{BurntOrange}{adulto}}|x)=1$ según $x=\texttt{\textcolor{BurntOrange}{peso.total}}$. En el gráfico derecho de la \ref{fig:model2} se muestra la curva ROC del clasificador. Sobre ella eligió un valor de decisión $p=0.71$ cercano al vertice (0,1). Utilizando este valor se evalua el rendimiento del clasificador sobre \texttt{\textcolor{BurntOrange}{data.te}}. En el Cuadro \ref{tab:medidas2} se resumen los resultados de la predicción, este modelo predice correctamente el $80\%$ de los casos, errando el $10\%$ restante.

<<echo=FALSE, eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(model2, newdata = test, type = "response")
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.74] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del segundo modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf2")
@

\begin{table}[b]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 254 & 116 \\ 
  Sí &  49 & 417 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.80\\
Precision 			& $TP/(TP+FP)$  & 0.89 \\
Recall 				& $TP/(TP+FN)$  & 0.78\\
\hline
\end{tabular}
\caption{\textbf{Izquierda:} Matriz de confusión comparando las predicciones del segundo modelo con los resultados reales en el conjunto de entrenamiento \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Derecha:} Medidas de desempeño del modelo de regresión logística utilizando la variable \texttt{\textcolor{BurntOrange}{peso.total}} como predictora sobre el conjunto de prueba \texttt{\textcolor{BurntOrange}{data.te}}}.
\label{tab:medidas2}
\end{table}


\begin{figure}[H]
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cache=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

plot(data.tr$peso.total[c(1:200)],
    data.tr$dummy[c(1:200)],
    col=data.tr$colour,
    pch=data.tr$symbol,
    xlab="Peso Total",
    ylab="Probabilidad de Adulto",
    cex.lab=0.75,
	cex.axis=0.75)

x<-seq(0,2.5,0.001)
coef2<-summary(model2)$coefficients
lines(x, exp(coef2[1]+coef2[2]*x)/(1+exp(coef2[1]+coef2[2]*x)), col="cadetblue3")

library(pROC)
prediccion <- predict(model2, type = "response")

r<-roc(data.tr$adulto,prediccion)
plot(x=1-r$specificities,y=r$sensitivities,
	col="cadetblue3",
	lwd=1,
	type="l",
	xlab="FPR",
    ylab="TPR",
    cex.lab=0.75,
	cex.axis=0.75)
points(1-r$specificities[892], r$sensitivities[892])
text(1-r$specificities[892]-0.03, r$sensitivities[892]+0.05, labels=round(r$thresholds[892],2), cex= 0.7)
@
\caption{\textbf{Izquierda:} Probabilidades estimadas de \texttt{\textcolor{BurntOrange}{adulto}} por el modelo logístico. \textbf{Derecha:} Curva ROC para el clasificador logístico sobre \texttt{\textcolor{BurntOrange}{data.tr}}. TPR es la fracción de casos clasificados correctamente como adultos. FPR la fracción de infantes clasificados incorrectamente como adultos. Se elige un umbral de decisión $p=0.71$ cerca del extremo (0,1).}
\label{fig:model2}
\end{figure}


\subsection{Tercer Modelo Logístico - Clasificación por anillos}

De igual forma que en los casos anteriores realizamos otro modelo de regresión logística utilizando como predictora \texttt{\textcolor{BurntOrange}{anillos}}. En la Figura \ref{fig:data3} se muestran los datos de \texttt{\textcolor{BurntOrange}{data.tr}} disponibles para entrenar. En el Cuadro \ref{tab:model3} se muestran las estimaciones de coeficientes en $logit = \beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{anillos}}$. La variable \texttt{\textcolor{BurntOrange}{anillos}} vuelve a ser significativa y con $\hat{\beta_1}=0.5$ indica variaciones positivas en \textit{logit} ante variaciones positivas en \texttt{\textcolor{BurntOrange}{anillos}}.

<<echo=FALSE, results = "asis">>=
model3 <- glm(adulto ~ anillos, data = data.tr, family = binomial)
xtable(summary(model3)$coefficients, caption = "Coeficientes de regresión logística para la probabilidad de un espécimen sea adulto dado su cantidad de anillos estimados con los datos data.tr", label = "tab:model3", table.placement = "h")
@

\begin{figure}[H]
\caption{\textbf{Izquierda:} La longitud y cantidad de anillos de doscientos especímenes en \texttt{\textcolor{BurntOrange}{data.tr}}. Los especímenes adultos se muestran en celeste, los infantes en naranja. Se muestran solo 200 casos por claridad. \textbf{Derecha:} Boxplot de la cantidad de anillos como función de la variable binaria \texttt{\textcolor{BurntOrange}{adulto}}.}
\label{fig:data3}
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cahce=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

plot(y=data.tr$anillos[c(1:200)],
	x=data.tr$longitud[c(1:200)],
	col=data.tr$colour,
	pch=data.tr$symbol,
    ylab="Anillos",
    xlab="Longitud",
    cex.lab=0.75,
	cex.axis=0.75)

boxplot(data.tr$anillos ~ data.tr$adulto ,
	col=c("darkorange3","cadetblue3"),
	ylab="Anillos",
	xlab="Adulto",
	cex.lab=0.75,
	cex.axis=0.75)
@

\end{figure}


\begin{figure}[t]
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cache=TRUE, fig.height=3>>=
par(mfrow=c(1,2), mar=c(4, 4, 1, 3))  

plot(data.tr$anillos[c(1:200)],
    data.tr$dummy[c(1:200)],
    col=data.tr$colour,
    pch=data.tr$symbol,
    xlab="Anillos",
    ylab="Probabilidad de Adulto",
    cex.lab=0.75,
	cex.axis=0.75)

x<-seq(0,25,1)
coef3<-summary(model3)$coefficients
lines(x, exp(coef3[1]+coef3[2]*x)/(1+exp(coef3[1]+coef3[2]*x)), col="cadetblue3")

prediccion <- predict(model3, type = "response")

r<-roc(data.tr$adulto,prediccion)
plot(x=1-r$specificities,y=r$sensitivities,
	col="cadetblue3",
	lwd=1,
	type="l",
	xlab="FPR",
    ylab="TPR",
    cex.lab=0.75,
	cex.axis=0.75)
points(1-r$specificities[9], r$sensitivities[9])
text(1-r$specificities[9]-0.03, r$sensitivities[9]+0.05, labels=round(r$thresholds[9],2), cex= 0.7)
@
\caption{\textbf{Izquierda:} Probabilidades estimadas de \texttt{\textcolor{BurntOrange}{adulto}} por el modelo logístico. \textbf{Derecha:} Curva ROC para el clasificador logístico sobre \texttt{\textcolor{BurntOrange}{data.tr}}. TPR es la fracción de casos clasificados correctamente como adultos. FPR la fracción de infantes clasificados incorrectamente como adultos. Se elige un umbral de decisión $p=0.61$ cerca del extremo (0,1).}
\label{fig:model3}
\end{figure}


En base a la curva ROC de la Figura \ref{fig:model3} se elige el umbral de decisión $p=0.61$. El el Cuadro \ref{tab:medidas3} se resumen el rendimiento del clasificador sobre los datos de prueba \texttt{\textcolor{BurntOrange}{data.te}}.


<<echo=FALSE, eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(model3, newdata = test, type = "response")
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.61] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del tercer modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf3")
@

\begin{table}[h]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 207 &  92 \\ 
  Sí &  96 & 441 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.78\\
Precision 			& $TP/(TP+FP)$  &  0.82\\
Recall 				& $TP/(TP+FN)$  & 0.83\\
\hline
\end{tabular}
\caption{\textbf{Izquierda:} Matriz de confusión comparando las predicciones del tercer modelo con los resultados reales en el conjunto de entrenamiento \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Derecha:} Medidas de desempeño del modelo de regresión logística utilizando la variable \texttt{\textcolor{BurntOrange}{anillos}} como predictora sobre el conjunto de prueba \texttt{\textcolor{BurntOrange}{data.te}}}.
\label{tab:medidas3}
\end{table}

\subsection{Cuarto Modelo Logístico - Clasificación por todas las medidas}

El último modelo contempla todas las variables predictoras utilizadas, $logit = \beta_0 + \beta_1\texttt{\textcolor{BurntOrange}{longitud}} + \beta_2 \texttt{\textcolor{BurntOrange}{peso.total}} + \beta_3 \texttt{\textcolor{BurntOrange}{anillos}}$. Los resultados del ajuste se muestran en el Cuadro \ref{tab:model4}. La probabilidad de umbral se elige según la curva ROC de la Figura \ref{fig:model4} como $p=0.63$. Evaluando el modelo en el conjunto de entrenamiento se obtienen los resultados del Cuadro \ref{tab:model4}. 

<<echo=FALSE, results = "asis">>=
model4 <- glm(adulto ~ longitud + peso.total + anillos, data = data.tr, family = binomial)
xtable(summary(model4)$coefficients, caption = "Para data.tr, coeficientes estimados de regresión logística para la probabilidad de un espécimen sea adulto dado su cantidad de anillos, peso total y longitud", label = "tab:model4", table.placement = "h")
@

\begin{figure}[t]
<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cache=TRUE, fig.height=3>>=

layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = TRUE))


plot(data.tr$peso.total[c(1:200)],
    data.tr$dummy[c(1:200)],
    col=data.tr$colour,
    pch=data.tr$symbol,
    xlab="Peso Total",
    ylab="Probabilidad de Adulto",
    cex.lab=0.9,
	cex.axis=0.9)

coef4<-summary(model4)$coefficients

x<-seq(0,2.5,0.1)
for (y in seq(0,0.7,0.1)) {
  for (z in seq(1,20,5)) {
    
lines(x, exp(coef4[1]+coef4[2]*0.4+coef4[3]*x+coef4[4]*z)/(1+exp(coef4[1]+coef4[2]*0.4+coef4[3]*x+coef4[4]*z)),
      col="cadetblue3")
  }
}

prediccion <- predict(model4, type = "response")

r<-roc(data.tr$adulto,prediccion)
plot(x=1-r$specificities,y=r$sensitivities,
	col="cadetblue3",
	lwd=1,
	type="l",
	xlab="FPR",
    ylab="TPR",
    cex.lab=0.9,
	cex.axis=0.9)
points(1-r$specificities[1288], r$sensitivities[1288])
text(1-r$specificities[1288]-0.03, r$sensitivities[1288]+0.05, labels=round(r$thresholds[1288],2), cex= 0.9)
@
\caption{\textbf{Izquierda:} Probabilidades estimadas del modelo con todas la variables en función de \texttt{\textcolor{BurntOrange}{peso.total}} con \texttt{\textcolor{BurntOrange}{longitud=0.4}} y $\texttt{\textcolor{BurntOrange}{anillos}} \in (1,6,11,16)$. \textbf{Derecha:} Curva ROC para el modelo.}
\label{fig:model4}
\end{figure}

<<echo=FALSE,eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(model4, newdata = test, type = "response")
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.63] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del tercer modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf3")
@


\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 237 &  81 \\ 
  Sí &  66 & 452 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.82\\
Precision 			& $TP/(TP+FP)$  &  0.87\\
Recall 				& $TP/(TP+FN)$  & 0.85\\
\hline
\end{tabular}
\caption{\textbf{Izquierda:} Matriz de confusión comparando las predicciones del cuarto modelo con los resultados reales en el conjunto de entrenamiento \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Derecha:} Medidas de desempeño del modelo de regresión logística utilizando todas las variables como predictoras sobre el conjunto de prueba \texttt{\textcolor{BurntOrange}{data.te}}}.
\label{tab:medidas4}
\end{table}

Resulta interesante que en el Cuadro \ref{tab:model4} el coeficiente de la variable longitud se vuelve negativo respecto al primer modelo. Mirando la matriz de correlación entre todas las predictoras vemos que \texttt{\textcolor{BurntOrange}{anillos}} y \texttt{\textcolor{BurntOrange}{peso.total}} tiene mucha correlación (0.89) lo que debe estar generando un efecto de colinealidad.


\vspace{5cm}

\section{Modelo LDA}
 De los modelos anteriores podemos estar seguros que todas las variables predictoras contribuyen a explicar el comportamiento de la variable \texttt{\textcolor{BurntOrange}{adulto}}. Luego tiene sentido construir ahora un clasificador por \textit{LDA} que utilice todas las predictoras. Entonces buscamos estimar las funciones discriminantes para las clases $k$, \texttt{\textcolor{BurntOrange}{adulto}} e \texttt{\textcolor{BurntOrange}{infante}}, y construir límites entre clases donde las funciones se igualan. 

\begin{equation}
\delta_k(x) = x^T\Sigma^{-1}\mu_k -0.5 \mu_k^T\Sigma^{-1}\mu_k + log \pi_k
\end{equation}

 En el Cuadro \ref{tab:LDA1} se muestran los parámetros estimados por frecuencias relativas sobre las observaciones del conjunto \texttt{\textcolor{BurntOrange}{data.tr}}. La probabilidad a priori de la clase \texttt{\textcolor{BurntOrange}{adulto=Sí}} estimada es $\hat{\pi_{Sí}}=0.69$, es la probabilidad de que una muestra aleatoria provenga del grupo \texttt{\textcolor{BurntOrange}{adulto=Sí}}, es decir, el $69\%$ del conjunto de entrenamiento es adulto. Para la clase \texttt{\textcolor{BurntOrange}{adulto=No}}} (Infante), $\hat{\pi_{No}}=0.31$ ($31\%$). El Cuadro \ref{tab:LDA1} también da las medias muestrales de cada predictora dentro de las clases, sugeriendo que las tres predictoras aumentan en casos de abulones adultos. 

En la Figura \ref{fig:LDA1} se muestra el umbral de decisión Bayesiano obtenido para el conjunto de entrenamiento dado por $(\hat{\mu_{Sí}}+\hat{\mu_{No}})/2$. 

<<echo=FALSE, result='asis'>>=
library(MASS)
lda.fit<-lda(adulto~longitud+peso.total+anillos,
             data=data.tr)
@

Para terminar y antes de evaluar elegimos un umbral de decisión para la probabilidad a posteriori $P(\texttt{\textcolor{BurntOrange}{adulto}} |\\ (\texttt{\textcolor{BurntOrange}{longitud}}, \texttt{\textcolor{BurntOrange}{peso.total}}, \texttt{\textcolor{BurntOrange}{anillos}}))$. Para ello utilizamos nuevamente una curva ROC de la Figura \ref{fig:LDA1} variando la probabilidad a posteriori de \texttt{\textcolor{BurntOrange}{adulto==Sí}} y elegimos un valor de probabilidad $0.67$ cercano al ideal $(0,1)$. El Cuadro \ref{tab:LADconf} muestra los resultados de aplicar con esta regla el modelo LDA sobre los datos de evaluación \texttt{\textcolor{BurntOrange}{data.te}}. El modelo predice que de en la muestra hay 527 adultos, de los cuales 451 efectivamente lo son y 76 son infantes. Clasifica 309 como infantes, errando en 82 casos. Globalmente, la tasa de aciertos es $81\%$ y $18\%$ de fallas. Entre los infantes se clasificó incorrectamente el $26\%$. Entre los adultos se clasificó incorrectamente el $14\%$.



\begin{figure}[H]

<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cache=TRUE, fig.height=3>>=

lda.val <- predict(lda.fit)
lda.val$colour[lda.val$class=="No"]="darkorange3"
lda.val$symbol[lda.val$class=="No"]=3
lda.val$colour[lda.val$class=="Sí"]="cadetblue3"
lda.val$symbol[lda.val$class=="Sí"]=1


a <- data.tr[,c(2:4,7)]
a<-a[a$adulto=="Sí",]
a<-a[,c(1:3)]

i <- data.tr[,c(2:4,7)]
i<-i[i$adulto=="No",]
i<-i[,c(1:3)]


z1<-t(lda.fit$scaling)%*%t(a)
z2<-t(lda.fit$scaling)%*%t(i)

layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = TRUE))


plot(z1[1,c(1:100)],rep(2,100), col="cadetblue3",
	pch=3,
	xlab="Eje Discriminante",
	ylab="",
	cex.lab=1,
	cex.axis=1)
abline(v=(mean(z1)+mean(z2))/2,  lwd=1, lty="dashed")
points(z2[1,1:100],rep(2,100),col="darkorange3", pch=1)

lda.val <- predict(lda.fit)

r<-roc(data.tr$adulto,lda.val$posterior[,2])
plot(x=1-r$specificities,y=r$sensitivities,
	col="cadetblue3",
	lwd=1,
	type="l",
	xlab="FPR",
    ylab="TPR",
    cex.lab=1,
	cex.axis=1)
points(1-r$specificities[1252], r$sensitivities[1252])
text(1-r$specificities[1252]-0.03, r$sensitivities[1252]+0.05, labels=round(r$thresholds[1252],2), cex= 1)

@
\caption{\textbf{Izquierda:} Umbral de decisión Bayesiano estimado para \texttt{\textcolor{BurntOrange}{data.tr}}. Se grafican 100 especímenes del conjunto de training, en celeste los adultos y naranja los infantes. \textbf{Derecha:} Curva ROC sobre el conjunto de probabilidades a posteriori de \texttt{\textcolor{BurntOrange}{data.tr}}. Para $\hat{p_k}=0.67$ se minimiza la distancia a vértice (0,1).}
\label{fig:LDA1}
\end{figure}


\begin{table}[h]
\centering
\begin{tabular}{rr}
  \hline
  		&$\hat{\pi_k}$ \\
  \hline
  No (Infante) & 0.31  \\ 
  Sí (Adulto) & 0.69\\
   \hline
\end{tabular}
\hspace{1cm}
\begin{tabular}{lccc}
  \hline
  & \multicolumn{3}{c}{$\hat{\mu_k}$} \\
  \hline
  & longitud & peso.total & anillos \\
  No (Infante) & 0.42 & 0.42 & 7.88 \\
  Sí (Adulto) & 0.57 & 1.00 & 11.01\\ 
   \hline
\end{tabular}
\hspace{1cm}
\begin{tabular}{lc}
  \hline
  & Coef. \\
  \hline
  longitud & 3.44 \\
  peso.total & 1.15 \\
  anillos & 0.11 \\
  \hline
\end{tabular}
\caption{Estimaciones de parámetros del modelo de LDA sobre el conjunto de entrenamiento \texttt{\textcolor{BurntOrange}{data.tr}}. \textbf{Izquierda:} Estimaciones de probabilidades a \textit{priori} de pertenencia a cada clase. \textbf{Centro:} Medias muestrales de cada predictora dentro de cada clase. \textbf{Derecha:} Coeficientes de discriminantes. Son los multiplicadores $3.44\texttt{\textcolor{BurntOrange}{longitud}} + 1.15\texttt{\textcolor{BurntOrange}{peso.total}} + 0.11\texttt{\textcolor{BurntOrange}{anillos}}$ que dan la mejor separación entre las dos clases}
\label{tab:LDA1}
\end{table}

<<echo=FALSE, eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(lda.fit,test)
prediccion<-prediccion$posterior[,2]
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.67] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del tercer modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf3")
@


\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 227 &  82 \\ 
  Sí &  76 & 451 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.81\\
Precision 			& $TP/(TP+FP)$  & 0.86 \\
Recall 				& $TP/(TP+FN)$  & 0.85\\
\hline
\end{tabular}
\caption{Rendimiento del clasificador LDA sobre los datos \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Izquierda:} Matriz de confusión para los 836 casos. \textbf{Derecha:} Medidas de desempeño del modelo de LDA.} 
\label{tab:LDAconf}
\end{table}
              
Podemos intentar hacer al modelo de LDA un poco más flexible. Por intuición debe haber algún tipo de interacción entre \texttt{\textcolor{BurntOrange}{longitud}} y \texttt{\textcolor{BurntOrange}{peso.total}} debido a una densidad de la carne, lo que sugiere que puede ser apropiado incluir un término de interacción entre ellas \texttt{\textcolor{BurntOrange}{longitud:peso.total}}. En el Cuadro \ref{tab:LDAconf2} se muestran los resultados de evaluar este nuevo modelo sobre \texttt{\textcolor{BurntOrange}{data.te}} con un umbral $p=0.71$. Las predicciones mejoran ligeramente, intentar agregar más términos cruzados seguramente sea rebundante y resulte en \textit{overfitting} 


<<echo=FALSE>>=
lda.fit<-lda(adulto~longitud+peso.total+anillos+longitud:peso.total,
             data=data.tr)
@

<<echo=FALSE, eval=FALSE, results = "asis">>=
test <- data.te
prediccion <- predict(lda.fit,test)
prediccion<-prediccion$posterior[,2]
predichos = rep("No", length(prediccion))
predichos[prediccion > 0.71] <- "Sí"
observados <- test$adulto
confusion <- table(predichos, observados)
xtable(confusion, caption="Matriz de confusión comparando las predicciones del tercer modelo con los resultados reales en el conjunto de entrenamiento data.te.", label="tab:conf3")
@

\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 & No & Sí \\ 
  \hline
No & 232 &  74 \\ 
  Sí &  71 & 459 \\ 
   \hline
\end{tabular}
\hspace{4cm}
\begin{tabular}{l c r}
\hline
Medida 				& Definición		& Valor\\
\hline
Accuracy 			& $(TN+TP)/TOT$ & 0.83\\
Precision 			& $TP/(TP+FP)$  & 0.87 \\
Recall 				& $TP/(TP+FN)$  & 0.86\\
\hline
\end{tabular}
\caption{Rendimiento del clasificador LDA sobre los datos \texttt{\textcolor{BurntOrange}{data.te}}. \textbf{Izquierda:} Matriz de confusión para los 836 casos. \textbf{Derecha:} Medidas de desempeño del modelo de LDA.} 
\label{tab:LDAconf2}
\end{table}

\clearpage
\section{Comparación de Modelos}

En la Figura \ref{fig:comp} se muestra la comparación del desempeño de los distintos modelos. Inicialmente se planteó el \textbf{criterio de maximizar la medida \textit{accuracy} en la clasificación de los datos de \texttt{\textcolor{BurntOrange}{data.te}}}. Con este criterio el mejor modelo es el LDA con una variable de interacción. Sin embargo todos los modelos tienen un buen desempeño, lo que sugiere que el verdadero límite de decisión para \texttt{\textcolor{BurntOrange}{adulto}} que tratamos de estimar sea lineal como suponen los métodos de regresión logística y LDA, y que métodos más flexibles como QDA o KNN tengan resultados más pobres.


\begin{figure}[t]

<<echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, cache=TRUE, fig.height=2>>=
	
layout(matrix(c(1,1,1), nrow = 1, ncol = 3, byrow = TRUE))

y <- c(0.78, 0.8, 0.78, 0.82, 0.81, 0.83)

barplot(y,  ylab="Accuracy",
        col= c( "darkorange3", "darkorange3", "darkorange3", "darkorange3", "cadetblue3", "cadetblue3"),
        ylim=c(0.75,0.85),
        names.arg = c( "Logístico 1", "Logístico 2", "Logístico 3", "Logístico 4", "LDA1", "LDA2"))
@
\caption{\textbf{Izquierda:} Comparación entre la medida \textit{accuracy} de los distintos modelos sobre el conjunto de evaluación \texttt{\textcolor{BurntOrange}{data.te}}.}
\label{fig:comp}
\end{figure}


\section{Bibliografía}

\begin{itemize}
	\item Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani - An Introduction to Statistical Learning  with Applications in R (2015, Springer) - Capítulo IV
	\item Trevor Hastie,  Robert Tibshirani, Jerome Friedman - The Elements of  Statistical Learning, Data Mining, Inference, and Prediction. (2013, Springer) - Capítulo IV
	\item David G. Kleinbaum, Mitchel Klein - Logistic Regression A Self-learning Text (2002, Springer) - Capítulos I a III
	\end{itemize}


\end{document}