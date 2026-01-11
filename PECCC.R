# ==========================================================
# SECCIÓN 2: Contexto y objetivo del estudio
# ==========================================================

if (!require("haven")) install.packages("haven")
library(haven)   # para leer el archivo spss .dat
datos <- read.table("E:/SOFT/PEC4/gbsg_ba_ca.dat", header = TRUE)

#Del servidor hbiostats.org escogimos el dataset german breast cancer data. dataset German Breast Cancer Study Group, utilizado para estudiar la supervivencia y recurrencia del cáncer de mama*
#Especificad qué objetivos o preguntas queréis responder...*


  
# ==========================================================
# SECCIÓN 2: Prospección y preparación de los datos*
# ==========================================================


## 2.1 Descripción de los datos

head(datos)   # Dataframe de 686 observaciones repartidas en 18 variables

summary(datos)  # descripción de cada columna
dim(datos)
str(datos)
colSums(is.na(datos))   # ¿valores ausentes?

# Se ha importado un fichero de texto plano (.dat) con 686 observaciones y 18 variables. El dataset contiene información clínica detallada sobre pacientes con cáncer de mama, incluyendo factores demográficos, características del tumor y variables de seguimiento. Clasificación de las variables: Cualitativas Nominales: id, meno (estado menopáusico), hormon (tratamiento con tamoxifeno); Cualitativas Dicotómicas (Codificadas): censrec (0=censura, 1=evento), gradd1 y gradd2 (variables auxiliares de grado); Cualitativa Ordinal: grade (Grado del tumor: 1, 2, 3); Cuantitativas Continuas: age, size (mm), enodes (proporción), pgr, er, rectime (días de seguimiento) y X_t; Cuantitativas Discretas: nodes (conteo de ganglios).  No se observan valores NA en ninguna de las 18 variables. El dataset es robusto y está completo para el análisis.

# Se detectan columnas constantes que no aportan información al análisis: X_st (siempre es 1) y X_t0 (siempre es 0). Se recomienda eliminarlas.
# Transformaciones necesarias: Las variables meno y hormon vienen como texto (chr). Es imprescindible transformarlas a factores para realizar comparaciones estadísticas. Lo mismo aplica para grade y censrec
# 1. Quitar las columnas constantes que no sirven
datos$X_st <- NULL
datos$X_t0 <- NULL
# 2. Convertir a factores
datos$meno <- as.factor(datos$meno)
datos$hormon <- as.factor(datos$hormon)
datos$grade <- as.factor(datos$grade)
datos$censrec <- as.factor(datos$censrec)

###### AÑADIR CAPTURAS DE PANTALLA DE CADA PASO Y TABLA (TIPOS VARIABLE) ######


## 2.2 Preguntas objetivo

#¿Existe una diferencia notable en la edad media y el tamaño del tumor entre las pacientes que recibieron tratamiento con tamoxifeno (hormon) y las que no?
##Objetivo: Comparar variables continuas según un factor categórico.

#¿Cuántas pacientes menores de 50 años tienen un número de ganglios afectados (nodes) superior a 5?
##Objetivo: Realizar una consulta basada en rangos numéricos múltiples (similar a las consultas de filtrado de la PEC1).

#¿Cómo se distribuyen los grados de agresividad del tumor (grade) entre aquellas pacientes que efectivamente sufrieron una recurrencia o muerte (censrec == 1)?
##Objetivo: Analizar la frecuencia de una variable ordinal en un subgrupo crítico

#¿Podemos clasificar a las pacientes en categorías de "Riesgo Clínico" (Alto/Bajo) basándonos en si cumplen simultáneamente tener un tumor grande (>30mm) y más de 3 ganglios afectados?
##Objetivo: Aplicar la definición de funciones para automatizar la clasificación de datos (metodología del LAB3 y similar a la lógica vista en aplicaciones Shiny ).



# ==========================================================
# SECCIÓN 3: Análisis descriptivo y gráfico
# ==========================================================

# --- 3.1.

# ------ Análisis detallado de la variable 'age'
mean_age <- mean(datos$age)
sd_age <- sd(datos$age)
range_age <- range(datos$age)
cat("Edad media:", mean_age, "\nDesviación típica:", sd_age, "\nRango:", range_age)
#edad media ($53.05$ años): Indica que la muestra se centra en mujeres en torno a la quinta década de vida. Es un dato coherente con la literatura médica sobre el cáncer de mama, donde la incidencia suele aumentar tras la menopausia.Desviación típica ($10.12$ años): Existe una dispersión moderada. Esto significa que la mayoría de las pacientes tienen edades comprendidas aproximadamente entre los $43$ y los $63$ años (media $\pm$ una desviación típica). No es un grupo excesivamente joven ni muy envejecido, sino bastante variado.Rango ($21$ a $80$ años): La amplitud es muy grande ($59$ años de diferencia). El estudio es inclusivo, ya que abarca desde adultos muy jóvenes hasta la tercera edad, lo que permite analizar cómo afecta la enfermedad en distintas etapas del ciclo vital.

# ------ GRAFICAS
if (!require("ggplot2")) install.packages("ggplot2")
#### ----------  Histograma edad
ggplot(datos, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()
#comentario...

#### ----------  Diagrama barras: grado del tumor
ggplot(datos, aes(x = grade, fill = grade)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Pacientes por Grado de Tumor", x = "Grado", y = "Nº de Pacientes") +
  theme_light()
#comentario...

#### ----------  Boxplot: recurrencia vs Hormon
ggplot(datos, aes(x = hormon, y = rectime, fill = hormon)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Impacto del Tratamiento Hormonal en la Recurrencia",
       x = "Tratamiento (Tamoxifeno)", y = "Días hasta recurrencia") +
  theme_classic()
#comentario...

#### ----------  Scatter plot: Tamaño vs Ganglios
ggplot(datos, aes(x = size, y = nodes)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación Tamaño del Tumor y Ganglios Afectados",
       x = "Tamaño (mm)", y = "Nº de Ganglios") +
  theme_minimal()
#comentario...

# --- 3.2.
...


# ==========================================================
# SECCIÓN 4: Modelos de aprendizaje automático
# ==========================================================

# Conviene realizar un enfoque mixto que combine ambos modelos porque la información que ofrecen es complementaria. El supervisado es conveniente porque disponemos de una variable objetivo etiquetada (censrec), que indica si la paciente sufrió recurrencia o fallecimiento. Podemos usarlo para entrenar un modelo a predecir este resultado en nuevas pacientes en función de sus características clínicas.
## El no supervisado es fundamental para el descubrimiento de patrones ocultos en los datos. Permite segmentar los pacientes en grupos con perfiles clínicos similares. Esto es clave para la personalización de tratamientos e identificación de grupos de riesgo
# La elección se justifica por la  naturaleza del problema médico: - Predicción. para cuantificar el riesgo individual de cada paciente (supervisado); Exploración. para entender si existen subgrupos que requieran protocolos diferentes (no supervisado)


# --- 4.1. APRENDIZAJE NO SUPERVISADO (K-means) ---
# Objetivo: Agrupar pacientes por similitud biológica

# 1. Selección y escalado de variables numéricas
datos_cluster <- datos[, c("age", "size", "nodes", "pgr", "er")]
datos_escalados <- scale(datos_cluster)

# 2. Ejecución de K-means (k=3 grupos)
set.seed(123) 
modelo_km <- kmeans(datos_escalados, centers = 3, nstart = 25)

# 3. Guardar el grupo en el dataset y graficar
datos$cluster_perfil <- as.factor(modelo_km$cluster)

library(ggplot2)
ggplot(datos, aes(x = age, y = nodes, color = cluster_perfil)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clustering (No Supervisado): Perfiles de Pacientes",
       x = "Edad", y = "Nº Ganglios", color = "Grupo") +
  theme_minimal()


# --- 4.2. APRENDIZAJE SUPERVISADO (Regresión Logística) ---
# Objetivo: Predicción de recurrencia (Variable objetivo: censrec)

# 1. División Entrenamiento (80%) y Prueba (20%)
set.seed(123)
indices <- sample(1:nrow(datos), 0.8 * nrow(datos))
train_data <- datos[indices, ]
test_data  <- datos[-indices, ]

# 2. Entrenamiento del modelo
modelo_sup <- glm(censrec ~ age + size + grade + nodes + hormon, 
                  data = train_data, family = "binomial")

# 3. Ver resultados de significancia (Los asteriscos)
summary(modelo_sup)

# 4. Predicción y Evaluación con el grupo de Prueba
pred_prob <- predict(modelo_sup, newdata = test_data, type = "response")
pred_clase <- ifelse(pred_prob > 0.5, 1, 0)

# 5. Matriz de Confusión y Precisión
matriz <- table(Real = test_data$censrec, Predicho = pred_clase)
print("Matriz de Confusión:")
print(matriz)

accuracy <- sum(diag(matriz)) / sum(matriz)
cat("\nPrecisión del modelo supervisado:", round(accuracy * 100, 2), "%\n")