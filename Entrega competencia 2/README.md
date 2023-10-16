# Entrega segunda competencia Kaggle DMEYF 2023

El directorio principal de esta carpeta contiene los dos códigos utilizados para generar la submission definitiva en Kaggle. La carpeta "construccion de hiperparámetros" contiene el código utilizado para optimizar los hiperparámetros del modelo, así como su documentación.

Para optimizar los hiperparámetros utilicé el archivo jsg_BO.R, el cual se corrió sobre un dataset sin ningún tipo de transformación.

Posteriormente, utilicé esos hiperparámetros para correr el archivo jsg_lightgbm_final, el cual genera columnas lag_1 y lag_2 de todas las variables, obteniendo así el lag del mes anterior del cliente, así como el anterior a ese. Por último agregé el rank de todas las variables, indicando el orden de cada cliente en un mes particular para cada variable.

Como comentario adicional, se intentó realizar una optimización contra el score público de Kaggle, combinando el código de la optimización de LightGBM y el de la optimización contra el público de Rpart. No se obtuvo resultados satisfactorios en el puntaje público.