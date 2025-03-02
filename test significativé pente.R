annees <- 1993:2020 # Années de 1993 à 2021
temperatures <- c(12.57686339, 13.19546719,
                  13.59053638,
                  12.39851984,
                  13.17039536,
                  12.98299982,
                  13.43709125,
                  12.91626393,
                  13.16383395,
                  13.33075899,
                  13.25756055,
                  13.03650437,
                  13.26846344,
                  13.18359455,
                  13.54774015,
                  12.99425599,
                  13.06617251,
                  12.33531343,
                  13.21729363,
                  12.98454053,
                  12.38802992,
                  14.06245351,
                  13.3525738,
                  13.51380051,
                  13.72153454,
                  13.56049436,
                  13.61355904,
                  13.69854387) # Exemple de données

# Étape 2 : Régression linéaire
modele <- lm(temperatures ~ annees) # Ajustement d'une régression linéaire
summary(modele) # Résumé du modèle

# Étape 3 : Tester la significativité de la pente
p_value <- summary(modele)$coefficients[2, 4] # Extraction de la p-value de la pente
pente <- summary(modele)$coefficients[2, 1]  # Extraction de la pente
stderr <- summary(modele)$coefficients[2, 2] # Erreur standard de la pente

# Afficher les résultats
cat("Pente estimée :", pente, "\n")
cat("Erreur standard :", stderr, "\n")
cat("P-value :", p_value, "\n")

# Interprétation des résultats
if (p_value < 0.05) {
  cat("La tendance est significative (p < 0.05). Il y a une augmentation significative des températures.\n")
} else {
  cat("La tendance n'est pas significative (p >= 0.05).\n")
}