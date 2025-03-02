#### code R pour voir s'il y a un lien entre l'annee, le type d'étude et le satde de dvpm étudié 


# Installer et charger les packages nécessaires
install.packages("FactoMineR")   # Pour l'ACM
install.packages("factoextra")   # Pour la visualisation
install.packages("ggplot2")      # Pour améliorer les graphiques

library(FactoMineR)
library(factoextra)
library(ggplot2)

# 🔹 Importer le fichier CSV
data <- read.csv("C:/Users/cleme/Documents/CESURE/CNRS/BIBLIO/annee_type_dev.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)

# 🔹 Vérifier l'importation des données
str(data)  # Affiche la structure du tableau pour s'assurer que tout est bien lu
head(data) # Affiche les premières lignes du tableau

# 🔹 Convertir les variables catégorielles en facteur si nécessaire
data$année <- as.factor(data$année)
data$type.étude <- as.factor(data$type.étude)

rownames(data) <- NULL
data$type.étude <- as.character(data$type.étude)  # Convertir en texte
data$type.étude[is.na(data$type.étude)] <- "Non_precise"  # Remplacer NA
data$type.étude <- as.factor(data$type.étude)  # Reconvertir en facteur

data$larves <- as.factor(data$larves)
data$juvéniles <- as.factor(data$juvéniles)
data$adultes <- as.factor(data$adultes)

# 🔹 Réalisation de l'ACM (seulement sur les variables catégorielles)
acm <- MCA(data[, c("année", "type.étude", "larves", "juvéniles", "adultes")], graph = FALSE)

# 🔹 Afficher les pourcentages d'inertie (importance des axes)
print(acm$eig)

# 🔹 Visualisation des résultats
fviz_mca_biplot(acm, 
                repel = TRUE,   # Évite le chevauchement des labels
                ggtheme = theme_minimal()) + 
  ggtitle("Analyse des Correspondances Multiples (ACM)")

# 🔹 Visualiser les contributions des variables aux dimensions principales
fviz_mca_var(acm, choice = "mca.cor", repel = TRUE) + 
  ggtitle("Contribution des Variables aux Axes de l'ACM")

# 🔹 Visualiser la projection des individus (si pertinent)
fviz_mca_ind(acm, repel = TRUE) + 
  ggtitle("Projection des Individus dans l'ACM")

#anova a 2 facteurs 

dataa <- read.csv("C:/Users/cleme/Documents/CESURE/CNRS/BIBLIO/annee_type_dev_anova.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)

#dataa$année <- as.factor(dataa$année)
library(tidyverse)
library(ggpubr)
library(rstatix)

dataa %>% sample_n_by(type.étude, stade_dev, size = 1)

dataa %>%
  group_by(type.étude, stade_dev) %>%
  get_summary_stats(année, type = "mean_sd")

#visualiser le boxplot
bxp <- ggboxplot(
  dataa, x = "stade_dev", y = "année",
  color = "type.étude", palette = "jco"
)
bxp

#verifier les valeurs abérrantes
dataa %>%
  group_by(type.étude, stade_dev) %>%
  identify_outliers(année)

#construire le modele
# Construire le modèle linéaire
model  <- lm(année ~ type.étude*stade_dev,
             data = dataa)
# Créer un QQ plot des résidus
ggqqplot(residuals(model))

#tester la normalité (si test significatif, alors pas normal) (ici pas normal mais a cause des pts de 1958)
shapiro_test(residuals(model))

#test homogeneité des variances (ici ok)
dataa %>% levene_test(année ~ type.étude*stade_dev)

#test anova
res.aov <- dataa %>% anova_test(année ~ type.étude*stade_dev)
res.aov
# --> les résultats ne sont pas significatifs, l'année d'étude n'a pas d'impact sur le type d'étude et le stade de dvpm étudié 

#autre technique d'anova
res.lm=lm(formula = année ~ type.étude*stade_dev, data = dataa)
par(mfrow=c(2,2))
plot(res.lm)
summary(res.lm)

