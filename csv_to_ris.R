### script pour convertir un csv en .ris pour etre lu dans vosviewer 

# Charger le package nécessaire
library(dplyr)

# Lire le fichier CSV
df <- read.csv2("C:/Users/cleme/Documents/CESURE/CNRS/BIBLIO/articles_tot.csv",header = TRUE, sep = ";", stringsAsFactors = FALSE)


# Fonction pour générer une entrée RIS
#fonctionne et est lu par voqviewer !!!!
generate_ris <- function(row) {
  ris_entry <- sprintf(
    "TY  - JOUR
TI  - %s
AU  - %s
AB  - %s
PY  - %s
ER  - ",
    row[['title']], row[['authors']], row[['abstract']], row[['year']] 
  )
  return(ris_entry)
}

# Appliquer la fonction à toutes les lignes du dataframe
ris_entries <- apply(df, 1, generate_ris)

# Écrire le fichier RIS
writeLines(ris_entries, "articles.ris")

cat("Conversion terminée! Utilise 'articles.ris' pour VOSviewer.")
