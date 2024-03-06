# Homework_3
##Exercice 1
# Créer un data frame avec les variables existantes
base_donnees1 <- data.frame(
  Identifiant = c(1:16),
  noms = c("MBB", "KC", "KD", "Diawa", "Bathie", "Hilda", "Ameth", "Sarah", "Célina", "Tam", "Salam", "NIASS", "Ange", "Jeanne", "Malick", "PAN"),
  Age = c(24,18,19,20,18,18,22,20,21,20,19,21,17,23,18,19),
  Sexe = factor(c("Homme", "Femme", "Femme", "Femme", "Homme", "Femme", "Homme", "Femme", "Femme", "Homme", "Homme", "Homme", "Homme", "Femme", "Homme", "Homme")),
  Nationalite = c("Sénégalaise", "Sénégalaise", "Sénégalaise", "Sénégalaise", "Sénégalaise", "Camerounaise", "Sénégalaise", "Camerounaise", "Camerounaise", "Sénégalaise", "Sénégalaise", "Sénégalaise", "Malgache", "Camerounaise", "Sénégalaise", "Sénégalaise"),
  Poids = c(69, 60, 62, 65, 67, 68, 72, 65, 65, 75, 70, 68, 67, 70, 70, 72)
)

# Générer des observations aléatoires pour les variables Note1 à Note9
set.seed(123)  # Pour la reproductibilité des résultats
for (i in 1:12) {
  base_donnees1[paste0("Note", i)] <- runif(16, min = 0, max = 20)
}

# Créer un vecteur de couleurs préférées et de nombres préférés
favorite_colors <- c("Bleu", "Rouge", "Vert", "Jaune", "Violet", "Orange", "Rose", "Bleu", "Vert", "Rouge", "Violet", "Jaune", "Bleu", "Vert", "Rose", "Orange")
favorite_number <- c(2,6,7,2,9,5,9,0,2,8,6,2,6,5,8,3)

# Ajouter les  variables "Favorite_Color" et "Favorite_Number" à la base de données existante
base_donnees1$Favorite_Color <- favorite_colors
base_donnees1$Favorite_Number <- favorite_number

# Générer de nouvelles observations aléatoires pour 284 élèves supplémentaires
nouvelles_lignes <- 284
base_donnees_complete <- base_donnees1

set.seed(123)  # Pour la reproductibilité des résultats
for (i in 1:nouvelles_lignes) {
  nouvelle_observation <- data.frame(
    Identifiant = max(base_donnees_complete$Identifiant) + i,
    noms = sample(c("Mariane", "Sophie", "Ousmane", "Fatou", "Abdou", "Aminata", "Ibrahim", "Aïssatou", "Moussa", "Mariam", "Cheikh", "Awa", "Mamadou", "Ndeye", "Alioune", "Rama"), 1),
    Age = sample(18:25, 1),
    Sexe = sample(c("Homme", "Femme"), 1),
    Nationalite = sample(c("Sénégalaise", "Camerounaise", "Malgache"), 1),
    Poids = sample(60:75, 1)
  )
  for (j in 1:12) {
    nouvelle_observation[paste0("Note", j)] <- runif(1, min = 0, max = 20)
  }
  nouvelle_observation$Favorite_Color <- sample(favorite_colors, 1)
  nouvelle_observation$Favorite_Number <- sample(favorite_number, 1)
  base_donnees_complete <- rbind(base_donnees_complete, nouvelle_observation)
}

# Renommer les colonnes spécifiques (de la 7e à la 11e)
colnames(base_donnees_complete)[7:10] <- c("Notes Français", "Notes Anglais", "Notes Micro 1", "Notes Algèbre")

# Vérifier les nouveaux noms de colonnes
colnames(base_donnees_complete)

# Sélection des colonnes contenant les notes de Français, d'Angalis, de Micro 1 et d'Algèbre
colonnes_selectionnees <- base_donnees_complete[, 7:10]

# Calculer la moyenne des colonnes sélectionnées
moyennes_colonnes <- colMeans(colonnes_selectionnees)

# Afficher les moyennes
moyennes_colonnes


# Afficher le data frame mis à jour
print(base_donnees_complete)

#Exporter ma base sur Excel
write.csv(base_donnees_complete,"Monfichier_try_R.csv")

# Importer mon fichier csv
monDataframe <- read.csv("C:/Users/HP/Desktop/ISEP2CoursR2024/Monfichier_try_R.csv")
print(monDataframe)


##Exercice 2
# Créer un tableau de contingence entre les variables Poids et Age
tableau <- table(base_donnees_complete$Poids, base_donnees_complete$Age)

# Calcul des fréquences attendues
frequences_attendues <- matrix(NA, nrow = nrow(tableau), ncol = ncol(tableau))
for (i in 1:nrow(tableau)) {
  for (j in 1:ncol(tableau)) {
    frequences_attendues[i, j] <- sum(tableau[i, ]) * sum(tableau[, j]) / sum(tableau)
  }
}

# Calcul de la statistique du Chi-deux
statistique_chi_deux <- sum((tableau - frequences_attendues)^2 / frequences_attendues)

# Calcul des degrés de liberté
degres_de_liberte <- (nrow(tableau) - 1) * (ncol(tableau) - 1)

# Calcul de la p-valeur
p_valeur <- pchisq(statistique_chi_deux, df = degres_de_liberte, lower.tail = FALSE)

# Afficher les résultats
cat("Statistique du Chi-deux :", statistique_chi_deux, "\n")
cat("Degrés de liberté :", degres_de_liberte, "\n")
cat("P-valeur :", p_valeur, "\n")

# Interprétation des résultats
niveau_signification <- 0.05
if (p_valeur < niveau_signification) {
  cat("Conclusion : Les variables Poids et Age sont significativement liées (rejet de l'hypothèse nulle d'indépendance).", "\n")
} else {
  cat("Conclusion : Il n'y a pas suffisamment de preuves pour rejeter l'hypothèse nulle selon laquelle les variables Poids et Age sont indépendantes.", "\n")
}

