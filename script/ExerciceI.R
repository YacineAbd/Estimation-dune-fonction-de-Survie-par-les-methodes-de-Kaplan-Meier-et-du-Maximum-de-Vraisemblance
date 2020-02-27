###_________________ Importation des données  _________________###

#Exporter la table "lung"
write.table(lung,"data/lung.txt")

# lire la table "lung"
lung <- read.table("data/lung.txt")

# 1. création d'une nouvelle base avec temps en annee et en mois.
lung <- lung %>% 
  mutate(time_Annee = (lung$time/365.25),
         time_month = (lung$time/365.25)*12)


###_________________ Estimateur de kaplan-meier _________________###

# 1. Model avec temps (en jours et en annees)
## 1.1 En Jours
kmfit_jours <- survfit(Surv(time,status)~1, 
                       data = lung, 
                       conf.type="plain",
                       type="kaplan-meier")
## 1.2 En Annees:
kmfit_Annee <- survfit(Surv(time_Annee,status)~1, 
                       data = lung, 
                       conf.type="plain",
                       type="kaplan-meier")
print(kmfit_Annee)
# 2. Probabilite de survie en fonction du temps.

## 2.1 Graphe avec temps(jours)
km_jours <- ggsurvplot(
  kmfit_jours, #Model survi
  palette = "#001d64", #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  risk.table = T,
  risk.table.height = 0.25,
  ggtheme = theme_ggplot()
  )
km_jours

## 2.1 Graphe avec temps(Annee)
km_Annee <- ggsurvplot(
  kmfit_Annee, #Model survi
  palette = "#001d64", #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  risk.table = T,
  risk.table.height = 0.25,
  legend.title = "kaplan_meier",
  ggtheme = theme_ggplot()
)
km_Annee

###_________________ Estimateur de Fleming-Harrington _________________###

# 1. Model avec temps (en jours et en annees)

## 1.1 En Jours
fhfit_Jours <- survfit(Surv(time,status)~1,
                       data = lung,
                       conf.type="plain",
                       type="fh")

## 1.2 En Anneee
fhfit_Annee <- survfit(Surv(time_Annee,status)~1,
                       data = lung,
                       conf.type="plain",
                       type="fh")

## 1.3 En mois : 
fhfit_mois <- survfit(Surv(time_month,status)~1, 
                       data = lung, 
                       conf.type="plain",
                       type="fh")

# 2. Superposer les courbe obteues avec les deux methodes en fonction des annees

## 2.1 Mettre les deux modele dans une meme liste.
fit_2_methode <- list(kaplan_meier = kmfit_Annee,Fleming_Harrington = fhfit_Annee)

## 2.2 Courbes superposees
courbe_superposee <- ggsurvplot_combine(
  fit_2_methode, #Model survi
  xlim = c(0,3),
  xlab = "Time (in Years)",
  ggtheme = theme_ggplot()
)
courbe_superposee

###_________________ Différence entre ces deux estimateurs _________________###
# difference km_fh
tt <- ggplot() + 
  geom_line(mapping = aes(x = kmfit_jours$time , 
                          y = kmfit_jours$surv-fhfit_Annee$surv),
            color = "#000000", size = 1.5) + 
  labs(y = "difference des estimateurs de survie",
       x = "Time (en Jours)") + 
  theme_ggplot()
tt

#Risque 
naH <- -(fhfit_mois$surv %>% 
              log())

time <- fhfit_mois$time

cc <- ggplot() + 
  geom_line(mapping = aes(x = time , 
                          y = naH,
                          colour = "Black"),
            size = 1.5) + 
  labs(x = "Time (in month)" , 
         y = "Cumulative risk H(t)") + 
  
  theme_ggplot()
cc

############## FIN EXERCICE 1  ####################



