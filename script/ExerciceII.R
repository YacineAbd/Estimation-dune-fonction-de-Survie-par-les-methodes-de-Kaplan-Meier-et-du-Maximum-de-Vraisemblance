
# Q2. Comparez l’estimateur de Kaplan Meier de la fonction de survie S. Estimez sa variance

## 1.1 Creation des vecteurs "time" et "status" pour 6mp et Placebo

## 6-MP data------
time.6mp <- c(6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35)
status.6mp  <- c(1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0)

MP <- data.frame(
  Time = time.6mp,
  Status = status.6mp
)
## Pacebo data------
time.placebo <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
status.placebo <- rep(1,21)

placebo <- data.frame(
  Time = time.placebo,
  Status = status.placebo
)

# Gathered data------
data_MP_placebo <- data.frame()
data_MP_placebo <- data_MP_placebo %>%
  bind_rows(placebo) %>%
  bind_rows(MP) %>% 
  mutate(grp = rep(0:1,21) %>% 
           sort())


# Transformation of the data in "censored" data------

## 1.3 Model de survi

survi_groupe <- survfit(Surv(time = Time, event = Status)~grp, 
        data = data_MP_placebo,
        conf.type="none")

survi_groupe %>% summary()

## 1.4 Graphe du modele
plot(survi_groupe,
     lty=1:2,xlab="Survival time in weeks",
     ylab="Survival rate")
legend("topright",c("Placebo","6-MP"),lty=1:2)

# Q3. Estimer les fonctions de survie de chaque groupe à l'aide de l'estimateur de Kaplan-Meier
## MP
km_MP <- survfit(Surv(time.6mp,status.6mp)~1, 
                       data = MP, 
                       conf.type="plain",
                       type="kaplan-meier")
km_MP %>% 
  summary()

graph_MP <- ggsurvplot(
  km_MP, #Model survi
  data = MP,
  palette = c("#001d64","#2E9FDF"), #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  risk.table = T,
  risk.table.height = 0.25,
  break.time.by = 5,
  legend.title = "kaplan-meier",
  xlab="Survival time in in weeks",
  ylab="Survival rate",
  ggtheme = theme_ggplot()
)
graph_MP


## Placebo
km_Placebo <- survfit(Surv(time.placebo,status.placebo)~1, 
                 data = placebo, 
                 conf.type="plain",
                 type="kaplan-meier")
km_Placebo %>% 
  summary()

graph_placebo <- ggsurvplot(
  km_Placebo, #Model survi
  data = placebo,
  palette = c("#001d64","#2E9FDF"), #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  risk.table = T,
  risk.table.height = 0.25,
  break.time.by = 5,
  legend.title = "kaplan-meier",
  xlab="Survival time in in weeks",
  ylab="Survival rate",
  ggtheme = theme_ggplot()
)


#################### FIN EXERCICE II #########################