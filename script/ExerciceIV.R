# Q1. Generer un vecteur de taille 100 suivant la loi expo et representation graphique.
## Fixer la graine.
set.seed(10121994)
## 1.1 generer un vecteur expo de taille 100.

### Fonction de structure theorique
t <- seq(0,5,5/(100-1))
Surv_theor <- exp(-1.1*t)

### Fonction de structure empirique
n <- 100
Surv_Emp <- rexp(n,rate = 1.1)

graph_theo_emp <- ggplot() + 
  geom_line(mapping = aes(x = t , y = Surv_theor, color = "black"),
            size = 1.5) + 
  labs(y = "S(t)",
       title = "Fonction de survie de X~exp(1.1)") + 
  
  geom_line(mapping = aes(y = 1:n/n , x = sort(Surv_Emp,decreasing = T),color = "blue"),
            size = 1.5) + 
  scale_color_identity(name = "Fonction de survie",
                       breaks = c("black","blue"),
                       labels = c("Fonction de survie théorique",
                                  "Fonction de survie empirique"),
                       guide = "legend")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  theme_ggplot()
graph_theo_emp

# Q2. Estimation avec un echantillon complet de taille n

## fixer la graine
set.seed(10121994)
## 2.1 Creation des vecteur Surv_Emp1,data_T,dlta,
Surv_Emp <- rexp(n,rate = 1.1)# Rate = 1.1
Surv_Emp1 <- rexp(n,rate = 1) # Rate = 1

delta <- (Surv_Emp < Surv_Emp1) # dela est une indicatrice 1.{Surv_Emp <= Surv_Emp1}
delta <- ifelse(delta=="TRUE",1,0) # convertir les T et F en 0 et 1



Data_T <- data.frame(
  delta = delta,
  Surv_Emp = Surv_Emp,
  Surv_Emp1 = Surv_Emp1
) %>% 
  mutate(min_Surv = ifelse(Surv_Emp >= Surv_Emp1,Surv_Emp1,Surv_Emp)) %>% 
  dplyr::select(delta,min_Surv)

## 2.2 Estimation de Kaplan-Mieir

### Modele kaplan-meier
kmfit_Emp1_Emp <- survfit(Surv(min_Surv,delta)~1, 
                       data = Data_T, 
                       conf.type="plain",
                       type="kaplan-meier")

kmfit_Emp1_Emp %>% 
  print()

kmfit_Emp1_Emp %>% 
  summary()

### Graphe Survie
plot(kmfit_Emp1_Emp)
plot(kmfit_Emp1_Emp,
     mark.time = T,
     xscale = 1,
     xlab = "time(in years)",
     ylab = "Survival S(t)",
     main = "Kaplan-Mieir function estimation ")
legend("topright",
       c("Kaplan-Mieir function", "95% pointwise CI"), 
       lty = 1:2)

## 2.3 Valeur de l'estimateur de theta.
V2 <- Data_T$min_Surv %>% 
  sum()
delta2 <- Data_T$delta %>% 
  sum()
theta_chapeau <- delta2/V2

## Representation graphique.
t1 <- seq(0,3,3/(100-1))
Surv_theor1 <- exp(-1.1*t)

Surv_VS <- exp(-theta_chapeau*t)

kmfit_Emp1_Emp <- kmfit_Emp1_Emp

toto <- ggplot() + 
  
  geom_line(mapping = aes(x = t , y = Surv_theor1,colour = "black"),size = 1.5) + 
  
  geom_line(mapping = aes(x = t , y = Surv_VS, colour = "blue"),size = 1.5) + 
  
  geom_line(mapping = aes(x = kmfit_Emp1_Emp$time , y = kmfit_Emp1_Emp$surv, colour = "red"),size = 1.5) + 
  
  labs(y = "S(t)",x = "Time (in years)", title = "Fonction de survie de X") + 
  
  scale_color_identity(name = "Fonction de survie",
                       breaks = c("black","blue","red"),
                       labels = c("Fonction de survie théorique",
                                  "Estimation par maximum de vraisemblance",
                                  "Estimation de Kaplan-Mieir"),
                       guide = "legend")+
  
  theme(legend.position="bottom", legend.box = "horizontal")+
  
  theme_ggplot()

toto

# Q3. Donnees non censurees

# 3.1 Estimation de Kaplan-Mieir
data_non_censuree <- Data_T %>% 
  dplyr::filter(delta == 1)

kmfit_Emp1_Emp2 <- survfit(Surv(min_Surv,delta)~1, 
                          data = data_non_censuree, 
                          conf.type="plain",
                          type="kaplan-meier") 
vv <- ggplot() + 
  geom_line(mapping = aes(x = kmfit_Emp1_Emp2$time, y =kmfit_Emp1_Emp2$surv, color = "black"),
            size = 1.5) + 
  geom_line(mapping = aes(x = t1 , y = Surv_theor1 , color = "blue"),
            size = 1.5) + 
  labs(y = "S(t)",x = "Time (in days)", title = "Kaplan-Mieir function estimation") + 
  
  scale_color_identity(name = "Fonction de survie:",
                       breaks = c("black","blue"),
                       labels = c("Estimation de Kaplan-Mieir",
                                  "Fonction de survie théorique"
                                  ),
                       guide = "legend")+
  
  theme(legend.position="bottom", legend.box = "horizontal")+
  
  theme_ggplot()
vv

# 3.2 Vraisemblance sans donnes censurees.

V3 <- data_non_censuree$min_Surv %>% 
  sum()

theta_chapeau2 <- 43/V3

# 3.3 Representation graphique

t2 <- seq(0,4,4/(100-1))

Surv_theor2 <- exp(-1.1*t2)

S_MV_sans_censor <- exp(-theta_chapeau2*t)


oo <- ggplot() + 
  geom_line(mapping = aes(x = t2 , y = Surv_theor2, color = "black"),
            size = 1.5) + 
  geom_line(mapping = aes(x = t2 , y = S_MV_sans_censor , color = "blue"),
            size = 1.5) + 
  labs(y = "S(t)",x = "Time (in days)", title = "Estimation par MV") + 
  
  scale_color_identity(name = "Fonction de survie:",
                       breaks = c("black","blue"),
                       labels = c( "Fonction de survie théorique",
                                  "Estimation de MV"
                       ),
                       guide = "legend")+
  
  theme(legend.position="bottom", legend.box = "horizontal")+
  
  theme_ggplot()
oo




Surv_VS_avec_censor <- exp(-theta_chapeau*t)

kmfit_Emp1_Scensure <- survfit(Surv(min_Surv,delta)~1, 
                          data = data_non_censuree, 
                          conf.type="plain",
                          type="kaplan-meier")

kmfit_Emp1_Emp <- kmfit_Emp1_Emp

vv2 <- ggplot() +
  
  geom_line(mapping = aes(x = kmfit_Emp1_Scensure$time, y =kmfit_Emp1_Scensure$surv, 
                          color = "black"),
            size = 1.5) + 
  geom_line(mapping = aes(x = t2 , y = Surv_theor2 ,
                          color = "blue"),
            size = 1.5) +
  
  geom_line(mapping = aes(x = t2 , y = S_MV_sans_censor ,
                          color = "#6f0022"),
            size = 1.5) + 
  
  geom_line(mapping = aes(x = t , y = Surv_VS, colour = "red"),
            size = 1.5) + 
  
  geom_line(mapping = aes(x = kmfit_Emp1_Emp$time , y = kmfit_Emp1_Emp$surv, colour = "#2e4d0b"),
            size = 1.5) + 
  
  labs(y = "S(t)",x = "Time (in years)", title = "Fonction de survie de X") + 
  
  scale_color_identity(name = "Fonction de survie:",
                       breaks = c("black","blue","#6f0022","red","#2e4d0b"),
                       labels = c("Estimation de KM non censuree",
                                  "Fonction de survie théorique",
                                  "Estimation par MV non censuree",
                                  "Estimation par MV avec censure",
                                  "Estimation de KM avec censuree"
                                  ),
                       guide = "legend")+
  
  theme(legend.position="right", legend.box = "horizontal")+
  
  theme_ggplot()
vv2

# Q4. diffrentes valeurs de n,

## 4.1 Pour n = 2000
set.seed(10121994)

t4 <- seq(0,5,5/(100-1))

n3 = 800

Surv_theor5 <- exp(-1.1*t4)

Surv_Emp5 <- rexp(n3,rate = 1.1)# Rate = 1.1
Surv_Emp6 <- rexp(n3,rate = 1) # Rate = 1

delta3 <- (Surv_Emp5 <= Surv_Emp6) # dela est une indicatrice 1.{Surv_Emp <= Surv_Emp1}
delta3 <- ifelse(delta3=="TRUE",1,0) # convertir les T et F en 0 et 1


Data_T3 <- data.frame(
  delta3 = delta3,
  Surv_Emp5 = Surv_Emp5,
  Surv_Emp6 = Surv_Emp6
) %>% 
  mutate(min_Surv1 = ifelse(Surv_Emp5 >= Surv_Emp6,Surv_Emp6,Surv_Emp5)) %>% 
  dplyr::select(delta3,min_Surv1)

data_non_censuree2 <- Data_T3 %>% 
  dplyr::filter(delta3 == 1)


kmfit_Emp8 <- survfit(Surv(min_Surv1,delta3)~1, 
                      data = Data_T3, 
                      conf.type="plain",
                      type="kaplan-meier")

kmfit_Emp9 <- survfit(Surv(min_Surv1,delta3)~1, 
                      data = data_non_censuree2, 
                      conf.type="plain",
                      type="kaplan-meier")

V3 <- Data_T3$min_Surv1 %>% 
  sum()
delta4 <- Data_T3$delta3 %>% 
  sum()
theta_chapeau1 <- delta4/V3
VM_avec_censu <- exp(-theta_chapeau1*t4)


N <- length(data_non_censuree2$delta)

V4 <- data_non_censuree2$min_Surv %>% 
  sum()

theta_chapeau4 <- N/V4

VM_sans_censu <- exp(-theta_chapeau4*t4)

vv3 <- ggplot() +
  
  geom_line(mapping = aes(x = kmfit_Emp9$time, y =kmfit_Emp9$surv, 
                          color = "black"),
            size = 1.5) + 
  geom_line(mapping = aes(x = t4 , y = Surv_theor5 ,
                          color = "blue"),
            size = 1.5) +
  
  geom_line(mapping = aes(x = t4 , y = VM_sans_censu ,
                          color = "#6f0022"),
            size = 1.5) + 
  
  geom_line(mapping = aes(x = t4 , y =VM_avec_censu, colour = "red"),
            size = 1.5) + 
  
  geom_line(mapping = aes(x = kmfit_Emp8$time , y = kmfit_Emp8$surv, colour = "#2e4d0b"),
            size = 1.5) + 
  
  labs(y = "S(t)",x = "Time (in years)", title = "Fonction de survie de X") + 
  
  scale_color_identity(name = "Fonction de survie:",
                       breaks = c("black","blue","#6f0022","red","#2e4d0b"),
                       labels = c("Estimation de KM non censuree",
                                  "Fonction de survie théorique",
                                  "Estimation par MV non censuree",
                                  "Estimation par MV avec censure",
                                  "Estimation de KM avec censuree"
                       ),
                       guide = "legend")+
  
  theme(legend.position="right", legend.box = "horizontal")+
  
  theme_ggplot()
vv3 


# Q5. diffrentes types d'e n'IC,
set.seed(10121994)

t4 <- seq(0,5,5/(100-1))

n3 = 200

Surv_theor5 <- exp(-1.1*t4)

Surv_Emp5 <- rexp(n3,rate = 1.1)# Rate = 1.1
Surv_Emp6 <- rexp(n3,rate = 1) # Rate = 1

delta3 <- (Surv_Emp5 <= Surv_Emp6) # dela est une indicatrice 1.{Surv_Emp <= Surv_Emp1}
delta3 <- ifelse(delta3=="TRUE",1,0) # convertir les T et F en 0 et 1


Data_T3 <- data.frame(
  delta3 = delta3,
  Surv_Emp5 = Surv_Emp5,
  Surv_Emp6 = Surv_Emp6
) %>% 
  mutate(min_Surv1 = ifelse(Surv_Emp5 >= Surv_Emp6,Surv_Emp6,Surv_Emp5)) %>% 
  dplyr::select(delta3,min_Surv1)

kmfit_Emp_plain <- survfit(Surv(min_Surv1,delta3)~1, 
                      data = Data_T3, 
                      conf.type="plain",
                      type="kaplan-meier")
graph_plain <- ggsurvplot(
  kmfit_Emp_plain, #Model survi
  data = Data_T3,
  palette = c("#001d64","#2E9FDF"), #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  legend.title = "kaplan-meier",
  xlab="time (in years)",
  ylab="S(t)",
  ggtheme = theme_ggplot()
)
graph_plain

kmfit_Emp_2log <- survfit(Surv(min_Surv1,delta3)~1, 
                      data = Data_T3, 
                      conf.type="log-log",
                      type="kaplan-meier")
graph_2log <- ggsurvplot(
  kmfit_Emp_2log, #Model survi
  data = Data_T3,
  palette = c("#001d64","#2E9FDF"), #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  legend.title = "kaplan-meier",
  xlab="time (in years)",
  ylab="S(t)",
  ggtheme = theme_ggplot()
)
graph_2log

kmfit_Emp_log <- survfit(Surv(min_Surv1,delta3)~1, 
                          data = Data_T3, 
                          conf.type="log",
                          type="kaplan-meier")
graph_log <- ggsurvplot(
  kmfit_Emp_log, #Model survi
  data = Data_T3,
  palette = c("#001d64","#2E9FDF"), #Choix du couleur
  linetype = "solid", # Type de ligne (pointiee, continue...)
  surv.scale = "percent", # pour ne pas afficher les pourcetage "surv.scale = default"
  conf.int = T, #Pour dessiner l'intervalle de confiance.
  conf.int.fill = "#0587ff", # couleur de la bonde de confience
  censor = T, # dessiner les censurers
  legend = "top",
  legend.title = "kaplan-meier",
  xlab="time (in years)",
  ylab="S(t)",
  ggtheme = theme_ggplot()
)
graph_log

################# FIN EXERCICE IV ####################################