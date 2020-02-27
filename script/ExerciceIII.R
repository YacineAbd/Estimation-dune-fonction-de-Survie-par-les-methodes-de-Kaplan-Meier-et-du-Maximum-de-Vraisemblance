
# Données -----

  time_1=c(1,1,1,1,4,5,7,8,10,10,12,16,16,16)
  
  status_1=c(1,1,1,0,0,1,1,1,1,0,0,0,0,0)


# Estimation de Kaplan-Meier de la fonction de survie -----
  
kmfit_ex03 <- survfit(Surv(time_1,status_1)~1,
                  conf.type="none",
                  type="kaplan-meier")
kmfit_ex03 %>% 
  summary()

# Estimation de Harrington-Fleming de la fonction de survie -----

fhfit_ex03 <- survfit(Surv(time_1,status_1)~1,
                 conf.type="none",
                 type="fh")
fhfit_ex03 %>% 
  summary()

# Représentation graphique

ben1 <- ggplot() +
  
  geom_line(mapping = aes(x = kmfit_ex03$time , y = kmfit_ex03$surv, colour = "black"),
            size = 1.5) + 

  geom_line(mapping = aes(x = fhfit_ex03$time , y = fhfit_ex03$surv ,colour = "blue"),
            size = 1.5)  
  
ben2 <- ben1 + 
  labs(y = "S(t)",
       x = "Time (in weeks)",
       title = "Estimation S(t) ")+
  scale_color_identity(name = "Fonction de survie",
                       breaks = c("black","blue"),
                       labels = c("Estimation de Kaplan-Meier",
                                  "Estimation de Harrington-Fleming"),
                       guide = "legend")+
  
  theme(legend.position="bottom", legend.box = "horizontal")+
  
  theme_ggplot()
ben2 

# Superposition des trois graphes

lambda_exo3 <- sum(status_1)/sum(time_1)

time_exo3 <- seq(0,15,0.01)

expsurv_exo3 <- exp(-lambda_exo3*time_exo3)

ben3 <- ben1 + 
  geom_line(mapping = aes(x = time_exo3 , y = expsurv_exo3 , colour  = "red"))

ben3 <- ben3 + 
  labs(y = "S(t)",
       x = "Time (in weeks)",
       title = "Estimation S(t) ")+
  scale_color_identity(name = "Estimatios de S(t):",
                                      breaks = c("black","blue","red"),
                                      labels = c("Estimation de Kaplan-Meier",
                                                 "Estimation de Harrington-Fleming",
                                                 "estimation par la loi exponentielle"),
                                      guide = "legend")+
  
  theme(legend.position="rigth", legend.box = "horizontal")+
  
  theme_ggplot()
ben3 
 ############################# FIN EXO 3 ################################################