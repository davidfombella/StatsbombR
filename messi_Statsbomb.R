# 1º Instalar devtools
# install.packages("devtools")
# 2º devtools::install_github("statsbomb/StatsBombR")

library(tidyr)
library(StatsBombR)

Comp <- FreeCompetitions() %>%
filter(competition_id==11 & season_name=="2005/2006")

Matches <- FreeMatches(Comp)

StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

StatsBombData <- allclean(StatsBombData)

############################################
# 1  Tiros y Goles por equipo
############################################
shots_goal_by_team<- StatsBombData %>%
             group_by(team.name) %>%
             summarise(
               shots = sum(type.name=="Shot", na.rm = TRUE),
               goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)
              )

# Tiros y Goles por  equipo y  partido
shots_goals_by_match <- StatsBombData %>%
                        group_by(team.name) %>%
                       summarise(
                         num_partidos = n_distinct(match_id),
					              shots = sum(type.name=="Shot", na.rm =TRUE)/n_distinct(match_id),
                       goals = sum(shot.outcome.name=="Goal", na.rm =TRUE)/n_distinct(match_id)
					             )

library(ggplot2)

############################################
# 2 Mostrar un grafico con los datos
############################################

ggplot(data = shots_goal_by_team, 
       aes(x = reorder(team.name, shots), y = shots)) +
  geom_bar(stat = "identity", width = 0.5)+
  labs(y="Shots") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous( expand = c(0,0)) +
  coord_flip() +
  theme_SB()









player_shots <- StatsBombData %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE))


player_minutes <- get.minutesplayed(StatsBombData)

player_minutes <- player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

player_shots <- left_join(player_shots, player_minutes)

player_shots <- player_shots %>% mutate(nineties = minutes/90)

player_shots <- player_shots %>% mutate(shots_per90 = shots/nineties)


# 4 Representar pases graficamente
# Necesario instalar la libreria SBpitch
# devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)




messidata <- StatsBombData %>%
              filter(player.name=='Lionel Andrés Messi Cuccittini')


# con esto te quitas corners saques de medio campo  saques de falta y saques de banda con
# el siguiente filtro is.na(pass.type.name) que esto sea vacio

passes <- messidata %>%
  filter(type.name=="Pass" & is.na(pass.type.name) & player.id==5503)%>%
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18)

#############################################################
# crear la grafica de pases al area
#############################################################
create_Pitch() +
  geom_segment(data = passes, 
               aes(x = location.x, y = location.y,
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +
  labs(title = "Lionel Messi, Completed Box Passes", subtitle = "La Liga, 2005/2006") +
  coord_fixed(ratio = 105/100)





#############################################################
# crear la grafica anterior pero coloreando los pases en funcion del patron de juego
#############################################################
create_Pitch() +
  geom_segment(data = passes, 
               aes(x = location.x, y = location.y,
                   xend = pass.end_location.x, yend = pass.end_location.y,
                   colour=play_pattern.name),
               lineend = "round", size = 0.8, arrow = arrow(length = unit(0.08, "inches"))) +
  labs(title = "Lionel Messi, Completed Box Passes", subtitle = "La Liga, 2005/2006") +
  coord_fixed(ratio = 105/100)




