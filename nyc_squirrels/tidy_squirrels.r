library(readr)
library(tidyverse)
libarry(emo)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Filter squirrels by behaviour 
scared <- nyc_squirrels %>% filter(runs_from == TRUE) %>% select (-c(approaches,indifferent)) %>% rename(behave=runs_from) 
scared$behave <- "Scared"

approach <- nyc_squirrels %>% filter(approaches == TRUE)  %>% select (-c(runs_from,indifferent)) %>% rename(behave=approaches) 
approach$behave <- "Friendly"

indiffer <- nyc_squirrels %>% filter(indifferent == TRUE) %>% select (-c(runs_from,approaches)) %>% rename(behave=indifferent) 
indiffer$behave <- "Indifferent"

# Concatenate behaviours
squirrels <- rbind(scared,approach)
squirrels <- rbind(squirrels, indiffer)

# Grouping by age and behaviour and compute percentages
squirrels <- squirrels %>% subset(age != "?") %>% group_by(age,behave) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

# Plotting
ggplot(squirrels,aes(x = factor(age), y = perc*100, fill = factor(behave))) +
  geom_bar(stat="identity",position="dodge", width = 0.7) +
labs(title="Are young squirrels friendlier ?",x = "Age", y = "Percent", fill = "Behaviour", caption="Source : Squirrel Census 2019") +
    theme_minimal(base_size = 14)+
    theme(plot.title = element_text(hjust = 0.5,family="Helvetica"),
          axis.text = element_text(family="Helvetica"))+ coord_flip()