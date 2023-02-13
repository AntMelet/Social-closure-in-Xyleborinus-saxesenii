library (ggplot2)
library (readxl)

behavstackedbars <- read_excel("//NAS/home/Antoine/Projects/Social closure/Scan observations/Behavior_stackedbarplot.xlsx")
behavstackedbars$treatment <- factor (behavstackedbars$treatment, levels = c("Control", "Intrapopulation", "Interpopulation", "Interspecies"))
behavstackedbars$behaviour <- factor (behavstackedbars$behaviour, levels = c("rest", "walk", "dig", "feed", "clean","shuffle", "groom", "copulate", "cannibalise"))
behavstackedbars$stage <- factor (behavstackedbars$stage, levels = c("females", "larvae"))
behavstackedbars$time <- factor (behavstackedbars$time, levels = c("after", "before"))

femalesao <- behavstackedbars [behavstackedbars$stage == "females" & behavstackedbars$time == "after", ]
ggplot (femalesao, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs (title = "Female behaviour after opening")

femalesao_2pop <- femalesao [femalesao$treatment %in% c ("Intrapopulation", "Interpopulation"), ]
ggplot (femalesao_2pop, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs (title = "Female behaviour after opening")

femalesbo <- behavstackedbars [behavstackedbars$stage == "females" & behavstackedbars$time == "before", ]
ggplot (femalesbo, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar (position = "fill", stat = "identity") +
  labs (title = "Female behaviour before opening")

larvaeao <- behavstackedbars [behavstackedbars$stage == "larvae" & behavstackedbars$time == "after", ]
ggplot (larvaeao, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar (position = "fill", stat = "identity") +
  labs (title = "Larval behaviour after opening")

larvaeao_2pop <- larvaeao [larvaeao$treatment %in% c ("Intrapopulation", "Interpopulation"), ]
ggplot (larvaeao_2pop, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs (title = "Larval behaviour after opening")

larvaebo <- behavstackedbars [behavstackedbars$stage == "larvae" & behavstackedbars$time == "before", ]
ggplot (larvaebo, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar (position = "fill", stat = "identity") +
  labs (title = "Larval behaviour before opening")

afteropening <- behavstackedbars [behavstackedbars$time == "after", ]
ggplot (afteropening, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar (position = "fill", stat = "identity") +
  labs (title = "Behaviours after opening")

afteropening_2pop <- afteropening [afteropening$treatment %in% c ("Intrapopulation", "Interpopulation"), ]
ggplot (afteropening_2pop, aes (fill = behaviour, y = value, x = treatment)) + 
  geom_bar (position = "fill", stat = "identity") +
  labs (title = "Behaviours after opening")
