library(readxl)

FIRST_SKY_DATA_GROUP <- read_excel("FIRST SKY DATA  FOR NANA.xlsx", 
                                   sheet = "GROUP", skip = 1)


FIRST_SKY_DATA_INSURANCE <- read_excel("FIRST SKY DATA  FOR NANA.xlsx", 
                                       sheet = "INSURANCE", skip = 1)


FIRST_SKY_DATA_HOTEL <- read_excel("FIRST SKY DATA  FOR NANA.xlsx", 
                                   sheet = "HOTEL", col_types = c("numeric", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric"), skip = 1)




FIRST_SKY_DATA_COMODITIES <- read_excel("FIRST SKY DATA  FOR NANA.xlsx", 
                                        sheet = "COMODITIES", skip = 1)


FIRST_SKY_DATA_CONSTRUCTION <- read_excel("FIRST SKY DATA  FOR NANA.xlsx", 
                                          sheet = "CONSTRUCTION")

FIRST_SKY_DATA_CONSTRUCTION$`# of Child..` <- as.character(FIRST_SKY_DATA_CONSTRUCTION$`# of Child..`)

first_sky_combine <- bind_rows(FIRST_SKY_DATA_COMODITIES, FIRST_SKY_DATA_CONSTRUCTION,
                               FIRST_SKY_DATA_GROUP, FIRST_SKY_DATA_HOTEL, FIRST_SKY_DATA_INSURANCE)



first_sky_combine$chairman_rating <- (rowMeans(subset(first_sky_combine, select = T1:T28), na.rm = TRUE))

first_sky_combine$supervisor_rating <- (rowMeans(subset(first_sky_combine, select = S1:S28), na.rm = TRUE))

first_sky_combine$engagement_rating <- (rowMeans(subset(first_sky_combine, select = E1:E10), na.rm = TRUE))

first_sky_combine$performance_rating <- (rowMeans(subset(first_sky_combine, select = P1:P5), na.rm = TRUE))


first_sky2 <- select(first_sky_combine, Number, "Current Status", "Marital status",
                     "Educat. Level", "Year at FSG", "Hours per week", 
                     Gender, Age, chairman_rating, supervisor_rating,
                     engagement_rating, performance_rating)
View(first_sky2)
names(first_sky2)
first_sky2$Number <- NULL
unique(first_sky2$`Current Status`)
unique(first_sky2$`Marital status`)
unique(first_sky2$`Educat. Level`)
unique(first_sky2$`Year at FSG`)
unique(first_sky2$`Hours per week`)
unique(first_sky2$Gender)
unique(first_sky2$Age)





ggplot(first_sky2, aes(x=reorder(`Current Status`, performance_rating), y=performance_rating, fill=`Marital status`)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  xlab("State")


ggplot(first_sky2, aes(x=Gender, y=performance_rating, fill=`Marital status`)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

ggplot(first_sky2, aes(x=supervisor_rating, y=engagement_rating, colour=Gender)) + geom_point()


ggplot(first_sky2, aes(x=supervisor_rating, y=engagement_rating, shape=Gender, colour=Gender)) +
  geom_point() +
  scale_shape_manual(values=c(1,2)) +
  scale_colour_brewer(palette="Set1") +
  ggtitle("Supervisor Rating Against Engagement") +
  xlab("Supervisor Rating") + ylab("Engagement")


ggplot(first_sky2, aes(x=chairman_rating, y=engagement_rating, shape=Gender, colour=Gender)) +
  geom_point() +
  stat_smooth(method=lm, se=FALSE) +
  scale_shape_manual(values=c(1,2)) +
  scale_colour_brewer(palette="Set1") +
  ggtitle("Supervisor Rating Against Engagement") +
  xlab("Chairman Rating") + ylab("Engagement") +
  theme_bw()


ggplot(first_sky2, aes(x=performance_rating, y=engagement_rating, shape=Gender, colour=Gender)) +
  geom_point() +
  scale_shape_manual(values=c(1,2)) +
  scale_colour_brewer(palette="Set1") +
  ggtitle("Supervisor Rating Against Engagement") +
  xlab("Supervisor Rating") + ylab("Engagement")







first_sky2 <- filter(first_sky2, engagement_rating <= 5)

model <- lm(performance_rating ~ supervisor_rating + chairman_rating, data = first_sky2)
summary(model)

model2 <- lm(performance_rating ~ `Educat. Level` + engagement_rating, data = first_sky2)
summary(model2)


engageModel <- lm(engagement_rating ~ supervisor_rating + chairman_rating, data = first_sky2)
summary(engageModel)

perfEngage <- lm(performance_rating ~ engagement_rating, data = first_sky2)
summary(perfEngage)

allModel <- lm(performance_rating ~ `Current Status` + `Marital status` + 
                 `Educat. Level` + `Year at FSG` +
                 `Hours per week` + Gender + `Age` + `engagement_rating`,  data = first_sky2)


allModel2 <- lm(performance_rating ~ `Educat. Level` + Age + engagement_rating, data = first_sky2)

summary(allModel)
summary(engageModel)
summary(perfEngage)