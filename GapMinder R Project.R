install.packages("gapminder")
library(gapminder)
data(gapminder)
library(arules)
library(arulesViz)
library(dbplyr)
library(gghighlight)
library(ggplot2)
library(ggraph)
library(ggrepel)
library(ggthemes)
library(plotly)
library(shiny)
library(tibble)


#1A: Data Wrangling

gapminder_1950 <- gapminder[gapminder$year > 1950,]
head(gapminder_1950)

group_gapminder <- gapminder_1950 %>%
  group_by(continent, year) %>%
  summarise(
    avg_life_expectancy = mean(lifeExp, na.rm = TRUE),
    avg_gdp_per_capita = mean(gdpPercap, na.rm = TRUE),
    total_population = sum(pop, na.rm = TRUE)
  )
print(group_gapminder)

#2 Data Visualizations with ggplot2

ggplot(group_gapminder, aes(x = year, y = avg_life_expectancy, color = continent)) +
  geom_line(size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs( title = "Average Life Expectancy Over Time by Continent", x = "Year", y = "Average Life Expectancy" ) +
  theme_classic()

ggplot(group_gapminder, aes(y = avg_gdp_per_capita, x = avg_life_expectancy, color = year, size = total_population)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs( title = "Average GDP by Average Life Expectancy", y = "Average GDP", x = "Average Life Expectancy" ) +
  theme_classic()

ggplot(group_gapminder, aes(x = continent, y = total_population, fill = continent, year==2007)) +
  geom_bar(stat = "identity") +
  labs( title = "Total Population by Continent", x = "Continent", y = "Total Population" ) +
  theme_minimal()

ggplot(group_gapminder, aes(x = continent, y = avg_life_expectancy, fill = continent)) +
  geom_boxplot(size=0.5) +
  labs(title = "Life Expentancy by Continent",x = "Continent", y = "Life expentancy" ) +
  theme_minimal()
  
ggplot(group_gapminder, aes(x = avg_gdp_per_capita, y = avg_life_expectancy)) +
  geom_point(size = 1.5) + 
  facet_wrap(~ year) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per capita vs life expectancy", x = "Average GDP Per Capita", y = "Average Life Expectancy" )+
  theme_test()

ggplot(group_gapminder, aes(x = avg_life_expectancy, fill=continent)) +
  geom_density() +
  labs(title = "Life Expentancy across Continents", x = "Life Expentancy", y = "Population Density") +
  theme_test()

#3 Interactive Visualizations with Plotly

p <- ggplot(group_gapminder, aes(x = year, y = avg_life_expectancy, color = continent)) +
  geom_line(size = 1.0) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs( title = "Average Life Expectancy Over Time by Continent", x = "Year", y = "Average Life Expectancy" ) +
  theme_classic()
ggplotly(p)

p2 <- ggplot(group_gapminder, aes(x = avg_gdp_per_capita, y = avg_life_expectancy)) +
  geom_point() + 
  facet_wrap(~ year) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per capita vs life expectancy", x = "Average GDP Per Capita", y = "Average Life Expectancy" )+
  theme_test()
ggplotly(p2)
