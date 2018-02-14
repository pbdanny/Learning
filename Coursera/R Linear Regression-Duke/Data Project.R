# Coursera-Duke
# Linear Regression & Modeling
# Data Project

load("/Users/Danny/Documents/R Project/R-Coursera-Duke/movies.Rdata")

str(movies)

dim(movies)

# Univariate analysis
# Frequency table
table(movies$title_type)
table(movies$genre)
hist(movies$runtimes)
table(movies$mpaa_rating)
hist(movies$thtr_rel_year)
hist(movies$imdb_rating)

# Bivariate analysis

plot(data = movies, imdb_rating ~ log(imdb_num_votes))
plot(lm(data = movies, imdb_rating ~ log(imdb_num_votes)))

library(GGally)
ggpairs(data = movies, columns = c(13, 16, 18))
# imdb score correlated with both critics and audience score
# critics vs audience score less correlated

with(movies, table(best_pic_nom, best_pic_win))

# Found pic never nominated but awarded

# Chi-squared test of independent

t <- with(movies, table(best_pic_nom, best_pic_win))
# p-value <- 2.2e-6, then reject H0 that varibles are independents
# Anova test of different mean between group
test <- aov(data = movies, imdb_rating ~ title_type)
summary(test)
             # Df Sum Sq Mean Sq F value Pr(>F)    
# genre        10  174.5  17.446   18.91 <2e-16 ***
# Residuals   640  590.4   0.922                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# From result show there is one pairs have different means

posthoc <- TukeyHSD(test, "title_type")
posthoc$title_type

                               # diff       lwr        upr       p adj
# Feature Film-Documentary -1.2816121 -1.621144 -0.9420805 0.000000000
# TV Movie-Documentary     -1.6290909 -2.754080 -0.5041020 0.002051018
# TV Movie-Feature Film    -0.3474788 -1.429120  0.7341627 0.730916765

# 1. Feature Film - Documentary different with mean = -1.28
# 2. TV Movie-Documentary  different with mean = -1.62
# 3. TV Movie-Feature Film not significant different in mean

plot(posthoc)