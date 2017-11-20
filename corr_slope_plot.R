make_plot <- function(species) {
    df <- iris[iris$Species == species,]

    corr <- round(cor(df$Sepal.Width, df$Sepal.Length), 3)
    model <- lm(Sepal.Width ~ Sepal.Length, data = df)
    slope <- round(coef(model)[2], 3)
    df %>%
        ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = paste("Species:", species), 
             subtitle = paste("Correlation:", corr, "\nSlope:", slope))
}

make_plot("versicolor")
