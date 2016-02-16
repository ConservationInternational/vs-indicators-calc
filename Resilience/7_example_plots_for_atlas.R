library(ggplot2)
library(dplyr)

d <- read.csv('Resilience_Data.csv')

for (categ in d$Category) {
    if (categ == 'Climate') next()
    ggplot(filter(d, Category %in% c(categ, 'Climate'))) +
        geom_point(aes(Variable, Estimate), size=3) +
        geom_linerange(aes(Variable, ymin=Estimate - Std_Error,
                          ymax=Estimate + Std_Error), size=2) +
        geom_linerange(aes(Variable, ymin=Estimate - 2*Std_Error,
                          ymax=Estimate + 2*Std_Error), size=.5) +
        facet_grid(.~Category, scales='free_x') + 
        ylab('Effect on yield (kg / ha)') +
        theme(axis.title.x=element_blank()) +
    ggsave(paste0('example_plot_', categ, '.jpg'))
}
