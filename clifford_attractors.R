library(Rcpp)
library(ggplot2)
library(dplyr)

opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            panel.border     = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')
 
a=1.37458046630025
b=1.22191834103316 
c=-1.89590817030519 
d=-1.92866735205054
 
df=createTrajectory(1000000, 0, 0, a, b, c, d)
 
png("Clifford.png", units="px", width=1446, height=828, res=300)
ggplot(df, aes(x, y)) + 
    geom_point(color="black", shape=46, alpha=.01) + opt
dev.off()
