#=====================================
# Prakarsho - 2022                             05/01/2022
# Kurtosis: Diving in its Controversy
#=====================================
library(ggplot2)
library(extraDistr)
library(latex2exp)

rm(list = ls())

# Theme ----
theme_set(theme_light())

theme_new = theme(plot.title = element_text(size = 22, 
                                            face = "bold", 
                                            hjust = 0.5), 
                  plot.subtitle = element_text(size = 20, 
                                               hjust = 0.5), 
                  plot.caption = element_text(size = 14, 
                                              face = "bold"), 
                  plot.margin = margin(1, 0.7, 0.7, 0.7, "cm"), 
                  legend.title = element_text(size = 16, 
                                              face = "bold"), 
                  legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 15, 
                                            face = "bold"), 
                  axis.text = element_text(size = 13, 
                                           face = "bold"))

#--------------------------------------------------------
# Old School Concepts
#---------------------
std_unif = sqrt(12)/2

ggplot() + 
     scale_color_manual(values = c("red2", "blue", "green4"), 
                         name = element_blank(), 
                         labels = c("Mesokurtic: Standard Normal", 
                                    "Platykurtic: Uniform", 
                                    "Leptokurtic: Laplace")) + 
     stat_function(fun = dnorm, lwd = 1.1, 
                   aes(col = "1")) + 
     stat_function(fun = dunif, lwd = 1.1, 
                   args = list(min = -std_unif, 
                               max = std_unif), 
                   lty = 4, xlim =  std_unif * c(-1, 1), 
                   aes(col = "2")) + 
     stat_function(fun = dlaplace, lwd = 1.1, lty = 2, 
                   aes(col = "3")) + 
     labs(title = "Mesokurtic, Leptokurtic & Platykurtic Distributions", 
          subtitle = "(Equal Variances)", 
          x = "\nVariable", y = "Density\n") + 
     xlim(-4, 4) + 
     theme(legend.position = c(0.87, 0.9)) + 
     theme_new

#--------------------------------------------------------
# Different 't'
#--------------
sim = function(m, cols)
{
     s = round(sqrt(m/(m-2)), 2)
     k_s = 6/(m - 4)
     
     p = ggplot() + 
          stat_function(fun = dt, args = list(df = m), 
                        lwd = 1.1, col = cols, xlim = c(-4, 4)) + 
          labs(title = TeX(paste0("$t_{", m, "}$ distribution"), bold = T), 
               subtitle = bquote(mu == 0~","~sigma == .(s)), 
               x = "\nVariable", y = "Density\n") + 
          geom_vline(xintercept = c(-3, 3) * s, col = "red3", 
                     lty = 2, lwd = 1.1) + 
          annotate("text", x = c(-3, 3) * s, y = 0.43, 
                   label = c(bquote(mu-3*sigma(-.(3*s))), bquote(mu+3*sigma(.(3*s)))), 
                   parse = T, size = 6) + 
          coord_cartesian(y = c(0, 0.4), clip = "off") + 
          theme_new
     
     return(list(Plot = p, Kurtosis = k_s))
}

sim(5, 4)   
sim(15, "#cc7a00")
sim(25, "green3")

#-----------------------------------------------------------
# N(0,5/3) & t_5
#----------------
ggplot() + 
     scale_color_manual(values = c(4, 2),
                         name = element_blank(),
                         labels = c(bquote(t[5]), 
                                    "N (0, 1.67)")) +
     stat_function(fun = dt, args = list(df = 5), lwd = 1.1, 
                   aes(col = "1")) + 
     stat_function(fun = dnorm, lwd = 1.1, lty = 2, 
                   aes(col = "2"), args = list(sd = sqrt(5/3))) + 
     labs(title = TeX("Normal & $t_5$ Distribution", bold = T), 
          subtitle = TeX(r"(Equal Variance = $\frac{5}{3}$)"), 
          x = "\nVariable", y = "Density\n") + 
     xlim(-5, 5) + ylim(0, 0.4) + 
     theme_new +
     theme(legend.position = c(0.87, 0.9), 
           legend.text = element_text(size = 18))

#-----------------------------------------------------------
# Beta (0.5, 1)
#--------------
beta_plot = function(m, n)
{
     mu = m/(m + n)
     sd2 = m * n/((m + n + 1) * (m + n)^2)
     
     k = 6 * (((m-n)^2 * (m + n + 1)) - m * n * (m + n + 2))/(m * n * (m + n + 2) * (m + n + 3))
     
     ggplot() + 
          scale_color_manual(values = c(4, 2), 
                         name = element_blank(), 
                         labels = c(paste0("Beta (", m, ", ", n, ")"), 
                                    paste0("N (", round(mu, 2), ", ", round(sd2, 2), ")"))) + 
          stat_function(fun = dbeta, lwd = 1.1, aes(col = "1"), 
                        args = list(shape1 = m, shape2 = n)) + 
          stat_function(fun = dnorm, lwd = 1.1, lty = 2, 
                        aes(col = "2"), 
                        args = list(mean = mu, sd = sqrt(sd2))) + 
          labs(title = paste0("Beta (", m, ", ", n, ") Distribution"), 
               subtitle = TeX(paste0(r"((Kurtosis: $\gamma_2$ = )", round(k, 3), ")")), 
               x = "\nVariable", y = "Density\n") + 
          theme_new + 
          theme(legend.position = c(0.87, 0.9), 
                legend.text = element_text(size = 16))
}

a = 0.5; b = 1
beta_plot(a, b)

#-----------------------------------------------------------
# Normal Mixtures
#-----------------
mix_norm = function(m, s, p, k = c(3, 3))
{
     l = min(m) - k[1] * s[which.min(m)]
     r = max(m) + k[2] * s[which.max(m)]
     
     mu = sum(p * m)
     v = sum(p * s^2) + sum(p * (m - mu)^2)
     
     ggplot() + 
          scale_color_manual(values = c(4, 2), 
                         name = element_blank(), 
                         labels = c("Mixed Normal", 
                                    paste0("N (", round(mu, 2), ", ", round(v, 2), ")"))) + 
          stat_function(fun = dmixnorm, lwd = 1.1, 
                        args = list(mean = m, sd = s, 
                                    alpha = p), 
                        aes(col = "1")) + 
          stat_function(fun = dnorm, aes(col = "2"), 
                        args = list(mean = mu, sd = sqrt(v)), 
                        lwd = 1.1, lty = 2) + 
          labs(title = paste0("Mixture of Two Normals: ", p[1], "N (", m[1], ", ", s[1]^2, ") + ", p[2], "N (", m[2], ", ", s[2]^2, ")"), 
               subtitle = TeX(paste0(r"((Kurtosis: $\gamma_2$ = )", -1.324, ")")), 
               x = "\nVariable", y = "Density\n") + 
          xlim(l, r) + 
          theme_new + 
          theme(legend.position = c(0.87, 0.9), 
                legend.text = element_text(size = 16))
}

means = c(0, 4); sd = c(1, 1); prop = c(0.3, 0.7) 
lim = c(5, 6)

mix_norm(means, sd, prop, lim) + ylim(0, 0.3)

#------------------------------------------------------------
# Numerical Examples
#--------------------
library(moments)

set.seed(0)
x = rbinom(20, 7, 0.5); x
mean(x)
kurtosis(x) - 3

set.seed(0)
# repeat 8 times to get a value 722
y = abs(round(rcauchy(20))); y        
mean(y)
kurtosis(y) - 3

#-----------------------------------------------------------
