## Version: 0.0.1
## Time-stamp: <2020-05-08T19:39:08Z>

## The SHIMURA, Ken Problem for JAGS.

library(rjags)
library(coda)
#library(HDInterval)
#source('DBDA2E-utilities.R')
library(ggplot2)
#Sys.setenv('LANGUAGE'='EN')

cat('SK Problem 01\n')
m <- jags.model('sk_problem01.jag', list(N = 100, n.fam.sk = 5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))

cat('SK Problem 02\n')
m <- jags.model('sk_problem02.jag', list(N = 100, n.fam.sk = 5, n.risk = 10), list(list(p.pos.sk = 0.01, pi.risk = 0.1, pi.nrisk = 0.1)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'mag'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))

cat('SK Problem 03\n')
m <- jags.model('sk_problem03.jag', list(N = 100, n.fam.sk = 5, n.risk = 10), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01, pi.risk = 0.1, pi.nrisk = 0.1)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'mag'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 04\n')
N <- 100
m.pos <- rep(1, N)
m.pos[2] <- N
m <- jags.model('sk_problem04.jag', list(N = N, n.fam.sk = 5, n.risk = 10), list(list(p.pos.sk = 0.01, n.pos.sk = 50, pi.risk = 0.1, pi.nrisk = 0.1, m.pos = m.pos)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'mag'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 05\n')
m <- jags.model('sk_problem05.jag', list(N = 100, n.fam.sk = 5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 06\n')
m <- jags.model('sk_problem06.jag', list(N = 100, n.fam.sk = 5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 07\n')
m <- jags.model('sk_problem07.jag', list(N = 100, n.fam.sk = 5, n.risk = 10), list(list(p.pos.sk = 0.01, pi.risk = 0.1, pi.nrisk = 0.1)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'mag'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 08\n')
N <- 100
m.pos <- rep(1, N)
m.pos[2] <- N
m <- jags.model('sk_problem08.jag', list(N = N, n.fam.sk = 5, n.risk = 10), list(list(p.pos.sk = 0.01, n.pos.sk = 50, pi.risk = 0.1, pi.nrisk = 0.1, m.pos = m.pos)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'mag'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 09\n')
m <- jags.model('sk_problem09.jag', list(N = 100, n.fam.sk = 5, n.risk = 50, pi.risk = 0.1, pi.nrisk = 0.00001), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 10\n')
N <- 100
m.pos <- rep(1, N)
m <- jags.model('sk_problem10.jag', list(N = 100, n.fam.sk = 5, n.risk = 80, pi.risk = 0.1, pi.nrisk = 0.001), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01, m.pos = m.pos)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 11\n')
m <- jags.model('sk_problem11.jag', list(N = 100, n.fam.sk = 5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 12\n')
m <- jags.model('sk_problem12.jag', list(N = 100, n.fam.sk = 5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 13\n')
N <- 100
f.pos <- rep(1, N)
m <- jags.model('sk_problem13.jag', list(N = N, n.fam.sk = 5, filter.fam = 0.5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01, f.pos = f.pos)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 14\n')
N <- 100
n.fam.sk = 5
mf.pos <- rep(1, n.fam.sk)
mf.f <- rep(1, n.fam.sk)
m <- jags.model('sk_problem14.jag', list(N = N, n.fam.sk = n.fam.sk, filter.fam = 0.5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01, mf.pos = mf.pos, mf.f = mf.f, n.pos.sk = n.fam.sk)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 15\n')
m <- jags.model('sk_problem15.jag', list(N = N, n.fam.sk = n.fam.sk, filter.fam = 0.5), list(list(p.pos.sk = 0.01, p.pos.nsk = 0.01)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 16\n')
m <- jags.model('sk_problem16.jag', list(N = 100, n.fam.sk = 5, filter.fam = 0.5, n.risk = 10, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(list(p.pos.sk.temp = 0.1, p.pos.nsk.temp = 0.1, n.pos.sk = 50, n.pos.sk.risk = 5, n.pos.nsk = 50, n.pos.nsk.risk = 5, n.pos.sk.fam = 1)), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'n.pos.fam', 'n.pos.fam2'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))
#diagMCMC(post.list, c('n.pos.sk'))

m <- jags.model('sk_problem16.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk = 50, n.pos.sk.risk = 5, n.pos.nsk = 50, n.pos.nsk.risk = 5, n.pos.sk.fam = 1)), n.chains=1)
#m <- jags.model('sk_problem16.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk = 50, n.pos.sk.risk = 5, n.pos.nsk = 50, n.pos.nsk.risk = 5, n.pos.sk.fam = 1), n.chains=4)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'n.pos.sk.fam', 'n.pos.sk.fam2'), n.iter=10000 * 10)
print(summary(post.list))
print(effectiveSize(post.list))



cat('SK Problem 17\n')
m <- jags.model('sk_problem17.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.sk.fam.f = 1, n.pos.sk.fam = 1)), n.chains=1)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'n.pos.sk.fam', 'n.pos.sk.fam.f'), n.iter=10000 * 10)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 18\n')
mu = 1.0/3 # == alpha / (alpha + beta)
kappa = 1.5 # == alpha + beta
kappa = 3
alpha = mu * kappa
beta = (1 - mu) * kappa

n.fam.sk <- 100
mf.fam <- rep(1, n.fam.sk)
mf.f <- rep(1, n.fam.sk)
m <- jags.model('sk_problem18.jag', list(N = 100000000, n.fam.sk = n.fam.sk, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, mf.fam = mf.fam, mf.f = mf.f)), n.chains=1)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000 * 10)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 19\n')
m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1)), n.chains=1)
m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000 * 10)
print(summary(post.list))
print(effectiveSize(post.list))
#plot(post.list)


cat('SK Problem 20\n')
m <- jags.model('sk_problem20.jag', list(), list(n1 = 100, n11 = 100), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n1', 'n2', 'diff'), n.iter=10000, thin=1)
print(summary(post.list))
print(effectiveSize(post.list))
#print(hdi(post.list))
print(HPDinterval(post.list)) # same as hdi
#diagMCMC(post.list, "n1")
#plot(post.list)
#plot(post.list, trace=FALSE)


cat('SK Problem 21\n')
m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))
print(sum(unlist(lapply(post.list, "nrow"))))

m <- jags.model('sk_problem21.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.5, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'n.pos.fam.f'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))
post.list2 <- lapply(post.list, function (x) {
    as.mcmc(x[x[, "n.pos.fam.f"] >= 1,])
})
print(sum(unlist(lapply(post.list2, "nrow"))))


cat('SK Problem 22\n')
m <- jags.model('sk_problem22.jag', list(N = 100000000, n.fam.sk = 100, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 0.5, beta.pos = 1.0, n.pos.pub = 200, filter.pub = 0.001, filter.pub.fam = 0.001 * 10, n.pos.pub.fam = 1), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 100, n.pos.nsk.risk = 100, n.pos.fam = 1, n.pos.nsk.risk.pub = 100, n.pos.nsk.nrisk.pub = 100, n.pos.sk.risk.pub = 100, n.pos.sk.nrisk.pub = 99), n.chains=1)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 23\n')
m <- jags.model('sk_problem23.jag', list(N = 100000000, n.fam.sk = 100, alpha.pos = 0.5, beta.pos = 1.0, n.pos.pub = 200, filter.pub = 0.001, filter.pub.fam = 0.001 * 10, n.pos.pub.fam = 1), list(p.pos.sk = 0.001, p.pos.nsk = 0.001), n.chains=1)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000 * 1000, thin=1000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 24\n')
m <- jags.model('sk_problem24.jag', list(N = 100000000, n.fam.sk = 100, alpha.pos = 0.5, beta.pos = 1.0, alpha.filter = 0.5, beta.filter = 1.0, n.pos.pub = 200, filter.fam.mag = 10.0, n.pos.pub.fam = 1), list(p.pos.sk = 0.001, p.pos.nsk = 0.001, n.pos.fam = 1), n.chains=1)
update(m, 10000 * 10)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr', 'filter.pub.sk', 'filter.pub.nsk'), n.iter=10000 * 1000, thin=1000)
print(summary(post.list))
print(effectiveSize(post.list))


cat('SK Problem 19\n')
m <- jags.model('sk_problem19.jag',
                data = list(
                    N = 100000000,
                    n.fam.sk = 100,
                    n.risk = 1000000,
                    filter.fam = 0.01,
                    mag.risk = 2, mag.all = 10,
                    alpha.pos = 1, beta.pos = 2
                ),
                inits = list(
                    p.pos.sk.temp = 0.001,
                    p.pos.nsk.temp = 0.001,
                    n.pos.sk.risk = 5,
                    n.pos.nsk.risk = 5,
                    n.pos.fam.f = 1
                ),
                n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'),
                          n.iter=10000)
print(summary(post.list))
#diff      1.652e+06 3.261e+06 1.631e+04      1.820e+04
#gr        6.984e-01 4.589e-01 2.295e-03      2.409e-03
#n.pos.nsk 3.352e+06 2.370e+06 1.185e+04      1.185e+04
#n.pos.sk  5.003e+06 2.257e+06 1.129e+04      1.392e+04
print(effectiveSize(post.list))
#     diff        gr n.pos.nsk  n.pos.sk 
# 32117.47  36317.01  40000.00  26286.33 
vec.sk <- unlist(as.vector(post.list[, 'n.pos.sk']))
vec.nsk <- unlist(as.vector(post.list[, 'n.pos.nsk']))
df <- data.frame(
    n.pos=factor(rep(c("sk", "nsk"), each=length(vec.sk))),
    vl=c(vec.sk, vec.nsk)
)
p <- ggplot(df, aes(x=vl, fill=n.pos, color=n.pos)) +
    geom_density(alpha=0.4) +
    labs(x=NULL, y=NULL)
#ggsave(p, file = 'fig-n-pos-sk-nsk.png', dpi=96, w=7, h=5)
ggsave(p, file = 'fig-n-pos-sk-nsk.png', dpi=96, w=6, h=4)


m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.9, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 1, beta.pos = 2), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
#diff      3.179e+05 3.295e+06 1.648e+04      3.875e+04
#gr        5.462e-01 4.979e-01 2.489e-03      4.736e-03
#n.pos.nsk 3.355e+06 2.378e+06 1.189e+04      1.192e+04
#n.pos.sk  3.673e+06 2.291e+06 1.145e+04      3.692e+04
print(effectiveSize(post.list))
#     diff        gr n.pos.nsk  n.pos.sk 
#  7267.52  11122.85  39784.59   3860.55 

m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.01, n.risk = 1000000, mag.risk = 10, mag.all = 10, alpha.pos = 1, beta.pos = 2), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
#diff      1.606e+06 3.533e+06 1.767e+04      2.108e+04
#gr        6.796e-01 4.666e-01 2.333e-03      2.613e-03
#n.pos.nsk 3.609e+06 2.561e+06 1.280e+04      1.285e+04
#n.pos.sk  5.215e+06 2.432e+06 1.216e+04      1.701e+04
print(effectiveSize(post.list))
#     diff        gr n.pos.nsk  n.pos.sk 
# 28102.76  31900.55  39707.28  20483.54 

m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.01, n.risk = 1000000, mag.risk = 2, mag.all = 10, alpha.pos = 2, beta.pos = 4), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
#diff      9.021e+05 2.527e+06 1.263e+04      1.421e+04
#gr        6.438e-01 4.789e-01 2.394e-03      2.550e-03
#n.pos.nsk 3.380e+06 1.803e+06 9.015e+03      9.015e+03
#n.pos.sk  4.282e+06 1.767e+06 8.834e+03      1.108e+04
print(effectiveSize(post.list))
#     diff        gr n.pos.nsk  n.pos.sk 
# 31632.33  35291.91  40000.00  25417.18 

m <- jags.model('sk_problem19.jag', list(N = 100000000, n.fam.sk = 100, filter.fam = 0.9, n.risk = 1000000, mag.risk = 10, mag.all = 10, alpha.pos = 2, beta.pos = 4), list(p.pos.sk.temp = 0.001, p.pos.nsk.temp = 0.001, n.pos.sk.risk = 5, n.pos.nsk.risk = 5, n.pos.fam.f = 1), n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'), n.iter=10000)
print(summary(post.list))
#diff      7.064e+03 2725747.4 1.363e+04      6.021e+04
#gr        5.021e-01       0.5 2.500e-03      8.679e-03
#n.pos.nsk 3.641e+06 1943851.8 9.719e+03      9.676e+03
#n.pos.sk  3.648e+06 1924074.9 9.620e+03      6.132e+04
print(effectiveSize(post.list))
#      diff         gr  n.pos.nsk   n.pos.sk 
# 2076.5247  3341.9115 40368.5865   989.3227 
