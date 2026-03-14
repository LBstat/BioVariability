
# Gamm4 models configurations
models_gamm4 <- list(
  LINmixedFarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    s(YearNum, bs = "ps", by = Season) + s(YearNum, bs = "ps", by = RiskIBR) + s(YearNum, bs = "ps", by = RiskPT),
    random = ~ (1 | Farm)
  ),
  
  LINmixedYear.FarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    s(YearNum, bs = "ps", by = Season) + s(YearNum, bs = "ps", by = RiskIBR) + s(YearNum, bs = "ps", by = RiskPT),
    random = ~ (1 | Farm) + (YearNum | Farm)
  ),
  
  LINmixedFarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    s(YearNum, bs = "ps", by = Season) + s(YearNum, bs = "ps", by = RiskIBR) + s(YearNum, bs = "ps", by = RiskPT),
    random = ~ (1 | Farm)
  ),
  
  LINmixedYear.FarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    s(YearNum, bs = "ps", by = Season) + s(YearNum, bs = "ps", by = RiskIBR) + s(YearNum, bs = "ps", by = RiskPT),
    random = ~ (1 | Farm) + (YearNum | Farm)
  )
)

# GAMLSS models configuration (CBT & SCC)
models_gamlss <- list(
  LINFarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'NO(mu.link = "identity", sigma.link = "log")'
  ),

  LINFarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'NO(mu.link = "identity", sigma.link = "log")'
  ),

  LINYear.FarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'NO(mu.link = "identity", sigma.link = "log")'
  ),

  LINYear.FarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'NO(mu.link = "identity", sigma.link = "log")'
  ),

  BCTFarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  BCTFarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  BCTYear.FarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  BCTYear.FarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  ST2FarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  ST2FarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  ST2Year.FarmCBT = list(
    formula = LogCBT ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogSCC + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogSCC + Season:RiskIBR + Season:RiskPT + LogSCC:RiskIBR + LogSCC:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  ),

  ST2Year.FarmSCC = list(
    formula = LogSCC ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ YearNum|Farm),
    sigma = ~ Season + LogCBT + (RiskIBR + RiskAGAL + RiskPT)^2 +
    Season:LogCBT + Season:RiskIBR + Season:RiskPT + LogCBT:RiskIBR + LogCBT:RiskPT +
    pb(YearNum, by = Season) + pb(YearNum, by = RiskIBR) + pb(YearNum, by = RiskPT) + re(random = ~ 1|Farm),
    family_str = 'ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")'
  )
)
