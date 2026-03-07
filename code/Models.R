
# GAMLSS models configuration (CBT & SCC)
models_gamlss <- list(
  LINFarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  LINFarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  LINYear.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  LINYear.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  BCTFarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  BCTFarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  BCTYear.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
    ),

  BCTYear.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2Year.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2Year.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  )
)

# GAMLSS restricted models configurations (CBT & SCC)
models_gamlss_restricted <- list(
  GammaFarmCBT = list(
    formula = LogCBT ~  Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt_restricted"
  ),

  GammaFarmSCC = list(
    formula = LogSCC ~  Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt_restricted"
  ),

  GammaYear.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt_restricted"
  ),

  GammaYear.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt_restricted"
  ),

  GeneralizedGammaFarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt_restricted"
  ),

  GeneralizedGammaFarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt_restricted"
  ),

  GeneralizedGammaYear.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt_restricted"
  ),

  GeneralizedGammaYear.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt_restricted"
  ),

  ExGaussFarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt_restricted"
  ),

  ExGaussFarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt_restricted"
  ),

  ExGaussYear.FarmCBT = list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt_restricted"
  ),

  ExGaussYear.FarmSCC = list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt_restricted"
  )
)

test_models <- list(
  testmodCBTFarm =list(
  formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
  sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
  family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_train"
  ),

  testmodSCCFarm =list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ YearNum|Farm),
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_train"
  ),

  testmodCBT =list(
    formula = LogCBT ~ Season + Year + RiskIBR + RiskAGAL + RiskPT,
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "train_ignote"
  ),
  
  testmodSCC =list(
    formula = LogSCC ~ Season + Year + RiskIBR + RiskAGAL + RiskPT,
    sigma = ~ Season + Year + RiskIBR + RiskAGAL + RiskPT + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "train_ignote"
  )
)
