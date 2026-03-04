
# GAMLSS models configuration (CBT & SCC)
models_gamlss <- list(
  LINFarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
    ),

  LINYear.FarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
    ),

  LINFarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  LINYear.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    family = NO(mu.link = "identity", sigma.link = "log"), data = "dt_analysis"
  ),

  BCTFarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
    ),

  BCTYear.FarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
    ),

  ST2FarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2Year.FarmCBT = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  BCTFarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  BCTYear.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  ),

  ST2Year.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = "dt_analysis"
  )
)

# GAMLSS restricted models configurations (CBT & SCC)
models_gamlss_restricted <- list(
  GammaFarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt"
  ),

  GammaFarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt"
  ),

  GammaYear.FarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt"
  ),

  GammaYear.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GA(mu.link = "identity", sigma.link = "log"), data = "dt"
  ),

  GeneralizedGammaFarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt"
  ),

  GeneralizedGammaFarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt"
  ),

  GeneralizedGammaYear.FarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt"
  ),

  GeneralizedGammaYear.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = GG(mu.link = "identity", sigma.link = "log", nu.link = "identity"), data = "dt"
  ),

  ExGaussFarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt"
  ),

  ExGaussFarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt"
  ),

  ExGaussYear.FarmTBC = list(
    formula = `Log CBT` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt"
  ),

  ExGaussYear.FarmSCC = list(
    formula = `Log SCC` ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ Year_num|Farm),
    sigma = ~ Season + Year + `Risk - IBR` + `Risk - AGAL` + `Risk - PT` + re(random = ~ 1|Farm),
    family = exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log"), data = "dt"
  )
)

