# Calculate concentrations at each time-point for the individual
	# Using mrgsolve - analytical solutions
	# This compiled model is used for simulating the individual's estimated profile and 95% prediction intervals
    code <- '
    $PARAM    POPCL = 14.6076,  //Population parameter values - fixed effects
              POPV = 76.1352,
              POPKA = 0.66668,
              POPF = 1,
              ERR_CL = 0, //Place holders for Bayesian estimated ETAs
              ERR_V = 0,
              ERR_KA = 0,
              ERR_F = 0,
              WT_CL = 0.75, //Covariate effects
              WT_V = 1,
              SDAC_F = -0.179735,
              PROD0_KA = 0,
              PROD1_KA = 1.41279,
              PROD2_KA = -0.488444,
              PROD3_KA = 0.0222383,
              PROD4_KA = -0.348731,
              WT = 70,  //Place holders for patient covariate values
              SDAC = 0,
              PROD = 0

    $INIT     GUT = 0, CENT = 0

    $PKMODEL  ncmt = 1,
              depot = TRUE,
              trans = 2

    $OMEGA    block = TRUE
              labels = s(ETA_CL,ETA_V,ETA_KA,ETA_F)
              0.035022858
              0.0044077284  0.0054543827
              0.10313184 -0.0034085583 0.45608978
              0.016587014 0.002349424 -0.15735995 0.52338442

    $SIGMA    block = FALSE
              labels = s(ERR_PRO)
              0.101285

    $MAIN     double CL = POPCL*pow(WT/70,WT_CL)*exp(ETA_CL+ERR_CL);
              double V = POPV*pow(WT/70,WT_V)*exp(ETA_V+ERR_V);
              double KA = POPKA;
              if (PROD == 0) KA = POPKA*(1+PROD0_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 1) KA = POPKA*(1+PROD1_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 2) KA = POPKA*(1+PROD2_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 3) KA = POPKA*(1+PROD3_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 4) KA = POPKA*(1+PROD4_KA)*exp(ETA_KA+ERR_KA);
              double F = POPF*(1+SDAC_F*SDAC)*exp(ETA_F+ERR_F);
              F_GUT = F;

    $TABLE    table(IPRE) = CENT/V;
              table(DV) = table(IPRE)*(1 + ERR_PRO);

    $CAPTURE  CL V KA F
    '
    mod <- mcode("popAPAP",code)  #Compile the model code on application initiation
    #There is opportunity to simply update model parameters after the model code has been compiled
