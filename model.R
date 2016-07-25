# Calculate concentrations at each time-point for the individual
	# Using mrgsolve - analytical solutions
	# This compiled model is used for simulating the individual's estimated profile and 95% prediction intervals
    code <- '
		$PKMODEL  ncmt = 1,	// 1-compartment model
              depot = TRUE,	// Depot compartment for oral absorption
              trans = 2	// Parameterised in terms of CL, V and KA

		$INIT     // Initial conditions for compartments
							GUT = 0, CENT = 0

    $PARAM    //Population parameter values - fixed effects
							POPCL = 14.6076,	// Clearance, L/h
              POPV = 76.1352,	// Volume of central compartment, L/70 kg
              POPKA = 0.66668,	// First-order absorption rate constant, h^-1
              POPF = 1,	// Relative bioavailability

							//Default ETAs (replaced by Bayesian estimated values)
              ERR_CL = 0,
              ERR_V = 0,
              ERR_KA = 0,
              ERR_F = 0,

							//Default covariate values
              WT = 70,	// Weight (kg)
              SDAC = 0,	// Single-dose activated charcoal (0 or 1)
              PROD = 0,	// Acetaminophen product category (0, 1, 2, 3, or 4)

							//Covariate effects
              WT_CL = 0.75,	// Effect of WT on CL
              WT_V = 1,	// Effect of WT on V
              SDAC_F = -0.179735,	// Effect of SDAC on F
              PROD0_KA = 0,	// Effect of acetaminophen alone on KA
              PROD1_KA = 1.41279,	// Effect of acetaminophen+antihistamine combination on KA
              PROD2_KA = -0.488444,	// Effect of acetaminophen+opioid combination on KA
              PROD3_KA = 0.0222383,	// Effect of acetaminophen+other combination on KA
              PROD4_KA = -0.348731	// Effect of extended-release acetaminophen on KA

    $OMEGA    // Inter-individual variability - random effects
							block = TRUE
              labels = s(ETA_CL,ETA_V,ETA_KA,ETA_F)
              0.035022858
              0.0044077284  0.0054543827
              0.10313184 -0.0034085583 0.45608978
              0.016587014 0.002349424 -0.15735995 0.52338442

    $SIGMA    // Intra-individual variability - random effects
							block = FALSE
              labels = s(ERR_PRO)
              0.101285

    $MAIN     // Individual parameter values
							double CL = POPCL*pow(WT/70,WT_CL)*exp(ETA_CL+ERR_CL);
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
