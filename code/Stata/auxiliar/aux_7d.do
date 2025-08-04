


	tempvar var1_c var2_c var3_c var4_c var5_c var6_c var7_c var8_c kap_c
	tempvar var1_g var2_g var3_g var4_g var5_g var6_g var7_g var8_g wag_g 
	if "`step'"=="raw" | "`step'"=="bfm_norep_pre"  {
		qui gen `var1_c' = wag_c
		qui gen `var2_c' = wag_c + mix_c
		qui gen `var3_c' = wag_c + mix_c + cap_c
		qui gen `var4_c' = wag_c + mix_c + cap_c + pen_c
		qui gen `var5_c' = wag_c + mix_c + cap_c + pen_c + imp_c

		qui gen `kap_c'  = cap_c
		qui gen `wag_g'  = wag_g
	}
	if "`step'"=="rescaled"  {
		qui gen `var1_c' = wag_sca_c
		qui gen `var2_c' = wag_sca_c + mix_sca_c
		qui gen `var3_c' = wag_sca_c + mix_sca_c + cap_sca_c
		qui gen `var4_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c
		qui gen `var5_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c

		qui gen `kap_c'  = cap_sca_c
		qui gen `wag_g'  = wag_sca_g
	}

	if "`step'"=="uprofits"  {
		qui gen `var1_c' = wag_sca_c
		qui gen `var2_c' = wag_sca_c + mix_sca_c
		qui gen `var3_c' = wag_sca_c + mix_sca_c + cap_sca_c
		qui gen `var4_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c
		qui gen `var5_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c
		qui cap gen `var6_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c + upr_2_c

		qui gen `kap_c'  = cap_sca_c + upr_2_c
		qui gen `wag_g'  = wag_sca_g
	}

	if "`step'"=="natinc"  {
		qui gen `var1_c' = wag_sca_c
		qui gen `var2_c' = wag_sca_c + mix_sca_c
		qui gen `var3_c' = wag_sca_c + mix_sca_c + cap_sca_c
		qui gen `var4_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c
		qui gen `var5_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c
		qui cap gen `var6_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c + upr_2_c
		qui cap gen `var7_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c + upr_2_c + indg_pre_c
		qui cap gen `var8_c' = wag_sca_c + mix_sca_c + cap_sca_c + pen_sca_c + imp_sca_c + upr_2_c + indg_pre_c + lef_c

		qui gen `kap_c'  = cap_sca_c + upr_2_c
		qui gen `wag_g'  = wag_sca_g
	}

	if "`step'"=="raw" | "`step'"=="bfm_norep_pre"  {
		qui gen `var1_g' = wag_g
		qui gen `var2_g' = mix_g
		qui gen `var3_g' = cap_g
		qui gen `var4_g' = pen_g
		qui gen `var5_g' = imp_g

		*qui gen `kap_g'  = cap_g
	}
	if "`step'"=="rescaled"  {
		qui gen `var1_g' = wag_sca_g
		qui gen `var2_g' = mix_sca_g
		qui gen `var3_g' = cap_sca_g
		qui gen `var4_g' = pen_sca_g
		qui gen `var5_g' = imp_sca_g

		*qui gen `kap_g'  = cap_sca_g 
	}

	if "`step'"=="uprofits"  {
		qui gen `var1_g' = wag_sca_g
		qui gen `var2_g' = mix_sca_g
		qui gen `var3_g' = cap_sca_g
		qui gen `var4_g' = pen_sca_g
		qui gen `var5_g' = imp_sca_g
		qui cap gen `var6_g' = upr_2_g

		*qui gen `kap_g'  = cap_sca_g + upr_2_g
	}

	if "`step'"=="natinc"  {
		qui gen `var1_g' = wag_sca_g
		qui gen `var2_g' = mix_sca_g
		qui gen `var3_g' = cap_sca_g
		qui gen `var4_g' = pen_sca_g
		qui gen `var5_g' = imp_sca_g
		qui cap gen `var6_g' = upr_2_g
		qui cap gen `var7_g' = indg_pre_g
		qui cap gen `var8_g' = lef_g

		*qui gen `kap_g'  = cap_sca_g + upr_2_g
	}
	
