######################### pflow.r #########################

##01 ====== Atmosphere (N01) ======
# Atmospheric deposition
PF["N01","N02",]=(DTA-DIW-DAL)*PAD1
# Atmospheric deposition
PF["N01","N03",]=DIW*PAD1
# Atmospheric deposition
PF["N01","N04",]=DMW1*PAD2_1+DMW2*PAD2_2
# Atmospheric deposition
PF["N01","N07",]=DAL*PAD1

##02 ====== Non-arable land (N02) ======
# Weathering
PF["N02","N02",]=(DTA-DIW-DAL)*PWT 
# Weathering
PF["N02","N07",]=DAL*PWT 
# Runoff and leaching
PF["N02","N03",]=(DTA-DIW-DAL)*PRL 
# Wind erosion
PF["N02","N01",]=(DTA-DIW-DAL)*PWE 
# Woods
PF["N02","N1003",]=DWD*PWD1*PWD2

##03 ====== Inland waters (N03) ======
# Naturally grown freshwater products
PF["N03","N1001",]=DFW1*PFW1*DNFW/(DNFW+DAFW)+DFW2*PFW2*DNFW/(DNFW+DAFW)+DFW3*PFW3*DNFW/(DNFW+DAFW)+DFW4*PFW4*DNFW/(DNFW+DAFW)

##04 ====== Marine waters (N04) ======
# Sea spray
PF["N04","N01",]=(DMW1+DMW2)/DGMW*PSS 
# Naturally grown seawater products
PF["N04","N1001",]=DSW1*PSW1*DNSW/(DNSW+DASW)+DSW2*PSW2*DNSW/(DNSW+DASW)+DSW3*PSW3*DNSW/(DNSW+DASW)+DSW4*PSW4*DNSW/(DNSW+DASW)+DSW5*PSW5*DNSW/(DNSW+DASW)

##05 ====== Mining (N05) ======
# Produced natural phosphate rocks
PF["N0501","N050101",]=DPR*PPR 
# Produced gangues
PF["N0501","N050102",]=DPR*PGG1*PGG2
# Net exported natural phosphate rocks
PF["N050101","N14",]=DPRNE*PPR 
# Natural phosphate rocks for beneficiation
PF["N050101","N0502",]=p((PF["N0501","N050101",]-PF["N050101","N14",])*PBF)
# Natural phosphate rocks for chemical production
PF["N050101","N06",]=p((PF["N0501","N050101",]-PF["N050101","N14",])*(1-PBF))
# Gangues for disposal
PF["N050102","N02",]=PF["N0501","N050102",]
# Phosphate ores
PF["N02","N0501",]=suma(PF["N0501",,])
# Beneficiated phosphate rocks
PF["N0502","N050201",]=PF["N050101","N0502",]*PCR 
# Wastewater generated from beneficiation
PF["N0502","N050202",]=142/(62*0.3)*PF["N0502","N050201",]*PWWG1
# Tailings
PF["N0502","N050203",]=p(suma(PF[,"N0502",])-suma(PF["N0502",,]))
# Beneficiated phosphate rocks
PF["N050201","N06",]=PF["N0502","N050201",]
# Wastewater discharged from beneficiation with no treatment
PF["N050202","N03",]=PF["N0502","N050202",]*(1-PWW)
# Wastewater generated from beneficiation for further treatment
PF["N050202","N12",]=PF["N0502","N050202",]*PWW 
# Tailings
PF["N050203","N02",]=PF["N0502","N050203",]

##06 ====== Chemical production (N06) ======
##0601 Fertilizer production
# Produced fertilizers
PF["N0601","N060101",]=DFT*62/142
# Wastewater generated from fertilizer production
PF["N0601","N060102",]=DFT*(1-PFLT)/PFT*PWWG2
# Produced phosphogypsums
PF["N0601","N060103",]=DFT*(1-PFLT)/PFT*PPG1*PPG2*62/142
# Net exported fertilizers
PF["N060101","N14",]=(DFTNE1*PFT1+DFTNE2*PFT2+DFTNE3*PFT3+DFTNE4*PFT4+DFTNE5*PFT5+DFTNE6*PFT6+DFTNE7*PFT7+DFTNE8*PFT8+DFTNE9*PFT9+DFTNE10*PFT10+DFTNE11*PFT11+DFTNE12*PFT12)*62/142
# Consumed fertilizers
PF["N060101","N07",]=p(PF["N0601","N060101",]-PF["N060101","N14",])
# Wastewater discharged from fertilizer production with no treatment
PF["N060102","N03",]=PF["N0601","N060102",]*(1-PWW)
# Wastewater generated from fertilizer production for further treatment
PF["N060102","N12",]=PF["N0601","N060102",]*PWW 
# Phosphogypsums to non-arable land
PF["N060103","N02",]=PF["N0601","N060103",]*(1-PPG3)
# Recycled phosphogypsums
PF["N060103","N0606",]=PF["N0601","N060103",]*PPG3

##0602 Feed additive production
# Produced feed additives
PF["N0602","N060201",]=DFA*PFA 
# Wastewater generated from feed additive production
PF["N0602","N060202",]=DFA*PWWG3*PFAWW*62/142
# Solid waste generated from feed additive production
PF["N0602","N060203",]=DFA*PSWG*PFASW*62/142
# Net exported feed additives
PF["N060201","N14",]=DFANE*PFA 
# Consumed feed additives
PF["N060201","N1002",]=p(PF["N0602","N060201",]-PF["N060201","N14",])
# Wastewater discharged from feed additive production with no treatment
PF["N060202","N03",]=PF["N0602","N060202",]*(1-PWW)
# Wastewater generated from feed additive production for further treatment
PF["N060202","N12",]=PF["N0602","N060202",]*PWW 
# Solid waste to non-arable land
PF["N060203","N02",]=PF["N0602","N060203",]

##0603 Elemental P production
# Produced elemental P
PF["N0603","N060301",]=DEP 
# Wastewater generated from elemental P production
PF["N0603","N060302",]=DEP*PWWG4
# Ferrophosphorus byproduct generated from elemental P production
PF["N0603","N060303",]=DEP*PFP1*PFP2
# Slag generated from elemental P production
PF["N0603","N060304",]=DEP*PSL1*PSL2*62/142
# Net exported elemental P
PF["N060301","N14",]=DEPNE 
# Wastewater discharged from elemental P production with no treatment
PF["N060302","N03",]=PF["N0603","N060302",]*(1-PWW)*POLICY 
# Wastewater generated from elemental P production for further treatment
PF["N060302","N12",]=PF["N0603","N060302",]*(1-(1-PWW)*POLICY)
# Recycled ferrophosphorus
PF["N060303","N0606",]=PF["N0603","N060303",]
# Recycled slags
PF["N060304","N0606",]=PF["N0603","N060304",]*PSL3
# Slags to non-arable land
PF["N060304","N02",]=PF["N0603","N060304",]*(1-PSL3)

##0604 Pesticide production
# Produced organophosphorus pesticides
PF["N0604","N060401",]=DP*POP1*POP2
# Wastewater generated from organophosphorus pesticide production
PF["N0604","N060402",]=DP*POP1*PWWG5
# Net exported organophosphorus pesticides
PF["N060401","N14",]=DPNE*POP1*POP2
# Consumed organophosphorus pesticides
PF["N060401","N07",]=p(PF["N0604","N060401",]-PF["N060401","N14",])
# Wastewater discharged from organophosphorus pesticide production with no treatment
PF["N060402","N03",]=PF["N0604","N060402",]*(1-PWW)
# Wastewater generated from organophosphorus pesticide production for further treatment
PF["N060402","N12",]=PF["N0604","N060402",]*PWW 
# Elemental P use for organophosphorus pesticide production
PF["N060301","N0604",]=suma(PF["N0604",,])

##0605 Detergent production
# Produced soap and detergents
PF["N0605","N060501",]=(DDG1+DDG2)*PDG1*PDG2+(DDG1+DDG2)*(1-PDG1)*PDG3
# Wastewater generated from soap and detergent production
PF["N0605","N060502",]=(DDG1+DDG2)*PWWG6
# Net exported soap and detergents
PF["N060501","N14",]=(DDGNE1+DDGNE2)*PDG1*PDG2+(DDGNE1+DDGNE2)*(1-PDG1)*PDG3
# Consumed soap and detergents
PF["N060501","N11",]=p(PF["N0605","N060501",]-PF["N060501","N14",])
# Wastewater discharged from soap and detergent production with no treatment
PF["N060502","N03",]=PF["N0605","N060502",]*(1-PWW)
# Wastewater generated from soap and detergent production for further treatment
PF["N060502","N12",]=PF["N0605","N060502",]*PWW 
# Elemental P use for soap and detergent production 
PF["N060301","N0605",]=suma(PF["N0605",,])

##0606 Other applications
# Elemental P use for other applications 
PF["N060301","N0606",]=p(suma(PF[,"N060301",])-suma(PF["N060301",,]))
# Stock changes
PF["N0606","N15",]=suma(PF[,"N0606",])
# Balance
PF["N06","N0601",]=suma(PF["N0601",,])
PF["N06","N0602",]=suma(PF["N0602",,])
PF["N06","N0603",]=suma(PF["N0603",,])

##07 ====== Cultivation (N07) ======
# Produced raw crops
PF["N07","N070101",]=DRC1*PRC1
PF["N07","N070102",]=DRC2*PRC2
PF["N07","N070103",]=DRC3*PRC3
PF["N07","N070104",]=DRC4*PRC4
PF["N07","N070105",]=DRC5*PRC5
PF["N07","N070106",]=DRC6*PRC6
PF["N07","N070107",]=DRC7*PRC7
PF["N07","N070108",]=DRC8*PRC8
PF["N07","N070109",]=DRC9*PRC9
PF["N07","N070110",]=DRC9*PRC10*PCC
PF["N07","N070111",]=DRC11*PRC11
PF["N07","N070112",]=DRC12*PRC12
PF["N07","N070113",]=DRC13*PRC13+DRC14*PRC14+DRC15*PRC15
PF["N07","N070116",]=DRC16*PRC16
PF["N07","N070117",]=DRC17*PRC17
PF["N07","N070118",]=DRC18*PRC18
PF["N07","N070119",]=DRC19*PRC19
PF["N07","N070120",]=DRC20*PRC20
# Net exported raw crops
PF["N070101","N14",]=DRCNE1*PRC1
PF["N070102","N14",]=DRCNE2*PRC2
PF["N070103","N14",]=DRCNE3*PRC3
PF["N070105","N14",]=DRCNE5*PRC5
PF["N070106","N14",]=DRCNE6*PRC6
PF["N070107","N14",]=DRCNE7*PRC7
PF["N070108","N14",]=DRCNE8*PRC8
PF["N070109","N14",]=DRCNE9*PRC9
PF["N070111","N14",]=DRCNE11*PRC11
PF["N070112","N14",]=DRCNE12*PRC12
PF["N070113","N14",]=DRCNE13*PRC15
PF["N070116","N14",]=DRCNE16*PRC16
PF["N070117","N14",]=DRCNE17*PRC17
PF["N070118","N14",]=DRCNE18*PRC18
PF["N070119","N14",]=DRCNE19*PRC19
PF["N070120","N14",]=DRCNE20*PRC20
# Seeds
PF["N070101","N0702",]=DSA1*PSD1*PRC1
PF["N070102","N0702",]=DSA2*PSD2*PRC2
PF["N070103","N0702",]=DSA3*PSD3*PRC3
PF["N070104","N0702",]=DSA4*PSD4*PRC4
PF["N070105","N0702",]=DSA5*PSD5*PRC5
PF["N070106","N0702",]=DSA6*PSD6*PRC6
PF["N070107","N0702",]=DSA7*PSD7*PRC7
PF["N070110","N0702",]=DSA9*PSD9*PRC10
PF["N070111","N0702",]=DSA11*PSD11*PRC11
PF["N070112","N0702",]=DSA12*PSD12*PRC12
# Consumed raw crops
PF["N070101","N1001",]=p(suma(PF[,"N070101",])-suma(PF["N070101",,]))
PF["N070102","N1001",]=p(suma(PF[,"N070102",])-suma(PF["N070102",,]))
PF["N070103","N1001",]=p(suma(PF[,"N070103",])-suma(PF["N070103",,]))
PF["N070104","N1001",]=p(suma(PF[,"N070104",])-suma(PF["N070104",,]))
PF["N070105","N1001",]=p(suma(PF[,"N070105",])-suma(PF["N070105",,]))
PF["N070106","N1001",]=p(suma(PF[,"N070106",])-suma(PF["N070106",,]))
PF["N070107","N1001",]=p(suma(PF[,"N070107",])-suma(PF["N070107",,]))
PF["N070108","N1001",]=p(suma(PF[,"N070108",])-suma(PF["N070108",,]))
PF["N070109","N1001",]=p(suma(PF[,"N070109",])-suma(PF["N070109",,]))
PF["N070110","N1001",]=p(suma(PF[,"N070110",])-suma(PF["N070110",,]))
PF["N070111","N1001",]=p(suma(PF[,"N070111",])-suma(PF["N070111",,]))
PF["N070112","N1001",]=p(suma(PF[,"N070112",])-suma(PF["N070112",,]))
PF["N070113","N1001",]=p(suma(PF[,"N070113",])-suma(PF["N070113",,]))
PF["N070116","N1001",]=p(suma(PF[,"N070116",])-suma(PF["N070116",,]))
PF["N070117","N1001",]=p(suma(PF[,"N070117",])-suma(PF["N070117",,]))
PF["N070118","N1001",]=p(suma(PF[,"N070118",])-suma(PF["N070118",,]))
PF["N070119","N1001",]=p(suma(PF[,"N070119",])-suma(PF["N070119",,]))
PF["N070120","N1001",]=p(suma(PF[,"N070120",])-suma(PF["N070120",,]))
# Seeds
PF["N0702","N07",]=suma(PF[,"N0702",])
# Produced crop straws 
PF["N07","N0703",]=DRC1*PCS1_1*PCS2_1+DRC2*PCS1_2*PCS2_2+DRC3*PCS1_3*PCS2_3+DRC4*PCS1_4*PCS2_4+DRC5*PCS1_5*PCS2_5+DRC6*PCS1_6*PCS2_6+DRC7*PCS1_7*PCS2_7+DRC8*PCS1_8*PCS2_8+DRC9*PCS1_9*PCS2_9+DRC11*PCS1_11*PCS2_11+DRC12*PCS1_12*PCS2_12+DRC13*PCS1_13*PCS2_13+DRC14*PCS1_14*PCS2_14+DRC15*PCS1_15*PCS2_15+DRC16*PCS1_16*PCS2_16+DRC17*PCS1_17*PCS2_17+DRC18*PCS1_18*PCS2_18+DRC19*PCS1_19*PCS2_19
# Crop straws returned to arable land
PF["N0703","N07",]=PF["N07","N0703",]*PCS3_1
# Crop straws used as feeds
PF["N0703","N1002",]=PF["N07","N0703",]*PCS3_2
# Crop straws used as fuels
PF["N0703","N11",]=PF["N07","N0703",]*PCS3_3
# Unutilized crop straws 
PF["N0703","N02",]=PF["N07","N0703",]*(1-PCS3_1-PCS3_2-PCS3_3)
# Wind erosion
PF["N07","N01",]=DAL*PWE 
# Runoff
PF["N07","N0704",]=(DAL-DPAL)*PRF1+DPAL*PRF2
# Leaching
PF["N07","N0705",]=(DAL-DPAL)*PLH1+DPAL*PLH2
# Runoff
PF["N0704","N03",]=PF["N07","N0704",]
# Leaching
PF["N0705","N03",]=PF["N07","N0705",]

##08 ====== Animal husbandry (N08) ======
# Stock changes
PF["N080101","N15",]=DEDLA1*PLA1
PF["N080102","N15",]=DEDLA2*PLA2
PF["N080103","N15",]=DEDLA3*PLA3
PF["N080104","N15",]=DEDLA4*PLA4
PF["N080105","N15",]=DEDLA5*PLA5+DEDLA6*PLA6+DEDLA8*PLA8
PF["N080106","N15",]=DEDLA7*PLA7
PF["N080107","N15",]=DEDLA9*PLA9
# Produced live animals
PF["N08","N080101",]=p(DLA1*PLA1+PF["N080101","N15",])
PF["N08","N080102",]=p(DLA2*PLA2+PF["N080102","N15",])
PF["N08","N080103",]=p(DLA3*PLA3+PF["N080103","N15",])
PF["N08","N080104",]=p(DLA4*PLA4+PF["N080104","N15",])
PF["N08","N080105",]=p(DLA5*PLA5+DLA6*PLA6+DLA8*PLA8+PF["N080105","N15",])
PF["N08","N080106",]=p(DLA7*PLA7+PF["N080106","N15",])
PF["N08","N080107",]=p(DLA9*PLA9+PF["N080107","N15",])
# Net exported live animals
PF["N080101","N14",]=DLANE1*PLA1
PF["N080102","N14",]=DLANE2*PLA2
PF["N080103","N14",]=DLANE3*PLA3
PF["N080104","N14",]=DLANE4*PLA4
PF["N080105","N14",]=DLANE5*PLA5
# Consumed live animals
PF["N080101","N1001",]=p(suma(PF[,"N080101",])-suma(PF["N080101",,]))
PF["N080102","N1001",]=p(suma(PF[,"N080102",])-suma(PF["N080102",,]))
PF["N080103","N1001",]=p(suma(PF[,"N080103",])-suma(PF["N080103",,]))
PF["N080104","N1001",]=p(suma(PF[,"N080104",])-suma(PF["N080104",,]))
PF["N080105","N1001",]=p(suma(PF[,"N080105",])-suma(PF["N080105",,]))
PF["N080106","N1001",]=p(suma(PF[,"N080106",])-suma(PF["N080106",,]))
PF["N080107","N1001",]=p(suma(PF[,"N080107",])-suma(PF["N080107",,]))
# Produced diary and egg products
PF["N08","N080201",]=DDR*PDR
PF["N08","N080202",]=DEG*PEG 
# Diary and egg for further processing
PF["N080201","N1001",]=PF["N08","N080201",]
PF["N080202","N1001",]=PF["N08","N080202",]
# Produced animal excreta
PF["N08","N080301",]=DDLA1*PAE1_1*365/1e6
PF["N08","N080302",]=DDLA2*PAE1_2*365/1e6
PF["N08","N080303",]=DDLA3*PAE1_3*365/1e6
PF["N08","N080304",]=DDLA4*PAE1_4*365/1e6
PF["N08","N080305",]=(DDLA5*PAE1_5+DDLA6*PAE1_6+DDLA8*PAE1_8)*365/1e6
PF["N08","N080306",]=DDLA7*PAE1_7*365/1e6
PF["N08","N080307",]=DDLA9*PAE1_9*365/1e6
# Animal excreta reused to arable land
PF["N080301","N07",]=PF["N08","N080301",]*PAE2_1
PF["N080302","N07",]=PF["N08","N080302",]*PAE2_2
PF["N080303","N07",]=PF["N08","N080303",]*PAE2_3
PF["N080304","N07",]=PF["N08","N080304",]*PAE2_4
PF["N080305","N07",]=PF["N08","N080305",]*PAE2_5
PF["N080306","N07",]=PF["N08","N080306",]*PAE2_5
PF["N080307","N07",]=PF["N08","N080307",]*PAE2_6
# Animal excreta to inland waters
PF["N080301","N03",]=PF["N08","N080301",]*PAE3_1
PF["N080302","N03",]=PF["N08","N080302",]*PAE3_2
PF["N080303","N03",]=PF["N08","N080303",]*PAE3_3
PF["N080304","N03",]=PF["N08","N080304",]*PAE3_4
PF["N080305","N03",]=PF["N08","N080305",]*PAE3_5
PF["N080306","N03",]=PF["N08","N080306",]*PAE3_5
PF["N080307","N03",]=PF["N08","N080307",]*PAE3_6
# Animal excreta to non-arable land
PF["N080301","N02",]=PF["N08","N080301",]*(1-PAE2_1-PAE3_1)
PF["N080302","N02",]=PF["N08","N080302",]*(1-PAE2_2-PAE3_2)
PF["N080303","N02",]=PF["N08","N080303",]*(1-PAE2_3-PAE3_3)
PF["N080304","N02",]=PF["N08","N080304",]*(1-PAE2_4-PAE3_4)
PF["N080305","N02",]=PF["N08","N080305",]*(1-PAE2_5-PAE3_5)
PF["N080306","N02",]=PF["N08","N080306",]*(1-PAE2_5-PAE3_5)
PF["N080307","N02",]=PF["N08","N080307",]*(1-PAE2_6-PAE3_6)
# Pastures
PF["N02","N08",]=PF["N08","N080102",]+PF["N08","N080103",]+PF["N08","N080105",]+PF["N08","N080107",]+PF["N08","N080302",]+PF["N08","N080303",]+PF["N08","N080305",]+PF["N08","N080307",]+PF["N08","N080201",]
# Feeds
PF["N1002","N08",]=PF["N08","N080101",]+PF["N08","N080104",]+PF["N08","N080106",]+PF["N08","N080301",]+PF["N08","N080304",]+PF["N08","N080306",]+PF["N08","N080202",]

##09 ====== Aquaculture (N09) ======
# Feeds for fish
PF["N1002","N090101",]=DFW1*PFF1_1*PFF2_1*DAFW/(DNFW+DAFW)
PF["N1002","N090102",]=DSW1*PSF1_1*PSF2_1*DASW/(DNSW+DASW)
# Artificially cultured fish
PF["N090101","N0901",]=DFW1*PFW1*DAFW/(DNFW+DAFW)
PF["N090102","N0901",]=DSW1*PSW1*DASW/(DNSW+DASW)
# Net exported fish
PF["N0901","N14",]=DAWNE1*PFW1
# Consumed fish
PF["N0901","N1001",]=p(suma(PF[,"N0901",])-suma(PF["N0901",,]))
# Aquatic P losses
PF["N090101","N03",]=p(PF["N1002","N090101",]-PF["N090101","N0901",])
PF["N090102","N04",]=p(PF["N1002","N090102",]-PF["N090102","N0901",])
# Feeds for shrimp/crabs
PF["N1002","N090201",]=DFW2*PFF1_2*PFF2_2*DAFW/(DNFW+DAFW)
PF["N1002","N090202",]=DSW2*PSF1_2*PSF2_2*DASW/(DNSW+DASW)
# Artificially cultured shrimp/crabs
PF["N090201","N0902",]=DFW2*PFW2*DAFW/(DNFW+DAFW)
PF["N090202","N0902",]=DSW2*PSW2*DASW/(DNSW+DASW)
# Net exported shrimp/crabs
PF["N0902","N14",]=DAWNE2*PFW2
# Consumed shrimp/crabs
PF["N0902","N1001",]=p(suma(PF[,"N0902",])-suma(PF["N0902",,]))
# Aquatic P losses
PF["N090201","N03",]=p(PF["N1002","N090201",]-PF["N090201","N0902",])
PF["N090202","N04",]=p(PF["N1002","N090202",]-PF["N090202","N0902",])
# Artificially cultured shellfish
PF["N090301","N0903",]=DFW3*PFW3*DAFW/(DNFW+DAFW)
PF["N090302","N0903",]=DSW3*PSW3*DASW/(DNSW+DASW)
# Net exported shellfish
PF["N0903","N14",]=DAWNE3*PFW3
# Consumed shellfish
PF["N0903","N1001",]=p(suma(PF[,"N0903",])-suma(PF["N0903",,]))
# Artificially cultured other aquatic products
PF["N090401","N0904",]=DFW4*PFW4*DAFW/(DNFW+DAFW)
PF["N090402","N0904",]=DSW4*PSW4*DASW/(DNSW+DASW)+DSW5*PSW5*DASW/(DNSW+DASW)
# Consumed other aquatic products
PF["N0904","N1001",]=p(suma(PF[,"N0904",])-suma(PF["N0904",,]))

##10 ====== Agricultural product processing (N10) ======
##1001 Food processing
# Produced husked/milled rice
PF["N1001","N100101",]=PF["N070101","N1001",]*PRI1
# Produced flour
PF["N1001","N100102",]=PF["N070102","N1001",]*PWH1
# Produced oil
PF["N1001","N100103",]=PF["N070110","N1001",]/PRC10*PPO1_1*PPO3_1+PF["N070112","N1001",]/PRC12*PPO1_4*PPO3_4+PF["N070107","N1001",]/PRC7*PPO1_2*PPO2_2*PPO3_2+PF["N070111","N1001",]/PRC11*PPO1_3*PPO2_3*PPO3_3+PF["N070113","N1001",]/PRC13*PPO1_5*PPO2_5*PPO3_5
# Produced sugar
PF["N1001","N100104",]=DSU*PSU
# Produced meat
PF["N1001","N100105",]=DMT1*(1-PMT2_1)*PMT1_1+DMT2*(1-PMT2_2)*PMT1_2+DMT3*(1-PMT2_3)*PMT1_3+DMT4*(1-PMT2_4)*PMT1_4+DMT5*(1-PMT2_5)*PMT1_5
# Produced rice chaff and bran
PF["N1001","N100106",]=PF["N070101","N1001",]*(1-PRI1)
# Produced wheat middling and bran
PF["N1001","N100107",]=PF["N070102","N1001",]*(1-PWH1)
# Produced oil meal
PF["N1001","N100108",]=p(PF["N070110","N1001",]+PF["N070112","N1001",]+PF["N070107","N1001",]*PPO2_2+PF["N070111","N1001",]*PPO2_3+PF["N070113","N1001",]*PPO2_5-PF["N1001","N100103",])
# Produced bagasse
PF["N1001","N100109",]=p(PF["N070117","N1001",]+PF["N070118","N1001",]-PF["N1001","N100104",])
# Produced animal bones
PF["N1001","N100110",]=p(PF["N080101","N1001",]+PF["N080102","N1001",]+PF["N080103","N1001",]+PF["N080104","N1001",]+PF["N080105","N1001",]+PF["N080106","N1001",]+PF["N080107","N1001",]-PF["N1001","N100105",])
# Directly consumed oil crops
PF["N1001","N100111",]=PF["N070111","N1001",]*(1-PPO2_3)+PF["N070113","N1001",]*(1-PPO2_5)
# Other grains
PF["N1001","N100112",]=PF["N070103","N1001",]+PF["N070104","N1001",]+PF["N070105","N1001",]+PF["N070106","N1001",]+PF["N070107","N1001",]*(1-PPO2_2)+PF["N070108","N1001",]
# Non-food
PF["N1001","N100113",]=PF["N070109","N1001",]+PF["N070116","N1001",]+PF["N070119","N1001",]
# Vegetable
PF["N1001","N100114",]=PF["N070120","N1001",]
# Dairy and egg
PF["N1001","N100115",]=PF["N080201","N1001",]
PF["N1001","N100116",]=PF["N080202","N1001",]
# Aquatic products
PF["N1001","N100117",]=PF["N03","N1001",]+PF["N04","N1001",]+PF["N0901","N1001",]+PF["N0902","N1001",]+PF["N0903","N1001",]+PF["N0904","N1001",]
# Net exported rice
PF["N100101","N14",]=DRINE*PRC1
# Net exported flour
PF["N100102","N14",]=DFLNE1*PRC3+DFLNE2*PRC6
# Net exported oil
PF["N100103","N14",]=DPONE1*PPO1_2+DPONE2*PPO1_3+DPONE3*PPO1_4+DPONE4*PPO1_5
# Net exported sugar
PF["N100104","N14",]=DSUNE*PSU
# Net exported meat
PF["N100105","N14",]=DMTNE1*PMT1_1+DMTNE2*PMT1_2+DMTNE3*PMT1_3+DMTNE4*PMT1_4+DMTNE5*PMT1_5
# Net exported dairy and egg
PF["N100115","N14",]=DDRNE*PDR
PF["N100116","N14",]=DEGNE*PEG
# Produced whole grains
PF["N100101","N100118",]=p(suma(PF[,"N100101",])-suma(PF["N100101",,]))
PF["N100102","N100118",]=p(suma(PF[,"N100102",])-suma(PF["N100102",,]))
PF["N100112","N100118",]=p(suma(PF[,"N100112",])-suma(PF["N100112",,]))
# Rice chaff to arable land
PF["N100106","N07",]=PF["N070101","N1001",]*PRI2
# Rice bran for feed production
PF["N100106","N1002",]=PF["N070101","N1001",]*(1-PRI1-PRI2)
# Wheat middling and bran for feed production
PF["N100107","N1002",]=PF["N1001","N100107",]
# Wastewater generated from oil production
AV4=(PF["N070110","N1001",]/PRC10+PF["N070112","N1001",]/PRC12+PF["N070107","N1001",]/PRC7*PPO2_2+PF["N070111","N1001",]/PRC11*PPO2_3+PF["N070113","N1001",]/PRC15*PPO2_5)*PWWG7
# Wastewater discharged from oil production with no treatment
PF["N100108","N03",]=AV4*(1-PWW)
# Wastewater generated from oil production for further treatment
PF["N100108","N12",]=AV4*PWW
# Oil meal for feed production
PF["N100108","N1002",]=p(PF["N1001","N100108",]-PF["N100108","N03",]-PF["N100108","N12",])
# Recycled bagasse
PF["N100109","N1003",]=PF["N1001","N100109",]
# Animal bones for feed production
PF["N100110","N1002",]=p(PF["N080101","N1001",]*PMT3_1+PF["N080102","N1001",]*PMT3_2+PF["N080103","N1001",]*PMT3_3+PF["N080104","N1001",]*PMT3_4+PF["N080105","N1001",]*PMT3_5+PF["N080106","N1001",]*PMT3_5+PF["N080107","N1001",]*PMT3_5-PF["N1001","N100105",])
# Wastewater generated from meat production
AV5=PF["N080101","N1001",]/PLA1*PWWG8_1+PF["N080102","N1001",]/PLA2*PWWG8_2+PF["N080103","N1001",]/PLA3*PWWG8_3+PF["N080104","N1001",]/PLA4*PWWG8_4+PF["N080105","N1001",]/PLA5*PWWG8_2+PF["N080106","N1001",]/PLA5*PWWG8_2+PF["N080107","N1001",]/PLA9*PWWG8_5
# Wastewater discharged from meat production with no treatment
PF["N100110","N03",]=AV5*(1-PWW)
# Wastewater generated from meat production for further treatment
PF["N100110","N12",]=AV5*PWW
# Animal bones for disposal
PF["N100110","N13",]=PF["N1001","N100110",]-PF["N100110","N1002",]-PF["N100110","N03",]-PF["N100110","N12",]
# Consumed food
PF["N100118","N11",]=(DUHM*PUCP+DRHM*PRCP)*PCP
PF["N100103","N11",]=p(suma(PF[,"N100103",])-suma(PF["N100103",,]))
PF["N100104","N11",]=p(suma(PF[,"N100104",])-suma(PF["N100104",,]))
PF["N100105","N11",]=p(suma(PF[,"N100105",])-suma(PF["N100105",,]))
PF["N100111","N11",]=p(suma(PF[,"N100111",])-suma(PF["N100111",,]))
PF["N100113","N11",]=p(suma(PF[,"N100113",])-suma(PF["N100113",,]))
PF["N100114","N11",]=p(suma(PF[,"N100114",])-suma(PF["N100114",,]))
PF["N100115","N11",]=p(suma(PF[,"N100115",])-suma(PF["N100115",,]))
PF["N100116","N11",]=p(suma(PF[,"N100116",])-suma(PF["N100116",,]))
PF["N100117","N11",]=p(suma(PF[,"N100117",])-suma(PF["N100117",,]))

##1002 Feed processing
# Feed products: DAF1*PAF1+DAF2*PAF2+DAF3*PAF3
# Net exported feeds
PF["N1002","N14",]=DAFNE*PAF1
# Grains for feed production
PF["N100118","N1002",]=p(suma(PF["N1002",,])-suma(PF[,"N1002",]))

##1003 Other applications
# Consumed non-food products
PF["N1003","N11",]=suma(PF[,"N1003",])

##11 ====== Human consumption (N11) ======
##1101 Urban consumption
# Generated human excreta
PF["N11","N110101",]=DUHM*PUHE1
# Reused human excreta
PF["N110101","N07",]=PF["N11","N110101",]*PUHE2
# Human excreta to inland waters
PF["N110101","N03",]=PF["N11","N110101",]*(1-PUHE2)*PUHE3
# Human excreta for further treatment
PF["N110101","N12",]=PF["N11","N110101",]*(1-PUHE2)*(1-PUHE3)
# Generated wastewater
PF["N11","N110102",]=DDW*DUHM/(DUHM+DRHM)*PWWG9*PUWW1*1e-3
# Wastewater discharged from urban consumption with no treatment
PF["N110102","N03",]=PF["N11","N110102",]*(1-PUWW2)
# Wastewater generated from urban consumption for further treatment
PF["N110102","N12",]=PF["N11","N110102",]*PUWW2
# Generated solid wastes
PF["N11","N110103",]=DUSW*PUSW
# Solid wastes for disposal
PF["N110103","N13",]=PF["N11","N110103",]

##1102 Rural consumption
# Generated human excreta
PF["N11","N110201",]=DRHM*PRHE1
# Reused human excreta
PF["N110201","N07",]=PF["N11","N110201",]*PRHE2
# Human excreta to non-arable land
PF["N110201","N02",]=PF["N11","N110201",]*(1-PRHE2)
# Generated wastewater
PF["N11","N110202",]=DDW*DRHM/(DUHM+DRHM)*PWWG9*PRWW*1e-3
# Wastewater discharged from rural consumption with no treatment
PF["N110202","N03",]=PF["N11","N110202",]
# Generated solid wastes
PF["N11","N110203",]=DRHM*PRSW1*PRSW2
# Solid waste to non-arable land
PF["N110203","N02",]=PF["N11","N110203",]
# Stock changes
PF["N11","N15",]=DEDHM*PHM

##12 ====== Wastewater treatment (N12) ======
# Effluents after treating beneficiation wastewater
PF["N1201","N120101",]=PF["N050202","N12",]*PWWD1/PWWG1
PF["N120101","N03",]=PF["N1201","N120101",]
# Sludge after treating beneficiation wastewater
PF["N1202","N120201",]=PF["N050202","N12",]*(1-PWWD1/PWWG1)
PF["N120201","N13",]=PF["N1202","N120201",]
# Effluents after treating wastewater from fertilizer production
PF["N1201","N120102",]=PF["N060102","N12",]*PWWD2/PWWG2
PF["N120102","N03",]=PF["N1201","N120102",]
# Sludge after treating wastewater from fertilizer production
PF["N1202","N120202",]=PF["N060102","N12",]*(1-PWWD2/PWWG2)
PF["N120202","N13",]=PF["N1202","N120202",]
# Effluents after treating wastewater from feed additive production
PF["N1201","N120103",]=PF["N060202","N12",]*PWWD3/(PFAWW*62/142)
PF["N120103","N03",]=PF["N1201","N120103",]
# Sludge after treating wastewater from feed additive production
PF["N1202","N120203",]=PF["N060202","N12",]*(1-PWWD3/(PFAWW*62/142))
PF["N120203","N13",]=PF["N1202","N120203",]
# Effluents after treating wastewater from elemental P production
PF["N1201","N120104",]=PF["N060302","N12",]*PWWD4/PWWG4*POLICY
PF["N120104","N03",]=PF["N1201","N120104",]
# Sludge after treating wastewater from elemental P production
PF["N1201","N120204",]=PF["N060302","N12",]*PWWD4/PWWG4*(1-POLICY)
PF["N1202","N120204",]=PF["N060302","N12",]*(1-PWWD4/PWWG4)
PF["N120204","N13",]=PF["N1202","N120204",]+PF["N1201","N120204",]
# Effluents after treating wastewater from organophosphorus pesticide production
PF["N1201","N120105",]=PF["N060402","N12",]*PWWD5/PWWG5
PF["N120105","N03",]=PF["N1201","N120105",]
# Sludge after treating wastewater from organophosphorus pesticide production
PF["N1202","N120205",]=PF["N060402","N12",]*(1-PWWD5/PWWG5)
PF["N120205","N13",]=PF["N1202","N120205",]
# Effluents after treating wastewater from soap and detergent production
PF["N1201","N120106",]=PF["N060502","N12",]*PWWD6/PWWG6
PF["N120106","N03",]=PF["N1201","N120106",]
# Sludge after treating wastewater from soap and detergent production
PF["N1202","N120206",]=PF["N060502","N12",]*(1-PWWD6/PWWG6)
PF["N120206","N13",]=PF["N1202","N120206",]
# Effluents after treating wastewater from oil production
PF["N1201","N120107",]=PF["N100108","N12",]*PWWD7/PWWG7
PF["N120107","N03",]=PF["N1201","N120107",]
# Sludge after treating wastewater from oil production
PF["N1202","N120207",]=PF["N100108","N12",]*(1-PWWD7/PWWG7)
PF["N120207","N13",]=PF["N1202","N120207",]
# Effluents after treating wastewater from meat production
PF["N1201","N120108",]=(PF["N080101","N1001",]/PLA1*PWWD8_1+PF["N080102","N1001",]/PLA2*PWWD8_2+PF["N080103","N1001",]/PLA3*PWWD8_3+PF["N080104","N1001",]/PLA4*PWWD8_4+PF["N080105","N1001",]/PLA5*PWWD8_2+PF["N080106","N1001",]/PLA5*PWWD8_2+PF["N080107","N1001",]/PLA9*PWWD8_5)*PWW
PF["N120108","N03",]=PF["N1201","N120108",]
# Sludge after treating wastewater from meat production
PF["N1202","N120208",]=p(PF["N100110","N12",]-PF["N1201","N120108",])
PF["N120208","N13",]=PF["N1202","N120208",]
# Effluents after treating urban municipal wastewater
PF["N1201","N120109",]=(PF["N110101","N12",]+PF["N110102","N12",])*(1-PWWT1)
PF["N120109","N03",]=PF["N1201","N120109",]
# Sludge after treating urban municipal wastewater
PF["N1202","N120209",]=(PF["N110101","N12",]+PF["N110102","N12",])*PWWT1
PF["N120209","N07",]=PF["N1202","N120209",]*PWWT2
PF["N120209","N13",]=PF["N1202","N120209",]*(1-PWWT2)
# Aggregation
PF["N12","N1201",]=suma(PF["N1201",,])
PF["N12","N1202",]=suma(PF["N1202",,])
# Riverine transfer (for N03)
PF["N03","N04",]=suma(PF[,"N03",])*PRT

##13 ====== Solid waste disposal (N13) ======
# Solid waste for disposal
AV6=suma(PF[,"N13",])
# Solid wastes for landfill
PF["N13","N1301",]=AV6*PSWD1*PSWD2
PF["N1301","N02",]=PF["N13","N1301",]
# Solid wastes for incineration
PF["N13","N1302",]=AV6*PSWD1*PSWD3
PF["N1302","N02",]=PF["N13","N1302",]
# Solid wastes for compost
PF["N13","N1303",]=AV6*PSWD1*(1-PSWD2-PSWD3)
PF["N1303","N07",]=PF["N13","N1303",]
# Solid wastes for stockpile
PF["N13","N02",]=AV6*(1-PSWD1)

##15 ====== Stock change (N15) ======
# Excluding interannual stock changes of human and animals
PF["N01","N15",]=suma(PF[,"N01",])-suma(PF["N01",,])
PF["N02","N15",]=suma(PF[,"N02",])-suma(PF["N02",,])
PF["N03","N15",]=suma(PF[,"N03",])-suma(PF["N03",,])
PF["N04","N15",]=suma(PF[,"N04",])-suma(PF["N04",,])
PF["N07","N15",]=suma(PF[,"N07",])-suma(PF["N07",,])

##16 ====== The balancing node (N16) ======
PF[,"N16",]=suma(PF,c(2,3))-suma(PF,c(1,3))

