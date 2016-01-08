######################### fig2a_2e.r #########################

fig=function(PF){
## ====== FIGURE 2A ======
# FIG2A_01 Wind erosion 
FIG2A_01=PF["N02","N01",]+PF["N07","N01",]
# FIG2A_02 Atmospheric deposition 
FIG2A_02=PF["N01","N02",]+PF["N01","N03",]+PF["N01","N04",]+PF["N01","N07",]
# FIG2A_03 Sea spray
FIG2A_03=PF["N04","N01",]
# FIG2A_04 Crop P
FIG2A_04=PF["N07","N070101",]+PF["N07","N070102",]+PF["N07","N070103",]+PF["N07","N070104",]+PF["N07","N070105",]+PF["N07","N070106",]+PF["N07","N070107",]+PF["N07","N070108",]+PF["N07","N070109",]+PF["N07","N070110",]+PF["N07","N070111",]+PF["N07","N070112",]+PF["N07","N070113",]+PF["N07","N070116",]+PF["N07","N070117",]+PF["N07","N070118",]+PF["N07","N070119",]+PF["N07","N070120",]
# FIG2A_05 Animal P
FIG2A_05=PF["N03","N1001",]+PF["N04","N1001",]+PF["N0901","N1001",]+PF["N0901","N14",]+PF["N0902","N1001",]+PF["N0902","N14",]+PF["N0903","N1001",]+PF["N0903","N14",]+PF["N0904","N1001",]+PF["N08","N080101",]-PF["N080101","N15",]+PF["N08","N080102",]-PF["N080102","N15",]+PF["N08","N080103",]-PF["N080103","N15",]+PF["N08","N080104",]-PF["N080104","N15",]+PF["N08","N080105",]-PF["N080105","N15",]+PF["N08","N080106",]-PF["N080106","N15",]+PF["N08","N080107",]-PF["N080107","N15",]+PF["N080202","N1001",]+PF["N080201","N1001",]
# FIG2A_06 P runoff
FIG2A_06=PF["N02","N03",]+PF["N050202","N03",]+PF["N060102","N03",]+PF["N060202","N03",]+PF["N060302","N03",]+PF["N060402","N03",]+PF["N060502","N03",]+PF["N0704","N03",]+PF["N0705","N03",]+PF["N080301","N03",]+PF["N080302","N03",]+PF["N080303","N03",]+PF["N080304","N03",]+PF["N080305","N03",]+PF["N080306","N03",]+PF["N080307","N03",]+PF["N090101","N03",]+PF["N090201","N03",]+PF["N100108","N03",]+PF["N100110","N03",]+PF["N110101","N03",]+PF["N110102","N03",]+PF["N110202","N03",]+PF["N120109","N03",]+PF["N120101","N03",]+PF["N120102","N03",]+PF["N120103","N03",]+PF["N120104","N03",]+PF["N120105","N03",]+PF["N120106","N03",]+PF["N120107","N03",]+PF["N120108","N03",]
# FIG2A_07 Riverine transfer
FIG2A_07=PF["N03","N04",]

## ====== FIGURE 2E ======
# FIG2E_01 Input to arable land
FIG2E_01=PF["N01","N07",]+PF["N02","N07",]+PF["N060101","N07",]+PF["N060401","N07",]+PF["N080301","N07",]+PF["N080302","N07",]+PF["N080303","N07",]+PF["N080304","N07",]+PF["N080305","N07",]+PF["N080306","N07",]+PF["N080307","N07",]+PF["N100106","N07",]+PF["N110101","N07",]+PF["N110201","N07",]+PF["N120209","N07",]+PF["N1303","N07",]
# FIG2E_02 Output from arable land
FIG2E_02=PF["N07","N01",]+PF["N070101","N1001",]+PF["N070102","N1001",]+PF["N070103","N1001",]+PF["N070104","N1001",]+PF["N070105","N1001",]+PF["N070106","N1001",]+PF["N070107","N1001",]+PF["N070108","N1001",]+PF["N070109","N1001",]+PF["N070110","N1001",]+PF["N070111","N1001",]+PF["N070112","N1001",]+PF["N070113","N1001",]+PF["N070116","N1001",]+PF["N070117","N1001",]+PF["N070118","N1001",]+PF["N070119","N1001",]+PF["N070120","N1001",]+PF["N0703","N1002",]+PF["N070101","N14",]+PF["N070102","N14",]+PF["N070103","N14",]+PF["N070105","N14",]+PF["N070106","N14",]+PF["N070107","N14",]+PF["N070108","N14",]+PF["N070109","N14",]+PF["N070111","N14",]+PF["N070112","N14",]+PF["N070113","N14",]+PF["N070116","N14",]+PF["N070117","N14",]+PF["N070118","N14",]+PF["N070119","N14",]+PF["N070120","N14",]+PF["N0703","N02",]+PF["N0704","N03",]+PF["N0705","N03",]+PF["N0703","N11",]
# FIG2E_03 Input to non-arable land
FIG2E_03=PF["N050102","N02",]+PF["N050203","N02",]+PF["N060103","N02",]+PF["N060203","N02",]+PF["N060304","N02",]+PF["N0703","N02",]+PF["N080301","N02",]+PF["N080302","N02",]+PF["N080303","N02",]+PF["N080304","N02",]+PF["N080305","N02",]+PF["N080306","N02",]+PF["N080307","N02",]+PF["N110201","N02",]+PF["N110203","N02",]+PF["N13","N02",]+PF["N1301","N02",]+PF["N1302","N02",]

# Return value
sapply(ls()[-which(ls()=="PF")],function(i)get(i))
}
