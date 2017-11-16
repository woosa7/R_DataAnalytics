library(dplyr)

cust = read.csv('Data_set.csv', encoding = 'utf-8')

cust[cust$OCCP_NAME_G == "*", "OCCP_NAME_G"] = "기타"
cust[cust$OCCP_NAME_G == "NULL", "OCCP_NAME_G"] = "기타"
cust[cust$MATE_OCCP_NAME_G == "*", "MATE_OCCP_NAME_G"] = "기타"
cust[cust$MATE_OCCP_NAME_G == "NULL", "MATE_OCCP_NAME_G"] = "기타"
cust[cust$LAST_CHLD_AGE == "NULL", "LAST_CHLD_AGE"] <- 0
cust[cust$AGE == "*", "AGE"] <- 45

summary(cust)

# HIGH_AMT_RATE = 대출총금액(신용대출)+대출총금액(은행) / 대출총금액
# LOW_AMT_RATE = 대출총금액(카드사/할부사/캐피탈) / 대출총금액
# LNIF_CNT = 전체 대출 건수
# DTI : 대출금액/추정소득

cust_v1 <- cust %>%
    mutate(HIGH_AMT_RATE = (cust$TOT_CLIF_AMT + cust$BNK_LNIF_AMT) / cust$TOT_LNIF_AMT) %>%
    mutate(LOW_AMT_RATE = cust$CPT_LNIF_AMT / cust$TOT_LNIF_AMT) %>%
    mutate(LNIF_CNT = cust$BNK_LNIF_CNT + cust$CPT_LNIF_CNT + cust$SPART_LNIF_CNT + cust$ECT_LNIF_CNT) %>%
    mutate(CUST_DTI = ifelse(cust$CUST_JOB_INCM > 0, cust$TOT_LNIF_AMT / cust$CUST_JOB_INCM, 0)) %>%
    select(CUST_ID, HIGH_AMT_RATE, LOW_AMT_RATE, LNIF_CNT, CUST_DTI)

summary(cust_v1)

# L_H_RATE = 카드캐피탈대출금액 / 신용,은행대출총금액 (신용.은행대출=0 제외)
# CPT_LNIF_BIG = 카드캐피탈대출 금액이 신용+은행대출 금액보다 많은지 여부
# CPT_LNIF_RATIO = 카드캐피탈대출 금액의 총대출금액 대비 비율

cust_v2 <- cust %>%
    mutate(L_H_RATE = ifelse( (cust$TOT_CLIF_AMT + cust$BNK_LNIF_AMT) > 0, 
                              cust$CPT_LNIF_CNT / (cust$TOT_CLIF_AMT + cust$BNK_LNIF_AMT), 
                              0)) %>% 
    mutate(CPT_LNIF_BIG = factor(ifelse(cust$CPT_LNIF_CNT > (cust$TOT_CLIF_AMT + cust$BNK_LNIF_AMT), 1, 0))) %>% 
    mutate(CPT_LNIF_RATIO = ifelse( cust$CPT_LNIF_CNT > 0 & cust$TOT_LNIF_AMT > 0, 
                                    cust$CPT_LNIF_CNT / cust$TOT_LNIF_AMT, 
                                    0)) %>%
    select(CUST_ID, L_H_RATE, CPT_LNIF_BIG, CPT_LNIF_RATIO)

summary(cust_v2)

# CRDT_CARD = 신용카드 총 건수 * 신용카드 유지기간 
# CRDT_GRAD_DIFF = 최근신용등급 – 최초신용등급
# TOTAL_DELAY_RATE = 최근1년신용대출연체율+보험료연체율+최근1년약대연체율

cust_v3 <- cust %>%
    mutate(CRDT_CARD = cust$CRDT_CARD_CNT*cust$CTCD_OCCR_MDIF) %>%
    mutate(CRDT_GRAD_DIFF = cust$STRT_CRDT_GRAD - cust$LTST_CRDT_GRAD) %>%
    mutate(LT1Y_DELAY_RATE = cust$LT1Y_CLOD_RATE + cust$PREM_OVDU_RATE + cust$LT1Y_SLOD_RATE) %>%
    select(CUST_ID, CRDT_CARD, CRDT_GRAD_DIFF, LT1Y_DELAY_RATE)

summary(cust_v3)
str(cust_v3)

# TEL_OVDU_RATE = 당월연체금액/납부요금 (납부요금이 0인사람 제외)
# OVDU_HIGH_RATE = 연간최대연체금액/납부요금 (납부요금이 0인사람 제외)
# FAIL_COUNT = 자동이체실패월수(보험료) + 납부미준수횟수(통신료) + 정지일수

cust_v4 <- cust %>%
    mutate(TEL_OVDU_RATE = ifelse(cust$MON_TLFE_AMT > 0, 
                                  cust$CRMM_OVDU_AMT / cust$MON_TLFE_AMT, 0)) %>%
    mutate(OVDU_HIGH_RATE = ifelse(cust$MON_TLFE_AMT > 0, 
                                   cust$LT1Y_MXOD_AMT / cust$MON_TLFE_AMT, 0)) %>%
    mutate(FAIL_COUNT = cust$AUTR_FAIL_MCNT + cust$TLFE_UNPD_CNT + cust$NUM_DAY_SUSP) %>%
    select(CUST_ID, TEL_OVDU_RATE, OVDU_HIGH_RATE, FAIL_COUNT)

summary(cust_v4)
str(cust_v4)


# TOT_LOAN_CNT = -0.4*대출 건수(은행) -0.88*대출 건수(카드/할부/캐피탈) + 
#     1.29*대출 건수(2산업분류) -0.8*대출 건수(기타)
# 
# lg <- glm(custsig$TARGET~custsig$BNK_LNIF_CNT+custsig$CPT_LNIF_CNT
#           +custsig$SPART_LNIF_CNT+custsig$ECT_LNIF_CNT, data=custsig, family = binomial())
# summary(lg)

cust_v5 <- cust %>%
    mutate(TOT_LOAN_CNT = -0.4*cust$BNK_LNIF_CNT-0.88*cust$CPT_LNIF_CNT+1.29*cust$SPART_LNIF_CNT-0.8*cust$ECT_LNIF_CNT) %>%
    select(CUST_ID, TOT_LOAN_CNT)

summary(cust_v5)
str(cust_v5)



# join data

custsig <- cust %>%
    left_join(cust_v1) %>%
    left_join(cust_v2) %>%
    left_join(cust_v3) %>%
    left_join(cust_v4) %>%
    left_join(cust_v5)

head(custsig)

write.csv(custsig, file="cust_data.csv", row.names=FALSE)







