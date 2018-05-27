library(dplyr)

col_grab <- select(PUF2015_102016, "AD_MDEA9", "adsmmdea", "adtmtnow" ,"adrxnow", "YO_MDEA9",
                   "yodsmmde", "yorxnow", "yotmtnow", "sexident", "irsex", "CATAG3", "irwrkstat", "NEWRACE2",
                   "IRPINC3", "eduhighcat", "PDEN10", "COUTYP2", "POVERTY3")
write.csv(col_grab, "Q4_comparision.csv")
