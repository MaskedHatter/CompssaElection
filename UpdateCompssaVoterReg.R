address <- "~/Desktop/CompssaElection/"

PreviousVoterReg <- read_excel(paste0(address, "RegisteredVotersModified.xlsx"),
                               col_types = c("Matric No" = "text" )) %>%
  clean_names() 

CurrentList <- read_excel(paste0(address, "CompssaStudents/CompssaList.xlsx"),
                          col_types = c("Matric No" = "text" )) %>%
  clean_names()

DuplicateReg <- PreviousVoterReg %>%
  group_by(matric_no) %>%
  count() %>%
  filter(n >= 2)

PreviousVoterReg <- PreviousVoterReg %>%
  arrange(desc(timestamp)) %>%
  distinct(matric_no, .keep_all = TRUE) %>%
  select(-level, -name, -department)

NewVoterReg <- PreviousVoterReg %>%
  left_join(CurrentList, "matric_no") %>%
  filter(!is.na(level))

UnregisteredCandidates <- CurrentList %>%
  filter(!(matric_no %in% NewVoterReg$matric_no))

write.xlsx(NewVoterReg, paste0(address, "UpdatedList/NewVoterRegister.xlsx"))
write.xlsx(UnregisteredCandidates, paste0(address, "UpdatedList/UnregisteredCandidates.xlsx"))

NewVoterReg <- NewVoterReg %>%
  select(-password)

write.xlsx(NewVoterReg %>% filter(department == "MBBS") %>% arrange(level), paste0(address, "UpdatedList/MBBSVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "BDS") %>% arrange(level), paste0(address, "UpdatedList/BDSVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "MLS") %>% arrange(level), paste0(address, "UpdatedList/MLSVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Physiotherapy") %>% arrange(level), paste0(address, "UpdatedList/PhysiotherapyVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Physiology") %>% arrange(level), paste0(address, "UpdatedList/PhysiologyVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Pharmacology") %>% arrange(level), paste0(address, "UpdatedList/PharmacologyVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Pharmacy") %>% arrange(level), paste0(address, "UpdatedList/PharmacyVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Nursing") %>% arrange(level), paste0(address, "UpdatedList/NursingVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Radiography") %>% arrange(level), paste0(address, "UpdatedList/RadiographyVoterRegister.xlsx"))
write.xlsx(NewVoterReg %>% filter(department == "Anatomy") %>% arrange(level), paste0(address, "UpdatedList/AnatomyVoterRegister.xlsx"))
