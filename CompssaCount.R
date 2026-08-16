source("~/Desktop/CompssaElection/Setup.R")
source("~/Desktop/CompssaElection/Functions.R")

address <- "~/Desktop/CompssaElection/"

VoterReg <- read_excel(paste0(address, "UpdatedList/NewVoterRegister.xlsx"),
                       col_types = c("Matric No" = "text" )) %>%
  clean_names() 

ElectVotes <- read_excel(paste0(address, "ElectionResponseTest.xlsx"),
                         col_types = c("Matric No" = "text" )) %>%
  clean_names() %>%
  select(-timestamp) %>%
  rename(electpassword = passkey)

## Adds a valid column to confirm match of election day password with registration password
## Add features to account for case and spacings and number passwords that add 0 to the front
VerifiedVotes <- ElectVotes %>%
  left_join(VoterReg, "matric_no") %>%
  mutate(valid = electpassword == password) 

## Selects only votes that were marked has valid
ValidVotes <- VerifiedVotes %>%
  filter(valid == TRUE)

## Selects only votes that were marked has invalid or not registered (NA)
InvalidVotes <- VerifiedVotes %>%
  filter(valid == FALSE | is.na(valid))

## Gets the matric numbers that voted more than once and stores them in the variable below
DuplicateVotes <- ValidVotes %>%
  group_by(matric_no) %>%
  count() %>%
  filter(n >= 2)

## Removes the matric numbers that voted more than once making them invalid
VerifiedVotes_D <- ValidVotes %>%
  filter(!(matric_no %in% DuplicateVotes$matric_no))


President <- as.data.frame(VerifiedVotes_D$president)
VicePresident <- as.data.frame(VerifiedVotes_D$vice_president)
GeneralSecretary <- as.data.frame(VerifiedVotes_D$general_secretary)
AstGeneralSecretary <- as.data.frame(VerifiedVotes_D$assistant_general_secretary)
PublicRelation <- as.data.frame(VerifiedVotes_D$public_relations_officer)
FinancialSecretary <- as.data.frame(VerifiedVotes_D$financial_secretary)
Treasurer <- as.data.frame(VerifiedVotes_D$treasurer)
WelfareSecretary <- as.data.frame(VerifiedVotes_D$welfare_secretary) 
DirectorSports <- as.data.frame(VerifiedVotes_D$sport_secretary)
DirectorSocials <- as.data.frame(VerifiedVotes_D$social_secretary)
HostelRepI <- as.data.frame(VerifiedVotes_D$hostel_rep_i)
HostelRepII <- as.data.frame(VerifiedVotes_D$hostel_rep_ii)

colnames(President) <- "candidate"
colnames(VicePresident) <- "candidate"
colnames(GeneralSecretary) <- "candidate"
colnames(AstGeneralSecretary) <- "candidate"
colnames(PublicRelation) <- "candidate"
colnames(FinancialSecretary) <- "candidate"
colnames(Treasurer) <- "candidate"
colnames(WelfareSecretary) <- "candidate"
colnames(DirectorSocials) <- "candidate"
colnames(DirectorSports) <- "candidate"
colnames(HostelRepI) <- "candidate"
colnames(HostelRepII) <- "candidate"

PieChart(President, "Official Presidential Election Results")
nrow(VerifiedVotes_D)


## Votes by Department
VotesbyDepartment <- VerifiedVotes_D %>%
  count(department, name="Votes") %>%
  arrange()

TotalVotersBarChartByDepartment(VerifiedVotes_D)

## Total Voters and Registered voters remaining 
TotalVotersChart(VerifiedVotes_D, VoterReg)

## Checking Election Progress
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "BDS")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "MBBS")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "MLS")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Nursing")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Physiotherapy")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Physiology")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Pharmacology")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Pharmacy")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Radiography")
TotalVotersPieChartByDepartment(VerifiedVotes_D, VoterReg, "Anatomy")


## Vertical Chart
VerticalChart(President, "Official Presidential Election Results")
VerticalChart(VicePresident, "Official Vice Presidential Election Results")
VerticalChart(GeneralSecretary, "Official General Secretary Election Results")
VerticalChart(AstGeneralSecretary, "Official Assistant General Secretary Election Results")
VerticalChart(PublicRelation, "Official Public Relations Officer Election Results")
VerticalChart(FinancialSecretary, "Official Financial Secretary Election Results")
VerticalChart(Treasurer, "Official Treasurer Election Results")
VerticalChart(WelfareSecretary, "Official Welfare Secretary Election Results")
VerticalChart(DirectorSocials, "Official Director of Socials Election Results")
VerticalChart(DirectorSports, "Official Director of Sports Election Results")
VerticalChart(HostelRepI, "Official Hostel Rep I Election Results")
VerticalChart(HostelRepII, "Official Hostel Rep II Election Results")

rm(
  President,
  VicePresident,
  GeneralSecretary,
  AstGeneralSecretary,
  PublicRelation,
  FinancialSecretary,
  Treasurer,
  WelfareSecretary,
  DirectorSocials,
  DirectorSports,
  HostelRepI,
  HostelRepII
)

rm(
  ValidVotes,
  VerifiedVotes
)

write.xlsx(InvalidVotes, paste0(address, "results/InvalidVotes.xlsx"))
write.xlsx(VotesbyDepartment, paste0(address, "results/VotesByDepartment.xlsx"))
write.xlsx(VerifiedVotes_D, paste0(address, "results/InvalidVotes.xlsx"))
