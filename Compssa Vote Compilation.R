showtext_auto()
font_add_google("Bangers", "bangers")

address <- "C:\\Users\\aokuy\\Desktop\\compssa\\"

#Importing CSV downloaded from google forms COMPSSA EXECUTVE ELECTIONS 2025 (Responses) - Form responses 1.csv
#Reg <- read_excel("C:\\Users\\Admin\\Desktop\\compssa\\RegisteredVoters.xlsx")
UncleanRegisteredVoters <- read.csv(paste0(address, "resources\\RegisteredVoters.csv"))
VotesCast <- read.csv(paste0(address,"Place CSV file here\\COMPSSA EXECUTVE ELECTIONS 2025 (Responses) - Form responses 1.csv"))
#UncleanRegisteredVoters$Matriculation.Number <- trimws(UncleanRegisteredVoters$Password)
UncleanRegisteredVoters$Password <- trimws(UncleanRegisteredVoters$Password)
UncleanRegisteredVoters$Password <- tolower(UncleanRegisteredVoters$Password)
UncleanRegisteredVoters$Password <- as.character(UncleanRegisteredVoters$Password)
VotesCast$Voting.Passkey <- trimws(VotesCast$Voting.Passkey) 
VotesCast$Voting.Passkey <- tolower(VotesCast$Voting.Passkey)
VotesCast$Voting.Passkey <- as.character(VotesCast$Voting.Passkey)
#unique(VotesCast$Matriculation.number)

#Converting Matriculation Number variable type from character to integer
#Matriculation No Column in Registered Voters CSV should be written as "Matriculation Number" 
UncleanRegisteredVoters$Matriculation.Number <- as.integer(UncleanRegisteredVoters$Matriculation.Number)
UncleanRegisteredVoters$Timestamp <- as.integer(UncleanRegisteredVoters$Timestamp)
VotesCast$Matriculation.Number <- as.integer(VotesCast$Matriculation.Number)

RegisteredVoters <- UncleanRegisteredVoters %>%
  group_by(Matriculation.Number) %>%
  arrange(Timestamp, .by_group = TRUE) %>%
  #filter(n() == 1) %>%
  slice_tail(n = 1) %>%
  ungroup()
rm(UncleanRegisteredVoters)
write.csv(RegisteredVoters, paste0(address,"storage\\registered.csv"))

# Total Number of Registered Voters Per Department Per Level
TotalRegVotesByDepartment_Level <- RegisteredVoters %>%
  group_by(Department, Level) %>%
  count()
write.csv(TotalRegVotesByDepartment_Level, paste0(address,"storage\\Total Registered Voters Dept.csv"))
rm(TotalRegVotesByDepartment_Level)

# Total Number of Votes Cast Per Department Per Level
TotalVoteCastByDepartment_Level <- VotesCast %>%
  group_by(Department, Level) %>%
  count()
write.csv(TotalVoteCastByDepartment_Level, paste0(address,"storage\\Total Current Voters Dept.csv"))
rm(TotalVoteCastByDepartment_Level)

#Selecting only useful data columns 
RegisteredVoters <-  select(RegisteredVoters, Matriculation.Number, Password)

# A list showing the Validation Status for All Votes Cast
VoteCastValidStatus <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  select(-Level, -Department,)

write.csv(VoteCastValidStatus, paste0(address,"storage\\ValidStatus.csv"))


# Validating votes with matching passwords, and removing invalid votes
Valid_votes <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  select(-Level, -Department, -Voting.Passkey, -Password)

write.csv(Valid_votes, paste0(address,"storage\\Validvote.csv"))
  
#Removes Duplicates on the validated votes
VotesCastWithoutDuplicates <- Valid_votes %>%
  group_by(Matriculation.Number) %>%
  filter(n() > 1) %>%
  #slice_tail(n = 1) %>%
  ungroup()

#Invalid Votes by Department 
Invalid_votes <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == FALSE) %>%
  select( -Level, -Department, -Voting.Passkey, -Password)

write.csv(Invalid_votes, paste0(address,"storage\\Total INValid Voters Dept.csv"))

write.csv(Valid_votes, paste0(address,"storage\\Total Valid Voters Dept.csv"))


UnregisteredVotes <-  VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(is.na(Valids)) %>%
  select( -Level, -Department, -Voting.Passkey, -Password)

RegisteredVoters1#Vote Summary
VoteSummary <- data.frame(
  Stats = c("Non Duplicate Registered Votes", "Votes Cast", "Valid Votes", "Invalid Votes", "Unregistered Votes"),
  Values = c(nrow(RegisteredVoters), nrow(VotesCast), nrow(Valid_votes), nrow(Invalid_votes), nrow(UnregisteredVotes))
)

write.csv(VoteSummary, paste0(address,"storage\\Summary.csv"))

InvalidVotesByDepartment <- Invalid_votes %>%
  group_by(Department) %>%
  count()
  
write.csv(InvalidVotesByDepartment, paste0(address,"storage\\InvalidVotesByDepartment.csv"))
# Count NAs in the data frame
# NAs = Unregistered Votes
# False = Void Votes

# Counting votes per level
Votes_per_level <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Department, Level)%>%
  count()




# Invalid Votes Number
Invalid_Votes_Value <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == FALSE) %>%
  count()

# Counting votes per department and adding invalid Votes
Votes_per_dept <- VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Department)%>%
  count() %>%
  ungroup() %>%
  add_row(Department = "Invalid Votes", n = Invalid_Votes_Value$n)




# Counting total votes for all positions and candidates
Vote_Count <- lapply(Valid_votes, table)


# length(Vote_Count[["PRESIDENT."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["VICE.PRESIDENT."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["GENERAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["ASSISTANT.GENERAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["The.Financial.Secretary."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["Treasurer."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["The.Director.of.Socials."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["The.Director.of.Sport."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["PRO"]]) <- length(Vote_Count[["PRESIDENT."]])


President <- as.data.frame(Vote_Count$The.President)
VP <- as.data.frame(Vote_Count$The.Vice.President)
GenSec <- as.data.frame(Vote_Count$The.Secretary.General)
AGS <- as.data.frame(Vote_Count$The.Assistant.Secretary.General)
Fin_Sec <- as.data.frame(Vote_Count$The.Financial.Secretary)
Treasurer <- as.data.frame(Vote_Count$Treasurer)
Social_Sec <- as.data.frame(Vote_Count$The.Director.of.Socials)
Sports_Sec <-as.data.frame(Vote_Count$The.Director.of.Sport)
PRO <- as.data.frame(Vote_Count$Public.Relations.Officer..PRO.)
Welfare_sec <- as.data.frame(Vote_Count$The.Welfare.Secretary)
Hostel_Rep <- as.data.frame(Vote_Count$Hostel.Representative.1)


# Export Total Vote COunt as csv
capture.output(Vote_Count, file = "Vote Count.csv")   



# Exporting data frames to working directory
write.csv(Votes_per_level,paste0(address, "storage\\Votes Per Level.csv", row.names = FALSE))
write.csv(Votes_per_dept,paste0(address, "storage\\Votes Per Department.csv", row.names = FALSE))

#### Visualizations
#Vote Summary
Votes_per_dept %>%
  ggplot(aes(Department, n, fill = Department)) +
  geom_col(width =  0.5) +
  geom_text(aes(label = n), 
            vjust = -0.3, size = 4, fontface = "bold") +
  xlab("\nDepartment") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Votes per department.png"))

#Total Gender
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender)%>%
  count() %>%
  ggplot(aes(Gender, n, fill = Gender)) +
  geom_col(width =  0.5) +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  xlab("\nGender") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Gender") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Votes per gender.png"))


VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level)%>%
  count() %>%
  ggplot(aes(Level, n, fill = Level)) +
  geom_col(width =  0.5) +
  xlab("\nLevel") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Votes per level 2.png"))





## Visualization of Vote counts for The President
President %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nPresidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Presidential Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\President.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.President)%>%
  count() %>%
  ggplot(aes(The.President, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPresidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Presidential Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\President Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.President)%>%
  count() %>%
  ggplot(aes(The.President, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPresidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Presidential Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\President Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.President)%>%
  count() %>%
  ggplot(aes(The.President, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPresidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Presidential Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\President Department.png"))




## Visualization of Vote counts for the vice president
VP %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nVice Presidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Vice Presidential Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Vice President.png"))



# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Vice.President)%>%
  count() %>%
  ggplot(aes(The.Vice.President, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nVice Presidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Vice Presidential Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Vice President Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Vice.President)%>%
  count() %>%
  ggplot(aes(The.Vice.President, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nVice Presidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Vice Presidential Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Vice President Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Vice.President)%>%
  count() %>%
  ggplot(aes(The.Vice.President, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nVice Presidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Vice Presidential Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Vice President Department.png"))



# Visualization of Vote counts for The Secretary General
GenSec %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nSecretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Secretary General Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Secretary General.png"))



# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Secretary.General, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nSecretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Secretary General Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Secretary General Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Secretary.General, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nSecretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Secretary General Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Secretary General Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Secretary.General, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nSecretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Secretary General Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Secretary General Department.png"))



# Visualization of Vote counts for the Assistant Secretary General
AGS %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nAssistant Secretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Assistant Secretary General Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Assistant Secretary General.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Assistant.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Assistant.Secretary.General, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nAssistant Secretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Assistant Secretary General Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Assistant Secretary General Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Assistant.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Assistant.Secretary.General, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nAssistant Secretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Assistant Secretary General Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Assistant Secretary General Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Assistant.Secretary.General)%>%
  count() %>%
  ggplot(aes(The.Assistant.Secretary.General, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nAssistant Secretary General Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Assistant Secretary General Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Assistant Secretary General Department.png"))






# Visualization of Vote counts for the Director of Sports
Sports_Sec %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nDirector of Sports Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Director of Sports Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Sports.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Director.of.Sport)%>%
  count() %>%
  ggplot(aes(The.Director.of.Sport, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Sports Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Sports Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Sports Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Director.of.Sport)%>%
  count() %>%
  ggplot(aes(The.Director.of.Sport, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Sports Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Sports Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Sports Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Director.of.Sport)%>%
  count() %>%
  ggplot(aes(The.Director.of.Sport, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Sports Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Sports Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Sports Department.png"))


# Visualization of Vote counts for the Director of Socials
Social_Sec %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4) +
  xlab("\nDirector of Socials Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Director of Socials Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Socials.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Director.of.Socials)%>%
  count() %>%
  ggplot(aes(The.Director.of.Socials, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Socials Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Socials Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Socials Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Director.of.Socials)%>%
  count() %>%
  ggplot(aes(The.Director.of.Socials, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Socials Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Socials Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Socials Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Director.of.Socials)%>%
  count() %>%
  ggplot(aes(The.Director.of.Socials, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nDirector of Socials Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Director of Socials Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Director of Socials Department.png"))





# Visualization of Vote counts for the Public Relations Officer
PRO %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2) +
  xlab("\nPublic Relations Officer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Public Relations Officer Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Public Relations Officer.png"))

# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, Public.Relations.Officer..PRO.)%>%
  count() %>%
  ggplot(aes(Public.Relations.Officer..PRO., n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPublic Relations Officer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Public Relations Officer Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Public Relations Officer Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, Public.Relations.Officer..PRO.)%>%
  count() %>%
  ggplot(aes(Public.Relations.Officer..PRO., n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPublic Relations Officer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Public Relations Officer Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Public Relations Officer Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, Public.Relations.Officer..PRO.)%>%
  count() %>%
  ggplot(aes(Public.Relations.Officer..PRO., n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nPublic Relations Officer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Public Relations Officer Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Public Relations Officer.png"))








# Visualization of Vote counts for the Treasurer
Treasurer %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2.5) +
  scale_fill_manual(
    values = rev(c(
    "#F87660", "#00BFC4"  # replace with your colors
  )),
  guide = guide_legend(
    direction = "horizontal",   # make legend horizontal
    keywidth = unit(2, "cm"),   # width of each legend box
    keyheight = unit(1, "cm"),  # height of each legend box
    title.position = "top",
    byrow = TRUE
  )
  ) +
  xlab("\nTreasurer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for the Treasurer Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"),
        )
ggsave(paste0(address, "results\\Treasurer.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, Treasurer)%>%
  count() %>%
  ggplot(aes(Treasurer, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nTreasurer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Treasurer Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Treasurer Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, Treasurer)%>%
  count() %>%
  ggplot(aes(Treasurer, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nTreasurer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Treasurer Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Treasurer Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, Treasurer)%>%
  count() %>%
  ggplot(aes(Treasurer, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nTreasurer Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Treasurer Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Treasurer Department.png"))







# Visualization of Vote counts for the Financial Secretary
Fin_Sec %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2.5) +
  xlab("\nFinancial Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Financial Secretary Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Financial Secretary.png"))

# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Financial.Secretary)%>%
  count() %>%
  ggplot(aes(The.Financial.Secretary, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nFinancial Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Financial Secretary Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Financial Secretary Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Financial.Secretary)%>%
  count() %>%
  ggplot(aes(The.Financial.Secretary, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nFinancial Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Financial Secretary Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Financial Secretary Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Financial.Secretary)%>%
  count() %>%
  ggplot(aes(The.Financial.Secretary, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nFinancial Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Financial Secretary Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Financial Secretary Department.png"))



# Visualization of Vote counts for the Welfare Secretary
Welfare_sec %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2.5) +
  xlab("\nWelfare Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Welfare Secretary Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Welfare Secretary.png"))

# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, The.Welfare.Secretary)%>%
  count() %>%
  ggplot(aes(The.Welfare.Secretary, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nWelfare Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Welfare Secretary Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Welfare Secretary Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, The.Welfare.Secretary)%>%
  count() %>%
  ggplot(aes(The.Welfare.Secretary, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nWelfare Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Welfare Secretary Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Welfare Secretary Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, The.Welfare.Secretary)%>%
  count() %>%
  ggplot(aes(The.Welfare.Secretary, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nWelfare Secretary Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Welfare Secretary Votes by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Welfare Secretary Department.png"))


Hostel_Rep %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2) +
  xlab("\nHostel Representative Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Hostel Representative Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Hostel Representave.png"))


# Visualization by gender statistics 
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender, Hostel.Representative.1)%>%
  count() %>%
  ggplot(aes(Hostel.Representative.1, n)) +
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nHostel Representative Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Hostel Representative Votes by Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Hostel Representative Gender.png"))


# Visualization by Class statistics 6 bar charts
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level, Hostel.Representative.1)%>%
  count() %>%
  ggplot(aes(Hostel.Representative.1, n)) +
  geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nHostel Representative Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Hostel Representative Votes by Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Hostel Representative Level.png"))

# Visualization by Department statistics
VotesCast %>%
  left_join(RegisteredVoters, "Matriculation.Number") %>%
  arrange(Department, Level, Matriculation.Number) %>%
  mutate(Valids = Voting.Passkey == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Department, Hostel.Representative.1)%>%
  count() %>%
  ggplot(aes(Hostel.Representative.1, n)) +
  geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
  xlab("\nHostel Representative Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Hostel Representative by Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave(paste0(address, "results\\Hostel Representative Department.png"))

