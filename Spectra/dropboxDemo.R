install.packages('rdrop2')


devtools::install_github("karthik/rdrop2")
11

library(rdrop2)
drop_auth()
# This will launch your browser and request access to your Dropbox account. You will be prompted to log in if you aren't already logged in.
# Once completed, close your browser window and return to R to complete authentication. 
# The credentials are automatically cached (you can prevent this) for future use.

# If you wish to save the tokens, for local/remote use

token <- drop_auth()
saveRDS(token, file = "c:/temp/test/token.rds")


library(dplyr)
drop_acc() %>% data.frame()
drop_dir('Data Exchange')  %>% data.frame()
