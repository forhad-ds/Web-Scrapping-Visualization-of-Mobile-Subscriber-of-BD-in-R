user_mail <- readLines("Cred/user_mail")
user_name <- readLines("Cred/user_name")


system(glue::glue('git config --global user.email "{user_mail}"'))
system(glue::glue('git config --global user.name "{user_name}"'))
system("git pull")
system("git merge")
system(glue::glue('git add {file_name}'))
system(glue::glue('git add README.md'))
system(glue::glue('git commit -m "updated {updated_date}"'))
system("git push -u origin main")
