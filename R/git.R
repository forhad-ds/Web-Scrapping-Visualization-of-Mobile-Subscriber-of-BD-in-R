library(git2r)
# Set your user identity
git_user <- person("forhad-ds", "forhad_isrt@outlook.com")

# Open the repository
repo <- repository("https://github.com/forhad-ds/Web-Scrapping-Visualization-of-Mobile-Subscriber-of-BD-in-R")

# Stage changes (replace "file.txt" with the actual file you want to commit)
add(repo, "Image/CMS.png")

# Commit changes
commit_msg <- "up"
commit(repo, author = git_user, committer = git_user, message = commit_msg)