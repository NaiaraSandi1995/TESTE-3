blogdown::install_hugo()
blogdown::install_hugo(force = TRUE)



library(blogdown)
serve_site()
blogdown::build_site()

# usethis::use_git()
# 3
# usethis::use_github()

usethis::use_github()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
usethis::use_github()
# git add .
# git commit -m "Update"
# git push 