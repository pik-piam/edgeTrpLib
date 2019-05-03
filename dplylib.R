require(git2r)
require(devtools)
cred <- cred_user_pass(username="gitlab+deploy-token-2", password="17g9jJM8s5TJ67LswyHW")
install_git("https://gitlab.pik-potsdam.de/REMIND/edgetrplib.git",
            credentials = cred, dependencies = F)


