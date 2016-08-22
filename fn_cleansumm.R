sm.cleansumm <- function(summpath,wd="~/R_MFAST") {
setwd(wd)
fr <- substr(summpath, 1, 1)
if (fr == "~") {
fdpath <- substring(summpath,2)
Sys.setenv(smpn = 0)
} else {
fdpath <- summpath
Sys.setenv(smpn = 1)
}
Sys.setenv(smfpath = fdpath)
system("bash cleansumms.sh")
Sys.unsetenv("smfpath")
Sys.unsetenv("smpn")
}
