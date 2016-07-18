ls -l | grep drwx | perl -lnpe 's/^.*\s(\w+$)//'
