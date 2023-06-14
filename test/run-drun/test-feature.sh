for i in region0-stable-mem-*.mo stable-mem-*.mo; do echo $i; if ../run.sh -d $i ; then echo; echo yay; echo; else echo `date` $i >> failures.log ; echo; echo "Failure."; echo; fi; done
