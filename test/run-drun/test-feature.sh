for i in stable-region-*.mo region0-stable-mem-*.mo stable-mem-*.mo; do
    echo $i;
    if ../run.sh -d $i ; then
	echo `date` $i >> success.log ;
	echo; echo "Success."; echo;
    else
	echo `date` $i >> failures.log ;
	echo; echo "Failure."; echo;
    fi;
done
