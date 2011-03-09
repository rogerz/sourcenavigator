_cntr=0
_maxTimes=1251
echo > badfortran.f
while [ $_cntr -lt $_maxTimes ]; do

	_cntr=$(($_cntr + 1))
	echo "      call Z(1,2,3,4,opsx(AA),opsy(AA))" >> badfortran.f
done
