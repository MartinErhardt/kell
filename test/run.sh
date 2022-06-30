$1 > bash.stdout 2> kell.errout
bashReturn = $?

kell $1 > kell.stdout 2> kell.errout
kellReturn = $?

returnCode = 0
if [[ bashReturn != kellReturn]]
then
  returnCode = 3
elif ! cmp -s *.stdout;
then
  returnCode = 1
elif ! cmp -s *.errout;
then
  returnCode = 2
else
  returnCode = 0
fi
rm *out
exit $returnCode

