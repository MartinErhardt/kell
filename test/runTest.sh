$1 > bash.stdout 2> bash.errout
bashReturn=$?

./kell $1 > kell.stdout 2> kell.errout
kellReturn=$?

echo "shell: $ShELL"
echo "user:  $(whoami)"

returnCode=0
if test $bashReturn != $kellReturn
then
  echo "exit code bash: $bashReturn\texit code kell: $kellReturn"
  returnCode=3
elif ! cmp -s *.stdout
then
  returnCode=1
elif ! cmp -s *.errout
then
  returnCode=2
else
  returnCode=0
fi
echo $returnCode
if test $returnCode -eq 1
then
  cat *.stdout
elif test $returnCode -eq 2
then
  cat *.errout
fi
rm *out
exit $returnCode
