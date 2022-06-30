x=1
y=1
#comment
while test $y -le 15
do
    x=$((x*y))
    echo "fac $y = $x" # comment2
    y=$((y+1))
done
#echo "final fac $y $x"
