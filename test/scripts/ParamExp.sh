echo ${var=$(ls)}
echo ${var-directory empty}
echo ${var2=${var3-var3 not found}}
echo ${var3=}
echo ${var3:=not null}

