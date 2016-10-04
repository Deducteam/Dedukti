#The first sed command replace each "#NAME module" by "#ENDMODULE.<NEWLINE>#NEWMODULE module."
#The second sed command add a #NAME at the beginning of the file
#The third sed command remove the first #ENDMOULE introduced before
#The fourth sed command add a #ENDMODULE at the end of the file

cat $@ | sed 's/#NAME \(.*\)\./\n#ENDMODULE\.\n#NEWMODULE \1\./' | sed '1s/.*/#NAME universo\./' | sed '4d' | sed "\$a#ENDMODULE\."
