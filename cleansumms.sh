#!/bin/bash
#removes numbers from summfile headers so they can be read into R. This script should not be run by itself. sm.read functions automatically call it or sm.cleansumm() can be used

if [ $smpn == 0 ]; then
FILES=$HOME$smfpath"/*"
cd $HOME$smfpath
else
FILES=$smfpath"/*"
cd $smfpath
fi
for f in $FILES
do
if [ ${f:(-5)} == .summ ]; then
sed -i '1 s/[0-9]//g' $f
sed -i '1 s/-//g' $f
sed -i '1 s/)//g' $f
sed -i '1 s/(//g' $f
else
echo "Error: not .summ file"
fi
done
