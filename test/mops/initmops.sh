npm i ic-mops
npx mops init -y
cat packages.txt | while read line
do
   npx mops add $line
done

npx mops remove base
