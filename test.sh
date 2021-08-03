for f in ./good/*.lt
do
     ./latte $f  > /dev/null 2> /dev/null
     if [ $? != 0 ];
     then
          echo -n "Wykryto błąd w programie: "
          echo $f
     fi
done

for f in ./bad/*
do
     ./latte $f > /dev/null 2> /dev/null
     if [ $? != 1 ];
     then
          echo -n "Program zakończył się sukcesem (a nie powinien): "
          echo $f
     fi
done


