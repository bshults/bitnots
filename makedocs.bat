del /s/f/q doctemp
mkdir doctemp
cd src
javadoc -source 1.5 -private -verbose -author -version -windowtitle "Bitnots" -doctitle "Bitnots" -footer "(C) Benjamin Shults" -header "(C) Benjamin Shults" -d ../doctemp -use -doctitle "Bitnots" -link "http://java.sun.com/j2se/1.5.0/docs/api" -sourcepath . bitnots.expressions bitnots.util bitnots.parse bitnots.tableaux bitnots.gui bitnots.equality bitnots.eventthreads bitnots.theories bitnots.prover
cd ..
cd doctemp
if EXIST index.html <
  cd ..
  del /s/f/q doc
  move /Y doctemp doc
>
pause
