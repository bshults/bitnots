cd src\bitnots\parse
echo "Making Bitnots parser files..."
java antlr.Tool bitnotsparse.g

echo "Making TPTP FOF parser files..."
java antlr.Tool fofparse.g

echo "Making TPTP CNF parser files..."
java antlr.Tool cnfparse.g
cd ..\..\..
