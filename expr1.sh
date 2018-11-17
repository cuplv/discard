MIX1='([(8,"withdraw"),(15,"deposit")],"current")'
MIX2='([(15,"deposit")],"current")'
MIX4='([(15,"withdraw")],"currentS")'

CLIENT="dist/build/cardr-experiment-client/cardr-experiment-client -c 2remote.yaml --time 20"

for RATE in 200 400 600 800 1000; do
    echo ""
    echo "* MIX 1 at $RATE req/s"
    $CLIENT --mix $MIX1 --rate $RATE
    echo ""
done

for RATE in 200 400 600 800 1000; do
    echo ""
    echo "* MIX 2 at $RATE req/s"
    $CLIENT --mix $MIX2 --rate $RATE
    echo ""
done

# for RATE in 200 400 600 800 1000; do
#     echo ""
#     echo "* MIX 3 at $RATE req/s"
#     $CLIENT --mix $MIX3 --rate $RATE
#     echo ""
# done

for RATE in 200 400 600 800 1000; do
    echo ""
    echo "* MIX 4 at $RATE req/s"
    $CLIENT --mix $MIX4 --rate $RATE
    echo ""
done
