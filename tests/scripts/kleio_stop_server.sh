# We quit through the debugging server
sleep 10
echo "wait_for_idle(60)." | nc localhost 4000 >>/dev/null
echo kleio server stopped.
