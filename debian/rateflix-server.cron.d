# Refresh IMDB data daily by signalling rateflix-server to reload
0 3 * * *  root  kill -HUP $(cat /run/rateflix-server/rateflix-server.pid 2>/dev/null) 2>/dev/null || true
