# Refresh IMDB data weekly and signal rateflix-server to reload
0 3 * * 0  rateflix  /usr/lib/rateflix-server/fetch-data && kill -HUP $(cat /run/rateflix-server/rateflix-server.pid 2>/dev/null) 2>/dev/null || true
