echo
echo $* | python3 -c "import sys; from urllib.parse import quote; print(quote(sys.stdin.read()));"
