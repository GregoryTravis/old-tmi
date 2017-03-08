python tmi.py

if [ -e recording ]; then
  echo recording/ already exists
  exit
fi
#rm -r recording
mkdir recording
python test-old.py tests/webtest0
diff -qr recording/out tests/webtest0/out
rm -r recording
./reset-db
