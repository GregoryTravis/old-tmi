python tmi.py

if [ -e recording ]; then
  echo recording/ already exists
  exit
fi
#rm -r recording
mkdir recording
python test-old.py tests/webtest0
diff -qr recording/out tests/webtest0/out

if [ $? -eq 0 ]; then
  rm -r recording
else
  diff -r recording/out tests/webtest0/out
fi

./reset-db
