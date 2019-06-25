#!/bin/bash
CURRENTFILE=$1
REFERENCEFILE=$CI_PROJECT_DIR/CITests/ReferenceFiles/${CURRENTFILE}
if [ ! -f "$CURRENTFILE" ]; then
  echo "$CURRENTFILE not found!"
  exit 1
fi
if [ ! -f "$REFERENCEFILE" ]; then
  echo "$REFERENCEFILE not found!"
  exit 1
fi

grep -v ^-- "${CURRENTFILE}" > "${CURRENTFILE}.clean"
if [ "$(echo "$CURRENTFILE" | grep -c slc6)" -eq 1 ]; then
  mv "${CURRENTFILE}.clean" "${CURRENTFILE}"
  grep -v "may conflict with libexpat.so.0" "${CURRENTFILE}" > "${CURRENTFILE}.clean"
fi
DIFFLINES=$(diff -u "${REFERENCEFILE}" "${CURRENTFILE}.clean")
rm "${CURRENTFILE}.clean"
if [ ! -z "$DIFFLINES" ]; then
  echo "Unexpected warnings detected!"
  echo "$DIFFLINES"
  exit 1
fi
exit 0 
