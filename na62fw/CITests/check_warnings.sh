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
mv "${CURRENTFILE}.clean" "${CURRENTFILE}"
uniq "${CURRENTFILE}" > "${CURRENTFILE}.clean"
if [ "$(echo "$CURRENTFILE" | grep -c slc6)" -eq 1 ]; then
  mv "${CURRENTFILE}.clean" "${CURRENTFILE}"
  grep -v "may conflict with libexpat.so.0" "${CURRENTFILE}" > "${CURRENTFILE}.clean"
fi
sed -i -e "s,/builds/[0-9a-zA-Z/-]*NA62FW,,g" "${CURRENTFILE}.clean"
# sort both ref and current files, to allow warning swapping
mv "${REFERENCEFILE}" "${REFERENCEFILE}.unsorted"
sort "${REFERENCEFILE}.unsorted" > "${REFERENCEFILE}"
mv "${CURRENTFILE}.clean" "${CURRENTFILE}.unsorted"
sort "${CURRENTFILE}.unsorted" > "${CURRENTFILE}.clean"
DIFFLINESUNSORTED=$(diff -u "${REFERENCEFILE}.unsorted" "${CURRENTFILE}.unsorted")
DIFFLINES=$(diff -u "${REFERENCEFILE}" "${CURRENTFILE}.clean")
rm "${CURRENTFILE}.clean"
rm "${CURRENTFILE}.unsorted"
rm "${REFERENCEFILE}.unsorted"
if [ ! -z "$DIFFLINES" ]; then
  echo "Unexpected warnings detected!"
  echo "$DIFFLINESUNSORTED"
  exit 1
fi
exit 0 
