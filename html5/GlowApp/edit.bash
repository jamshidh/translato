
set -e

fileName=$1
userAgent=$2

if [ "$fileName" == "" -o "$userAgent" == "" ]
then
    echo "Usage: $0 <filename> <useragent>" 1>&2
    exit -1
fi

parser parse "$1" | translato translate - "$2" | translato reorganize "$2" | parser generate html5 2>&1

