
if [ "$1" == "" ]
then
    echo "Error: You must supply a parameter to edit.bash"
fi

parser parse $1 | xmlstarlet tr /home/jim/GlowApps/html5/addDetailsSummaryEvents.xsl | xmlstarlet tr /home/jim/GlowApps/html5/reorganize.xsl | parser generate html5