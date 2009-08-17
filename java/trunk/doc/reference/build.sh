#! /bin/sh
xsltproc --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/chunk.xsl sarasvati-manual.xml
xsltproc --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/onechunk.xsl sarasvati-manual.xml

