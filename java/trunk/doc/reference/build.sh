#! /bin/sh
xsltproc --stringparam html.stylesheet ../docbook.css --output html/ --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/chunk.xsl src/sarasvati-manual.xml 
xsltproc --stringparam html.stylesheet ../docbook.css --output html_single/ --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/onechunk.xsl src/sarasvati-manual.xml

