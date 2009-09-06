#! /bin/sh
xsltproc --stringparam html.stylesheet ../docbook.css --output html/ --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/chunk.xsl src/sarasvati-manual.xml 
xsltproc --stringparam html.stylesheet ../docbook.css --output html_single/ --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/html/onechunk.xsl src/sarasvati-manual.xml
xsltproc --output pdf/sarasvati-manual.fo --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl src/sarasvati-manual.xml
fop -fo pdf/sarasvati-manual.fo -pdf pdf/sarasvati-manual.pdf
