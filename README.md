plist-lens
==========

A lens between an OSX plist and a JSON list of records

A wrapper around `/usr/libexec/PlistBuddy` that allows a JSON row projection.

Thinking is 

./plist-lens schema.json get X.plist > output.json
./plist-lens schema.json put X.plist < input.json








