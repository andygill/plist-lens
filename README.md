plist-lens
==========

A lens between an OSX plist and a JSON list of records.

A wrapper around `/usr/libexec/PlistBuddy` that allows a JSON row projections and updates.

Usage is

                 plist-lens schema.json get X.plist | ... json ...
  ... json ... | plist-lens schema.json put X.plist 








