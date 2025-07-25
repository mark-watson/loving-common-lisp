#cert1 = "Developer ID Application: Mark Watson"
cert1 = "Apple Development: Mark Watson"
#cert1 = "Apple Development: markwatson314@mac.com"
#cert1 = "Mark Watson"

aquaemacs:
	/Applications/Aquamacs.app/Contents/MacOS/bin/aquamacs *.lisp *.asd

build:
	/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build deliver.lisp
	#mkdir -p ~/KnowledgeGraphNavigator
	#rm -r -f ~/KnowledgeGraphNavigator/KnowledgeGraphNavigator.app ~/KnowledgeGraphNavigator/KnowledgeGraphNavigator.pkg
	#mv ~/KnowledgeGraphNavigator.app ~/KnowledgeGraphNavigator
	#cp app.entitlements ~/KnowledgeGraphNavigator
	cd ~/ ; codesign --options runtime -s $(cert1) -v KnowledgeGraphNavigator.app/Contents/PlugIns/libKnowledgeGraphNavigator-0-darwin-lw-objc.dylib
	cd ~/ ; codesign --options runtime -s $(cert1) -v KnowledgeGraphNavigator.app
	#pushd ~/KnowledgeGraphNavigator; codesign --deep --options runtime --entitlements app.entitlements -s $(cert1) -v KnowledgeGraphNavigator.app
	#pushd ~/KnowledgeGraphNavigator; cat KnowledgeGraphNavigator.app/Contents/_CodeSignature/CodeResources
	#pushd ~/KnowledgeGraphNavigator; productbuild --sign "3rd Party Mac Developer Installer: Mark Watson" --component KnowledgeGraphNavigator.app /Applications KnowledgeGraphNavigator.pkg

