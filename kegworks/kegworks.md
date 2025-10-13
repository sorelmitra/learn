# Overview

Kegworks Winery is an extension to [Wine](https://www.winehq.org) for MacOS.  It allows installing a full Wine and then installing and running various libraries and apps into it.


# Quick Start

## Installation, Empty Wrapper, and Default Mac Apps

Install [Kegworks](https://github.com/Kegworks-App/Kegworks).
-> Start Kegworks -> Install new Engine (the `+` button) -> Choose the default engine.
-> Click on Update Wrapper, wait for it to finish.
-> Create New Blank Wrapper -> Name it "Windows" -> Start it -> It should bring up the Kegworks Config showing "C:\nothing.exe" as the "Windows app" startup program.
-> If you're on a display with a high resolution -> Kegworks Config -> Tools -> winecfg -> Graphics -> Adjust DPI
-> Extract `kegworks-mac-apps.zip` in `~/Applications/Kegworks`.
-> Double click on "Notepad.app" in Finder.

Notes:

1. The "empty" wrapper takes 1.5+ Gb of hard disk space, as it includes Wine and supporting libraries.
2. The recommended method of creating a new blank wrapper _for each app_ would take those Gb of hard disk space for each and every app you install, **besides** the app itself.
3. The "New App" method recommended below requires some editing but only takes space for the app itself, leveraging the existing "Windows" app you created above.
4. Do not alter the config for the "Windows" app, as this will be your entry point for installing new software and common configuration.
5. To configure "Windows" settings, launch it, go to Tools -> Config Utility (winecfg).  It will require restarting the Wine processes, which usually involves quitting all "Windows" apps and waiting a bit.

Related reading:
https://www.pcmag.com/how-to/how-to-run-windows-apps-on-your-mac

## New Mac App (`.app`)

Download "MyApp" -- a hypothetical Windows App you want to install.
-> Get a suitable 'icons' file for it, say `MyApp.icns`.
-> IF you have the "Explorer" app, start it and run your Setup
-> ELSE
	-> Start "Windows" -> It should bring up the Kegworks Config -> Install Software -> Choose Setup Executable -> Follow the steps to install "MyApp" -> ⚠️ If Kegworks doesn't wait for the installer to finish -> Ignore the "failed" message.
	-> ⚠️ If Kegworks DOES recognize your installing is running, at the end it will force you to change the "Windows app" startup program -> Make sure to change it back to `"C:\nothing.exe"`.
-> If you already have the Mac App `.app` for it -> Extract or copy it in `~/Applications/Kegworks` and skip to ⭐️.
-> Head to `~/Applications/Kegworks`.
-> Copy "Explorer.app" as "MyApp.app" in this directory.
-> Right click "MyApp" in Finder and choose "Show package contents".
-> Copy `MyApp.icns` to Contents -> Resources.
-> Edit `info.plist` and change the following keys to suit your new app: `CFBundleIconFile`, `CFBundleIdentifier`, `CFBundleName`, `CFBundleDisplayName`, `Program Name and Path`.
-> Add your new app to `~/1data/w/settings/mac/kegworks/kegworks-mac-apps.zip`
-> ⭐️ If your Mac is stubbornly refusing to display your nice icon -> Copy `MyApp.app` -> Delete the original -> Rename the copy.
-> Double click `MyApp.app` in Finder -> Your app should start, assuming it works with Wine.

Notes:

1. If you need to configure your app, run `/Users/sorel/Applications/Kegworks/MyApp.app/Contents/MacOS/KegworksConfig`.
2. If you want to copy your app to a new PC, you need at a minimum to install Kegworks and then copy **both** "Windows" and your new app.
3. Keeping your "apps" in `~/1data/w/settings/mac/kegworks` makes it easy to keep them in sync between two computers.

## Steam

Install as usual, but then follow these steps to make it work:

1. Force downgrade.  Quit all Wine processes then add these arguments in Steam's `Info.plist`: `-forcesteamupdate -forcepackagedownload -overridepackageurl http://web.archive.org/web/20240520if_/media.steampowered.com/client -exitsteam`.  Now run Steam, it will "update" (it will download the downgraded version) then will close itself automatically.

2. Permanent run on the older version.  In `Info.plist` change arguments to `-noverifyfiles -nobootstrapupdate -skipinitialbootstrap -norepairfiles -overridepackageurl`.  Now run again and Steam should be working.

## Troubleshooting

1. My app doesn't start?
	- -> Run it from the terminal, like this: `/Users/sorel/Applications/Kegworks/MyApp.app/Contents/MacOS/Kegworks` and check the output.  Possible issues:
		* "MyApp" does not work with wine.
		* Missing libraries, e.g. .NET.
		* Missing Kegworks files.  When creating the Explorer app I  linked the following into it:
		
				ln -sf /Users/sorel/Applications/Kegworks/Windows.app/Contents/Resources/Scripts /Users/sorel/Applications/Kegworks/Explorer.app/Contents/Resources/
				ln -sf /Users/sorel/Applications/Kegworks/Windows.app/Contents/SharedSupport /Users/sorel/Applications/Kegworks/Explorer.app/Contents/
				ln -sf /Users/sorel/Applications/Kegworks/Windows.app/Contents/SharedSupport/Logs /Users/sorel/Applications/Kegworks/Explorer.app/Contents/
				ln -sf /Users/sorel/Applications/Kegworks/Windows.app/Contents/Frameworks  /Users/sorel/Applications/Kegworks/Explorer.app/Contents/
				ln -sf /Users/sorel/Applications/Kegworks/Windows.app/Contents/SharedSupport/prefix/drive_c /Users/sorel/Applications/Kegworks/Explorer.app/Contents/
		
		  Maybe Kegworks has new items that need to be linked.
		  
		  Note 2: For War2Combat Map Editor I copied the app then deleted and linked these:
		  
			ln -s /Applications/War2Combat.app/Contents/Frameworks /Users/sorel/Applications/Kegworks/War2Combat.app/Contents/
			ln -s /Applications/War2Combat.app/Contents/SharedSupport /Users/sorel/Applications/Kegworks/War2Combat.app/Contents/
			ln -s /Applications/War2Combat.app/Contents/Resources/Base.lproj /Users/sorel/Applications/Kegworks/War2Combat.app/Contents/Resources/
			ln -s /Applications/War2Combat.app/Contents/Resources/en.lproj /Users/sorel/Applications/Kegworks/War2Combat.app/Contents/Resources/
			ln -s /Applications/War2Combat.app/Contents/Resources/Scripts /Users/sorel/Applications/Kegworks/War2Combat.app/Contents/Resources
