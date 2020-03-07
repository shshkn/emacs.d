# Emacs client on macOS

### Creating application

Review and then paste the following code into Script Editor and save it, select "Application" in File Format dropdown.

Change `EMACS_PATH` variable if necessary. Point it to a directory with `emacs` and `emacsclient` binaries.

``` applescript
on prepend_path(cmd)
	set EMACS_PATH to "/usr/local/bin"
	return "PATH=\"" & EMACS_PATH & ":$PATH\" " & cmd
end prepend_path

on run
	open {}
end run

on open input
	set cmd to "emacsclient --create-frame --alternate-editor='' --no-wait"

	if input's length is not 0 then
		set inputString to item 1 of input
		set filePath to POSIX path of inputString
		set cmd to cmd & " \"" & filePath & "\""
	end if

	do shell script prepend_path(cmd)
	focus_frame()
end open

on open location input
	set cmd to "emacsclient --create-frame --alternate-editor='' --frame-parameters='((name . \"org-capture\"))' \"" & input & "\" &>/dev/null &"

	do shell script prepend_path(cmd)
	focus_frame()
end open location

on focus_frame()
	set cmd to "emacsclient --eval '(other-frame 0)'"
	do shell script prepend_path(cmd)
end focus_frame
```

### Handling org-protocol

To register the application as `org-protocol` handler you need to place it in `/Applications` and add the code below to `/Applications/AppNameUnderYouSavedIt.app/Contents/Info.plist`

``` xml
<key>CFBundleURLTypes</key>
<array>
	<dict>
		<key>CFBundleURLName</key>
		<string>org-protocol</string>
		<key>CFBundleURLSchemes</key>
		<array>
			<string>org-protocol</string>
		</array>
	</dict>
</array>
```
