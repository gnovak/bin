on run argv
	tell application "Safari"
		open "nonexistantfile"
		set URL of document 1 to item 1 of argv
	end tell
end run