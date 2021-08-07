# CodeStats Package for Lazarus IDE
Lazarus IDE plugin for Code::Stats service. Collects your programming experience (XP) and sends to codestats.net.

What is Code::Stats?
--------------------

Code::Stats is a free stats tracking service for programmers. You will be awarded with experience points for the amount of programming you do. Watch as your levels grow for each language you use. Identify your strong skill sets and use the data to see where you still have room for improvement. Show your personal statistics page to your friends and compare your progress with others. For more information visit https://codestats.net.

About the functionality
-----------------------

This plugin works by listening to the editor's text changes and periodically sending the amounts of XP to Code::Stats. The programming language is detected by the file extension. Note that at no point is any of your code sent to Code::Stats. Only the amounts of XP (roughly the amount of keystrokes) are sent.

Installing the plugin
---------------------

Using IDE's Online Package Manager:
1. Under Lazarus IDE, go to `Package` → `Online Package Manager` (Ctrl + Alt + O).
2. Search for `CodeStats`.
3. Check the package for installation.
4. Click `Install` → `From repository` and wait for the download.
5. Confirm rebuild Lazarus and wait until IDE restarts.
6. Configure (see [Configuring the plugin](#configuring-the-plugin)).

Installing manually:
1. Open the package file `CodeStats.lpk` in Lazarus IDE (`Package` → `Open package`).
2. Click the `Compile` button.
3. Click `Use` → `Install`.
4. Confirm rebuild Lazarus and wait until IDE restarts.
5. Configure (see below).

Configuring the plugin
----------------------

1. Inside Lazarus go to `Tools` → `Configure Code::Stats`.
2. Check `Enable Code::Stats` option.
3. Inform your [API token](https://codestats.net/my/machines).
4. Click `OK`.
5. Use the IDE normally and your XP will be tracked automatically.
6. Visit https://codestats.net to see your stats.

Note: Only change API URL if you know what you are doing.
