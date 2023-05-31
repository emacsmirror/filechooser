# filechooser.el: an Emacs based xdg-desktop-portal filechooser. 

filechooser is an Emacs package that enables the use of Emacs to select files in applications like Firefox by providing an `xdg-desktop-portal` backend implementing the [filechooser](https://flatpak.github.io/xdg-desktop-portal/#gdbus-org.freedesktop.impl.portal.FileChooser) interface. It uses minibuffer to select a single file, so the experience is familiar one for commands like `find-file`. To select multiple files Dired is used.

## Limitations
At this point `filechooser` has some serious limitations:
1. It requires a running Emacs instance with server also running. If Emacs or server aren't running no file selection dialogues will appear and no error will be shown. More on this below.
2. If the systemd service `xdg-desktop-portal.service` starts before Emacs, it will be need to be restarted after the server has started to make filechooser work. A possible mitigation is to have

```emacs-lisp
(with-eval-after-load 'server
  (start-process "restart-portal" nil "systemctl" "--user" "restart" "xdg-desktop-portal.service"))
```
in your config file (This likely has a race condition).

3. I don't know a way of reliably positioning Emacs windows so cooperation from compositor/window manager is required. For example, I use sway and I have,
```
for_window [app_id="emacs" title="filechooser-frame"] {
    floating enable
}

for_window [app_id="emacs" title="filechooser-miniframe"] {
    floating enable
}
```
so that the frame filechooser uses appears above tiling windows.

4. The choices combo boxes are not implemented. I don't what would be a good interface for them (suggestions welcome) and I have in my (limited) experience.

5. In my opinion minor compared to these is the fact the filechooser dialogues are not modal, i.e. you can still freely interact with the window that sent the file selection request.

6. Although `OpenFile` and `SaveFile` methods have been tested by me, `SaveFiles` method is completely untested since I don't know a way to trigger it.

## Installation
If despite these limitations you want to test the package:
1. Place `filechooser.el` file in the load path and generate the autoloads file for it.
2. Add your `$XDG_CURRENT_DESKTOP` to the `emacs.portal` file and copy it to `/usr/share/xdg-desktop-portal/portals/`.
3. Copy the `org.gnu.Emacs.FileChooser.service` file `/usr/share/dbus-1/services/`.
4. Restart `xdg-desktop-portal` by using `systemctl --user restart xdg-desktop-portal.service`.

## Usage
Start an application that uses `xdg-desktop-portal`. An example which is my main use case is to launch `GTK_USE_PORTAL=1 firefox` and initiate a file selection dialogue. Hopefully an Emacs frame will appear at this point. If the request is for the selection of a single file, the frame will just have a minibuffer. You can  select the file by the same means you do usually in minibuffer. The only new feature is that you can use different filters to determine which files are presented for completion. Some of these filters are defined by the request, but you can also define your own in `filechooser-filters`. To toggle a filter use `C-f`, this will present all filters grouped according to whether they are active or inactive. Select one to toggle it. Only those files which pass through an active filter are presented for completion.

For a request to select multiple files, the frame contains a dired buffer. To select files navigate to the desired directory. Mark the files to be selected and press `C-c C-c` to finish. `C-c C-k` aborts the selection process. Filters are available in this mode too and can be changed using `C-f`.

## More on limitations
Originally I wanted to write a portal which acted as a global minor mode i.e a user would run `M-x filechooser-mode` and Emacs would take over file selection dialogues. User would run `M-x filechooser-mode` again and Emacs would no longer meddle with these dialogues. However this seems not be possible. [xdg-desktop-portal does not allow user specific portal definitions](https://github.com/flatpak/xdg-desktop-portal/issues/804), as a result to place the `emacs.portal` file in the correct location root access is required. Once that file is placed and `xdg-desktop-portal` restarted, only Emacs can be used for `filechooser` interface. If Emacs doesn't claim the `dbus` service required, no file selection dialogues will appear and no error will be raised either (at least `firefox` fails completely silently) since there is no fallback mechanism to use a working service if the first one couldn't be started.
